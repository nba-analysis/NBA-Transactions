# Hi Reddit (: 

package.names <- c(
  "rvest",
  "xml2",
  "polite",
  "tidyverse",
  "purrr"
)

check.packages <- function(package) {
  if (!require(package, character.only = T)) {
    install.packages(package)
    library(package, character.only = T)
  } else {
    library(package, character.only = T)
  }
}

lapply(package.names, check.packages)

# Change the path to what you need. See Appendix at the end for scraping code - PLEASE do not overload bballreference
path <- "~/downloads/scraped.transactions.Rdata" 
scraped.list <- load(path)

# Functions to isolate transactions. Note that 
split_transaction <- function(vector, season) {
  list <- strsplit(vector, "\n\n")
  transactions <- map_dfr(list, take_single_transaction) %>% 
    mutate(id = paste0(date,"_", map(as.vector(table(date)[unique(date)]), ~ 1:.) %>% unlist))
  longer <- as.list(transactions$trans) %>% map(~ strsplit(., "; ")) %>% unlist(recursive = F)
  df <- data.frame(
    trans = unlist(longer),
    id = map_dbl(longer, length) %>% rep(transactions$id, times = .)
  ) %>% 
    mutate(date = map_chr(strsplit(id, "_"), 1),
           season = rep(season, length(unlist(longer))))
  return(df)
}

take_single_transaction <- function(vector) {
  vector <- map_chr(vector, ~ gsub(x = ., " \\s*\\([^\\)]+\\)", ""))
  df <- data.frame(
    date = rep(vector[1], length(vector) -1),
    trans = vector[-1]
  ) 
  return(df)
}

seasons <- data.frame(
  season = paste0(c(49:99, 0:22), "-", c(50:99, 0:23)),
  seasonID = 1:74)
trades <- map2_dfr(try, seasons$season, split_transaction) %>% 
  filter(grepl("traded", .$trans)) %>% 
  mutate(
    firsts = map_dbl(trans, ~ str_count(., pattern = "1st round draft pick")),
    seconds = map_dbl(trans, ~ str_count(., pattern = "2nd round draft pick")))
trades <- merge(trades, seasons, by = "season")

by.date <- trades %>% 
  group_by(date) %>% 
  summarise(
    seconds = sum(seconds),
    firsts = sum(firsts),
    season = first(season),
    seasonID = first(seasonID)
  )
by.season <- trades %>% 
  group_by(season) %>% 
  summarise(
    seconds = sum(seconds),
    firsts = sum(firsts),
    season = first(season),
    seasonID = first(seasonID)
  )




# Appendix: PLEASE do not use this because it will overload the servers, but this is how I got the original list that you load in with the .Rdata file
# I include it to show some of my work. There is more text wrangling for dealing with the transactions 

# years <- 1950:2023
# urls <- paste0("https://www.basketball-reference.com/leagues/NBA_", years, "_transactions.html")

#scrape <- function(url) {
  #out <- read_html(url) %>% 
    #html_elements(css = "ul.page_index") %>% 
    #html_elements("li") %>% html_text2()
  #return(out)
#}

#politely.scrape <- polite::politely(scrape)
#data <- map(urls, politely.scrape)