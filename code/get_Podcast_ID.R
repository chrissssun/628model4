setwd("C:/Users/HP/Desktop/Master/628/P4")

library(httr)
library(jsonlite)
library(dplyr)

client_id <- "03112b55c71c4cea865cad56887736ea"
client_secret <- "2b4d7cb803af4ce1b3cf8eeab2ca4235"

get_access_token <- function(client_id, client_secret) {
  res <- POST(
    url = "https://accounts.spotify.com/api/token",
    authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  stop_for_status(res)
  content(res)$access_token
}

access_token <- get_access_token(client_id, client_secret)

search_podcasts <- function(keyword, access_token, limit = 50, offset = 0) {
  url <- paste0(
    "https://api.spotify.com/v1/search?",
    "q=", URLencode(keyword), "&type=show",
    "&limit=", limit, "&offset=", offset
  )
  res <- GET(
    url = url,
    add_headers(Authorization = paste("Bearer", access_token))
  )
  stop_for_status(res)
  content(res, as = "parsed", simplifyDataFrame = TRUE)$shows$items
}

keyword <- "technology" 
search_results <- search_podcasts(keyword, access_token, limit = 50)

podcast_shows <- search_results %>%
  select(
    id,                
    name,              
    description,       
    total_episodes     
  )

write.csv(podcast_shows, "podcast_shows.csv", row.names = FALSE)

print(" podcast_shows.csv ！")
