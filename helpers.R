library(httr)
library(jsonlite)
library(tibble)
library(dplyr)

get_breweries <- function(state = NULL, city = NULL, type = NULL, per_page = 50, page = 1) {
  
  base_url <- "https://api.openbrewerydb.org/v1/breweries"
  
  # Initialize all query parameters
  by_state <- NULL
  by_city <- NULL
  brewery_type <- NULL
  
  if (!is.null(state)) {
    if (state == "North Carolina") {
      by_state <- "north_carolina"
      if (!is.null(city)) {
        if (city == "Durham") by_city <- "durham"
        else if (city == "Raleigh") by_city <- "raleigh"
      }
    } else if (state == "Virginia") {
      by_state <- "virginia"
      if (!is.null(city)) {
        if (city == "Virginia Beach") by_city <- "virginia_beach"
        else if (city == "Richmond") by_city <- "richmond"
        else if (city == "Fredericksburg") by_city <- "fredericksburg"
      }
    }
  }
  
  query <- list(
    by_state = by_state,
    by_city = by_city,
    by_type = type,
    per_page = per_page,
    page = page
  )
  
  # Remove NULLs from query list
  query <- query[!sapply(query, is.null)]
  
  # Make GET request
  response <- GET(url = base_url, query = query)
  
  if (status_code(response) != 200) {
    stop("API request failed with status code: ", status_code(response))
  }
  
  parsed <- fromJSON(rawToChar(response$content))
  
  #data <- content(response, as = "text", encoding = "UTF-8")
  #parsed <- fromJSON(data, flatten = TRUE)
  
  return(as_tibble(parsed))
}

# wrapper function for querying the API

get_data <- function(state, city, type) {
  get_breweries(state = state, city = city, type = type)
}


plot_breweries <- function(data) {
  ggplot(data, aes(x = brewery_type)) + geom_bar()
}

summarize_breweries <- function(data) {
  data %>%
    group_by(brewery_type) %>%
    summarise(count = n())
}
