library(httr)
library(jsonlite)
library(tibble)
library(dplyr)

# to use in the app
city_options_nc <- c("Durham", "Raleigh")
city_options_va <- c("Fredericksburg", "Richmond", "Virginia Beach")


# function used to query the API
get_breweries <- function(state = NULL, city = NULL, type = NULL, per_page = 50, page = 1) {
  
  base_url <- "https://api.openbrewerydb.org/v1/breweries"
  
  # start blank (GPP)
  by_state <- NULL
  by_city <- NULL
  brewery_type <- NULL
  
  # takes the string, makes spaces underscores and all the letters lowercase (to use in the API)
  if (!is.null(state)) {
    by_state <- tolower(gsub(" ", "_", state))
  }
  
  # same for cities
  if (!is.null(city)){
    by_city <- tolower(gsub(" ", "_", city))
  }
  
  # make the query list outside of GET()
  query <- list(
    by_state = by_state,
    by_city = by_city,
    by_type = type,
    per_page = per_page,
    page = page
  )
  
  # remove NULLS (so they are not in the API and will not cause issues)
  query <- query[!sapply(query, is.null)]
  
  # GET(), parse, and return this tibble
  response <- GET(url = base_url, query = query)
  parsed <- fromJSON(rawToChar(response$content))
  
  
  return(as_tibble(parsed))
}

# to help with plotting on the app
plot_breweries <- function(data) {
  ggplot(data, aes(x = brewery_type)) + geom_bar()
}

# potential to use for later
summarize_breweries <- function(data) {
  data %>%
    group_by(brewery_type) %>%
    summarise(count = n())
}
