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
  
  # if state is NULL or "Both/No Selection", default to NC and VA
  if (is.null(state) || state == "Both/No Selection") {
    state <- c("North Carolina", "Virginia")
  }
  
  # turn state names into API usable strings
  state <- tolower(gsub(" ", "_", state))
  
  
  # function that supports ONE state at a time (results to be combined later with multiple using map_df)
  one_state_at_a_time <- function(single_state) {
    query <- list(
      by_state = single_state,
      per_page = per_page,
      page = page
    )
    
    # if city is not empty (one state selected), the selected cities is put in the query list
    if (!is.null(city)) {
      query$by_city <- tolower(gsub(" ", "_", city))
    }
    
    # if type is not empty, the same thing happens
    if (!is.null(type)) {
      query$brewery_type <- type
    }
    
    # use GET() and parse it and make it a tibble
    response <- GET(url = base_url, query = query)
    parsed <- fromJSON(rawToChar(response$content))
    data <- as_tibble(parsed)
    
    # west virginia has been sliding through to the app, so filter that out explicitly here
    data <- filter(data, state %in% c("Virginia", "North Carolina"))
    
    return(data)
  }
  
  # loop over all states (queried individually) and bind the results
  results <- map_df(state, one_state_at_a_time)
  
  return(results)
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
