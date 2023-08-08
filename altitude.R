library(httr)
#
get_elevation <- function(latitude, longitude) {
  base_url <- "https://api.open-elevation.com/api/v1/lookup"
  query_params <- list(locations = paste(latitude, longitude, sep = ","))
  
  tryCatch({
    response <- GET(url = base_url, query = query_params)
    data <- content(response, as = "parsed")
    altitude <- data$results[[1]]$elevation
    return(altitude)
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# It is a Teryan-puskin crossroas's coordinates
latitude <-   40.18201285634474
longitude <- 44.513614322344765


altitude <- get_elevation(latitude, longitude)

if (!is.null(altitude)) {
  cat("The altitude at latitude", latitude, "longitude", longitude, "is", altitude, "meters.\n") 
}



