Project2
================
Nataliya Peshekhodko
2023-09-28

## Overall Goal

Vignettes are explanations of some concept, package, etc. with text,
code, and output interweaved. Our goal with this project is to create a
vignette about contacting an API using functions you’ve created to
query, parse, and return well-structured data.

### Packages

``` r
library(httr)
library(dplyr)
library(tibble)
library(jsonlite)
```

Function to get states for the provided country.

``` r
get_states <- function(country) {
  url = 'http://api.airvisual.com/v2/states'
  key = '97d5c2b8-7656-4974-827e-9d59486f7777'
  
  resp = GET(url, query = list(key = key, 
                             country = country))
  parsed = fromJSON(rawToChar(resp$content))
  return (as.list(parsed$data))
}
```

Function to get cities for the provided country and state

``` r
get_cities <- function(country, state) {
  url = 'http://api.airvisual.com/v2/cities'
  key = '97d5c2b8-7656-4974-827e-9d59486f7777'
  
  resp = GET(url, query = list(key = key, 
                             country = country,
                             state = state))
  parsed = fromJSON(rawToChar(resp$content))
  return (as.list(parsed$data))
}
```

``` r
get_air_quality_per_city <- function(country, 
                                     state, 
                                     city = NULL,
                                     weather_metrics = TRUE,
                                     sample_size = 5) {
  
  results = tibble()
  url = 'http://api.airvisual.com/v2/city'
  key = '97d5c2b8-7656-4974-827e-9d59486f7777'
  
  if (is.null(city)) {
    cities = get_cities(country, state)
    print(length(cities$city))
  
    if (length(cities$city) > sample_size) {
      print(length(cities))
      set.seed(123)
      cities$city = sample(cities$city, size = sample_size)
    }
    
    for (city in cities$city) {
      Sys.sleep(15)
      resp = GET(url, query = list(key = key, 
                                   country = country,
                                   state = state,
                                   city = city))
      parsed = fromJSON(rawToChar(resp$content))
      
      if (weather_metrics == TRUE) {
        subset_df = tibble(aqius = parsed$data$current$pollution$aqius, 
                           aqicn = parsed$data$current$pollution$aqicn, 
                           city = parsed$data$city,
                           state = parsed$data$state,
                           country = parsed$data$country,
                           temp_cels = parsed$data$current$weather$tp,
                           humidity = parsed$data$current$weather$hu, 
                           wind_speed = parsed$data$current$weather$ws,
                           atm_pressure = parsed$data$current$weather$pr)
      } else {
        subset_df = tibble(aqius = parsed$data$current$pollution$aqius, 
                           aqicn = parsed$data$current$pollution$aqicn, 
                           city = parsed$data$city,
                           state = parsed$data$state,
                           country = parsed$data$country)
      }
      print(subset_df)
      results <- bind_rows(results, subset_df)
    
  }
    } else {
      resp = GET(url, query = list(key = key, 
                                   country = country,
                                   state = state,
                                   city = city))
      parsed = fromJSON(rawToChar(resp$content))
      if (weather_metrics == TRUE) {
        subset_df = tibble(aqius = parsed$data$current$pollution$aqius, 
                           aqicn = parsed$data$current$pollution$aqicn, 
                           city = parsed$data$city,
                           state = parsed$data$state,
                           country = parsed$data$country,
                           temp_cels = parsed$data$current$weather$tp,
                           humidity = parsed$data$current$weather$hu, 
                           wind_speed = parsed$data$current$weather$ws,
                           atm_pressure = parsed$data$current$weather$pr)
      } else {
        subset_df = tibble(aqius = parsed$data$current$pollution$aqius, 
                           aqicn = parsed$data$current$pollution$aqicn, 
                           city = parsed$data$city,
                           state = parsed$data$state,
                           country = parsed$data$country)
      }
      print(subset_df)
      results <- bind_rows(results, subset_df)
  }
  
  #cities = get_cities(country, state)
  #print(length(cities$city))
  
  #if (length(cities$city) > sample_size) {
  #  print(length(cities))
  #  cities$city = sample(cities$city, size = sample_size)
  #}
  
  #for (city in cities$city) {

    #Sys.sleep(15)
    #resp = GET(url, query = list(key = key, 
    #                             country = country,
    #                             state = state,
    #                             city = city))
    #parsed = fromJSON(rawToChar(resp$content))
    #print(parsed)
    
    #subset_df = tibble(aqius = parsed$data$current$pollution$aqius, 
    #               aqicn = parsed$data$current$pollution$aqicn, 
    #               city = parsed$data$city,
    #               state = parsed$data$state,
    #               country = parsed$data$country,
    #               temp_cels = parsed$data$current$weather$tp,
    #               humidity = parsed$data$current$weather$hu, 
    #               wind_speed = parsed$data$current$weather$ws,
    #               atm_pressure = parsed$data$current$weather$pr
    #               )
    #print(subset_df)
    #results <- bind_rows(results, subset_df)

  #}
  return (results)
}

#cities = get_cities('France', 'Brittany')

#states = get_states('France')
#res = get_air_quality_per_city ('France', 'Brittany')
#get_air_quality_per_state('China')
```

``` r
res_usa_cal =  get_air_quality_per_city (country = 'USA', state = 'California', city = 'Diablo', sample_size = 1, weather_metrics = FALSE)
```

    ## # A tibble: 1 × 5
    ##   aqius aqicn city   state      country
    ##   <int> <int> <chr>  <chr>      <chr>  
    ## 1    16     6 Diablo California USA

``` r
res_usa_cal
```

    ## # A tibble: 1 × 5
    ##   aqius aqicn city   state      country
    ##   <int> <int> <chr>  <chr>      <chr>  
    ## 1    16     6 Diablo California USA
