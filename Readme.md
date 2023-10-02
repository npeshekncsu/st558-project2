ST-558, Project 2
================
Nataliya Peshekhodko
2023-10-01

- <a
  href="#1-vignette-for-reading-and-summarizing-data-from-an-iqair-apis"
  id="toc-1-vignette-for-reading-and-summarizing-data-from-an-iqair-apis">1
  Vignette for reading and summarizing data from an IQAir APIs</a>
  - <a href="#11-overall-goal" id="toc-11-overall-goal">1.1 Overall Goal</a>
  - <a href="#12-packages" id="toc-12-packages">1.2 Packages</a>
  - <a href="#13-functions-for-reading-data-from-iqair-apis"
    id="toc-13-functions-for-reading-data-from-iqair-apis">1.3 Functions for
    reading data from IQAir APIs</a>
    - <a href="#131-get-states-function"
      id="toc-131-get-states-function">1.3.1 Get states function</a>
    - <a href="#132-get-cities-function"
      id="toc-132-get-cities-function">1.3.2 Get cities function</a>
    - <a
      href="#133-get-air-quality-and-weather-metrics-based-on-country-state-and-city"
      id="toc-133-get-air-quality-and-weather-metrics-based-on-country-state-and-city">1.3.3
      Get air quality and weather metrics based on country, state and city</a>
    - <a href="#134-get-air-quality-and-weather-metrics-for-country"
      id="toc-134-get-air-quality-and-weather-metrics-for-country">1.3.4 Get
      air quality and weather metrics for country</a>
  - <a href="#14-exploratory-data-analysis"
    id="toc-14-exploratory-data-analysis">1.4 Exploratory Data Analysis</a>
    - <a href="#141-eda-for-countries" id="toc-141-eda-for-countries">1.4.1
      EDA for countries</a>
    - <a href="#142-eda-for-the-us-states"
      id="toc-142-eda-for-the-us-states">1.4.2 EDA for the US states</a>
  - <a href="#15-summary" id="toc-15-summary">1.5 Summary</a>

# 1 Vignette for reading and summarizing data from an IQAir APIs

## 1.1 Overall Goal

Vignettes are explanations of some concept, package, etc. with text,
code, and output interweaved. Our goal with this project is to create a
vignette about contacting an [IQAir
APIs](https://www.iqair.com/dashboard/api) using custom functions to
query, parse, and return well-structured data. IQAir APIs provide
information about **real time air quality data**.

## 1.2 Packages

Packages required to run code included in that analysis:

``` r
library(httr)
library(dplyr)
library(tibble)
library(jsonlite)
library(ggplot2)
library(RColorBrewer)
```

- `httr` - required for making HTTP requests
- `dplyr` - required for data manipulation
- `tibble` - required for formatting data frame
- `jsonlite` - required for converting JSON data from/to R objects
- `ggplot2` - required for creating graphs
- `RColorBrewer` - provides color schemes for maps

Define variable for API key required for the authentication during HTTP
request:

``` r
key = '97d5c2b8-7656-4974-827e-9d59486f7777'
```

## 1.3 Functions for reading data from IQAir APIs

This section is dedicated for helper functions required for HTTP
requests.

### 1.3.1 Get states function

Function to retrieve `states` based on the `country` name. States
returned as a list.

``` r
get_states <- function(country, api_key = key) {
  url = 'http://api.airvisual.com/v2/states'
  resp = GET(url, query = list(key = api_key, 
                               country = country))
  parsed = fromJSON(rawToChar(resp$content))
  
  return (as.list(parsed$data)$state)
}
```

### 1.3.2 Get cities function

Function to get `cities` based on the provided `country` and `state`.
Cities returned as a list.

``` r
get_cities <- function(country, state, api_key = key) {
  url = 'http://api.airvisual.com/v2/cities'
  
  resp = GET(url, query = list(key = api_key, 
                               country = country,
                               state = state))
  parsed = fromJSON(rawToChar(resp$content))
  return (as.list(parsed$data)$city)
}
```

### 1.3.3 Get air quality and weather metrics based on country, state and city

Function returns air quality based on the `country`, `state` and `city`.
If `city` is not provided, random sample of the size `sample_size` will
be chosen from the cities which belong to the `state`. If
`weather_metrics = TRUE` in addition to air quality metrics, weather
metrics like, temperature, humidity, wind speed and pressure will be
returned.

Functions returns data frame in `tibble` format with following columns:

- `country`
- `state`
- `city`
- `aqius` - AQI value based on US EPA standard
- `aqicn` - AQI value based on China MEP standard

If `weather_metrics = TRUE` in addition to the columns above, function
also returns:

- `temp_cels` - temperature in Celsius
- `humidity` - humidity %
- `wind_speed` - wind speed (m/s)
- `atm_pressure` - atmospheric pressure in hPa

``` r
get_air_quality_per_city <- function(country, 
                                     state, 
                                     city = NULL,
                                     weather_metrics = TRUE,
                                     sample_size = 5,
                                     api_key = key) {
  
  results = tibble()
  url = 'http://api.airvisual.com/v2/city'
 
  if (is.null(city)) {
    cities = get_cities(country, state)
  
    if (length(cities) > sample_size) {
      set.seed(123)
      cities = sample(cities, size = sample_size)
    }
    
    for (city in cities) {
      Sys.sleep(15) # required to avoid error - too many requests
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
      results <- bind_rows(results, subset_df)
  }
  return (results)
}
```

### 1.3.4 Get air quality and weather metrics for country

Function returns air quality and weather metrics based on the country
name. Random sample of the size `sample_size` is thrown from from the
states which belongs to the country. For each state, random city is
selected and metrics returned.

Functions returns data frame in `tibble` format with following columns:

- `country`
- `state`
- `city`
- `aqius` - AQI value based on US EPA standard
- `aqicn` - AQI value based on China MEP standard
- `temp_cels` - temperature in Celsius
- `humidity` - humidity %
- `wind_speed` - wind speed (m/s)
- `atm_pressure` - atmospheric pressure in hPa

``` r
get_air_quality_per_country <- function(country, 
                                        weather_metrics = TRUE,
                                        sample_size = 5,
                                        api_key = key) {
  
  results = tibble()
  url = 'http://api.airvisual.com/v2/city'
  
  
  states = get_states(country, api_key = key)
  
  set.seed(2)
  states = sample(states, size = sample_size)
  
  
  for (state in states) {
    Sys.sleep(15) # required to avoid error - too many requests
    cities = get_cities(country, state, api_key = key)
    set.seed(4)
    city = sample(cities, size = 1)
    
    Sys.sleep(15) # required to avoid error - too many requests
    resp = GET(url, query = list(key = api_key, 
                                 country = country,
                                 state = state,
                                 city = city))
    parsed = fromJSON(rawToChar(resp$content))
    subset_df = tibble(aqius = parsed$data$current$pollution$aqius, 
                       aqicn = parsed$data$current$pollution$aqicn, 
                       city = parsed$data$city,
                       state = parsed$data$state,
                       country = parsed$data$country,
                       temp_cels = parsed$data$current$weather$tp,
                       humidity = parsed$data$current$weather$hu, 
                       wind_speed = parsed$data$current$weather$ws,
                       atm_pressure = parsed$data$current$weather$pr)
    results <- bind_rows(results, subset_df)
  }
  return (results)
}
```

## 1.4 Exploratory Data Analysis

### 1.4.1 EDA for countries

Let’s look at air pollution and weather metrics for different countries
across the globe. Chosen countries: China, USA, Poland, Australia,
Norway.

*Note: Sample size and set of countries chosen to be small due to API
limitations and time required to render the whole document*

``` r
sample = 5
air_usa = get_air_quality_per_country(country = 'USA', sample_size = sample)
air_china = get_air_quality_per_country(country = 'China', sample_size = sample)
air_poland = get_air_quality_per_country(country = 'Poland', sample_size = sample)
air_australia = get_air_quality_per_country(country = 'Australia', sample_size = sample)
air_norway = get_air_quality_per_country(country = 'Norway', sample_size = sample)
```

Combine all obtained data frames together in one data frame.

``` r
all = bind_rows(air_australia, air_poland, air_china, air_usa, air_norway)
```

Let’s look at data frame with combined records:

``` r
all
```

    ## # A tibble: 24 × 9
    ##    aqius aqicn city           state             country   temp_cels humidity wind_speed atm_pressure
    ##    <int> <int> <chr>          <chr>             <chr>         <int>    <int>      <dbl>        <int>
    ##  1    28    31 West Footscray Victoria          Australia        23       33      10.8          1014
    ##  2     8     6 Scottsdale     Tasmania          Australia        16       49       3.55         1016
    ##  3     8     3 Forrest        ACT               Australia        22       53       3.09         1020
    ##  4    19    21 Joondalup      Western Australia Australia        15       62       6.26         1015
    ##  5    21     7 Trzebnica      Lower Silesia     Poland           15       75       0.89         1023
    ##  6    17     6 Goldap         Warmia-Masuria    Poland            8       96       2.54         1021
    ##  7    36    12 Lublin         Lublin            Poland            7       88       2.57         1023
    ##  8    24    10 Wloszczowa     Swietokrzyskie    Poland           10       64       2.04         1024
    ##  9    49    31 Warsaw         Mazovia           Poland           11       81       0            1022
    ## 10    29    17 Yushu          Qinghai           China             9       72       0.98         1012
    ## # ℹ 14 more rows

Now, we need to create categorical variables for `aqius`, `aqicn` and
`humidity`.

If `aqius` value is less than **50**, the air quality is considered to
be `good`. If `aqius` value is greater or equal **50** and less or equal
**100**, the air quality is considered to be `moderate`. If `aquis`
value is greater than **100**, the air quality is considered
`unhealthy`.

If `aqicn` value is less than **50**, the air quality is considered to
be `excellent`, if `aqicn` value is greater or equal **50** and lees or
equal **100**, the air quality is considered to be `good`. If `aqicn`
value is greater than **100**, the air quality is considered `polluted`.

If `humidity` level is less or equal **40%**, it is considered `low`. If
`humidity` level is greater than **40%** and less than or equal to
**60%**, it is considered `normal`. If `humidity` is greater than
**60%**, it is considered `high`.

Creating new categorical variables based on the rules outlined above and
displaying updated data frame.

``` r
all <- all %>%
  mutate(aqius_category = case_when(
    aqius < 50 ~ "good",
    aqius >= 50 & aqius <= 100 ~ "moderate",
    aqius > 100 ~ "unhealthy"
  )) %>%
  mutate(aqicn_category = case_when(
    aqicn < 50 ~ "excellent",
    aqicn >= 50 & aqicn <= 100 ~ "good",
    aqicn > 100 ~ "polluted"
  )) %>%
  mutate(humidity_category = case_when(
    humidity <= 40 ~ 'low',
    humidity > 40 & humidity <= 60 ~ 'normal',
    humidity > 60 ~ 'high'
  ))
 all 
```

    ## # A tibble: 24 × 12
    ##    aqius aqicn city         state country temp_cels humidity wind_speed atm_pressure aqius_category aqicn_category humidity_category
    ##    <int> <int> <chr>        <chr> <chr>       <int>    <int>      <dbl>        <int> <chr>          <chr>          <chr>            
    ##  1    28    31 West Footsc… Vict… Austra…        23       33      10.8          1014 good           excellent      low              
    ##  2     8     6 Scottsdale   Tasm… Austra…        16       49       3.55         1016 good           excellent      normal           
    ##  3     8     3 Forrest      ACT   Austra…        22       53       3.09         1020 good           excellent      normal           
    ##  4    19    21 Joondalup    West… Austra…        15       62       6.26         1015 good           excellent      high             
    ##  5    21     7 Trzebnica    Lowe… Poland         15       75       0.89         1023 good           excellent      high             
    ##  6    17     6 Goldap       Warm… Poland          8       96       2.54         1021 good           excellent      high             
    ##  7    36    12 Lublin       Lubl… Poland          7       88       2.57         1023 good           excellent      high             
    ##  8    24    10 Wloszczowa   Swie… Poland         10       64       2.04         1024 good           excellent      high             
    ##  9    49    31 Warsaw       Mazo… Poland         11       81       0            1022 good           excellent      high             
    ## 10    29    17 Yushu        Qing… China           9       72       0.98         1012 good           excellent      high             
    ## # ℹ 14 more rows

*Note: in the following sections I will be doing EDA and results
interpretation. Given the data, I am retrieving every time I render
document, is real time data, it could be some discrepancies between
values and I my interpretation, but I will try my best to be consistent
with values and interpretation.*

Create 2-way contingency table for `aqius_category` vs `country`.

``` r
table(all$aqius_category, all$country)
```

    ##           
    ##            Australia China Norway Poland USA
    ##   good             4     1      5      5   4
    ##   moderate         0     4      0      0   1

As it could be seen from the table above, `China` has at least one city
with `unhealthy` air based on `aqius` values. All sampled cities for
`Australia` and `Norway` have `good` air quality.

Create 2-way contingency table for `aqicn_category` vs `country`.

``` r
table(all$aqicn_category, all$country)
```

    ##            
    ##             Australia China Norway Poland USA
    ##   excellent         4     3      5      5   5
    ##   good              0     2      0      0   0

Based on the `aqicn` values, all selected cities for all countries,
except `China` have `excellent` air quality.

Create 2-way contingency table for `aqius_category` vs
`humidity_category`.

``` r
table(all$aqius_category, all$humidity_category)
```

    ##           
    ##            high low normal
    ##   good       15   1      3
    ##   moderate    3   1      1

Create 2-way contingency table for `aqicn_category` vs
`humidity_category`.

``` r
table(all$aqicn_category, all$humidity_category)
```

    ##            
    ##             high low normal
    ##   excellent   18   1      3
    ##   good         0   1      1

Based on the two tables above, the most observations for the `excellent`
and `good` air quality are for the `high` humidity level.

Now, let’s group by `country` and calculate `aqius` mean, `aqicn` mean
and standard deviations for each group.

``` r
result <- all %>%
  group_by(country) %>%
  summarize(mean_aqius = mean(aqius), 
            mean_aqicn = mean(aqicn), 
            sd_aquis = sd(aqius),
            sd_aqicn = sd(aqicn)) 
result
```

    ## # A tibble: 5 × 5
    ##   country   mean_aqius mean_aqicn sd_aquis sd_aqicn
    ##   <chr>          <dbl>      <dbl>    <dbl>    <dbl>
    ## 1 Australia       15.8       15.2     9.67    13.1 
    ## 2 China           56.2       36.6    17.0     19.9 
    ## 3 Norway          18.4        8      14.2      6.71
    ## 4 Poland          29.4       13.2    13.0     10.2 
    ## 5 USA             30.2       10.6    19.3      6.50

Based on the values above, `China` has the highest values for `aqius`
and `aqicn`, while `Australia` has the lowest values for `aqius` and
`aqicn`.

Visualizing obtained results for `aqius`.

``` r
ggplot(result, aes(x = country, y = mean_aqius)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  labs(title = "Means of AQIUS per country", x = "Country", y = "Mean of AQIUS")
```

![](README_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

Visualizing obtained results for `aqicn`.

``` r
ggplot(result, aes(x = country, y = mean_aqicn)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.5) +
  labs(title = "Means of AQICN per country", x = "Country", y = "Mean of AQUCN")
```

![](README_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

The graphs above shows means for the `aqius` and `aqicn`. `China` has
the highest values for `aqius` and `aqicn`, while `Australia` has the
lowest values for `aqius` and `aqicn`.

Let’s look at `aqius` distribution for each country. Creating box plot
for `aqius` per `country`.

``` r
ggplot(all, aes(x = country, y = aqius, fill = country)) +
  geom_boxplot() +
  labs(title = "AQIUS values distribution per country", x = "Country", y = "AQIUS") +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-44-1.png)<!-- --> Creating
heatmap for numerical variables

``` r
data <- as.matrix(all %>% select(aqius, aqicn, temp_cels, humidity, wind_speed, atm_pressure))
heatmap(data,
        Rowv=NA,
        Colv=NA, 
        labCol=colnames(data), 
        col= colorRampPalette(brewer.pal(8, "Oranges"))(25), 
        scale="column",
        main="Heatmap for numerical variables")
legend(x="bottomright", legend=c("min", "ave", "max"), 
     fill=colorRampPalette(brewer.pal(8, "Oranges"))(3))
```

![](README_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

Let’s looks at correlation matrix for all numerical variables.

``` r
cor(data)
```

    ##                    aqius      aqicn   temp_cels    humidity  wind_speed atm_pressure
    ## aqius         1.00000000  0.8464929  0.25795412 -0.25343059  0.02431849  -0.02549951
    ## aqicn         0.84649287  1.0000000  0.23699622 -0.55302600  0.30363110  -0.18387626
    ## temp_cels     0.25795412  0.2369962  1.00000000 -0.42664059  0.50490503   0.04942332
    ## humidity     -0.25343059 -0.5530260 -0.42664059  1.00000000 -0.49931872   0.07868689
    ## wind_speed    0.02431849  0.3036311  0.50490503 -0.49931872  1.00000000  -0.12277031
    ## atm_pressure -0.02549951 -0.1838763  0.04942332  0.07868689 -0.12277031   1.00000000

From the matrix above, it could be seen that there are some positive
linear correlation between `aqius` and `atm_pressure`, `aqius` and
`temp_cels`, `aqicn` and `temp_cels`, `aqicn` and `atm_pressure`. There
are some negative linear correlation between `aqius` and `wind_speed`,
`aqicn` and `humidity`, `aqicn` and `wind_speed`.

Let’s check if there are any linear dependency between current
temperature and observed `aquis` values.

``` r
ggplot(all, aes(x = temp_cels, y = aqius)) +
  geom_point() +          
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE) +
  labs(title = "Temperature vs AQIUS",
       x = "Temperature, Celsius",
       y = "AQIUS")  
```

![](README_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

As, it could be seen from the graph above, there are might be some
relationship between temperature and observed `aquis` values. The higher
the temperature, the higher the `aquis` values.

### 1.4.2 EDA for the US states

Now, let’s look at several states across `USA` and compare air quality.
Chosen states: *California*, *Colorado*, *Minnesota*, *Florida* and
*Michigan*. Sample size for each state is equal to `5`.

Reading data for each state using function `get_air_quality_per_city`:

``` r
sample = 5
air_california = get_air_quality_per_city(country = 'USA', state = 'California', sample_size = sample)
air_colorado = get_air_quality_per_city(country = 'USA', state = 'Colorado', sample_size = sample)
air_minnesota = get_air_quality_per_city(country = 'USA', state = 'Minnesota', sample_size = sample)
air_florida = get_air_quality_per_city(country = 'USA', state = 'Florida', sample_size = sample)
air_michigan = get_air_quality_per_city(country = 'USA', state = 'Michigan', sample_size = sample)
```

Combine results in one data frame:

``` r
all_per_state = bind_rows(air_michigan, air_florida, air_minnesota, air_colorado, air_california)
all_per_state
```

    ## # A tibble: 25 × 9
    ##    aqius aqicn city             state    country temp_cels humidity wind_speed atm_pressure
    ##    <int> <int> <chr>            <chr>    <chr>       <int>    <int>      <dbl>        <int>
    ##  1    54    19 Harbor Springs   Michigan USA            17       90       0            1023
    ##  2    39    14 Whitehall        Michigan USA            17       88       1.34         1025
    ##  3    70    30 Milford          Michigan USA            16       93       1.58         1024
    ##  4    60    24 Caro             Michigan USA            17       77       0            1025
    ##  5    49    17 Rogers City      Michigan USA            18       92       2.06         1024
    ##  6     0     0 Lehigh Acres     Florida  USA            24       98       5.66         1013
    ##  7    24     8 Progress Village Florida  USA            25       85       6.69         1016
    ##  8    30    10 Crystal River    Florida  USA            23       84       3.09         1018
    ##  9    15     5 Venice           Florida  USA            25       94       7.2          1015
    ## 10     7     2 Olustee          Florida  USA            20       78       2.06         1020
    ## # ℹ 15 more rows

Now, let’s use the same rules as outlined above, to create categorical
variables `aqius_category`, `aqicn_category` and `humidity_category` and
display updated data frame.

``` r
all_per_state <- all_per_state %>%
  mutate(aqius_category = case_when(
    aqius < 50 ~ "good",
    aqius >= 50 & aqius <= 100 ~ "moderate",
    aqius > 100 ~ "unhealthy"
  )) %>%
  mutate(aqicn_category = case_when(
    aqius < 50 ~ "excellent",
    aqius >= 50 & aqius <= 100 ~ "good",
    aqius > 100 ~ "polluted"
  )) %>%
  mutate(humidity_category = case_when(
    humidity <= 40 ~ 'low',
    humidity > 40 & humidity <= 60 ~ 'normal',
    humidity > 60 ~ 'high'
  ))
all_per_state 
```

    ## # A tibble: 25 × 12
    ##    aqius aqicn city         state country temp_cels humidity wind_speed atm_pressure aqius_category aqicn_category humidity_category
    ##    <int> <int> <chr>        <chr> <chr>       <int>    <int>      <dbl>        <int> <chr>          <chr>          <chr>            
    ##  1    54    19 Harbor Spri… Mich… USA            17       90       0            1023 moderate       good           high             
    ##  2    39    14 Whitehall    Mich… USA            17       88       1.34         1025 good           excellent      high             
    ##  3    70    30 Milford      Mich… USA            16       93       1.58         1024 moderate       good           high             
    ##  4    60    24 Caro         Mich… USA            17       77       0            1025 moderate       good           high             
    ##  5    49    17 Rogers City  Mich… USA            18       92       2.06         1024 good           excellent      high             
    ##  6     0     0 Lehigh Acres Flor… USA            24       98       5.66         1013 good           excellent      high             
    ##  7    24     8 Progress Vi… Flor… USA            25       85       6.69         1016 good           excellent      high             
    ##  8    30    10 Crystal Riv… Flor… USA            23       84       3.09         1018 good           excellent      high             
    ##  9    15     5 Venice       Flor… USA            25       94       7.2          1015 good           excellent      high             
    ## 10     7     2 Olustee      Flor… USA            20       78       2.06         1020 good           excellent      high             
    ## # ℹ 15 more rows

Create 2-way contingency table for `aqius_category` vs `state`.

``` r
table(all_per_state$aqius_category, all_per_state$state)
```

    ##           
    ##            California Colorado Florida Michigan Minnesota
    ##   good              5        5       5        2         3
    ##   moderate          0        0       0        3         2

Based on the table above, Michigan has 4 states out of 5 with `moderate`
air quality. California, Colorado and Florida have all 5 cities with
`good` air quality.

Create 2-way contingency table for `aqicn_category` vs `state`.

``` r
table(all_per_state$aqicn_category, all_per_state$state)
```

    ##            
    ##             California Colorado Florida Michigan Minnesota
    ##   excellent          5        5       5        2         3
    ##   good               0        0       0        3         2

Based on the table above, Michigan has 4 states out of 5 with `good` air
quality.

Create 2-way contingency table for `aqius_category` vs
`humidity_category`.

``` r
table(all_per_state$aqius_category, all_per_state$humidity_category)
```

    ##           
    ##            high low normal
    ##   good       10   4      6
    ##   moderate    5   0      0

Most of the observations with `good` air quality are from observations
with `high` humidity.

Create 2-way contingency table for `aqicn_category` vs
`humidity_category`.

``` r
table(all_per_state$aqicn_category, all_per_state$humidity_category)
```

    ##            
    ##             high low normal
    ##   excellent   10   4      6
    ##   good         5   0      0

Most of the observations with `exellent` air quality are from
observations with `high` humidity.

Let’s looks at correlation matrix for all numerical variables.

``` r
data <- as.matrix(all_per_state %>% select(aqius, aqicn, temp_cels, humidity, wind_speed, atm_pressure))
cor(data)
```

    ##                    aqius      aqicn   temp_cels   humidity wind_speed atm_pressure
    ## aqius         1.00000000  0.9909675 -0.01531437  0.3664107 -0.2699507    0.6690159
    ## aqicn         0.99096753  1.0000000 -0.06479270  0.3678349 -0.3030117    0.6656170
    ## temp_cels    -0.01531437 -0.0647927  1.00000000 -0.1212147  0.7300851   -0.1081371
    ## humidity      0.36641072  0.3678349 -0.12121469  1.0000000 -0.1672169    0.6216370
    ## wind_speed   -0.26995068 -0.3030117  0.73008513 -0.1672169  1.0000000   -0.3233333
    ## atm_pressure  0.66901589  0.6656170 -0.10813710  0.6216370 -0.3233333    1.0000000

From the matrix above, it could be seen that there are some positive
linear correlation between `aqius` and `atm_pressure`, `aqius` and
`temp_cels`, `aqius` and `humidity`, `aqicn` and `temp_cels`, `aqicn`
and `atm_pressure`, `aqicn` and `humidity`. There are some negative
linear correlation between `aqius` and `wind_speed`, `aqicn` and
`wind_speed`.

Now, let’s group by `state` and calculate `aqius` mean, `aqicn` mean and
standard deviations for each group.

``` r
result <- all_per_state %>%
  group_by(state) %>%
  summarize(mean_aqius = mean(aqius), 
            mean_aqicn = mean(aqicn), 
            sd_aquis = sd(aqius),
            sd_aqicn = sd(aqicn)) 
result
```

    ## # A tibble: 5 × 5
    ##   state      mean_aqius mean_aqicn sd_aquis sd_aqicn
    ##   <chr>           <dbl>      <dbl>    <dbl>    <dbl>
    ## 1 California       15.6        5.6    10.2      3.51
    ## 2 Colorado         10.4        3.8     3.58     1.30
    ## 3 Florida          15.2        5      12.2      4.12
    ## 4 Michigan         54.4       20.8    11.6      6.30
    ## 5 Minnesota        46.8       17       9.68     4.80

Let’s check if there are any linear dependency between wind speed and
observed `aquis` values.

``` r
ggplot(all_per_state, aes(x = wind_speed, y = aqius)) +
  geom_point() +                 
  geom_smooth(method = "lm", 
              formula = y ~ x,
              se = TRUE) + 
  labs(title = "Temperature vs AQIUS", 
       x = "Wind speed",
       y = "AQIUS")  
```

![](README_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

As it could be seen from the graph, the stronger the wind, the lower the
`aqius` values.

Let’s check if there are any linear dependency between temperature and
observed `aquis` values.

``` r
ggplot(all_per_state, aes(x = temp_cels, y = aqius)) +
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~ x,
              se = TRUE) + 
  labs(title = "Temperature vs AQIUS", 
       x = "Temperature", 
       y = "AQIUS")  
```

![](README_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

As it could be seen from the graph above, the higher the temperature,
the higher `aqius` values.

Let’s check if there are any linear dependency between humidity level
and observed `aquis` values.

``` r
ggplot(all_per_state, aes(x = humidity, y = aqius)) +
  geom_point() + 
  geom_smooth(method = "lm",
              formula = y ~ x, 
              se = TRUE) + 
  labs(title = "Humidity vs AQIUS", 
       x = "Humidity",
       y = "AQIUS") 
```

![](README_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

As it could be seen from the graph above, there is may be some linear
relationship between humidity levels and `aqius` values. The higher the
humidity, the higher the `aquis` values.

Let’s check if there are any linear dependency between atmospheric
pressure and observed `aquis` values.

``` r
ggplot(all_per_state, aes(x = atm_pressure, y = aqius)) +
  geom_point() + 
  geom_smooth(method = "lm",
              formula = y ~ x, 
              se = TRUE) + 
  labs(title = "Atmospheric pressure vs AQIUS", 
       x = "Atmospheric pressure in hPa",
       y = "AQIUS") 
```

![](README_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

As it could be seen from the graph above, the higher the atmospheric
pressure, the higher the `aqius`.

Let’s look at `aqius` distribution for each `state`. Creating box plot
for `aqius` per `state`.

``` r
ggplot(all_per_state, aes(x = state, y = aqius, fill = state)) +
  geom_boxplot() +
  labs(title = "AQIUS per state", x = "State", y = "AQIUS") +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

## 1.5 Summary

At this project we created functions to query **IQAir APIs** to retrieve
data quality and weather metrics per country, state and city. Due to
free account limitations, small samples were derived for several
countries across the globe and several states across the United States.
For both data frames EDA was conducted. The EDA showed that:

- there are negative linear correlation between wind speed and air
  quality
- air quality is better when humidity level is high
- air quality is deacresing when atmospheric pressure is increasing.
