library(dtplyr) 
library(dplyr)
library(data.table)
library(leaflet)
library(tidyverse)
library(lubridate)

### Load data from Public Safety Data Portal
# Shooting & Firearm Discharges
shooting <-
  read_csv('https://raw.githubusercontent.com/Chan-Y/JSC370_Final_Project/main/docs/resources/data/Shootings_and_Firearm_Discharges.csv',
           na = c('', 'NULL'))
# Rename column names
names(shooting) <- tolower(names(shooting))

# Break and Enter
break_enter <-
  read_csv('https://raw.githubusercontent.com/Chan-Y/JSC370_Final_Project/main/docs/resources/data/Break_and_Enter.csv',
           na = c('', 'NULL'))
names(break_enter) <- tolower(names(break_enter))

# Auto Theft
auto_theft <-
  read_csv('https://raw.githubusercontent.com/Chan-Y/JSC370_Final_Project/main/docs/resources/data/Auto_Theft.csv',
           na = c('', 'NULL'))
names(auto_theft) <- tolower(names(auto_theft))

# Robbery
robbery <-
  read_csv('https://raw.githubusercontent.com/Chan-Y/JSC370_Final_Project/main/docs/resources/data/Robbery.csv',
           na = c('', 'NULL'))
names(robbery) <- tolower(names(robbery))


### Keep necessary columns, and standardize column names
shooting <- shooting %>% 
  select(
    event_unique_id,
    occurrence_date,
    occurrence_year,
    month,
    day_of_week,
    division,
    hood_id,
    neighbourhood,
    longitude,
    latitude
  ) %>% 
  rename(
    occurrencedate = occurrence_date,
    occurrenceyear = occurrence_year,
    occurrencemonth = month,
    occurrencedayofweek = day_of_week
  ) %>% 
  mutate(
    crime_cat = 'Shooting & Firearm Discharge',
    neighbourhood = gsub(" \\(\\d+\\)", "", neighbourhood)
  )

break_enter <- break_enter %>% 
  select(
    event_unique_id,
    occurrencedate,
    occurrenceyear,
    occurrencemonth,
    occurrenceday,
    occurrencedayofweek,
    division,
    hood_id,
    neighbourhood,
    long,
    lat,
    location_type,
    premises_type
  ) %>% 
  rename(
    longitude = long, 
    latitude = lat
  ) %>% 
  mutate(
    crime_cat = 'Break & Enter'
  )

auto_theft <- auto_theft %>% 
  select(
    event_unique_id,
    occurrencedate,
    occurrenceyear,
    occurrencemonth,
    occurrenceday,
    occurrencedayofweek,
    division,
    hood_id,
    neighbourhood,
    longitude,
    latitude,
    location_type,
    premises_type,
    offence
  )  %>% 
  mutate(
    crime_cat = 'Auto Theft'
  )

robbery <- robbery %>% 
  select(
    event_unique_id,
    occurrencedate,
    occurrenceyear,
    occurrencemonth,
    occurrenceday,
    occurrencedayofweek,
    division,
    hood_id,
    neighbourhood,
    longitude,
    latitude,
    location_type,
    premises_type,
    offence
  ) %>% 
  mutate(
    crime_cat = 'Robbery'
  )


### Combine crime data together
crime_data <- bind_rows(shooting, break_enter, auto_theft, robbery)


### Basic EDA to clean data
# Rename columns
crime_data <- crime_data %>% 
  rename(
    date = occurrencedate,
    year = occurrenceyear,
    month = occurrencemonth,
    day = occurrenceday,
    dayofweek = occurrencedayofweek
  )

# Convert month and day of week columns to numbers
crime_data$month_num <- 
  as.integer(factor(crime_data$month, levels = month.name))

crime_data$dayofweek_num <- 
  as.integer(factor(crime_data$dayofweek, 
                    levels = c("Monday","Tuesday","Wednesday","Thursday","Friday",
                               "Saturday","Sunday"), 
                    ordered=TRUE))

# Set 0 latitude/longitude/year/day to NA
crime_data <- crime_data %>%
  mutate(
    latitude = ifelse(latitude == 0, NA, latitude),
    longitude = ifelse(longitude == 0, NA, longitude),
    year = ifelse(year == 0, NA, year),
    day = ifelse(day == 0, NA, day),
  )

# Convert date to Date type, then use it to fill NAs 
#   for columns year/month/month_num/dayofweek/day 
crime_data <- crime_data %>% 
  mutate(
    date = as.POSIXct(date)
  ) %>% 
  mutate(
    year = ifelse(is.na(year), year(date), year),
    month = ifelse(is.na(month), format(date,"%B"), month),
    month_num = ifelse(is.na(month_num), month(date), month_num),
    day = ifelse(is.na(day), day(date), day),
    dayofweek_num = ifelse(is.na(dayofweek_num), 
                           wday(date, week_start = 1), 
                           dayofweek_num)
  )

# Handle outliers
crime_data <- crime_data %>% 
  filter(
    year >= 2014,
    year <= 2021,
    longitude <= -79,
    longitude >=
      quantile(longitude, .25, na.rm=TRUE) - IQR(longitude, na.rm=TRUE)*1.5,
    latitude <= 43.85,
    latitude >=
      quantile(latitude, .25, na.rm=TRUE) - IQR(latitude, na.rm=TRUE)*1.5
  )


### Load in data for inflation and unemployment rate 
# Inflation
inflation <-
  read_csv('https://raw.githubusercontent.com/Chan-Y/JSC370_Final_Project/main/docs/resources/data/Inflation_(%25).csv')
# Rename column names
names(inflation) <- c('month', 'rate')

# Unemployment 
unemployment <-
  read_csv('https://raw.githubusercontent.com/Chan-Y/JSC370_Final_Project/main/docs/resources/data/Unemployment_Rate_(%25).csv')
# Rename column names
names(unemployment) <- c('month', 'rate')

### Merge rates in one dataframe
rate_data <- merge(
  inflation, unemployment, 
  by.x = c('month'), 
  by.y = c('month'), 
  all.x = FALSE, 
  all.y = FALSE
) %>% 
  rename(inflation = rate.x, unemployment = rate.y)

rate_data <- rate_data %>% 
  mutate(
    month_date = month,
    year = year(month),
    month = month(month)
  ) 



### Load neighbourhoods data
# library(sf)
# neighbor_data <- st_read("resources/data/Neighbourhoods.geojson")

library(opendatatoronto)

# Neighbourhoods package
# https://open.toronto.ca/dataset/neighbourhoods/
resources <- list_package_resources("neighbourhoods")
# identify datastore resources; 
# by default, Toronto Open Data sets datastore resource format to CSV 
#   for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
# load the first datastore resource as a sample
neighbour_data <- filter(datastore_resources, row_number()==2) %>% get_resource()
# Add hood_id column to merge with crime_data
neighbour_data$hood_id <- as.integer(neighbour_data$AREA_SHORT_CODE)


# # Neighbourhood Profiles 2016 package
# # https://open.toronto.ca/dataset/neighbourhood-profiles/
# resources <- list_package_resources("6e19a90f-971c-46b3-852c-0c48c436d1fc")
# # identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
# datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
# # load the first datastore resource as a sample
# neighbour_profile_data <- filter(datastore_resources, row_number()==1) %>% get_resource()


# Toronto - Demographics package, only need 2016 population
# https://open.toronto.ca/dataset/wellbeing-toronto-demographics/
resources <- list_package_resources("15a37448-b27e-44d2-9c2e-52e148c8894f")
population_data <- filter(resources, row_number()==1) %>% get_resource()
population_data <- population_data$`016 Pop_TotalChange`


population_data <- population_data %>% 
  select(
    HoodID, Neighbourhood, Pop2016
  ) %>% 
  rename(
    hood_id = HoodID, 
    neighbourhood = Neighbourhood, 
    pop_2016 = Pop2016
  ) %>% 
  filter(!is.na(hood_id)) 