#### Sports Map Code


###########################################################
# Loading Packages
###########################################################
library(readxl)
library(zipcodeR)
library(leaflet)
library(tidyverse)


###########################################################
# Loading in Data and Merging Datasets
###########################################################
raw_data <- read_excel('~/desktop/projects/sport_location_raw_tab.xlsx', sheet = 1)
raw_data[120, 6] <- '89109' # fixing Vegas Knights zip code

# Join with zip package to get lat/lng 
merged_1data <- raw_data %>% 
    left_join(.,
              geocode_zip(raw_data$zip) %>% 
                  mutate(zipcode = as.character(zipcode)), 
              by = c("zip" = "zipcode"))

# Merge Canadian codes with Canadian csv file
cc <- (read_csv("~/desktop/projects/CanadianPostalCodes202208.csv")
       %>% select(POSTAL_CODE, LATITUDE, LONGITUDE)
       %>% rename(zip="POSTAL_CODE", lat="LATITUDE", lng="LONGITUDE"))

merged_2data <- merged_1data %>%
    left_join(cc, by = 'zip') %>%
    mutate(lat.x = coalesce(lat.x, lat.y),
           lng.x = coalesce(lng.x, lng.y)) %>%
    rename('lat' = lat.x, 'lng' = lng.x) %>%
    select(-lat.y, -lng.y)
# Need to manually add in Mexico lat/lng. Only 1 team there


# Next steps: Get image links to post and customize how the map looks in code below
# After: Move to next sport







###########################################################
# Creating Map
###########################################################
merged_2data %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~ lng, ~ lat, popup = ~ as.character(team_name))






