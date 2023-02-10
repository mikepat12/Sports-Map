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
icons <- makeIcon(
    iconUrl = merged_2data$link2,
    iconWidth = 31 * 215/230, iconHeight = 31,
    iconAnchorX = 31 * 215/230/2, iconAnchorY = 16
)

merged_2data %>%
    filter(league == 'NFL') %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~ lng, ~ lat, popup = ~ team_name, icon = icons)


# Use test to try other image types. Popup format only works for unfiltered dataset
test <- merged_2data %>%
    filter(merged_2data$league == 'NFL')
merged_2data %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~ lng, ~ lat, icon = icons, 
               popup = paste(merged_2data$team_name, '<br>', merged_2data$stadium, '<br>')) 





