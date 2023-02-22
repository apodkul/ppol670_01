### Lab 06 

# Project 1: Aiming to create a choropleth map of German 
# election results of the party list vote for SPD by state 
library(dplyr)

## Step 0: Collect Election Results 
library(rvest)
url <- 'https://en.wikipedia.org/wiki/Results_of_the_2021_German_federal_election'

web_data <- read_html(url)

elec_results <- web_data %>% 
  html_table() %>%
  .[[14]]

## Step 0.5: Data Cleaning 
elec_results %>%
  View()

### Remove Non Data Rows 
names(elec_results) <- elec_results[1,]
elec_results <- elec_results %>% 
  slice(3:18)

### Convert data type to numeric 
elec_results$SPD <- as.numeric(elec_results$SPD)

library(ggplot2)
library(rgeos)
library(maptools)

## Step 1: Load Shape file
#install.packages("rgdal")
de_shp <- rgdal::readOGR(dsn = 'Germany_shapefile', 
                         layer = 'de_admin1')

de_shp %>% 
  glimpse()


## Step 2: Fortify Data Set 
de_shp_f <- ggplot2::fortify(de_shp, region = 'NAME_1')

de_shp_f %>% 
  glimpse()

## Step 2.5: Check the map
library(ggplot2)

ggplot(de_shp_f) + 
  geom_polygon(aes(x = long, y = lat, 
                   group = group))

## Step 3: Join Data Sets
table(de_shp_f$id)
table(elec_results$State)

elec_results <- elec_results %>% 
  mutate(id = case_when(
    State == 'Bavaria' ~ 'Bayern',
    State == 'Hesse' ~ 'Hessen',
    State == 'Lower Saxony' ~ 'Niedersachsen', 
    State == 'North Rhine-Westphalia' ~ 'Nordrhein-Westfalen',
    State == 'Rhineland-Palatinate' ~ 'Rheinland-Pfalz',
    State == 'Saxony' ~ 'Sachsen', 
    State == 'Saxony-Anhalt' ~ 'Sachsen-Anhalt',
    State == 'Thuringia' ~ 'Thüringen',
    TRUE ~ State
  ))

de_shp_f <- de_shp_f %>% 
  left_join(elec_results, by = 'id')

de_shp_f %>% 
  glimpse()

## Step 4: Plot 
ggplot(de_shp_f) + 
  geom_polygon(aes(x = long, y = lat, 
                   group = group, 
                   fill = SPD), 
               color = 'gray')



## Step 5: Adjust 
ggplot(de_shp_f) + 
  geom_polygon(aes(x = long, y = lat, 
                   group = group, 
                   fill = SPD), 
               color = 'gray') + 
  coord_map() + 
  scale_fill_gradient(low = 'white', 
                      high = '#EB001F') + 
  theme_void() + 
  theme(legend.position = 'bottom')


# Project 2: Using sf, let's create a choropleth map of 
# disposable income of private households in Europe (NUTS-2)
library(sf)

#Load data
eu_data <- read.csv('https://github.com/apodkul/ppol670_01/raw/main/Data/eu_disp_income2016.csv')
  
#Load mapping file   
eu_shape <- read_sf('EU/nuts2.shp')

eu_shape %>% 
  left_join(eu_data, by = c('NUTS_ID' = 'geo')) %>%
  ggplot() + 
  geom_sf()

eu_shape %>% 
  left_join(eu_data, by = c('NUTS_ID' = 'geo')) %>%
  ggplot(aes(fill = values, color = values)) + 
  geom_sf() 

eu_crop <- st_crop(eu_shape, 
                   c(xmin = -20, 
                     xmax = 40, 
                     ymin = 30, 
                     ymax = 75))
eu_crop %>% 
  left_join(eu_data, by = c('NUTS_ID' = 'geo')) %>%
  ggplot(aes(fill = values, color = values)) + 
  geom_sf() +
  coord_sf() + 
  theme_void()