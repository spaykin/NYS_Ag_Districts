library(tmap)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(rgeos)

setwd("~/Desktop/Fall 2019/Spatial Data Science/Assignment 1")

# load NYS County boundary data
# SOURCE: NYS http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=927

ny_counties <- readOGR("NY Counties/cty036.shp")

counties <- readOGR("NYS_Civil_Boundaries_SHP/Counties.shp")

counties <- st_as_sf(counties)

hv_counties <- counties %>% filter(NAME %in% c("Albany", "Greene",
                                                       "Ulster", "Orange", "Westchester",
                                                       "Putnam", "Dutchess",
                                                       "Columbia", "Rensselaer"))

# load NYS Ag District data
# SOURCE: https://cugir.library.cornell.edu/catalog/cugir-009010

ag_districts <- readOGR("NYS Ag Districts/nyAg2019.shp")
ag_districts <- st_as_sf(ag_districts)

hv_ag_districts <- ag_districts %>% filter(County %in% c("Albany", "Greene",
                                                         "Ulster", "Orange", "Westchester",
                                                         "Putnam", "Dutchess",
                                                         "Columbia", "Rensselaer"))

# load NYS Farmers Markets data
# SOURCE: https://data.ny.gov/Economic-Development/Farmers-Markets-in-New-York-State/qq4h-8p86/data

markets <- read.csv("Farmers__Markets_in_New_York_State.csv")

# convert to spatial points
market_points <- st_as_sf(markets,
                          coords = c("Longitude", "Latitude"),
                          crs = 4326,
                          agr = "constant")

hv_markets <- market_points %>% filter(County %in% c("Albany", "Greene",
                                                     "Ulster", "Orange", "Westchester",
                                                     "Putnam", "Dutchess",
                                                     "Columbia", "Rensselaer"))

# transform projection
hv_counties <- st_transform(hv_counties, 4326)
st_crs(hv_counties)

hv_ag_districts <- st_transform(hv_ag_districts, 4326)
st_crs(hv_ag_districts)

#########################################
##### Point in Polygon calculations #####
#########################################

# MARKETS PiP
# join markets to counties
markets_in_county <- st_join(hv_markets, hv_counties, join = st_within)

# count markets per county
markets_count <- count(as_tibble(markets_in_county), NAME) %>% print()

# AG DISTRICTS PiP
# join ag districts to counties
ag_districts_in_county <- st_join(hv_ag_districts, hv_counties, join = st_within)

# fill in missing values in Name
ag_districts_in_county <- ag_districts_in_county %>% mutate(NAME = coalesce(County))

# count ag districts per county
ag_districts_count <- count(as_tibble(ag_districts_in_county), NAME) %>% print()

###################
##### MAPPING #####
###################

# Simple points map of Ag Districts and Markets

tm_shape(hv_counties) +
  tm_borders(col = 'gray', alpha = .8) +
  tm_text(text = "NAME",
          size = 0.8,
          col = 'black',
          fontface = 1,
          alpha = .9) +
  tm_shape(hv_ag_districts) +
  tm_fill(col = 'brown', alpha = .3) +
  tm_shape(hv_markets) +
  tm_dots(col = "gold", alpha = .6, size = .04) +
  tm_layout(title = "Agriculture Districts & Farmers Markets \nHudson Valley, NY",
            title.size = 1.5,
            title.position = c("left", "TOP"),
            title.fontface = 1,
            frame = FALSE, 
            legend.show = TRUE)


# CHLOROPLETH MAP for AG DISTRICTS

# merge count data with county shapefile
counties_with_count <- merge(hv_counties, ag_districts_count, by = "NAME")

# rename "n" variable
colnames(counties_with_count)[colnames(counties_with_count) == "n"] <- "# Ag Districts"

# MAP Number of Ag Districts by County
tm_shape(counties_with_count) +
  tm_borders(col = 'gray', alpha = .4) +
  tm_fill(col = "# Ag Districts", style = "jenks") +
  tm_text(text = "NAME",
          size = 0.8,
          col = 'black',
          fontface = 1,
          alpha = .9) +
  tm_shape(hv_ag_districts) +
  tm_fill(col = "black", alpha = .3) +
  tm_layout(legend.title.size = 1,
            legend.title.fontface = 2,
            frame = FALSE)

# CHLOROPLETH MAP for FARMERS MARKETS 

# merge count data with county shapefile
counties_with_count <- merge(counties_with_count, markets_count, by = "NAME")

# rename "n" variable
colnames(counties_with_count)[colnames(counties_with_count) == "n"] <- "# Farmers Markets"

# MAP Number of Farmers Markets by County
tm_shape(counties_with_count) +
  tm_borders(col = 'gray', alpha = .4) +
  tm_fill(col = "# Farmers Markets", style = "jenks", palette = "Blues") +
  tm_text(text = "NAME",
          size = 0.8,
          col = 'black',
          fontface = 1,
          alpha = 0.9) +
  tm_shape(hv_markets) +
  tm_dots(col = "black", alpha = .3, size = .04) +
  tm_layout(legend.show = TRUE,
            legend.title.size = 1,
            legend.title.fontface = 2,
            frame = FALSE)


# CHLOROPLETH MAP for POPULATION

# Change POP2010 variable to numeric
counties_with_count$POP2010 <- as.numeric(as.character(counties_with_count$POP2010))

# Change name
colnames(counties_with_count)[colnames(counties_with_count) == "POP2010"] <- "Population (2010)"

# MAP District and Market points onto Population chloropleth 

tm_shape(counties_with_count) +
  tm_borders(col = 'gray', alpha = .4) +
  tm_fill(col = "Population (2010)", style = "jenks", palette = "Greys") +
  tm_text(text = "NAME",
          size = 0.8,
          col = 'black',
          fontface = 1,
          alpha = .9) +
  tm_shape(hv_ag_districts) +
  tm_fill(col = "brown", alpha = .3) +
  tm_shape(hv_markets) +
  tm_dots(col = "gold", alpha = .5, size = .04) +
  tm_layout(legend.show = TRUE,
            legend.title.size = .8,
            legend.position = c("LEFT", "TOP"),
            legend.text.size = 0.8,
            legend.title.fontface = 2,
            frame = FALSE)

