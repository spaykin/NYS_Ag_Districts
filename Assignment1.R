library(tmap)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(stargazer)

setwd("~/Desktop/Fall 2019/Spatial Data Science/Assignment 1")

# load NYS County boundary data
# SOURCE: NYS http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=927

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

# transform projections
hv_counties <- st_transform(hv_counties, 32115) #32115 is projection for NY East https://epsg.io/32115
st_crs(hv_counties)

hv_ag_districts <- st_transform(hv_ag_districts, 32115)
st_crs(hv_ag_districts)

# Add variable calculating Area (sq meters)
hv_ag_districts$Area_sqm <- st_area(hv_ag_districts) %>%
  as.numeric(as.character(hv_ag_districts$Area))

# Area - square miles
hv_ag_districts$Area_sqmi <- hv_ag_districts$Area_sqm / 2590000

# Area - acres
hv_ag_districts$Area_acres <- hv_ag_districts$Area_sqmi * 640

# load NYS Farmers Markets data
# SOURCE: https://data.ny.gov/Economic-Development/Farmers-Markets-in-New-York-State/qq4h-8p86/data

markets <- read.csv("Farmers__Markets_in_New_York_State.csv")

# convert to spatial points
market_points <- st_as_sf(markets,
                          coords = c("Longitude", "Latitude"),
                          crs = 32115,
                          agr = "constant")

hv_markets <- market_points %>% filter(County %in% c("Albany", "Greene",
                                                     "Ulster", "Orange", "Westchester",
                                                     "Putnam", "Dutchess",
                                                     "Columbia", "Rensselaer"))


#########################################
##### Point in Polygon calculations #####
#########################################

# MARKETS PiP
# join markets to counties
markets_in_county <- st_join(hv_markets, hv_counties, join = st_within) %>% 
  mutate(NAME = coalesce(County))

# count markets per county
markets_count <- count(as_tibble(markets_in_county), NAME) %>% print()

# AG DISTRICTS PiP
# join ag districts to counties
ag_districts_in_county <- st_join(hv_ag_districts, hv_counties, join = st_within) %>%
  mutate(NAME = coalesce(County))

# count ag districts per county
ag_districts_count <- count(as_tibble(ag_districts_in_county), NAME) %>% print()

# count ag district AREAS (acres and sq mi) per county
ag_districts_area <- ag_districts_in_county %>% group_by(NAME) %>%
  summarize_at(c("Area_acres", "Area_sqmi"), sum, na.rm=TRUE)

st_geometry(ag_districts_area) <- NULL #remove geometry

# population dataset
hv_populations <- data_frame("NAME" = hv_counties$NAME,
                             "POP2010" = hv_counties$POP2010)

# merge data into one table
total_count <- merge(ag_districts_count, ag_districts_area, by = "NAME")
total_count <- merge(total_count, markets_count, by = "NAME")
total_count <- merge(total_count, hv_populations, by = "NAME")

colnames(total_count) <- c("Name", "# Districts", "Districts (Acres)", "Districts (SqMi)", "# Markets", "Population")

table1 <- stargazer(total_count, 
                    summary = FALSE, 
                    type = "text", 
                    font.size = "tiny",
                    title = "Table 1: County Totals")

# CHLOROPLETH MAP for AG DISTRICTS
# merge count data with county shapefile
counties_with_count <- merge(hv_counties, ag_districts_count, by = "NAME")
# rename "n" variable
colnames(counties_with_count)[colnames(counties_with_count) == "n"] <- "# Ag Districts"
# merge area data with county shapefile
counties_with_area <- merge(hv_counties, ag_districts_area, by = "NAME")
# rename "sum" variable
colnames(counties_with_area)[colnames(counties_with_area) == "Area_acres"] <- "Total Acres: Ag Districts"

###################
##### MAPPING #####
###################

# Change POP2010 variable to numeric
counties_with_count$POP2010 <- as.numeric(as.character(counties_with_count$POP2010))
# Change name
colnames(counties_with_count)[colnames(counties_with_count) == "POP2010"] <- "Population"

# Calculate Ag Districts density (sq mi) by population 
counties_with_count$Ag_Density_Pop <- counties_with_area$Area_sqmi / counties_with_count$`Population`
# Change name
colnames(counties_with_count)[colnames(counties_with_count) == "Ag_Density_Pop"] <- "Ag District Density by Population"

# Calculate Ag Districts density(sq mi) by county area
counties_with_count$Ag_Area_Density <- counties_with_area$Area_sqmi / counties_with_count$CALC_SqMi
# Change name
colnames(counties_with_count)[colnames(counties_with_count) == "Ag_Area_Density"] <- "Ag District Density by Area"

# Calculate Ag District (acres) density by population
#counties_with_count$Ag_Pop_Density <- counties_with_area$`Total Acres: Ag Districts` / counties_with_count$`Population (2010)`

# MAP Points map of Ag Districts and Markets

figure_1 <- 
  tm_shape(hv_counties) +
  tm_borders(col = 'gray', alpha = .8) +
  tm_fill(col = 'gray', alpha = .3) +
  tm_text(text = "NAME",
          size = 0.8,
          col = 'black',
          fontface = 1,
          alpha = .9) +
  tm_shape(hv_markets) +
  tm_dots(col = "gold", alpha = .7, size = .07) +
  tm_shape(hv_ag_districts) +
  tm_fill(col = 'brown', alpha = .3) +
  tm_layout(frame = FALSE, 
            legend.show = TRUE)

figure_1

# MAP District and Market points onto Population chloropleth 

figure_2 <- 
  tm_shape(counties_with_count) +
  tm_borders(col = 'gray', alpha = .4) +
  tm_fill(col = "Population", style = "jenks", palette = "Greys") +
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
            legend.width = 1.2,
            frame = FALSE)

figure_2

# CHLOROPLETH MAP for FARMERS MARKETS 

# merge count data with county shapefile
counties_with_count <- merge(counties_with_count, markets_count, by = "NAME")

# rename "n" variable
colnames(counties_with_count)[colnames(counties_with_count) == "n"] <- "# Farmers Markets"

# MAP Number of Farmers Markets by County
figure_3 <- 
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

figure_3



# MAP Ag Acreage Density by Population

districts_pop <- 
  tm_shape(counties_with_count) +
  tm_borders(col = 'gray', alpha = .4) +
  tm_fill(col = "Ag District Density by Population", style = "jenks", palette = "BuPu") +
  tm_text(text = "NAME",
          size = 0.8,
          col = 'black',
          fontface = 1,
          alpha = 0.9) +
  tm_layout(legend.title.size = 1.5,
            legend.title.fontface = 2,
            frame = FALSE)

districts_pop


# MAP Ag Acreage Density by Area
districts_area <- 
  tm_shape(counties_with_count) +
  tm_borders(col = 'gray', alpha = .4) +
  tm_fill(col = "Ag District Density by Area", style = "jenks", palette = "BuPu") +
  tm_text(text = "NAME",
          size = 0.8,
          col = 'black',
          fontface = 1,
          alpha = 0.9) +
  tm_layout(legend.title.size = 1.5,
            legend.title.fontface = 2,
            frame = FALSE)

districts_area

## Arrange side by side

figure_4 <- tmap_arrange(districts_pop, districts_area)

figure_4

# MAP Ag Acreage Density by Population - OLD

# tm_shape(counties_with_count) +
#   tm_borders(col = 'gray', alpha = .4) +
#   tm_fill(col = "Ag Acreage Density by Population", style = "jenks", palette = "BuPu") +
#   tm_text(text = "NAME",
#           size = 0.8,
#           col = 'black',
#           fontface = 1,
#           alpha = 0.9) +
#   tm_layout(legend.title.size = 1.5,
#             legend.title.fontface = 2,
#             frame = FALSE)



# # MAP Number of Ag Districts by County 
# tm_shape(counties_with_count) +
#   tm_borders(col = 'gray', alpha = .4) +
#   tm_fill(col = "# Ag Districts", style = "jenks") +
#   tm_text(text = "NAME",
#           size = 0.8,
#           col = 'black',
#           fontface = 1,
#           alpha = .9) +
#   tm_shape(hv_ag_districts) +
#   tm_fill(col = "black", alpha = .3) +
#   tm_layout(legend.title.size = 1.5,
#             legend.title.fontface = 2,
#             frame = FALSE)
# 
# 
# # MAP Acres of Ag Districts by County
# tm_shape(counties_with_area) +
#   tm_borders(col = 'gray', alpha = .4) +
#   tm_fill(col = "Total Acres: Ag Districts", style = "jenks") +
#   tm_text(text = "NAME",
#           size = 0.8,
#           col = 'black',
#           fontface = 1,
#           alpha = .9) +
#   tm_shape(hv_ag_districts) +
#   tm_fill(col = "black", alpha = .3) +
#   tm_layout(legend.title.size = 1.5,
#             legend.title.fontface = 2,
#             frame = FALSE)





