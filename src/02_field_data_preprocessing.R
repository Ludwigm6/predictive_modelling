# This script loads and preprocesses field data about vegetation cover on Fogo Island

library(tidyverse)
library(sf)

df = read.csv("data/field_data/fogo_plots.csv")
summary(df)


df = df %>% dplyr::select(PLOT, LAT, LONG, ELEVgps, Date, vegtype_1, vegtype_2,
                   Locality.Relief, EXP, INCL, Tree_cover, Tree_height, Shrub_cover, Shrub_height, Herb_cover, Herb_height,
                   Soil, Rock, Dead_Wood, Litter)
df = na.omit(df)
df = df %>% dplyr::mutate(Vegetation_Cover = Tree_cover + Shrub_cover + Herb_cover)

dfsp = sf::st_as_sf(df, coords = c("LONG", "LAT"), crs = 4326)
sf::st_write(dfsp, "data/field_data/fogo_plots.gpkg")


