rm(list=ls())

# Load libraries
library(sf) # reads and writes shapefiles
library(dplyr) # for data processing (e.g. filter)
library(svMisc) # for progress print-out

# Set global options for script
options(stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE)
setwd("~/Code/paho_rabies_hf")

# Load shapefiles
lac_shapefile <- read_sf("manuscript/data/ShapeFiles_2/America_Adm_2/Adm2_AMRO_Sep2013.shp")

# create folder for processed data
dir.create("manuscript/data/ShapeFiles_2/America_Adm_2/processed")

#----- Process and save shapefiles ---------------------------------------------

# Subset shapefile for only Latin American countries
lac_sub <- lac_shapefile %>%
  filter(BIGRegion == "Latin America")

# Get a list of countries
countries <- sort(unique(lac_sub$CNTRY_NAME))

# Loop through countries to subset out shapefiles, then save as output
for(i in 1:length(countries)){

  # Subset for country i
  country_sub <- lac_sub %>%
    filter(CNTRY_NAME == countries[i])

  # Set filepath and create folder if it doesn't already exist
  file_path = paste0("manuscript/data/ShapeFiles_2/America_Adm_2/processed/", gsub(" ", "_", countries[i]))
  ifelse(!dir.exists(file.path(file_path)), dir.create(file.path(file_path)), FALSE)

  # Set filename based on country
  file_name = paste0(gsub(" ", "_", countries[i]), "_ADM2.shp")

  # Save output
  write_sf(country_sub, paste0(file_path, "/", file_name), append=F)

  # Print progress to console
  progress(i, max.value = length(countries))
}

#----- Check a few shapefiles --------------------------------------------------

# Load and view Ecuador
ecuador_shp = lac_shapefile <- read_sf("manuscript/data/ShapeFiles_2/America_Adm_2/processed/Ecuador/Ecuador_ADM2.shp")
plot(ecuador_shp$geometry)

# Load and view Nicaragua
nicaragua_shp = lac_shapefile <- read_sf("manuscript/data/ShapeFiles_2/America_Adm_2/processed/Nicaragua/Nicaragua_ADM2.shp")
plot(nicaragua_shp$geometry)
