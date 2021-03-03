### SIRVERA DATA: Mexico & Brazil yearly classification from 2005 to 2015
## The script takes SIRVERA data (raw SIRVERA data with attempted name corrections),
## subsets for Mexico and Brazil and produces consolidated monthly time series of cases
## between Jan 1995 and now by country and by state.

rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(maptools)
library(rgdal)
library(sp)
library(zoo)
library(dplyr)
library(lubridate)
library(ISOcodes)
source("~/Code/paho_rabies_hf/manuscript/R/states_ts.R")

## data
dogs <- read.csv("~/Code/paho_rabies_hf/manuscript/data/SIRVERA_dogs16(clean_statenames).csv")

## edit country names to match ISO 3166-1 names
dogs$Pais[which(dogs$Pais=="Bolivia")] <- "Bolivia, Plurinational State of"
dogs$Pais[which(dogs$Pais=="French Guyana")] <- "French Guiana"
dogs$Pais[which(dogs$Pais=="United States of America")] <- "United States"
dogs$Pais[which(dogs$Pais=="Venezuela")] <- "Venezuela, Bolivarian Republic of"

## set geographic data
countries <- c(unique(dogs$Pais))
adm.level <- "adm1"

## set all dates from Jan 1995 to Dec 2015
current.date <- as.Date("2015-12-01") ## NOTE: this should be automated for each time data is updated
dates <- seq(as.Date("1995-01-01"), current.date, by="months")
dates <- strftime(strptime(dates, format="%Y-%m-%d"),"%Y-%m")

###-------------------------------Subset Cases Data-------------------------------###

## set variable names which is used in writing the output csv 
names <- countries

for (l in 1:length(countries)){
  ## define country and yrs of interest
  cn = countries[l]
  yr = 1995

  ## generate 3 letter country code (ISO 3166-1 Alpha-3)
  ISO.row.cn <- filter(ISO_3166_1, Name == cn)
  alpha.cn <- ISO.row.cn$Alpha_3
  
  ## Create path to country shape files directory
  country.dir <- paste0("data/ShapeFiles_2/", alpha.cn, "_adm_shp")
  country.adm <- paste0(alpha.cn, "_", adm.level)
  
  ## check if shape data exists for country[l] at admn level specified. If true, continue.
  if (file.exists(paste0(country.dir, "/", country.adm, ".shp"))) { 
    
    print(paste("Shape data available for", cn))
    
    ## subset country and yrs of interest
    country <- subset(dogs, Pais == cn & Ano >= yr)
    yrs <- sort(unique(country$Ano))
    
    ## read shape file into Spatial object
    sp.country <- rgdal::readOGR(dsn=country.dir, layer=country.adm)
    states <- sp.country@data$NAME_1
    
    ## sort out full dates (months included): Converted month 0 to 1 bc do not want to exclude evidence of circulation
    country$Mes[which(country$Mes=="0")] <- 1
    country$date <- as.POSIXct(as.yearmon(paste(country$Ano, country$Mes, sep="-")))
    country$date <- strftime(strptime(country$date, format="%Y-%m-%d"),"%Y-%m")
    
    ## timeseries: sum all cases for each month and year/ state
    country_ts = states_ts(dates = dates, states = states, data = country)
    fb <- paste0("output_new/", cn, "_monthly_cases_state_", adm.level, ".csv")
    write.csv(country_ts,fb,row.names=F)
    
    print(setdiff(unique(country$UnidMaior),states))
    print("-------------------- DONE ----------------------------")
    
  } else {
    print(paste("No shape data available for", cn))
    }
}

