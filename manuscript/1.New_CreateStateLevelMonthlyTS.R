### SIRVERA DATA: LAC yearly classification from 2005 to 2015
## The script takes SIRVERA data (raw SIRVERA data with attempted name corrections),
## subsets for countries and produces consolidated monthly time series of cases
## between Jan 1995 and now by country and by state.

rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(maptools)
library(rgdal) # for writing shape file to Spatial sp object
library(sp) # for handling sp object
library(zoo)
library(dplyr)
library(lubridate)
library(ISOcodes)
source("~/Code/paho_rabies_hf/manuscript/R/adm1_ts.R")

## data
dogs <- read.csv("~/Code/paho_rabies_hf/manuscript/data/SIRVERA_dogs16_ISO_GID1.csv")

## set geographic data
countries <- c(unique(dogs$ADM0_ISO))
adm.level <- 1

## set all dates from Jan 1995 to Dec 2015
current.date <- as.Date("2015-12-01") ## NOTE: this should be automated for each time data is updated
dates <- seq(as.Date("1995-01-01"), current.date, by="months")
dates <- strftime(strptime(dates, format="%Y-%m-%d"),"%Y-%m")

###-------------------------------Subset Cases Data-------------------------------###

## set variable names which is used in writing the output csv 
# not sure what "names" is. See if it is in future scripts. 
names <- countries

for (l in 1:length(countries)){
  ## define country and yrs of interest
  cn = countries[l]
  yr = 1995
  
  ## Create path to country shape files directory
  country.dir <- paste0("data/ShapeFiles_3/", "gadm36_", cn, "_shp")
  country.shp <- paste0("gadm36_", cn, "_", adm.level)
  
  ## check if shape data exists for country[l] at admn level specified. If true, continue.
  if (file.exists(paste0(country.dir, "/", country.shp, ".shp"))) { 
    
    print(paste("Shape data available for", cn))
    
    ## subset country and yrs of interest
    # If there is no data from 1995 or later, the country subset will be empty and it will cause an error. 
    country <- subset(dogs, ADM0_ISO == cn & Ano >= yr)
    yrs <- sort(unique(country$Ano))
    
    if (nrow(country) == 0) { 
      print(" ****** NOTICE *****")
      print(paste("No dates in range for", cn))
      print("-------------------- DONE ----------------------------")
    } else { 
        
      ## read shape file into Spatial object and extract GID_1 codes for states
      sp.country <- rgdal::readOGR(dsn=country.dir, layer=country.shp)
      states <- sp.country@data$GID_1
      
      ## sort out full dates (months included): Converted month 0 to 1 bc do not want to exclude evidence of circulation
      country$Mes[which(country$Mes=="0")] <- 1
      country$date <- as.POSIXct(as.yearmon(paste(country$Ano, country$Mes, sep="-")))
      country$date <- strftime(strptime(country$date, format="%Y-%m-%d"),"%Y-%m")
      
      ## timeseries: sum all cases for each month and year/ state
      country_ts = adm1_ts(dates = dates, states = states, data = country)
      fb <- paste0("output_new/", cn, "_ADM", adm.level, "_monthly_cases.csv")
      write.csv(country_ts,fb,row.names=F)
      
      #print(setdiff(unique(country$UnidMaior),states))
      print("-------------------- DONE ----------------------------")
      
      }
    
  } else {
    print(paste("No shape data available for", cn))
    }
}

