## fucntion for drawing an appropriate time series

## want to get one nice data structure for the time series
library(reshape2)
library(magrittr)
library(dplyr)

join_series_info = function(input_classifications, input_ts){
  msc = input_classifications
  mts = input_ts
  msc = melt(msc, id.vars = "state")
  colnames(msc) = c("State", "Date", "Classification")
  msc$Date = as.Date(msc$Date, format = "X%Y.%m.%d")
  msc$State = gsub("[.]", " ", msc$State)
  
  mts = melt(mts, id.vars = "date")
  colnames(mts) = c("Date","State","Value")
  mts$State = gsub("[.]"," ", mts$State)
  mts$Date = paste0(mts$Date,"-01")
  mts$Date = as.Date(mts$Date, format = "%Y-%m-%d")
  dat = full_join(mts,msc)
}

msc <- read.csv("output/Mexico_classified_monthly.csv") # monthly state classificiation
mts <- read.csv("output/Mexico_monthly_cases_state_WildRemoved.csv") # monthly state time series
mex = join_series_info(msc,mts)
saveRDS(mex,file = "paho_shiny/Mexico.RData")

msc <- read.csv("output/Brazil_classified_monthly.csv") # monthly state classificiation
mts <- read.csv("output/Brazil_monthly_cases_state.csv") # monthly state time series
bra = join_series_info(msc,mts)
saveRDS(bra, file = "paho_shiny/Brazil.RData")

# x = filter(mex, State == "Chihuahua")
