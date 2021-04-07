### SIRVERA DATA: Mexico & Brazil - preparing monthly case data by state
## This script takes SIRVERA data (raw SIRVERA data with attempted name corrections), 
## subsets for Mexico and Brazil and produces
## a) consolidated monthly time series of cases between Jan 2005 and Dec 2015 by country and by state,
## b) barplots of data completness at state and municiplaity level.

rm(list=ls())
#setwd("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis")
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(maptools)
library(zoo)
library(lubridate)
source("R/states_ts.r")

## data
dogs <- read.csv("data/SIRVERA_dogs16(clean_statenames).csv")
countries <- c("Mexico", "Brazil")

## set all dates from Jan 2005 to Dec 2015 
dates <- seq(as.Date("2005-01-01"), as.Date("2015-12-01"), by="months")
dates <- strftime(strptime(dates, format="%Y-%m-%d"),"%Y-%m")

###-------------------------------Mexico and Brazil-------------------------------###
names <- countries 

for (l in 1:length(countries)){
  ## subset country and yrs of interest
  cn = countries[l] #country of interest
  
  ## directories setting 
  mainDir <- "C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis/ClassFrameMS"
  subDirOut <- paste0("output/",cn)
  subDirFig <- paste0("figs/",cn)
  dir.create(path=file.path(mainDir, subDirOut), recursive=T)
  dir.create(path=file.path(mainDir, subDirFig), recursive=T)
  
  unique(dogs$Ano) #1970-2015 -> subset to 2005-2015
  yr = 2005

  country <- subset(dogs, Pais == cn & Ano >= yr)
  yrs <- sort(unique(country$Ano))
  
  country.f <- paste0("data/America_Adm_1/", cn, ".shp")
  sp.country <- readShapePoly(country.f)
  states <- sp.country@data$ADM1_NAME  
  
  ## completeness of information at administrative units
  units1 = table(country$Ano, country$UnidMaior, useNA = "ifany")
  units2 = table(country$Ano, country$UnidMenor, useNA = "ifany")
  
  no_info1 = units1[,which(dimnames(units1)[[2]] == "Sin Informacion")]
  no_info2 = units2[,which(dimnames(units2)[[2]] == "Sin Informacion")]
  
  no_info3 = units1[,which(dimnames(units1)[[2]] == "NULL")]
  if(length(no_info3)==0){no_info3 <- 0}
  no_info4 = units2[,which(dimnames(units2)[[2]] == "NULL")]
  if(length(no_info4)==0){no_info4 <- 0}
  
  # # pdf
  # p <- paste0("ClassFrameMS/figs/", cn, "/",cn, "_units0515.pdf")
  # pdf(p, width=5, height=7)
  # png
  p <- paste0("ClassFrameMS/figs/", cn, "/",cn, "_units0515.png")
  png(p, width=5, height=7, units = 'in', res = 300)

  par(mfrow=c(2,1), plt = c(0.2, 0.9, 0.3, 0.75), cex=0.7)
  barplot(t(cbind(apply(units1, 1, sum)-no_info1-no_info3, no_info1)),
          las=3, legend = c("known", "unknown"), args.legend = list(x="topright"), 
          main="Major administrative unit", bty="n")
  barplot(t(cbind(apply(units2, 1, sum)-no_info2-no_info4, no_info2)),
          las=3, legend = c("known", "unknown"), args.legend = list(x="topright"), 
          main="Minor administrative unit", bty="n")
  dev.off()
  
  ## sort out full dates (months included): Converted month 0 to 1 bc do not want to exclude evidence of circulation
  country$Mes[which(country$Mes=="0")] <- 1
  country$date <- as.POSIXct(as.yearmon(paste(country$Ano,country$Mes, sep="-")))
  country$date <- strftime(strptime(country$date, format="%Y-%m-%d"),"%Y-%m")
  
  ## sum all cases for each month and year
  cases <- vector(mode = "numeric", length(dates))
  for(i in 1:length(dates)){
    index <- which(country$date == dates[i])
    cases[i] <- sum(country$TotalCasos[index], na.rm=TRUE)
  }
  
  monthly.cases <- cbind(dates, cases)
  if(sum(country$TotalCasos)!=sum(cases)){
    stop("cases don't match")
  }
  
  fa <- paste0("ClassFrameMS/output/",cn,"/", cn, "_MonthlyCases0515.csv")
  #write.csv(monthly.cases, fa, row.names=F)
  
  ## timeserie: sum all cases for each month and year/ per each state
  country_ts = states_ts(dates = dates, states = states, data = country) 
  fb <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_state0515.csv")
  #write.csv(country_ts,fb,row.names=F)
  
  # timeseries: no information 
  SI_ts <- states_ts(dates = dates, states = "Sin Informacion", data = country)
  NULL_ts <- states_ts(dates = dates, states = "NULL", data = country)
  if(sum(NULL_ts[,1])>0){SI_ts <- cbind(Null=NULL_ts[,1], SI_ts)}
  fc <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_SI0515.csv")
  #write.csv(SI_ts,fc, row.names=F)
  
  print(l)
  print(setdiff(unique(country$UnidMaior),states))
}







