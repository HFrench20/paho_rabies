### SIRVERA DATA: Case data preparation for static incursion maps for Mexico (+Guatemala)
### and Brazil (+Bolivia)

rm(list=ls())
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(maps)
library(rgdal)
library(raster)
library(zoo)
source("R/approxmatch.r")
source("R/states_ts.r")
source("R/translate.r")

## shapefiles and data
LAC2 <- readOGR("data/America_Adm_2/Adm2_AMRO_2016.shp","Adm2_AMRO_2016" ) 
LAC1 <- readOGR("data/America_Adm_1/adm1_amro_lat_long.shp", "adm1_amro_lat_long")

dogs <- read.csv("data/SIRVERA_dogs16(clean_statenames).csv")

###------------------------------------Mexico------------------------------------###
## incursions
incursions <- read.csv("ClassFrameMS/output/Mexico/Mexico_IncursionClassification.csv")

## crop shapefiles
LAC2@data$ADM1_NAME <- language(LAC2@data$ADM1_NAME)
mex <- LAC2[which(LAC2@data$CNTRY_NAME=="Mexico"),]
mexico <- LAC1[which(LAC1@data$CNTRY_NAME=="Mexico"),]

## subset data
time <- 2005:2015 # DECIDE ABOUT TIME PERIOD
cases.mex <- subset(dogs, Pais == "Mexico" & Ano %in% c(time))
cases.guat <- subset(dogs, Pais == "Guatemala" & Ano %in% c(time)) 
cases <- cases.mex
sum(cases$TotalCasos) #442 cases from 2005 to 2015
    
## look at completness at municipality level
sort(unique(cases$Ano[which(cases$UnidMenor=="NULL")]))
sort(unique(cases$Ano[which(cases$UnidMenor=="Sin Informacion")]))

sum(cases$TotalCasos[which(cases$UnidMenor=="NULL")]) # 75 
sum(cases$TotalCasos[which(cases$UnidMenor=="Sin Informacion")]) # 311
# in total 386 case with no location info at municipality level

## look at completness at state level
cases[which(cases$UnidMaior=="NULL"),] # none
sum(cases$TotalCasos[which(cases$UnidMaior=="Sin Informacion")]) #34 
# in total 34 cases with no location infro at state level
cases <- cases[-which(cases$UnidMaior=="Sin Informacion"),]

### MATCH names: cases, incursions, shp 
## major unit
# incs vs. cases
incursions$state <- gsub("[.]", " ", incursions$state)
setdiff(incursions$state, cases$UnidMaior) #ok

# cases vs. shp
setdiff(cases$UnidMaior, unique(as.character(mex@data$ADM1_NAME))) #ok
setdiff(incursions$state, unique(as.character(mex@data$ADM1_NAME))) #ok

# minor unit
unique(cases$UnidMenor); unique(cases$matchcode)
setdiff(cases$matchcode,unique(as.character(mex@data$ADM2_CODE))) # ok, but one NA
cases[which(is.na(cases$matchcode) & cases$UnidMenor=="Mexico"),] ## this "Mexico" keeps coming up

unmatchedlist.m <- setdiff(cases$UnidMenor,unique(as.character(mex@data$ADM2_NAME)))
(agrepped<-approxmatch(x=unmatchedlist.m, y=unique(as.character(mex@data$ADM2_NAME)), 
                       d_increment=0.05, max_d=0.5))
agrepped$MasterNames <- as.character(agrepped$MasterNames)
# agrepped[3,2] <- "Tec?mac" #Tec\xe1mac
# agrepped[3,2] <- "Tec\xe1mac"
# agrepped[11,2] <- "Ezequiel Montes"
# agrepped[12,2] <- "Tekax"
# agrepped[13,2] <- "Sey?" #Sey\xe9
# agrepped[13,2] <- "Sey\xe9"

matched <- agrepped[which(!is.na(agrepped$MasterNames)),]
cases$UnidMenor <- as.character(cases$UnidMenor)
for (i in 1:dim(matched)[1]){
  inds <- which(cases$UnidMenor==as.character(matched$DataNames[i]))
  
  cases$UnidMenor[inds] <- as.character(matched$MasterNames[i])
}
setdiff(cases$UnidMenor,unique(mex@data$ADM2_NAME)) ##where is this Mexico??!!
cases$UnidMenorOrigin <- cases$UnidMenor
cases$UnidMenor[which(cases$UnidMenor=="Sin Informacion" | cases$UnidMenor=="NULL" | cases$UnidMenor=="Mexico")] <- NA
unique(cases$UnidMenor)

### Dates 
cases$Mes[which(cases$Mes=="0")] <- 1
cases$date <- as.POSIXct(as.yearmon(paste(cases$Ano,cases$Mes, sep="-")))
cases$date <- strftime(strptime(cases$date, format="%Y-%m-%d"),"%Y-%m-%d")

### Consolidate data in a single table with a single case per each observation 
sum(cases$TotalCasos) #408

mextotcas <- sum(cases$TotalCasos)
country <- vector(mode="character", mextotcas)
maior <- vector(mode="character", mextotcas)
menor <- vector(mode="character", mextotcas)
code <- vector(mode="character", mextotcas)
date <- as.POSIXct(rep(NA, mextotcas))

for (i in 1: nrow(cases)){
  l <- cases$TotalCasos[i]
  index <- seq(which(is.na(date))[1], (which(is.na(date))[1]+l-1),1) # will this work?
  country[index] <- rep(as.character(cases$Pais[i]),l)
  maior[index] <- rep(as.character(cases$UnidMaior[i]),l)
  menor[index] <- rep(as.character(cases$UnidMenor[i]),l)
  code[index] <- rep(as.character(cases$matchcode[i]),l)
  date[index] <- rep(cases$date[i],l)
  print(i)
}

mexico.df <- data.frame(country=country, maior=maior, menor=menor, code=code, date=date)

write.csv(mexico.df, "ClassFrameMS/output/Mexico/Mexico_Cases05_15bycase.csv", row.names=F)
write.csv(incursions, "ClassFrameMS/output/Mexico/Mexico_IncursionClassification.csv", row.names=F)

## Guatemala
sum(cases.guat$TotalCasos) #526
sort(unique(cases.guat$Ano))

# dates
dates <- seq(as.Date("2005-01-01"), as.Date("2015-12-01"), by="months")
dates <- strftime(strptime(dates, format="%Y-%m-%d"),"%Y-%m")

cases.guat$Mes[which(cases.guat$Mes=="0")] <- 1
cases.guat$date <- as.POSIXct(as.yearmon(paste(cases.guat$Ano,cases.guat$Mes, sep="-")))
cases.guat$date <- strftime(strptime(cases.guat$date, format="%Y-%m-%d"),"%Y-%m")

## sum all cases for each month and year
cases <- vector(mode = "numeric", length(dates))
for(i in 1:length(dates)){
  index <- which(cases.guat$date == dates[i])
  cases[i] <- sum(cases.guat$TotalCasos[index], na.rm=TRUE)
}

monthly.cases <- cbind(dates, cases)
sum(cases.guat$TotalCasos)==sum(cases)

write.csv(monthly.cases, "ClassFrameMS/output/Mexico/Guatemala_MonthlyCases0515.csv", row.names=F)

##------------------------------------Brazil------------------------------------###
## incursions
incursions <- read.csv("ClassFrameMS/output/Brazil/Brazil_IncursionClassification.csv")

## crop shapefiles
bra <- LAC2[which(LAC2@data$CNTRY_NAME=="Brazil"),]
brazil <- LAC1[which(LAC1@data$CNTRY_NAME=="Brazil"),]

## subset data
time <- 2005:2015 # DECIDE ABOUT TIME PERIOD
cases.bra <- subset(dogs, Pais == "Brazil" & Ano %in% c(time))

cases.bol <- subset(dogs, Pais == "Bolivia" & Ano %in% c(time)) 
cases.bol2 <- subset(dogs, Pais == "Bolivia"); sort(unique(cases.bol2$Ano))
# but that's strange, bc there should be cases on 2015!!

# Paraguay
cases.par <- subset(dogs, Pais == "Paraguay" & Ano %in% c(time)) 
sum(cases.par$TotalCasos) #only 10
sort(unique(cases.par$Ano))

# Peru
cases.par <- subset(dogs, Pais == "Peru" & Ano %in% c(time)) 
sum(cases.par$TotalCasos) #107
sort(unique(cases.par$Ano))

# Colombia
cases.par <- subset(dogs, Pais == "Colombia" & Ano %in% c(time)) 
sum(cases.par$TotalCasos) #31
sort(unique(cases.par$Ano))

# Venezuela
cases.par <- subset(dogs, Pais == "Venezuela" & Ano %in% c(time)) 
sum(cases.par$TotalCasos) #217
sort(unique(cases.par$Ano))

cases <- cases.bra
sum(cases$TotalCasos) #558 cases from 2005 to 2015
sort(unique(cases$Ano))

## look at completness at municipality level
sort(unique(cases$Ano[which(cases$UnidMenor=="NULL")]))
sort(unique(cases$Ano[which(cases$UnidMenor=="Sin Informacion")]))

sum(cases$TotalCasos[which(cases$UnidMenor=="Sin Informacion")]) # 326
# in totla 326 case with no location info at municipality level

## look at completness at state level
cases[which(cases$UnidMaior=="NULL"),] # none
sum(cases$TotalCasos[which(cases$UnidMaior=="Sin Informacion")]) #14
# in total 14 cases with no location infro at state level
cases <- cases[-which(cases$UnidMaior=="Sin Informacion"),]

### MATCH names: cases, incursions, shp 
## major unit
# incs vs. cases
incursions$state <- gsub("[.]", " ", incursions$state)
setdiff(incursions$state, cases$UnidMaior) #ok

# cases vs. shp
setdiff(cases$UnidMaior, unique(as.character(bra@data$ADM1_NAME))) #ok
setdiff(incursions$state, unique(as.character(bra@data$ADM1_NAME))) #ok

# minor unit
unique(cases$UnidMenor); unique(cases$matchcode)
setdiff(cases$matchcode,unique(as.character(bra@data$ADM2_CODE))) # ok, but one NA

setdiff(cases$UnidMenor,unique(as.character(bra@data$ADM2_NAME)))
cases$UnidMenor <- as.character(cases$UnidMenor)
# cases$UnidMenor[cases$UnidMenor=="PaAo do Lumiar"] <- "Pa?o do Lumiar"
# cases$UnidMenor[cases$UnidMenor=="PaAo do Lumiar"] <- "Pa\347o do Lumiar"
# cases$UnidMenor[cases$UnidMenor=="CansanA?o"] <-  "Cansan??o"
# cases$UnidMenor[cases$UnidMenor=="CansanA\343o"] <-  "Cansan\347\343o"

cases$UnidMenorOrigin <- cases$UnidMenor
cases$UnidMenor[cases$UnidMenor=="Sin Informacion"] <- NA
unique(cases$UnidMenor)

### Dates 
cases$Mes[which(cases$Mes=="0")] <- 1
cases$date <- as.POSIXct(as.yearmon(paste(cases$Ano,cases$Mes, sep="-")))
cases$date <- strftime(strptime(cases$date, format="%Y-%m-%d"),"%Y-%m-%d")

### Consolidate data in a single table with a single case per each observation 
sum(cases$TotalCasos) #544

bratotcas <- sum(cases$TotalCasos)
country <- vector(mode="character", bratotcas)
maior <- vector(mode="character", bratotcas)
menor <- vector(mode="character", bratotcas)
code <- vector(mode="character", bratotcas)
date <- as.POSIXct(rep(NA, bratotcas))

for (i in 1: nrow(cases)){
  l <- cases$TotalCasos[i]
  index <- seq(which(is.na(date))[1], (which(is.na(date))[1]+l-1),1) 
  country[index] <- rep(as.character(cases$Pais[i]),l)
  maior[index] <- rep(as.character(cases$UnidMaior[i]),l)
  menor[index] <- rep(as.character(cases$UnidMenor[i]),l)
  code[index] <- rep(as.character(cases$matchcode[i]),l)
  date[index] <- rep(cases$date[i],l)
  print(i)
}

brazil.df <- data.frame(country=country, maior=maior, menor=menor, code=code, date=date)

write.csv(brazil.df, "ClassFrameMS/output/Brazil/Brazil_Cases05_15bycase.csv", row.names=F)
write.csv(incursions, "ClassFrameMS/output/Brazil/Brazil_IncursionClassification.csv", row.names=F)

## Bolivia
sum(cases.bol$TotalCasos) #2749
sort(unique(cases.bol$Ano))

# dates
dates <- seq(as.Date("2005-01-01"), as.Date("2015-12-01"), by="months")
dates <- strftime(strptime(dates, format="%Y-%m-%d"),"%Y-%m")

cases.bol$Mes[which(cases.bol$Mes=="0")] <- 1
cases.bol$date <- as.POSIXct(as.yearmon(paste(cases.bol$Ano,cases.bol$Mes, sep="-")))
cases.bol$date <- strftime(strptime(cases.bol$date, format="%Y-%m-%d"),"%Y-%m")

## sum all cases for each month and year
cases <- vector(mode = "numeric", length(dates))
for(i in 1:length(dates)){
  index <- which(cases.bol$date == dates[i])
  cases[i] <- sum(cases.bol$TotalCasos[index], na.rm=TRUE)
}

monthly.cases <- cbind(dates, cases)
sum(cases.bol$TotalCasos)==sum(cases)

write.csv(monthly.cases, "ClassFrameMS/output/Brazil/Bolivia_MonthlyCases0515.csv", row.names=F)

