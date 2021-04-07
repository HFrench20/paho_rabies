### SIRVERA DATA: Static maps of incursions for Mexico and Brazil
## This script produces maps of incursions in Mexico and Brazil with 
## a) states color-shaded depending on for how many months between Jan 2005 and Dec 2015 rabies was present,
## b) locations of incursions indicated by blue points 
## sized accroding to how many cases each incursion resulted in and shaded according to their recency

rm(list=ls())
#setwd("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis")
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(maptools)
library(maps)
library(rgdal)
library(raster)
library(RColorBrewer)

## shps
LAC2 <- readOGR("data/America_Adm_2/Adm2_AMRO_2016.shp","Adm2_AMRO_2016" ) 
LAC1 <- readOGR("data/America_Adm_1/adm1_amro_lat_long.shp", "adm1_amro_lat_long")

## for boundaries
world <- readOGR("ShapeFiles/countries_shp/countries.shp", "countries")
SA <- world[which(world@data$CONTINENT=="South America" | world@data$CONTINENT=="North America"),]

GTM <- readOGR("ShapeFiles/GTM_adm_shp/GTM_adm0.shp", "GTM_adm0")
MEX <- readOGR("ShapeFiles/MEX_adm_shp/MEX_adm0.shp", "MEX_adm0")
  
###------------------------------------------Mexico------------------------------------------###
## import data
mex <- LAC2[which(LAC2@data$CNTRY_NAME=="Mexico"),]
mexico <- LAC1[which(LAC1@data$CNTRY_NAME=="Mexico"),]

## import data
cases <- read.csv("ClassFrameMS/output/Mexico/Mexico_Cases05_15bycase.csv") 
guat <- read.csv("ClassFrameMS/output/Mexico/Guatemala_MonthlyCases0515.csv")

incursions <- read.csv("ClassFrameMS/output/Mexico/Mexico_IncursionClassification.csv")
incursions <- incursions[which(incursions$incursion=="Y"),] #32

## Guatemala
table(guat$cases) 
guat
#132 months in total, 68 months with cases, last 42 months (3.5 years) with no cases

### for how many months in 132 month there were cases detected in each state
states <- sort(unique(mexico@data$ADM1_NAME))
monthcases <- numeric(length(states))
casesstates <- unique(cases$maior)

for(i in 1:length(casesstates)){ 
  ind <- which(states==as.character(casesstates[i]))
  
  dates <- cases$date[which(cases$maior==as.character(casesstates[i]))]
  monthcases[ind] <- length(unique(dates))
}

monthcases <- data.frame(state=states, cases=monthcases)

## add monthcases to shapefile
(rows <- match(states, mexico@data$ADM1_NAME))
mexico@data$monthcases <- NA
mexico@data$monthcases[rows] <- as.character(monthcases$cases)

## Create colours for choropleth map of no. of exposures
max(as.numeric(mexico@data$monthcases)); min(as.numeric(mexico@data$monthcases)) #62 cases/month max
quantile(as.numeric(mexico@data$monthcases))
sort(unique(as.numeric(mexico@data$monthcases)))
breaks <- c(0, 1, 2, 4, 10, 15, 20, 25, 30, 60, 62)
colours <- colorRampPalette(c("white", "red"))(11)

## generate spatial points for each incursion
xycases <- matrix(nrow=nrow(incursions), ncol=2)

set.seed(1666318)
for(i in 1:nrow(incursions)){
  temp.state <- incursions$state[i]
  casesSP <- spsample(mexico[which(mexico@data$ADM1_NAME==as.character(temp.state)),], type="random", n=1,iter=10)
  xycases[i,] <- coordinates(casesSP)  
  print(i)
}
incursions <- cbind(incursions, xycases)
colnames(incursions)[10:11] <- c("x", "y")

## how many cases resulted form each incursion? 
cases$date <- substr(cases$date,1,7)
incursions$cases <- NA
for(i in 1: nrow(incursions)){
  temp.cases <- cases[which(cases$maior==as.character(incursions$state[i])),]
  temp.cases <- temp.cases[order(temp.cases$date),]
  s <- which(temp.cases$date==incursions$date[i])
  e <- which(temp.cases$date==incursions$end[i])
  if(length(e)>0){
    incursions$cases[i] <- e[length(e)]-s[1]
  }else{
    incursions$cases[i] <- length(s)
  }  
}
sort(unique(incursions$cases)) #1, 2, 3, 26

## create colours according to how historic incursions are
sort(unique(as.numeric(incursions$date))) 
breaks.IC <- seq(1,48,12) 

## PLOTTING
pdf("ClassFrameMS/figs/Mexico/StaticIncursionMap.pdf", width=8, height=6)
## plot Mexico 
par(mar=c(0,0,0,0))
plot(mexico,  bg="lightgrey")
plot(LAC2, add=T, col="white", border="white")
plot(mexico, add=T, col="white", border="gray40")

## shade states according to how many months with cases
plot(mexico, add=T, border=NA, col=colours[findInterval(as.vector(as.character(mexico@data$monthcases)),
                                                       breaks,all.inside=T)])

## Guatemala: N.B. no cases in Guatemala in the last 42 months, but 68 months with cases in total!! 
#guat.trans <- adjustcolor("orangered", alpha.f = 0.5) # colour for Guatemala; #FF0000, firebrick4
#plot(SA[which(SA@data$COUNTRY=="Guatemala"),],add=T,col=guat.trans, border="black",lwd=1)
#plot(SA[which(SA@data$COUNTRY=="Guatemala"),],add=T, border="orangered",lwd=1)
plot(GTM,add=T, border="orangered",lwd=1)

## add borders
plot(mexico, add=T, border="gray40")
#plot(SA[which(SA@data$COUNTRY=="Mexico"),],add=T,border="black",lwd=1)
plot(MEX,add=T,border="black",lwd=1)

# blue version
colours.IC <- brewer.pal((9), "Blues")
#colours.IC <- colours.IC[4:9]
colours.IC <- adjustcolor(colours.IC, alpha.f = 0.6) 
plot(SpatialPoints(data.frame(x=incursions$x, y=incursions$y)),
     add=T,pch=21,cex=log(incursions$cases+1)+.3,
     col="darkblue", bg=colours.IC[findInterval(as.numeric(incursions$date),breaks.IC,all.inside=T)])

## legend 
legend(-118,18.9,
       legend=c(leglabs(breaks, under="<",over=">")),
       pch=c(rep(22,length(breaks))),
       col = c(rep("black",length(breaks))),
       pt.bg=c(colours), pt.cex=1.5,
       title=c("No. of months with detected cases"),
       cex=0.7, bty="n")

dev.off()

###------------------------------------------Brazil------------------------------------------###
## import data
bra <- LAC2[which(LAC2@data$CNTRY_NAME=="Brazil"),]
brazil <- LAC1[which(LAC1@data$CNTRY_NAME=="Brazil"),]

## for boundaries
BRA <- readOGR("ShapeFiles/BRA_adm_shp/BRA_adm0.shp", "BRA_adm0")
BOL <- readOGR("ShapeFiles/BOL_adm_shp/BOL_adm0.shp", "BOL_adm0")
PER <- readOGR("ShapeFiles/PER_adm_shp/PER_adm0.shp", "PER_adm0")
PRY <- readOGR("ShapeFiles/PRY_adm_shp/PRY_adm0.shp", "PRY_adm0")
COL <- readOGR("ShapeFiles/COL_adm_shp/COL_adm0.shp", "COL_adm0")
VEN <- readOGR("ShapeFiles/VEN_adm_shp/VEN_adm0.shp", "VEN_adm0")

## import data
cases <- read.csv("ClassFrameMS/output/Brazil/Brazil_Cases05_15bycase.csv") 
bol <- read.csv("ClassFrameMS/output/Brazil/Bolivia_MonthlyCases0515.csv")

incursions <- read.csv("ClassFrameMS/output/Brazil/Brazil_IncursionClassification.csv")
incursions <- incursions[which(incursions$incursion=="Y"),] #54

## Bolivia
table(bol$cases) 
bol #132 months in total, 77 months with cases, last 43 months with no cases

### for how many months in 132 month there were cases detected in each state 
states <- sort(unique(brazil@data$ADM1_NAME))
monthcases <- numeric(length(states))
casesstates <- unique(cases$maior)

for(i in 1:length(casesstates)){ 
  ind <- which(states==as.character(casesstates[i]))
  
  dates <- cases$date[which(cases$maior==as.character(casesstates[i]))]
  monthcases[ind] <- length(unique(dates))
}

monthcases <- data.frame(state=states, cases=monthcases)

## add monthcases to shapefile
(rows <- match(states, brazil@data$ADM1_NAME))
brazil@data$monthcases <- NA
brazil@data$monthcases[rows] <- as.character(monthcases$cases)

## Create colours for choropleth map of no. of exposures
max(as.numeric(brazil@data$monthcases)); min(as.numeric(brazil@data$monthcases)) 
quantile(as.numeric(brazil@data$monthcases))
sort(unique(as.numeric(brazil@data$monthcases)))
breaks <- c(0, 1, 2, 4, 10, 15, 20, 25, 30, 60, 62)
colours <- colorRampPalette(c("white", "red"))(11)

## generate spatial points for each incursion
xycases <- matrix(nrow=nrow(incursions), ncol=2)

set.seed(1666318)
for(i in 1:nrow(incursions)){
  temp.state <- incursions$state[i]
  casesSP <- spsample(brazil[which(brazil@data$ADM1_NAME==as.character(temp.state)),], type="random", n=1,iter=10)
  xycases[i,] <- coordinates(casesSP)  
}
incursions <- cbind(incursions, xycases)
colnames(incursions)[10:11] <- c("x", "y")

## how many cases resulted form each incursion? 
cases$date <- substr(cases$date,1,7)
incursions$cases <- NA
for(i in 1: nrow(incursions)){
  temp.cases <- cases[which(cases$maior==as.character(incursions$state[i])),]
  temp.cases <- temp.cases[order(temp.cases$date),]
  s <- which(temp.cases$date==incursions$date[i])
  e <- which(temp.cases$date==incursions$end[i])
  if(length(e)>0){
    incursions$cases[i] <- e[length(e)]-s[1]
  }else{
    incursions$cases[i] <- length(s)
  }  
}
sort(unique(incursions$cases)) #1, 2, 3, 4, 9

## create colours according to how historic incursions are
sort(unique(incursions$date))
sort(unique(as.numeric(incursions$date))) #1-69
#breaks.IC <- seq(1,69,6) # by half a year
breaks.IC <- seq(1,69,12) # by a year

## PLOTTING
pdf("ClassFrameMS/figs/Brazil/StaticIncursionMap.pdf", width=8, height=6)
## plot Brazil
par(mar=c(0,0,0,0))
plot(brazil,  bg="lightgrey")
plot(LAC2, add=T, col="white", border="white")
plot(brazil, add=T, col="white", border="gray40")

## shade states according to how many months with cases
plot(brazil, add=T, border=NA, col=colours[findInterval(as.vector(as.character(brazil@data$monthcases)),
                                                        breaks,all.inside=T)])

## Bolivia
#bol.trans <- adjustcolor("orangered", alpha.f = 0.5) 
#plot(SA[which(SA@data$COUNTRY=="Bolivia"),],add=T,col=bol.trans, border="black",lwd=1)
plot(BOL,add=T, border="orangered",lwd=1)
plot(PRY,add=T, border="orangered",lwd=1)
plot(PER,add=T, border="orangered",lwd=1)
plot(COL,add=T, border="orangered",lwd=1)
plot(VEN,add=T, border="orangered",lwd=1)

## add borders
plot(brazil, add=T, border="gray40")
# plot(SA[which(SA@data$COUNTRY=="Brazil"),],add=T,border="black",lwd=1)
plot(BRA,add=T,border="black",lwd=1)

# blue version
colours.IC <- brewer.pal((9), "Blues")
#colours.IC <- colours.IC[4:9]
colours.IC <- adjustcolor(colours.IC, alpha.f = 0.6) 
plot(SpatialPoints(data.frame(x=incursions$x, y=incursions$y)),
     add=T,pch=21,cex=log(incursions$cases+1)+.3,
     col="darkblue", bg=colours.IC[findInterval(as.numeric(incursions$date)+1,breaks.IC,all.inside=T)])

## legend 
legend(-44,-23.9,
       legend=c(leglabs(breaks, under="<",over=">")),
       pch=c(rep(22,length(breaks))),
       col = c(rep("black",length(breaks))),
       pt.bg=c(colours), pt.cex=1.5,
       title=c("No. of months with detected cases"),
       cex=0.7, bty="n")

dev.off()



