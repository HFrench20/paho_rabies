## SIRVERA DATA: Classification maps for Mexico and Brazil 
## This script produces maps of Mexico and Brazil with states color-shaded depending on
## their classification of epi situation in 2005, 2010 and 2015 (with a separate figure for each year)
## and underlaied with ratser data of population density in the region (BW colour scale)

rm(list=ls())
#setwd("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis")
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(maptools)
library(spdep)
library(raster)
library(rgdal)
library(rgeos)

world <- readOGR("ShapeFiles/countries_shp/countries.shp", "countries")
proj4string(world)
LAC <- readOGR("data/America_Adm_1/adm1_amro_lat_long.shp","adm1_amro_lat_long")

###---------------------------------------Mexico---------------------------------------###
## import shapefile
mex <- readOGR("data/America_Adm_1/Mexico.shp","Mexico")
proj4string(LAC); proj4string(mex)
proj4string(mex) <- proj4string(LAC)

mex@data$ADM1_NAME <- language(mex@data$ADM1_NAME)
head(mex@data); dim(mex@data) #32 polygons

## import cases info
allstates <- read.csv("ClassFrameMS/output/Mexico/Mexico_classified_yearlyTW5yr.csv") # all years

## run for all by 5yrs periods
# pdf(paste0("ClassFrameMS/figs/Mexico/Mex_rabies_classification3panel.pdf"), width=13, height=5)
# par(mfrow=c(1,3))
want.raster <- F
if(want.raster==T){
  file.name="ClassFrameMS/figs/Mexico/Mex_rabies_classification4panel.pdf"
}else{
  file.name="ClassFrameMS/figs/Mexico/Mex_rabies_classification4panelNORASTER.pdf"
}

pdf(paste0(file.name), width=18, height=4.5)
par(mfrow=c(1,4), mar=c(5,2,5,0))

yrs <- c(2005, 2010, 2015)
for(i in 1:length(yrs)){
  yr <- yrs[i]
  states <- data.frame(state=as.character(allstates$state), 
                       phase=as.character(allstates[,colnames(allstates)==paste0("yr",yr)]))
  states$state <- gsub("[.]", " ", states$state)
  head(states)
  
  ## match
  SA <- world[which(world@data$CONTINENT=="South America" | world@data$CONTINENT=="North America"),]
  (rows <- match(states$state, mex@data$ADM1_NAME))
  states$phase <- as.character(states$phase)
  mex@data$classification <- states$phase
  unique(states$phase)
  
  ## PLOTTING
  plot(mex, main=yr, cex.main=2)
  plot(SA, add=T, col="lightgrey")
  options(scipen=5)
  
  if(want.raster==T){
    ## raster
    pop <- readAsciiGrid("~/Dropbox/PAHOsurveillance/Analysis/data/rasters/Human_pop_highres.asc")
    pop.rast <- raster(pop, layer=1, values=T)
    
    pop <- aggregate(pop.rast, fact=6, fun=sum) #0.25 x 0.25 degrees per cell
    res(pop.rast); res(pop)
    
    ## raster
    colours<- colorRampPalette(c("white", "grey5"))(5)
    brk <- c(0,500,5000,50000,500000,7000000)
    arg <- list(at=c(550000,1900000,3700000,5500000,6800000),
                labels=c("<500","500-5000","5000-50000","50000-500000","<7000000"), cex.axis=0.45)
    plot(pop,add=T,col=colours,breaks=brk,legend=F)
    if(i==1){
      plot(pop, col=colours, horizontal=TRUE, smallplot=c(.15, .5, .95, .97),legend.width=0.5,
           legend.shrink=0.5, legend.only=TRUE,axis.args=arg)
    }
  }
  
  ## classification
  cols <- c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F") # Absent, Absent-Vulnerable, Intermittent, Declining, Endemic
  episit <-  c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
  cols.trans <- adjustcolor(cols, alpha.f = 0.6)
  colleg <- cols.trans
  
  absent = which(mex@data$classification=="Absent")
  absentvul = which(mex@data$classification=="Absent-Vulnerable")
  intermittent = which(mex@data$classification=="Intermittent")
  declining = which(mex@data$classification=="Declining")
  endemic = which(mex@data$classification=="Endemic")
  
  plot(mex, add=T, col="transparent")
  if(length(absent)>0){plot(mex[absent,], add=T, col=cols.trans[1])}
  if(length(absentvul)>0){plot(mex[absentvul,], add=T, col=cols.trans[2])}
  if(length(intermittent)>0){plot(mex[intermittent,], add=T, col=cols.trans[3])}
  if(length(declining)>0){plot(mex[declining,], add=T, col=cols.trans[4])}
  if(length(endemic)>0){plot(mex[endemic,], add=T, col=cols.trans[5])}
  
  # boundaries
  plot(SA[which(SA@data$COUNTRY=="Mexico"),],add=T,border="black",lwd=2.5)
  plot(SA,add=T,border="black",lwd=1)
  
  if(i==1){
    legend(-113.8122,18.70355,
           legend=c(episit),
           col = c(rep("black",length(colleg))),
           pch=c(rep(22,length(colleg))),pt.cex=1,
           pt.bg = c(colleg),
           cex=0.7, bty="n")
  }
  print(i)
}

## add 4 th panel: location description
remark.states <- c("Chiapas", "Distrito Federal", "Veracruz de Ignacio de la Llave", "Yucatan") 
remark.countries <- c("Guatemala") 
plot(mex, main="Regional setting", cex.main=2)
plot(SA, add=T, col="lightgrey", border="black")
plot(mex, add=T, border="black", col="white")
plot(SA[which(SA@data$COUNTRY %in% remark.countries),], add=T, col=adjustcolor("gray40", alpha.f = 0.6))
plot(mex[which(mex@data$ADM1_NAME %in% remark.states),], add=T, col=adjustcolor("gray40", alpha.f = 0.6))
plot(SA[which(SA@data$COUNTRY=="Mexico"),],add=T,border="black",lwd=2)

legend(-113.8122,18.70355,
       legend=paste0(1:length(c(remark.states, remark.countries)), ".", c(remark.states, remark.countries)),
       cex=0.7, bty="n")

dev.off()


###---------------------------------------Brazil---------------------------------------###
## import shapefile
bra <- readOGR("data/America_Adm_1/Brazil.shp","Brazil")
proj4string(LAC); proj4string(bra)
proj4string(bra) <- proj4string(LAC)

# state
bra@data$ADM1_NAME <- language(bra@data$ADM1_NAME)
head(bra@data); dim(bra@data) #27 polygons

## import cases info
#states <- read.csv("ClassFrameMS/output/Brazil/Brazil_Classified_states.csv")
allstates <- read.csv("ClassFrameMS/output/Brazil/Brazil_classified_yearlyTW5yr.csv") # all years

## run for all by 5yrs periods
want.raster <- F
if(want.raster==T){
  file.name="ClassFrameMS/figs/Brazil/Bra_rabies_classification4panel.pdf"
}else{
  file.name="ClassFrameMS/figs/Brazil/Bra_rabies_classification4panelNORASTER.pdf"
}

pdf(paste0(file.name), width=18, height=4.5)
par(mfrow=c(1,4), mar=c(5,2,5,0))

yrs <- c(2005, 2010, 2015)
for(i in 1:length(yrs)){
  yr <- yrs[i]
  states <- data.frame(state=as.character(allstates$state), 
                       phase=as.character(allstates[,colnames(allstates)==paste0("yr",yr)]))
  states$state <- gsub("[.]", " ", states$state)
  head(states)
  
  ## match
  SA <- world[which(world@data$CONTINENT=="South America" | world@data$CONTINENT=="North America"),]
  (rows <- match(states$state, bra@data$ADM1_NAME))
  states$phase <- as.character(states$phase)
  bra@data$classification <- states$phase
  unique(states$phase)
  
  ## PLOTTING
  plot(bra)
  plot(SA, add=T, col="lightgrey")
  options(scipen=5)
  
  if(want.raster==T){
    ## raster
    pop <- readAsciiGrid("~/Dropbox/PAHOsurveillance/Analysis/data/rasters/Human_pop_highres.asc")
    pop.rast <- raster(pop, layer=1, values=T)
    
    pop <- aggregate(pop.rast, fact=6, fun=sum) #0.25 x 0.25 degrees per cell
    res(pop.rast); res(pop)
    
    ## raster
    colours<- colorRampPalette(c("white", "grey5"))(5) 
    brk <- c(0,500,5000,50000,500000,7000000)
    arg <- list(at=c(550000,1900000,3700000,5500000,6800000),
                labels=c("<500","500-5000","5000-50000","50000-500000","<7000000"), cex.axis=0.45) 
    plot(pop,add=T,col=colours,breaks=brk,legend=F)
    if(i==1){
      plot(pop, col=colours, horizontal=TRUE, smallplot=c(.15, .5, .95, .97),legend.width=0.5,
           legend.shrink=0.5, legend.only=TRUE,axis.args=arg)
    }
  }
  
  ## classification
  cols <- c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F") # Absent, Absent-Vulnerable, Intermittent, Declining, Endemic
  episit <-  c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
  cols.trans <- adjustcolor(cols, alpha.f = 0.6)
  colleg <- cols.trans
  
  absent = which(bra@data$classification=="Absent")
  absentvul = which(bra@data$classification=="Absent-Vulnerable")
  intermittent = which(bra@data$classification=="Intermittent")
  declining = which(bra@data$classification=="Declining")
  endemic = which(bra@data$classification=="Endemic")
  
  plot(bra, add=T, col="transparent")
  if(length(absent)>0){plot(bra[absent,], add=T, col=cols.trans[1])}
  if(length(absentvul)>0){plot(bra[absentvul,], add=T, col=cols.trans[2])}
  if(length(intermittent)>0){plot(bra[intermittent,], add=T, col=cols.trans[3])}
  if(length(declining)>0){plot(bra[declining,], add=T, col=cols.trans[4])}
  if(length(endemic)>0){plot(bra[endemic,], add=T, col=cols.trans[5])}
  
  # # cols: rabies status
  # cols <- c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F")
  # # Absent, Absent-Vulnerable, Intermittent, Declining, Endemic
  # cols.trans <- adjustcolor(cols, alpha.f = 0.6) 
  # 
  # if(yr==2015 | yr==2010){
  #   episit <- c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
  #   colleg <- cols.trans
  #   plot(bra[which(bra@data$classification=="Absent"),],add=T, col=cols.trans[1]) 
  #   plot(bra[which(bra@data$classification=="Absent-Vulnerable"),],add=T, col=cols.trans[2])
  #   plot(bra[which(bra@data$classification=="Intermittent"),],add=T, col=cols.trans[3])
  #   plot(bra[which(bra@data$classification=="Declining"),],add=T, col=cols.trans[4]) 
  #   plot(bra[which(bra@data$classification=="Endemic"),],add=T, col=cols.trans[5]) 
  # }
  # 
  # if(yr==2005){
  #   episit <-  c("Absent", "Absent-Vulnerable", "Intermittent", "Endemic")           
  #   colleg <- cols.trans[c(1:3,5)]
  #   plot(bra[which(bra@data$classification=="Absent"),],add=T, col=cols.trans[1]) 
  #   plot(bra[which(bra@data$classification=="Absent-Vulnerable"),],add=T, col=cols.trans[2])
  #   plot(bra[which(bra@data$classification=="Intermittent"),],add=T, col=cols.trans[3])
  #   plot(bra[which(bra@data$classification=="Endemic"),],add=T, col=cols.trans[5])
  # }
  
  # boundaries
  plot(SA[which(SA@data$COUNTRY=="Brazil"),],add=T,border="black",lwd=2.5)
  plot(SA,add=T,border="black",lwd=1)
  
  if(i==1){
    legend(-45.59187,-23.96342,
           legend=c(episit),
           col = c(rep("black",length(colleg))),
           pch=c(rep(22,length(colleg))),pt.cex=1,
           pt.bg = c(colleg),
           cex=0.7, bty="n")
  }
  print(i)
}
  
## add 4 th panel: location description
## dark shaded with lines & numbers, legend: numbers with state names
remark.states <- c("Ceara", "Maranhao", "Mato Grosso do Sul", "Rio de Janeiro", "Rio Grande do Norte", "Sao Paulo") 
remark.countries <- c("Bolivia", "Venezuela") 
plot(bra)
plot(SA, add=T, col="lightgrey", border="black")
plot(bra, add=T, border="black", col="white")
plot(SA[which(SA@data$COUNTRY %in% remark.countries),], add=T, col=adjustcolor("gray40", alpha.f = 0.6))
plot(bra[which(bra@data$ADM1_NAME %in% remark.states),], add=T, col=adjustcolor("gray40", alpha.f = 0.6))
plot(SA[which(SA@data$COUNTRY=="Brazil"),],add=T,border="black",lwd=2)

legend(-45.59187,-23.96342,
       legend=paste0(1:length(c(remark.states, remark.countries)), ".", c(remark.states, remark.countries)),
       cex=0.7, bty="n")
  
dev.off()







