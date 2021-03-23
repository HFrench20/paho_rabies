## SIRVERA DATA: Mexico - Spatial Code
rm(list=ls())
setwd("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis")

library(maptools)
library(spdep)
library(raster)
library(rgdal)
library(rgeos)
source("R/translate.r")

## import shapefile
LAC <- readOGR("data/America_Adm_1/adm1_amro_lat_long.shp","adm1_amro_lat_long")
mex <- readOGR("data/America_Adm_1/Mexico.shp","Mexico")
proj4string(LAC); proj4string(mex)
proj4string(mex) <- proj4string(LAC)

mex@data$ADM1_NAME <- language(mex@data$ADM1_NAME)
head(mex@data); dim(mex@data) #32 polygons

###____________________________________________________________________________________________
### (1) Adjacency matrix
unique(mex@data$ADM1_CODE)

nb.units <- poly2nb(mex, queen=FALSE) 
##queen=FALSE: more than single shared boundary point required 
summary.nb(nb.units)
mat <- nb2mat(nb.units, style="B", zero.policy=TRUE) #Adjacency matrix of polygons
dim(mat)

## Check symmetrical matrix with 0s on diagonal (i.e. not neighbour with self)
sum(colSums(mat)-as.vector(rowSums(mat))) #should equal 0
sum(diag(mat))
mat[1:10,1:10]

## check whether "1" really do stands for the neighbouring units (adjacent)
plot(mex) 
i=17 
plot(mex[i,], col="red", add=T) #focal polygon
nbs <- which(mat[i,]==1) #rows which correspond to neighbours in shapefile
plot(mex[nbs,],col="blue",add=T) #adjacent polygons
write.csv(mat,"output/Mex_adjacency_matrix.csv",row.names=F)

## create a df with mex code names
mat.df <- data.frame(mat)
row.names(mat.df) <- mex@data$ADM1_CODE
colnames(mat.df) <- mex@data$ADM1_CODE
head(mat.df)

###____________________________________________________________________________________________
### (2) International Border
## the LAC shapefile has a lot of holes and mismatching boundaries -> poly2nb function
## doesn't work between Mexico and other countries -> used gIntersects insted
## !! also better to read in shapefiles using the readOGR command rather than readShapePoly (fixes some minor discrepancies)

## crop LAC to extent of mex
mex_box <- bbox(mex); mex_box <- extent(mex_box)
mexico <- crop(LAC,mex_box)

plot(mexico); plot(mex,add=T, col="red")

## establish unique code
head(mexico@data); dim(mexico@data) #78 polygons
unique(mexico@data$ADM1_CODE)

test <- gIntersects(mexico, byid=TRUE) 
which(mexico@data$ADM1_CODE=="MEX005")

i=43 
plot(mexico);plot(mexico[i,], col="red", add=T) #border polygon
nbs <- which(test[i,]=="TRUE") 
plot(mexico[nbs,],col="blue",add=T) #works

matint.df <- data.frame(test)
row.names(matint.df) <- mexico@data$ADM1_CODE
colnames(matint.df) <- mexico@data$ADM1_CODE
head(matint.df)

# mexico code rows
mexcode <- mexico@data$ADM1_CODE[39:70]
intb <- rep("N", length(mexcode))

for (i in 1:length(mexcode)){
  sums <- which(matint.df[i+38,c(1:38,71:78)]=="TRUE")
  if(length(sums)>0){
    intb[i] <- "Y"
  }
}

mex.info <- data.frame(unit_code=mexcode,InNat_boundary=intb); head(mex.info)
write.csv(mex.info,"output/Mex_boundary_info.csv",row.names=F)

###____________________________________________________________________________________________
### (3) Sea
## !! Our LAC shapefile is really noisy and when I try to extract the outline boundary there are still
## very many cordinates left inside the polygon
## download shapefile from the R database

## R shapefile
world <- getData("countries")
countries <- c("North America","South America")
am  <- gUnaryUnion(world[world$CONTINENT %in% countries,])
plot(am); proj4string(am); gIsValid(am)

mexico <- crop(am,mex_box)
proj4string(mexico)
plot(mexico)
plot(mex, add=T, col="red")

shore <- gRelate(mex, mexico, byid= TRUE) # no, they don't have different proj4 strings!
table(shore)
poly.border<-which(shore %in% c("212101212"))
plot(mex[poly.border,], add=T, col="blue") # awesome, works!!

# transfer the info
head(mex.info)
mex.info$shoreline <- rep("N", nrow(mex.info))
mex.info$shoreline[poly.border] <- "Y"
                   
# check if works: MEX002 should have int boundary and shoreline
plot(mex[2,], add=T, col="green")

write.csv(mex.info,"output/Mex_boundary_info.csv",row.names=F)

########################################################################################
## Mexico plotting with cases
# cases info
states <- read.csv("output/Mexico/Mexico_Classified_statesVAR.csv")
states$state <- gsub("[.]", " ", states$state)
head(states)

## plotting prep
SA <- world[which(world@data$CONTINENT=="South America" | world@data$CONTINENT=="North America"),]

(rows <- match(states$state, mex@data$ADM1_NAME))
states$phase <- as.character(states$phase)
mex@data$classification <- states$phase
unique(states$phase)

## raster
pop <- readAsciiGrid("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis/data/rasters/Human_pop_highres.asc")
pop.rast <- raster(pop, layer=1, values=T)

pop <- aggregate(pop.rast, fact=6, fun=sum) #0.25 x 0.25 degrees per cell
res(pop.rast); res(pop)

## do the plotting
pdf("figs/Mexico/Mex_rabies_classificationNOWILD.pdf", width=8, height=6)
plot(mex)
plot(SA, add=T, col="lightgrey")

options(scipen=5)

## raster 
colours<- colorRampPalette(c("white", "grey5"))(5) 
brk <- c(0,500,5000,50000,500000,7000000)
arg <- list(at=c(550000,1900000,3700000,5500000,6800000),
            labels=c("<500","500-5000","5000-50000","50000-500000","<7000000"), cex.axis=0.45) 
plot(pop,add=T,col=colours,breaks=brk,legend=F)
plot(pop, col=colours, horizontal=TRUE, smallplot=c(.15, .5, .95, .97),legend.width=0.5,
     legend.shrink=0.5, legend.only=TRUE,axis.args=arg)

# cols: rabies status
cols <- c("white","#FDFCD6","#FAC090","#FA684F")
cols.trans <- adjustcolor(cols, alpha.f = 0.6) 

plot(mex[which(mex@data$classification=="Absence"),],add=T, col=cols.trans[1]) 
plot(mex[which(mex@data$classification=="Absence, vulnerable"),],add=T, col=cols.trans[2])
plot(mex[which(mex@data$classification=="Declining"),],add=T, col=cols.trans[3]) 
plot(mex[which(mex@data$classification=="Endemic"),],add=T, col=cols.trans[4]) 

# boundaries
plot(SA[which(SA@data$COUNTRY=="Mexico"),],add=T,border="black",lwd=2.5)
plot(SA,add=T,border="black",lwd=1)

legend(-113.8122,18.70355,
       legend=c("Absence","Absence, incursion risk",
                "Declining","Endemic"),
       col = c(rep("black",4)),
       pch=c(rep(22,6)),pt.cex=1,
       pt.bg = c(cols),
       cex=0.7, bty="n") 
dev.off()
