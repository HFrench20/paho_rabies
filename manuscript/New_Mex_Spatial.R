## Create adjacency matrix of states within a country. 
# SIRVERA DATA: Mexico - Spatial Code

# Itsy uses the America_Adm1 shape files. 
# I am updating it to use the GADM shape files. 
# For country:country borders, I don't have GADM LAC specific, so I can use World file. 


rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(maptools)
library(spdep)
library(raster)
library(rgdal)
library(rgeos)
#source("R/translate.r")
library(sp)

library(sf)
library(textclean) # string processing


## Set up countries to loop through:
dogs <- read.csv("~/Code/paho_rabies_hf/manuscript/data/SIRVERA_dogs16_ISO_GID1.csv")
countries <- c(unique(dogs$ADM0_ISO))
countries2 <- countries[-5]
countries2 <- countries2[-24]
countries3 <- countries2[-1:-10] #-2:-3:-4:-5:-6:-7:-8:-9:-10]
cn = "CAN"

for (l in 1:length(countries3)){
  cn <- countries3[l] #country of interest
  
  ## Import shapefile: 
  shp.path <- paste0("data/ShapeFiles_3/", "gadm36_", cn, "_shp/gadm36_", cn, "_1.shp")
  sf.country <- read_sf(shp.path)
  
  ## Convert names from accented to ascii characters. 
  # remember, replacing non-ascii doesn't work at state level (at least for one state in brazil)
  # due to a non-ascii character being the only difference between names. 
  # This step is NOT REQUIRED becuase we have codes. Its just for aesthetics. 
  Encoding(sf.country$NAME_1) <- "UTF-8"
  sf.country$NAME_1 <- replace_non_ascii(sf.country$NAME_1, remove.nonconverted = FALSE)

  # summary of shape file
  head(sf.country)
  dim(sf.country)
  
  
  ###____________________________________________________________________________________________
  ### (1) Adjacency matrix
  unique(sf.country$GID_1)
  unique(sf.country$NAME_1)
  #unique(subset(sf.country, select=c(NAME_1, GID_1)))
  
  ## Function which looks for neighbouring units
  # queen=FALSE: more than single shared boundary point required 
  nb.units.sf <- poly2nb(sf.country, queen=FALSE) 
 
  # save summary of nb units to a file
  summ_file <- paste0("output_adj_mat/", cn, "_nb_units_summary.txt")
  sink(summ_file)
  print(paste("Country:", cn))
  summary.nb(nb.units.sf)
  sink()
  summary.nb(nb.units.sf)
  
  # Create matrix
  mat.sf <- nb2mat(nb.units.sf, style="B", zero.policy=TRUE) #Adjacency matrix of polygons
  dim(mat.sf)
  
  ## Check symmetrical matrix with 0s on diagonal (i.e. not neighbour with self)
  sum(colSums(mat.sf)-as.vector(rowSums(mat.sf))) #should equal 0
  sum(diag(mat.sf))
  paste("CURRENT COUNTRY IS", cn)
  #mat.sf[1:6,1:6]
  
  ## check whether "1" really do stands for the neighbouring units (adjacent)
  # did this manually for mexico because shape file is too large to plot. 
 
#  plot(sf.country)
#  i=17 
#  plot(sp.country[i,], col="red", add=T) #focal polygon
#  nbs <- which(mat.sp[i,]==1) #rows which correspond to neighbours in shapefile
#  plot(sp.country[nbs,],col="blue",add=T) #adjacent polygons
  
 
  # export csv (csv has blank column names at this stage)
  # I guess it relies on everything being in the same order. 
  path_csv_sf <- paste0("output_adj_mat/", cn, "_gadm_adjacency_matrix_sf.csv")
  write.csv(mat.sf, path_csv_sf, row.names=F)
  
  
  ## Create a df with GID_1 code col/row names and export as csv
  mat.sf.df <- data.frame(mat.sf)
  row.names(mat.sf.df) <- sf.country$GID_1
  colnames(mat.sf.df) <- sf.country$GID_1
  head(mat.sf.df)
  #View(mat.sf.df)
  
  path_csv_sf_code <- paste0("output_adj_mat/", cn, "_gadm_adjacency_matrix_sf_code.csv")
  write.csv(mat.sf.df, path_csv_sf_code, row.names=TRUE)
  
  
  ## Create a df with NAME_1 col/row names and export as csv
  # only works if names are first converted to UTF-8 then replace non-ascii
  mat.sf.names.df <- data.frame(mat.sf)
  row.names(mat.sf.names.df) <- sf.country$NAME_1
  colnames(mat.sf.names.df) <- sf.country$NAME_1
  head(mat.sf.names.df)
  #View(mat.sf.names.df)
  
  path_csv_sf_names <- paste0("output_adj_mat/", cn, "_gadm_adjacency_matrix_sf_names.csv")
  write.csv(mat.sf.names.df, path_csv_sf_names, row.names=TRUE)
  
}


# everything above runs fine. 

###____________________________________________________________________________________________
### (2) International Border
## the LAC shapefile has a lot of holes and mismatching boundaries -> poly2nb function
## doesn't work between Mexico and other countries -> used gIntersects insted
## !! also better to read in shapefiles using the readOGR command rather than readShapePoly (fixes some minor discrepancies)

## crop LAC to extent of mex
mex_box <- bbox(mex); mex_box <- extent(mex_box) # bbox = bounding box
mexico <- crop(LAC,mex_box) ## ISSUES!! 100s of errors

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
