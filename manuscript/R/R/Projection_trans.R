#get the projection right
p4s2 <- CRS(proj4string <- "+proj=utm +zone=36 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
proj4string(XX) <-p4s2
p4s_default <- CRS("+proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
XX.transformed <- spTransform(XX, p4s_default)
