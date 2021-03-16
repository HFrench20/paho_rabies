### Script for adding ADM0 ISO code to data.csv based on country name match.
## ISO codes are based on IS0_3166_1...
## ...compare SIRVERA names to the ISO names using anti_join, and update as required.

rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(ISOcodes)
library(dplyr)


## --------- Check what SIRVERA country names need updating to match ISO and edit -------------

## Input data
dogs <- read.csv("~/Code/paho_rabies_hf/manuscript/data/SIRVERA_dogs16(clean_statenames).csv")

## Compare surveillance (SIRVERA) country names to ISO country names using anti_join
sirv.countries <- subset(dogs, select=c(Pais))
sirv.countries <- rename(sirv.countries, Name = Pais) 
unmatched_countries <- anti_join(sirv.countries, ISO_3166_1, by = "Name", copy = TRUE)
unique(unmatched_countries)

## edit country names to match "Name" field in ISO_3166_1 
dogs$Pais[which(dogs$Pais=="Bolivia")] <- "Bolivia, Plurinational State of"
dogs$Pais[which(dogs$Pais=="French Guyana")] <- "French Guiana"
dogs$Pais[which(dogs$Pais=="United States of America")] <- "United States"
dogs$Pais[which(dogs$Pais=="Venezuela")] <- "Venezuela, Bolivarian Republic of"


## ------------------- Create Key from ISO table and merge ---------------------------------

## subset Name and Alpha_3 field together from ISO_3166 (to make a merging key)
alpha.codes <- subset(ISO_3166_1, select = c(Name, Alpha_3))

# Merge function Left outer join to add ISO Alpha_3 codes.
# => Return all rows from the left table, and any rows with matching keys from the right table.
# => Left table = dogs, match by "Pais"; Right table = subsetted ISOcodes, match by "Name" of country.
dogs.alpha <- merge(x = dogs, y = alpha.codes, by.x = "Pais", by.y = "Name", all.x = TRUE)


## ----------------- Tidy up output and export --------------------------------------

# Rename and move column (note the merging moved some columns)
dogs.alpha <- rename(dogs.alpha, ADM0_ISO = Alpha_3) 
dogs.alpha <- subset(dogs.alpha, select=c(Ano:Sub_Regiao, Pais, ADM0_ISO, Mes:matchcode))

# Create a new .csv
fb <- paste0("data/", "SIRVERA_dogs16_ISO.csv")
write.csv(dogs.alpha, file = fb, row.names=F)




