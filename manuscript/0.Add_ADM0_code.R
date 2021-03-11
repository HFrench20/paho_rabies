### Scripts for adding ADM0 ISO code to data.csv based on country name match.


rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(ISOcodes)
library(dplyr)

## Input data
dogs <- read.csv("~/Code/paho_rabies_hf/manuscript/data/SIRVERA_dogs16(clean_statenames).csv")

## edit country names to match "Name" field in ISO_3166_1 
dogs$Pais[which(dogs$Pais=="Bolivia")] <- "Bolivia, Plurinational State of"
dogs$Pais[which(dogs$Pais=="French Guyana")] <- "French Guiana"
dogs$Pais[which(dogs$Pais=="United States of America")] <- "United States"
dogs$Pais[which(dogs$Pais=="Venezuela")] <- "Venezuela, Bolivarian Republic of"

## subset Name and Alpha_3 field from ISO_3166
alpha.codes <- subset(ISO_3166_1, select = c(Name, Alpha_3))


# Merge function Left outer join.
# => Return all rows from the left table, and any rows with matching keys from the right table.
dogs.alpha <- merge(x = dogs, y = alpha.codes, by.x = "Pais", by.y = "Name", all.x = TRUE)


# Rename and move column
dogs.alpha <- rename(dogs.alpha, ADM0_ISO = Alpha_3) 
dogs.alpha <- subset(dogs.alpha, select=c(Ano:Pais, ADM0_ISO, Mes:matchcode))

# Create a new .csv
write.csv(dogs.alpha, file="clean_dogs_2016_ISO.csv", row.names=F)
