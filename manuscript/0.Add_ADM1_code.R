### Scripts for adding ADM1 GID_1 code to surveillancedata.csv using country keys.
## Country keys = data frames storing different spellings of state name matched to the correct GID code.
## GID codes are obtained from GADM shape file data frames. 

## Prerequisite 1: Updated Country Key must be prepared (use script 0.CreateStateName_Key)
## Prerequisite 2: Surveillance data must have ISO codes already added. 


rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(dplyr)


## ----------------------- Input data, get countries ----------------------------

## Input data
dogs.ISO <- read.csv("~/Code/paho_rabies_hf/manuscript/data/SIRVERA_dogs16_ISO.csv")

## Extract ISO code for list of countries I want to loop through.
countries <- c(unique(dogs.ISO$ADM0_ISO))

# Initialise a data frame with correct column names that the merged data frames will be stacked back to.
cols <- colnames(dogs.ISO)
cols.gid <- c(cols, "GID_1")
stacking.df <- as.data.frame(matrix(ncol = length(cols.gid), nrow=0, dimnames = list(NULL,cols.gid)))


## --------- Loop, merging surveillance data to key one country at a time --------------

for (l in 1:length(countries)){
  ## subset one country of interest at a time.
  cn = countries[l]

  # Load merge key for that country.
  key.path <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
  key.cn <- read.csv(key.path)

  # Filter surveillance data for that country
  filtered.cn <- filter(dogs.ISO, ADM0_ISO == cn)
  
  # Merge to add the codes to the filtered surveillance data. 
  # all.x = TRUE means all values of the x table are printed.
  merged.cn <- merge(x = filtered.cn, y = key.cn, by.x = "UnidMaior", by.y = "NAME_1", all.x = TRUE)
  
  # Stack is onto the stacking data frame
  stacking.df <- rbind(stacking.df, merged.cn)
}
 
## ------------------------- Tidy final output and export ---------------------------

# rearrange columns to a nice order
dogs.ISO.state <- subset(stacking.df, select=c(Ano:Mes, UnidMaior, GID_1, GeoCodUnidMaior, UnidMenor, GeoCodUnidMenor, Especie:matchcode))

# Create a new .csv
fb <- paste0("data/", "SIRVERA_dogs16_ISO_GID1.csv")
write.csv(dogs.ISO.state, file = fb, row.names=F)







