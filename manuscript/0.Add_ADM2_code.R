### Scripts for adding ADM2 GID_2 code to surveillancedata.csv using country keys.
## Country keys = data frames storing different spellings of sub-state name matched to the correct GID_2 code.
## GID codes are obtained from GADM shape file data frames. 

## Prerequisite 1: Updated Country Key must be prepared (use script 0.CreateAdm2Name_Keys)
## Prerequisite 2: Surveillance data must have ISO codes and GID_1 codes already added. 


rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(dplyr)
library(tibble)

total = 0

## ----------------------- Input data, get countries ----------------------------

## Input data (must be the set with GID1 included already)
dogs.ISO <- read.csv("~/Code/paho_rabies_hf/manuscript/data/SIRVERA_dogs16_ISO_GID1.csv")

## Extract ISO codes for list of countries I want to loop through.
countries <- c(unique(dogs.ISO$ADM0_ISO))

# Initialise a data frame with correct column names that the merged data frames will be stacked back to.
cols <- colnames(dogs.ISO)
cols.gid <- c(cols, "GID_2")
stacking.df <- as.data.frame(matrix(ncol = length(cols.gid), nrow=0, dimnames = list(NULL,cols.gid)))


## --------- Loop, merging surveillance data to key one country at a time --------------


for (l in 1:length(countries)){
  ## subset one country of interest at a time.
  cn = countries[l]

  # filter for current country only
  cn.dogs.ISO <- filter(dogs.ISO, ADM0_ISO == cn)
  
  ## Pull out states per country (because the keys are at state level)
  cn.dogs.ISO.GID1.vec <- unique(cn.dogs.ISO$GID_1)
  

  # Not all countries have adm2 level data. So first check if key exists. 
  # Use Sin Informacion file because all countries should have one... 
  # To make this more reliable I could use a wild card or create a marker file in the 0.createadm2key script.
  if (file.exists(paste0("data/cleaning/adm2_keys/", cn, "_Sin Informacion_adm2_key.csv"))) {

    print(paste(cn, "has ADM2 shape file"))
    
    for (gid in 1:length(cn.dogs.ISO.GID1.vec)){
      
      # subset one state of interest at a time.
      st = cn.dogs.ISO.GID1.vec[gid]
      print(st)
      
      # Load merge key for that state.
      key.path <- paste0("data/cleaning/adm2_keys/", cn, "_", st, "_adm2_key.csv")
      key.st <- read.csv(key.path)
      
      # Filter country surveillance data for that state.
      filtered.st <- filter(cn.dogs.ISO, GID_1 == st)
      
      unid.menor.vector <- as.vector(filtered.st$UnidMenor)
      
      # replace "NULL" values with "Sin Informacion"
      unid.menor.vector[which(unid.menor.vector=="NULL")] <- "Sin Informacion"
      # Convert UnidMenor to latin1
      Encoding(unid.menor.vector) <- "latin1"
      # convert UnidMenor to ASCII (note output is a vector by default...)
      unid.menor.vector <- replace_non_ascii(unid.menor.vector, remove.nonconverted = FALSE)
      # convert all to lower case
      unid.menor.vector <- tolower(unid.menor.vector)
     
       #partial <- length(unid.menor.vector)
      #print(partial)
      #total = total + partial
      #print(total)
      
      # join the vector back as column UnidMenor_lower
      filtered.st[,"UnidMenor_lower"] <- unid.menor.vector
      
      # Merge to add the codes to the filtered surveillance data. 
      # all.x = TRUE means all values of the x table are printed.
      merged.st.lower <- merge(x = filtered.st, y = key.st, by.x = "UnidMenor_lower", by.y = "NAME_2", all.x = TRUE)
      nrowsmerged <- nrow(merged.st.lower)
      print(paste("rows of merged table is", nrowsmerged))
      
      # Remove the UnidMenor_lower by subsetting
      merged.st <- subset(merged.st.lower, select=c(Ano:GID_2))
      
      
      # Stack onto the stacking data frame
      stacking.df <- rbind(stacking.df, merged.st)
      
      #print(paste(st, "done............"))
    }
  } else { 
    # if the country has no adm2 level shape file, I want the GID_2 code to say NA.
    
    print(paste(cn, "does NOT have adm2 shape file"))
    
    #cn.dogs.ISO.gid2 <- cn.dogs.ISO[,"GID_2"] <- NA
    cn.dogs.ISO.gid2 <- cn.dogs.ISO %>% add_column(GID_2 = NA)
    stacking.df <- rbind(stacking.df, cn.dogs.ISO.gid2)
    
    }
  
  dfrows <- nrow(stacking.df)
  print(paste(cn, "done....... stacking.df has rows:", dfrows))
}
 
nrow(dogs.ISO)

# Note, there is one extra row being added in Brazil and I don't know why!
# "BRA.10_1" has 86 rows before merge, but 87 rows after merge.
# tutoia has two entries in the key! BRA.10.210_1 and BRA.10.211_1 so they both get added!

# Next time, fast way to check is to check for duplications in the key first :)

# fix it in this instance using code to remove the false duplication:
new_stacking.df <- drop_row(stacking.df, "GID_2", "BRA.10.210_1")
nrow(new_stacking.df)

## ------------------------- Tidy final output and export ---------------------------

# rearrange columns to a nice order

#dogs.ISO.adm2 <- subset(stacking.df, select=c(Ano:UnidMenor, GID_2, GeoCodUnidMenor:matchcode))
dogs.ISO.adm2 <- subset(new_stacking.df, select=c(Ano:UnidMenor, GID_2, GeoCodUnidMenor:matchcode))

# Create a new .csv
fb <- paste0("data/", "SIRVERA_dogs16_ISO_GID1_GID2.csv")
write.csv(dogs.ISO.adm2, file = fb, row.names=F)







