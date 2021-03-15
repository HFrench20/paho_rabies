## Check which state names in the surveillance data do not match the state names in the shape files.
## Create a key for matching shape file state names to GID codes in shape files. 

rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(sf) # reads and writes shapefiles using sf object
library(dplyr)
library(textclean) # another thing to help string processing


# load surveillance data with ISO codes (output of script 0.Add_ADM0_code.R )
dogs.ISO <- read.csv("data/SIRVERA_dogs16_ISO.csv")

## Extract ISO code for list of countries I want to loop through.
countries <- c(unique(dogs.ISO$ADM0_ISO))

# Initialise a vector to store list of countries that need updating
countries.to.update <- vector()


for (l in 1:length(countries)){
  # subset one country of interest at a time.
  cn = countries[l]

  ## ------------------ load shape file state names and make ascii -------------------
  
  # create path to shape file for adm_1
  shp.path <- paste0("data/ShapeFiles_3/gadm36_", cn, "_shp/gadm36_", cn, "_1.shp")
  
  # Load shape file for country into object
  cn_ADM1_shp <- read_sf(shp.path)

  # remove geometry to convert to dataframe
  cn_ADM1_df <- st_set_geometry(cn_ADM1_shp, NULL)

  # create vector for NAME_1 only = required as vector by the replace ASCII characters function.
  vector.cn_ADM1_NAME_1 <- cn_ADM1_df$NAME_1

  # replace non-acsii characters in vector with plain ascii using textclean package
  ascii.vector.cn_ADM1_NAME_1 <- replace_non_ascii(vector.cn_ADM1_NAME_1)

  

  ## ----------------------- Creating the first merge key df ----------------------------------
  
  ## Create a data frame with state names and GID_1 codes from shape file.
  ## This data frame will be used specifically for the join.
  
  # Create a vector for GID_1 codes
  vector.cn_ADM1_GID_1 <- cn_ADM1_df$GID_1
  
  # Add "Sin Informacion" to the vectors for NAME_1 and GID_1
  ascii.vector.cn_ADM1_NAME_1 <- c(ascii.vector.cn_ADM1_NAME_1, "Sin Informacion")
  vector.cn_ADM1_GID_1 <- c(vector.cn_ADM1_GID_1, "Sin Informacion")
  
  # Make a data frame of the GID_1 codes and names with accents removed and Sin Informacion added.
  KEY.merge_ascii <- data.frame(ascii.vector.cn_ADM1_NAME_1, vector.cn_ADM1_GID_1)
  KEY.merge_ascii <- rename(KEY.merge_ascii, NAME_1 = ascii.vector.cn_ADM1_NAME_1, GID_1 = vector.cn_ADM1_GID_1) 
  
  # Subset the shape file data frame for NAME_1 and GID_1 to compare to key.
  # Just incase I want to visually check that everything looks ok in the ascii key.
  subset_non_ascii <- subset(cn_ADM1_df, select=c(NAME_1, GID_1))
 
  # Subset the KEY data frame for the anti_join
  KEY.merge_ascii_NAME_1 <- subset(KEY.merge_ascii, select=c(NAME_1))
  

  ## ------------------ prepare surveillance data state names ----------------------

  # filter for one country at a time
  cn.dogs.ISO <- filter(dogs.ISO, ADM0_ISO == cn)
  
  # pull out unique state names and create new df
  cn.dogs.ISO.states <- data.frame(unique(cn.dogs.ISO$UnidMaior))
  
  #rename the column to match for the anti_join function
  cn.dogs.ISO.states <- rename(cn.dogs.ISO.states, NAME_1 = unique.cn.dogs.ISO.UnidMaior.) 

  
  ## ---------------------- Compare state names to Key subset --------------------------------
  
  # anti_join => return all rows from x without a match in y.
  # x = the surveillance data state names
  # y = the shape files state names
  # by = NULL because there is only one variable anyway
  
  # Compare and generate list of surveillance states not matching shape file
  unmatched_states_ascii <- anti_join(cn.dogs.ISO.states, KEY.merge_ascii_NAME_1, by = "NAME_1", copy = TRUE)
  

  
  
  if (nrow(unmatched_states_ascii) == 0) {
    #print(paste("CURRENT COUNTRY =", cn, "is matching already"))
    
    cn.key <- KEY.merge_ascii
    
    # Create a new .csv
    fb <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
    write.csv(cn.key, file = fb, row.names=F)
    
  } else {
    print(paste(cn, "requires the following additions:"))
    #print(paste("State Names in the survieillance data NOT IN THE KEY"))
    print(unmatched_states_ascii)
    print(paste("Current key contains:"))
    print(KEY.merge_ascii)
    
    countries.to.update <- c(countries.to.update, cn)
  }
  
  # Assign country code to variable names so I can keep the objects
  assign(paste0("KEY.merge_ascii.", cn), KEY.merge_ascii)
  assign(paste0("unmatched_states_ascii.", cn), unmatched_states_ascii)
}
  


## --------------------- Manually add required additions --------------------------------

# Refer to Additions output and KEY.merge_ascii to manually check the correct code for unmatched states.
  
# Add the correct GID_1 codes to the unmatched state names data frame in a new column.
# Then I can stack the unmatched_states_ascii (additions) data frame with the current key data frame. 

# Must list them in correct order when adding... 
  
# unmatched_states_ascii.cn[,"GID_1"] <- c("GID", "GID", ..."" )
# unmatched_states_ascii.cn[,"GID_1"] <- cn.additions

# Then run the countries one at a time... 
# because I can't think how to loop through the variables using the cn part of it to match sets of variables.

print(countries.to.update)
# Manually collect codes in correct order to add new column to unmatched.
BOL.additions <- c("BOL.3_1") ## Beni     
CAN.additions <- c("CAN.5_1") ## Newfoundland
CHL.additions <- c("CHL.6_1") ## Biobio
DOM.additions <- c("DOM.7_1", "DOM.12_1", "DOM.2_1") ## El Seibo, Elias Pina, Baoruco
GTM.additions <- c("GTM.13_1") ## Quetzaltenango
MEX.additions <- c("MEX.7_1", "MEX.30_1", "MEX.16_1", "MEX.22_1") ## Coahuila de Zaragoza, Veracruz de Ignacio de la Llave, Michoacan de Ocampo, Queretaro Arteaga
PER.additions <- c("PER.7_1") ## Callao, Provincia Consitucional del


cn = "BOL"
# Add the codes to the unmatched and add the extra rows to the key
unmatched_states_ascii.BOL[,"GID_1"] <- BOL.additions
cn.key <- rbind(KEY.merge_ascii.BOL, unmatched_states_ascii.BOL)
# Create a new .csv
fb <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
write.csv(cn.key, file = fb, row.names=F)

cn = "CAN"
# Add the codes to the unmatched and add the extra rows to the key
unmatched_states_ascii.CAN[,"GID_1"] <- CAN.additions
cn.key <- rbind(KEY.merge_ascii.CAN, unmatched_states_ascii.CAN)
# Create a new .csv
fb <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
write.csv(cn.key, file = fb, row.names=F)

cn = "CHL"
# Add the codes to the unmatched and add the extra rows to the key
unmatched_states_ascii.CHL[,"GID_1"] <- CHL.additions
cn.key <- rbind(KEY.merge_ascii.CHL, unmatched_states_ascii.CHL)
# Create a new .csv
fb <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
write.csv(cn.key, file = fb, row.names=F)

cn = "DOM"
# Add the codes to the unmatched and add the extra rows to the key
unmatched_states_ascii.DOM[,"GID_1"] <- DOM.additions
cn.key <- rbind(KEY.merge_ascii.DOM, unmatched_states_ascii.DOM)
# Create a new .csv
fb <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
write.csv(cn.key, file = fb, row.names=F)

cn = "GTM"
# Add the codes to the unmatched and add the extra rows to the key
unmatched_states_ascii.GTM[,"GID_1"] <- GTM.additions
cn.key <- rbind(KEY.merge_ascii.GTM, unmatched_states_ascii.GTM)
# Create a new .csv
fb <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
write.csv(cn.key, file = fb, row.names=F)

cn = "MEX"
unmatched_states_ascii.MEX[,"GID_1"] <- MEX.additions

cn = "PER"
# Add the codes to the unmatched and add the extra rows to the key
unmatched_states_ascii.PER[,"GID_1"] <- PER.additions
cn.key <- rbind(KEY.merge_ascii.PER, unmatched_states_ascii.PER)
# Create a new .csv
fb <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
write.csv(cn.key, file = fb, row.names=F)



# I wanted to do a for loop method, but this is above my level in R. 
#countries.to.update <- c("BOL", "CAN", "CHL", "DOM", "GTM", "MEX", "PER")
#for (l in 1:length(countries.to.update)){
  # subset one country of interest at a time.
  #cn = countries.to.update[l]
  # somehow select the correct additions variable based on the value of cn. 
  # so for each country:
  #unmatched_states_ascii.cn[,"GID_1"] <- cn.additions
#}

