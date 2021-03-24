## Check which state names in the surveillance data do not match the state names in the shape files.
## Create a key for matching shape file state names to GID codes in shape files. 
## Update the key to add the alternative spellings with the manually selected correct code. 

# Checks countries one at a time (filter surveillance data, shape files are already separate)


rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(sf) # reads and writes shapefiles using sf object
library(dplyr)
library(textclean) # string processing


# Load surveillance data with ISO codes (output of scripts 0.Add_ADM0_code.R and 0.Add_ADM1_code.R)
dogs.ISO <- read.csv("data/SIRVERA_dogs16_ISO_GID1.csv")

## Extract unique ISO codes for list of countries I want to loop through.
countries <- c(unique(dogs.ISO$ADM0_ISO))

## or choose which countries I want to input
#countries <- c("MEX", "ARG", "BRA")

#Initialise a vector to store the list of countries with no ADM2 level
countries.noadm2 <- vector()

# Initialise vectors to store list of states that need updating
states.to.update <- vector()
all.states.to.update <- vector()

#Store the names of states that need updating in an index
yy <- list()
zz <- list()

for (l in 1:length(countries)){
  # subset one country of interest at a time.
  cn = countries[l]
  print(cn)
  
  # Initialise vector to flag fully matched country
  flag = 0
  
  ## -------------------- load country adm2 shape file and remove geometry  -------------------
  
  # create path to shape file for adm_2
  shp.path <- paste0("data/ShapeFiles_3/gadm36_", cn, "_shp/gadm36_", cn, "_2.shp")
  
  #If shape file for ADM2 exists... continue
  if (file.exists(shp.path)) { 
    
    # Load shape file for country into object
    cn_ADM2_shp <- read_sf(shp.path)
    # remove geometry to convert to dataframe
    cn_ADM2_tibble <- st_set_geometry(cn_ADM2_shp, NULL)
    cn_ADM2_df <- data.frame(cn_ADM2_tibble)
  
    
    ## Table to look up NAME_1 of State by GID
    st_name_lookup <- unique(subset(cn_ADM2_df, select=c(GID_1, NAME_1)))
    
    
    ## ------------------------ Prepare surveillance data ADM2 names --------------------------------
    # Note, only creates keys for states which are in the suerviellance data.
    # To create keys for every ADM2 district, need to re-think structure of script. 
    
    # filter for current country only
    cn.dogs.ISO <- filter(dogs.ISO, ADM0_ISO == cn)
    
    # pull out unique state GID_1 and create vector
    cn.dogs.ISO.GID1.vec <- unique(cn.dogs.ISO$GID_1)
    
    for (gid in 1:length(cn.dogs.ISO.GID1.vec)){
      
      # subset one state of interest at a time.
      st = cn.dogs.ISO.GID1.vec[gid]
      
      # filter the st_name_lookup table to get written name of state using GID
      st_GID_row <- filter(st_name_lookup, GID_1 == st)
      st_NAME_1 <- unique(st_GID_row$NAME_1)
      
      # subset country surveillance data for current State 
      cn.dogs.ISO.st_subset <- filter(cn.dogs.ISO, GID_1 == st)
      
      # replace "NULL" values with "Sin Informacion"
      cn.dogs.ISO.st_subset$UnidMenor[which(cn.dogs.ISO.st_subset$UnidMenor=="NULL")] <- "Sin Informacion"
      
      
      # surveillance data uses unknown encoding...
      
      # Convert UnidMenor to latin1
      Encoding(cn.dogs.ISO.st_subset$UnidMenor) <- "latin1"
      
      # convert UnidMenor to ASCII (note output is a vector by default...)
      cn.dogs.ISO.st_subset.ascii <- replace_non_ascii(cn.dogs.ISO.st_subset$UnidMenor, remove.nonconverted = FALSE)
   
      # convert all to lower case
      cn.dogs.ISO.st_subset.ascii <- tolower(cn.dogs.ISO.st_subset.ascii)
      
      # create a dataframe for the antijoin (and rename the column to NAME_2)
      cn.dogs.ISO.st_subset.adm2 <- data.frame(unique(cn.dogs.ISO.st_subset.ascii))
      cn.dogs.ISO.st_subset.adm2 <- rename(cn.dogs.ISO.st_subset.adm2, NAME_2 = unique.cn.dogs.ISO.st_subset.ascii.)
      
      
    
      ## ------------------------ Prepare Shape File ADM2 names --------------------------------
      
      # subset country Shape File for current State (from surveillance data)
      # the only reason the surveillance GID_1 should not be present in the shape file is if it is Sin Informacion.
      st_ADM2_df <- filter(cn_ADM2_df, GID_1 == st)
      
      # Shape File NAME_2 uses encoding UTF-8
      
      # convert NAME_2 UTF-8 to ASCII (note output is a vector by default...)
      ascii.vector.st_ADM2_NAME_2 <- replace_non_ascii(st_ADM2_df$NAME_2, remove.nonconverted = FALSE)
      
      # convert all to lower case
      ascii.vector.st_ADM2_NAME_2 <- tolower(ascii.vector.st_ADM2_NAME_2)
      
      ##
      
      # Create a vector for GID_2 codes only for current State from Shape File
      vector.st_ADM2_GID_2 <- st_ADM2_df$GID_2
      
      # Add "Sin Informacion" to the shape file vectors for NAME_2 and GID_2
      ascii.vector.st_ADM2_NAME_2 <- c(ascii.vector.st_ADM2_NAME_2, "sin informacion") # use lower case to match
      vector.st_ADM2_GID_2 <- c(vector.st_ADM2_GID_2, "Sin Informacion")
      
      # Make a data frame of the GID_2 codes and names with accents removed and Sin Informacion added.
      KEY.merge_ascii.st <- data.frame(ascii.vector.st_ADM2_NAME_2, vector.st_ADM2_GID_2)
      KEY.merge_ascii.st <- rename(KEY.merge_ascii.st, NAME_2 = ascii.vector.st_ADM2_NAME_2, GID_2 = vector.st_ADM2_GID_2) 
      
      # Subset the shape file data frame for NAME_2 and GID_2 to compare to key.
      # Just incase I want to visually check that everything looks ok in the ascii key.
      #subset_non_ascii <- subset(st_ADM2_df, select=c(NAME_2, GID_2))
      
      # Subset NAME_2 from the KEY data frame for the anti_join test later.
      KEY.merge_ascii.st_NAME_2 <- subset(KEY.merge_ascii.st, select=c(NAME_2))
      
      
      ## -------------------- Compare state names to Key subset (for given country) --------------------------------
      
      # anti_join => return all rows from x without a match in y. (input = two data frames)
      # x = the surveillance data state names
      # y = the shape files state names
      # by = NULL because there is only one variable anyway
      
      # Compare and generate list of surveillance states not matching shape file
      unmatched_adm2_ascii.st <- anti_join(cn.dogs.ISO.st_subset.adm2, KEY.merge_ascii.st_NAME_2, by = "NAME_2", copy = TRUE)
      
      # CHECKS ON THE WAY
      #print(paste("for state:", st, "unique adm 2 are:"))
      #print(cn.dogs.ISO.st_subset.adm2)
      #print(paste("of these, unmatched states are:"))
      #print(unmatched_adm2_ascii.st)
      #print(paste("and the top of the full list of adm2 for this state is:"))
      #print(head(ascii.vector.st_ADM2_NAME_2))
      #print("------------------ NEXT ---------------")
      
      
      if (nrow(unmatched_adm2_ascii.st) == 0) {
        #print(paste("CURRENT STATE =", st, "is matching already"))
        
        st.key <- KEY.merge_ascii.st
        
        # Create a new .csv PER STATE PER COUNTRY
        fb <- paste0("data/cleaning/adm2_keys/", cn, "_", st, "_adm2_key.csv")
        write.csv(st.key, file = fb, row.names=F)
        
      } else {
        print(paste(st, "=", st_NAME_1, "requires the following additions:"))
        #print(paste("ADM2 Names in the survieillance data NOT IN THE KEY"))
        print(unmatched_adm2_ascii.st)
        #print(paste("Current key contains:"))
        #print(KEY.merge_ascii.st)
        print("------------------------NEXT----------------------")
        
        flag = flag + 1
        
        # Assign state code to variable names so I can keep each object (useful for manually adding alternative spellings)
        
        yy[[st]] <- unmatched_adm2_ascii.st
        assign(paste0("unmatched_adm2_ascii.", st), unmatched_adm2_ascii.st)
        
        zz[[st]] <- KEY.merge_ascii.st
        assign(paste0("KEY.merge_ascii.", st), KEY.merge_ascii.st)
        
        
        #st.update <- paste0(st, ".additions")
        states.to.update <- c(states.to.update, paste0(st, ".additions"))
      }
      
    }
  } else {
    
    print(paste("shape file for", cn, "does NOT exist"))
    countries.noadm2 <- c(countries.noadm2, cn)
  }
  if (flag == 0) {
    print(paste("NO CHANGES REQUIRED FOR", cn))
  }
  
  #all.states.to.update <- c(all.states.to.update, states.to.update)
}
  
print(countries.noadm2) 
print(states.to.update)

###############################################################################################
  

## --------------------- Manually add required additions --------------------------------

# Refer to the unmatched_adm2 output and KEY.merge_ascii.st to manually check the correct code for unmatched adm2 names.
  
# Add the correct GID_2 codes to the unmatched adm2 names data frame in a new column.
# Then I can stack the unmatched_adm2_ascii (additions) data frame with the current key data frame. 

# Must list GID_2 codes in correct order when adding to unmatched states... 
  
# unmatched_states_ascii.cn[,"GID_2"] <- c("GID", "GID", ..."" )
# unmatched_states_ascii.cn[,"GID_2"] <- st.additions

# Then run the states one at a time... 
# because I can't think how to loop through the variables using the st part of it to match sets of variables.



## LOOP THROUGH LISTS TO MAKE THE ADDITIONS

# Note: The lists are indexed by POSITION (numerical) so they must all be the same LENGTH and same ORDER.
# yy and zz are autofilled so this is fine, but be careful when populating x!
# I have given them NAMES so this can be extracted for the country / state names, however, 
# the name is not what it is actually indexing by. Need to work out how to do that another day.


# Populate the x list first
# x = GIDs to be added "additions" (see below)
# yy and zz are already populated during the main scrip. But they look like this:

# yy = unmatched 
# yy[["ARG.10_1"]] <- unmatched_adm2_ascii.ARG.10_1
# yy[["ARG.17_1"]] <- unmatched_adm2_ascii.ARG.17_1
# yy[["BOL.2_1"]] <- unmatched_adm2_ascii.BOL.2_1

# zz = keys 
# zz[["ARG.10_1"]] <- KEY.merge_ascii.ARG.10_1
# zz[["ARG.17_1"]] <- KEY.merge_ascii.ARG.17_1
# zz[["BOL.2_1"]] <- KEY.merge_ascii.BOL.2_1

# Populate x list
x <- list()
x[["ARG.10_1"]] <- c("ARG.10.1_1") # dr. manuel belgrano = capital
x[["ARG.17_1"]] <- c("ARG.17.8_1") # general jose de san martin
x[["BOL.2_1"]] <- c("BOL.2.2_1", "BOL.2.1_1") # Bolivar = arque, tiraque = arani
x[["BOL.4_1"]] <- c("BOL.4.17_1") # Murillo = Pedro Domingo Murillo
x[["BOL.8_1"]] <- c("BOL.8.6_1", "BOL.8.10_1") # Warnes = Ignacio Warnes; Obispo Santisteban = Obispo Santistevan
x[["BRA.5_1"]] <- c("BRA.5.81_1") # cansanaao = cancacao 
x[["BRA.10_1"]] <- c("BRA.10.130_1", "BRA.10.86_1") # ; humberto de campos = humberto campos  
x[["BRA.11_1"]] <- c("BRA.11.68_1") # ladario = sapezal 
x[["COL.5_1"]] <- c("COL.5.8_1") # magangue = cicuco
x[["COL.19_1"]] <- c("COL.19.19_1", " COL.19.2_1") #  santa marta = santa marta (dist. esp.), ariguani(el dificil) = ariguani 
x[["COL.29_1"]] <- c("COL.29.40_1") # mariquita = san sebastian de mariquita
x[["CUB.2_1"]] <- c("CUB.2.9_1") # iro de enero = primero de enero  
x[["CUB.3_1"]] <- c("CUB.3.3_1", "CUB.3.2_1") # cienfuegos = ciefuegos; aguada = aguada de pasajeros
x[["CUB.4_1"]] <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA") # NOT COMPLETED, they are all over the place in different states!
x[["CUB.11_1"]] <- c("NA") 
x[["CUB.13_1"]] <- c("NA")
x[["CUB.16_1"]] <- c("NA")
x[["DOM.6_1"]] <- c("DOM.6.6_1") # san fco. de macoris = san francisco de macoris
x[["DOM.7_1"]] <- c("DOM.7.2_1") # el seybo = santa cruz del seybo
x[["DOM.9_1"]] <- c("DOM.9.2_1") # hato mayor = hato mayor del rey
x[["DOM.11_1"]] <- c("DOM.11.2_1", "Sin Informacion") # higuey = salvaleon de higuey; altagracia = name of province
x[["DOM.13_1"]] <- c("DOM.13.2_1") # romana = la romana
x[["DOM.23_1"]] <- c("DOM.23.3_1") # samana = santa barbara de samana
x[["DOM.26_1"]] <- c("DOM.26.5_1", "DOM.26.5_1") # s.j.de la maguana = san juan de la maguana;  san juan = san juan de la maguana
x[["DOM.27_1"]] <- c("DOM.27.4_1", "DOM.27.3_1") #  ing. Quisquella = quisquella; san jose de los llanos = los llanos 
x[["DOM.31_1"]] <- c("Sin Informacion", "Sin Informacion", "Sin Informacion", "DOM.31.6_1") # santo domingo = state name!; villa mella and distrito nacional = STATE OF distrito nacional; la victoria = santo domingo norte
x[["SLV.2_1"]] <- c("SLV.2.2_1") # villa dolores = dolores  
x[["SLV.4_1"]] <- c("SLV.4.9_1", "SLV.4.7_1") # s.jose guayabal = san jose guayabal; s.b.perulapia = san bartolome perulapia 
x[["SLV.5_1"]] <- c("SLV.5.12_1", "SLV.5.3_1") # san juan opico = opico; arce = ciudad arce 
x[["SLV.7_1"]] <- c("SLV.7.16_1") # sta rosa de lima = santa rosa de lima
x[["SLV.9_1"]] <- c("SLV.9.18_1", "SLV.9.16_1") # san rafael oriente = san rafael; san luis de la reyna = san luis de la reina 
x[["SLV.10_1"]] <- c("SLV.10.6_1", "SLV.10.5_1") #  el paisal = el paisnal; ciudad delgado = delgado   
x[["SLV.12_1"]] <- c("SLV.12.1_1") # cand.de la frontera = candelaria de la frontera
x[["SLV.13_1"]] <- c("SLV.13.10_1") # s.ant.del monte = san antonio del monte
x[["SLV.14_1"]] <- c("SLV.14.11_1", "SLV.14.6_1", "SLV.14.16_1", "SLV.14.19_1") # m.umana = mercedes umana; ereguaiquin = ereguayquin; san buena ventura = san buenaventura; sta elena = santa elena
x[["GTM.1_1"]] <- c("GTM.1.4_1") # fray b. de las cajas =  fray bartolome de las casas
x[["GTM.3_1"]] <- c("GTM.3.11_1") # s.m.jilotepeque = san martin jilotepeque 
x[["GTM.5_1"]] <- c("GTM.5.4_1") # s.a.acasaguastlan = san agustin acasaguastlan
x[["GTM.6_1"]] <-c("GTM.6.12_1") # siquinalan = siquinala
x[["GTM.7_1"]] <- c("Sin Informacion", "GTM.7.12_1", "GTM.7.10_1") # guatemala = Sin Informacion; san pdo. Sacatepequez = san pedro sacatepequez; s.j.sacatepequez = san juan sacatepequez    
x[["GTM.8_1"]] <- c("GTM.8.21_1", "GTM.8.14_1", "GTM.8.10_1", "GTM.8.10_1", "GTM.8.10_1", "GTM.8.10_1") # san rafael = san rafael petzal; ildefonso ixtahuacan = san ildefonso ixtahuacan; all other are Malacatancito based on coordinates.
x[["GTM.13_1"]] <- c("GTM.13.14_1", "GTM.13.16_1", "GTM.13.16_1", "GTM.13.7_1", "GTM.13.16_1", "GTM.13.15_1", "GTM.13.15_1") # san juan oztuncalco = ostuncalco; zona 1, zona 5, zona 3 = quetzaltenango; concepcion = concepcion chiquirichapa; palestina, p.de los altos = palestina de los altos 
x[["GTM.14_1"]] <- c("GTM.14.15_1", "GTM.14.19_1", "GTM.14.17_1") # santo antonio ilote = san antonio ilotenango; santa cruz = santa cruz del quiche; cotzal = san juan cotzal 
x[["GTM.17_1"]] <- c("GTM.17.16_1", " GTM.17.25_1", "GTM.17.1_1", "GTM.17.6_1", "GTM.17.27_1") #san antonio = san antonio sacatepequez; sibinal = san sibinal; tecunuman = ayutla; san jose el rode = el rodeo;  tasana = tacana
x[["GTM.19_1"]] <- c("GTM.19.13_1") #  ixtahuacan = santa catarina ixtahuacan
x[["GTM.20_1"]] <- c("GTM.20.10_1", "GTM.20.19_1") # s.f.zapotitlan = san francisco zapotitlan; santo tomas = santo tomas la union
x[["GTM.21_1"]] <- c("GTM.21.4_1", "GTM.21.5_1") # san cristobal = san cristobal totonicapan; san francisco = san francisco el alto
x[["MEX.5_1"]] <- c("MEX.5.104_1") # venustiano carr. = venustiano carranza 
x[["MEX.15_1"]] <- c("Sin Informacion") # mexico = state name!
x[["MEX.22_1"]] <- c("MEX.22.7_1") # e.montes = ezequiel montes
x[["MEX.31_1"]] <- c("MEX.31.79_1") # tekak = tekax 
x[["NIC.13_1"]] <- c("NIC.13.2_1") # silais = managua
x[["PRY.7_1"]] <- c("PRY.7.4_1") # buena vista = emboscada (caazapa)
x[["PER.16_1"]] <- c("Sin Informacion") # Lima = city, so wrong state.
x[["VEN.25_1"]] <- c("VEN.25.8_1", "VEN.25.7_1") # jesus enrique lozada = jesus enrique lossada; paez = guajira



#check x yy and zz are all the same length vector (otherwise it will get out of sync)
# I can control for length in this way, but not order, so order must be correct.
# (until I can set the string values as the actual index)
if (length(x) == length(yy) & length(x) == length(zz)) {
  
  # Extract names of objects from the x list (or any of the lists)
  cnst_names <- names(x)
  
  # loop through the indexed lists (not indexed by number so must all be same length)
  for (vec in 1:length(x)){
    unmatched = yy[vec]
    st.adds = x[vec]
    key = zz[vec]
    # subset state and country name from the index list
    st = cnst_names[vec]
    cn = substr(st, 1, 3)
    
    print(paste("X VARIABLE GIDs to add to", st, "are:")) 
    print(st.adds[[1]])
    
    print(paste("YY VARIABLE unmatched are:"))
    print(unmatched)
    
    unmatched.ready <- unmatched[[1]]
    
    # add the x additions to the unmatched in a new column
    unmatched.ready[,"GID_2"] <- st.adds[[1]]
    #print(unmatched.ready)
    
    
    key.ready <- key[[1]]
    
    # append the newly joined gids/additions to the main key
    st.key <- rbind(key.ready, unmatched.ready)
    
    # create path and write new csv to CREATE the key
    fb <- paste0("data/cleaning/adm2_keys/", cn, "_", st, "_adm2_key.csv")
    write.csv(st.key, file = fb, row.names=F)
    
    unmatched.ready <- vector()
    
  } 
}




## Single addition add to key

cn = "BOL"
# Add the codes to the unmatched and add the extra rows to the key
unmatched_states_ascii.BOL[,"GID_1"] <- BOL.additions
cn.key <- rbind(KEY.merge_ascii.BOL, unmatched_states_ascii.BOL)
# Create a new .csv
fb <- paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv")
write.csv(cn.key, file = fb, row.names=F)



