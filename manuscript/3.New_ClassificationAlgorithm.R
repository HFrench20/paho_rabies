### SIRVERA DATA: Yearly classification from 2000 to 2015
## The script takes pre-calculated monthly outputs for each state in each country from Jan 2000 to 2015
## and classifies each state monthly using the CLASSIFICATION ALGORITHM
# Note Itsy calls 2015 "now", hence current.date variable. 

rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

library(zoo)
source("R/gap_fc.R")
source("R/LRmodel_fc.R")
source("R/RollingWind_fc.R")
#source("R/RemoveWildlifeVar.R")

## --------------------- get list of countries and specify dates ---------------------------

dogs <- read.csv("~/Code/paho_rabies_hf/manuscript/data/SIRVERA_dogs16_ISO_GID1_GID2.csv")
countries <- c(unique(dogs$ADM0_ISO))

# create a vector containing each data (by month) from 2000-01-01 to current date
current.date <- as.Date("2015-12-01")
ms <- seq(as.Date("2000-01-01"), current.date, by="month")


## ----------------------------- run algorithm, loop countries --------------------

for (l in 1:length(countries)){
  cn <- countries[l] #country of interest
  
  # check if the first monthly output file exists and call it
  firstmonth <- paste0("output_script2/", cn, "_MonthlyOutputFor_2000-01-01.csv")
  if (file.exists(firstmonth)) { 
    
    ## import adjecency matrix (my one I created)
    # Use the version that doesn't have any headings (it's what Itsy used)
    adjMat <- read.csv(paste0("output_adj_mat/", cn, "_gadm_adjacency_matrix_sf.csv"))
    
    
    ### ------------- Import data: monthly output statistics on case data -------------
    
    # create empty vector to store the logistic regression coefficients from the model run in script 2. 
    LRcoefsList <- list()
    # for every month in the time frame 2000-01-01 to 2015-12-01, pull out the LRcoefs and store in the vector. 
    for(m in 1:length(ms)){
      LRcoefsList[[m]] <- read.csv(paste0("output_script2/", cn, "_MonthlyOutputFor_", ms[m], ".csv"))
    }
    state_names <- LRcoefsList[[1]]$state
    
    
    ### ------------------------------ Classify -----------------------------
    ## Add trend
    for(i in 1:length(ms)){
      # create a new empty column called trend to store output
      LRcoefsList[[i]]$trend <- NA
      
      # for every row in element of LRcoefsList... (note each row is a state)
      for (j in 1:nrow(LRcoefsList[[i]])){
        
        # ... make the classification for each month
        if(!is.na(LRcoefsList[[i]]$chsq.prob[j]) & LRcoefsList[[i]]$chsq.prob[j]<0.05){
          LRcoefsList[[i]]$trend[j] <- ifelse(LRcoefsList[[i]]$odds[j]>1,"increasing","decreasing")
        }else{
          LRcoefsList[[i]]$trend[j] <- "no trend"
        }
      }
    }
    
    ## run the algorithm
    criteria <- LRcoefsList
    
    for(j in 1:length(ms)){
      # create new empty column called phase
      criteria[[j]]$phase <- NA
      
      
      for(i in 1: nrow(criteria[[j]])){
        if(criteria[[j]]$cases_2years[i]==0){
          ## absent
          criteria[[j]]$phase[i] <- "Absent"
        }else{
          ##endemic or declinig
          conscas <- criteria[[j]]$consecutive_cases_2yr[i]
          criteria[[j]]$phase[i] <- ifelse(criteria[[j]]$trend[i]=="decreasing","Declining", "Endemic")
        }
        if(exists("conscas")){
          if(conscas=="N"){
            ## abs vulnerable, intermittent, declining
            criteria[[j]]$phase[i] <- ifelse(criteria[[j]]$t_nocase_prior_lastcase[i]>24,"Absent-Vulnerable", "Intermittent")
            criteria[[j]]$phase[i] <- ifelse(criteria[[j]]$phase[i]=="Intermittent" &
                                               criteria[[j]]$trend[i]=="decreasing","Declining", criteria[[j]]$phase[i])
          }
          rm(conscas) # consecutive cases
        }
      }
      
      ## Risk of incursions: if neighbours with endemic/declining -> "Absent-Vulnerable"
      ## Risk of incursions: if Chiapas or Mato Grosso do Sul "Absent" -> "Absent-Vulnerable"
      for(i in 1: nrow(criteria[[j]])){
        if (criteria[[j]]$phase[i]=="Absent"){
          nbs <- which(adjMat[i,]==1) #rows which correspond to neighbours in shapefile
          nbp <- criteria[[j]]$phase[nbs]
          ph <- which(nbp=="Endemic"); phh <- which(nbp=="Declining")
          if(length(ph>0) | length(phh>0)){
            criteria[[j]]$phase[i] <- "Absent-Vulnerable"
          }
        }
        ## Chiapas and Matto Grosso do Sol
        #N <- as.numeric(criteria[[j]]$state[i]=="Chiapas" | criteria[[j]]$state[i]=="Mato.Grosso.do.Sul") + as.numeric(criteria[[j]]$phase[i]=="Absent")
        #if (N==2){
        #criteria[[j]]$phase[i] <- "Absent-Vulnerable"
        #}
      }
    }
    
    ## unlist per period, store in matrix and and save ##
    classifications <- matrix(NA, nrow=length(state_names), ncol=length(ms))
    for(i in 1:length(ms)){
      # for each period i
      classifications[,i] <- criteria[[i]]$phase
    }
    classifications <- data.frame(classifications)
    colnames(classifications) <- ms
    classifications <- cbind(state=state_names, classifications)
    
    f <- paste0("output_script3_v2/", cn, "_classified_monthly.csv")
    write.csv(classifications, f, row.names=F)
  } else {
    print(paste("Monthly Output for", cn, "not found."))
  }
}





