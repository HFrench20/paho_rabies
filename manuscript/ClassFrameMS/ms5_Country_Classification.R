### SIRVERA DATA: Classification Algorithm for Rabies Epidemiological Situations
## This script takes output statistics from logistic regression models and adjacency matrices 
## from Mexico and Brazil classifies each state based on the series of critera decided as 
## CLASSIFICATION ALGORITHM (aka tool or framework)

rm(list=ls())
#setwd("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis")
setwd("~/Dropbox/PAHOsurveillance/Analysis")

countries <- c("Mexico", "Brazil") 

for (l in 1:length(countries)){
  ## subset country and yrs of interest
  cn <- countries[l]
  
  ## import
  adjMat <- read.csv(paste0("output/", cn, "/", cn, "_adjacency_matrix.csv"))
  LRcoefs <- read.csv(paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesLRcoeffs_states15.csv"))
  
  ## Add trend ##
  LRcoefs$trend <- NA
  for (i in 1:nrow(LRcoefs)){
    if(!is.na(LRcoefs$chsq.prob[i]) & LRcoefs$chsq.prob[i]<0.05){
      LRcoefs$trend[i] <- ifelse(LRcoefs$odds[i]>1,"increasing","decreasing")
    }else{
      LRcoefs$trend[i] <- "no trend"
    }
  }
  
  ### CLASSIFICATION ALGORITHM 
  ## run the algorithm
  criteria <- LRcoefs
  row.names(criteria) <- 1:nrow(criteria)
  criteria$phase <- NA
  
  for(i in 1: nrow(criteria)){  
    if(criteria$cases_2years[i]==0){
      ## absent?
      criteria$phase[i] <- "Absent" 
    }else{
      ##endemic or declinig
      conscas <- criteria$consecutive_cases_2yr[i]
      criteria$phase[i] <- ifelse(criteria$trend[i]=="decreasing","Declining", "Endemic")
    }
    if(exists("conscas")){
      if(conscas=="N"){
        ## abs vulnerable, intermittent, declining
        criteria$phase[i] <- ifelse(criteria$t_nocase_prior_lastcase[i]>24,"Absent-Vulnerable", "Intermittent")
        criteria$phase[i] <- ifelse(criteria$phase[i]=="Intermittent" & 
                                      criteria$trend[i]=="decreasing","Declining", criteria$phase[i])                         
      }
      rm(conscas)
    }
  }
  
  ## Risk of incursions?
  for(i in 1: nrow(criteria)){
    if (criteria$phase[i]=="Absent"){
      nbs <- which(adjMat[i,]==1) #rows which correspond to neighbours in shapefile
      nbp <- criteria$phase[nbs]
      ph <- which(nbp=="Endemic"); phh <- which(nbp=="Declining")
      if(length(ph>0) | length(phh>0)){
        criteria$phase[i] <- "Absent-Vulnerable"
      }
    }
    ## Chiapas and Matto Grosso do Sol
    N <- as.numeric(criteria$state[i]=="Chiapas" | criteria$state[i]=="Mato.Grosso.do.Sul") + as.numeric(criteria$phase[i]=="Absent")
    if (N==2){
      criteria$phase[i] <- "Absent-Vulnerable"
    }
  }
  
  head(criteria); tail(criteria)
  
  ## save
  write.csv(criteria,paste0("ClassFrameMS/output/", cn, "/", cn, "_Classified_states.csv"), row.names=F)
  print(l)
}








