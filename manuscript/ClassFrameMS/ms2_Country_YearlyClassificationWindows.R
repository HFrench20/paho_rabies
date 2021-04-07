### SIRVERA DATA: Mexico & Brazil yearly classification from 2005 to 2015
## 1. The first part of the script takes SIRVERA data (raw SIRVERA data with attempted name corrections), 
## subsets for Mexico and Brazil and produces
## consolidated monthly time series of cases between Jan 1995 to Dec 2015 by country and by state.

rm(list=ls())
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(maptools)
library(zoo)
library(lubridate)
source("R/states_ts.r")

## data
dogs <- read.csv("data/SIRVERA_dogs16(clean_statenames).csv")
countries <- c("Mexico", "Brazil")

## set all dates from Jan 1995 to Dec 2015
dates <- seq(as.Date("1995-01-01"), as.Date("2015-12-01"), by="months")
dates <- strftime(strptime(dates, format="%Y-%m-%d"),"%Y-%m")

###-------------------------------Subset Cases Data-------------------------------###
names <- countries 

for (l in 1:length(countries)){
  ## subset country and yrs of interest
  cn = countries[l] #country of interest
  yr = 1995 
  
  country <- subset(dogs, Pais == cn & Ano >= yr)
  yrs <- sort(unique(country$Ano))
  
  country.f <- paste0("data/America_Adm_1/", cn, ".shp")
  sp.country <- readShapePoly(country.f)
  states <- sp.country@data$ADM1_NAME  
  
  ## sort out full dates (months included): Converted month 0 to 1 bc do not want to exclude evidence of circulation
  country$Mes[which(country$Mes=="0")] <- 1
  country$date <- as.Date(as.yearmon(paste(country$Ano,country$Mes, sep="-")))
  country$date <- strftime(strptime(country$date, format="%Y-%m-%d"),"%Y-%m")
  
  ## timeseries: sum all cases for each month and year/ per each state
  country_ts = states_ts(dates = dates, states = states, data = country) 
  fb <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_state9515.csv")
  write.csv(country_ts,fb,row.names=F)
  
  print(setdiff(unique(country$UnidMaior),states))
}

###-------------------------------Create Timeseries-------------------------------###
## 2. The second part of the script takes monthly cases data for Mexico and Brazil (prepared above)
## and classifies each state yearly using different lengths/time windows of data fitted to
## logistic regresion (from 2yrs up to 10 yrs of data) via:
## a) case transformation to binary data expanded by using the rolling window method,
## b) output statistics from logistic regression models and pattern detected from case incidence data,
## c) applying CLASSIFIACTION ALGORITHM
## output: nine .csv files (each for one time window) for each country with states classified yearly from 2005 to 2015

rm(list=ls())
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(zoo)
source("R/gap_fc.r")
source("R/LRmodel_fc.r")
source("R/RollingWind_fc.r")

countries <- c("Mexico", "Brazil")

for (l in 1:length(countries)){
  cn = countries[l] #country of interest
  
  # import data
  fstates <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_state9515.csv") 
  states <- read.csv(fstates) # monthly detected cases by state 
  
  ## remove wildlife variants
  wildvars <- read.csv(paste0("ClassFrameMS/data/", cn, "_WildVariants.csv"))
  names <- gsub("[.]", " ", colnames(states)[1:(ncol(states)-1)])
  for(i in 1:nrow(wildvars)){
    cind <- which(names==wildvars$State[i])
    rind <- which(states$date==as.character(wildvars$Date[i]))
    states[rind, cind] <-  states[rind, cind]-1
  }
  
  # HARD CODE IMPORTANT VALUES (so not recalculating throughout functions etc)
  mths  <-  nrow(states)
  state_gaps <- list()
  state_names <- colnames(states)[1:(ncol(states)-1)]
  n  <-  length(state_names)
  yrs  <-  length(unique(strftime(strptime(as.Date(as.yearmon(states$date)), format="%Y-%m-%d"),"%Y")))
  Y2  <-  24 # period of two years
  
  ### for different time windows ###
  timewindows <- 2:10*12 # how many years of data to be fitted
  states.fulldata <- states 
  
  for (w in 1:length(timewindows)){
    ### CASE DETECTION - Logistic Regression
    LRcoefs <- data.frame(state = state_names,
                          chsq.prob = rep(NA, n),
                          odds = rep(NA, n),
                          pval = rep(NA, n), 
                          cases_yearly=numeric(n),
                          max_cases_monthly=numeric(n),
                          cases_2years=numeric(n),
                          max_gaplength=numeric(n),
                          mean_gaplength=numeric(n),
                          t_since_last_case=numeric(n),
                          t_nocase_prior_lastcase=numeric(n),
                          consecutive_cases_2yr=rep(NA, n))
    
    ## case presnce: rolling window method
    RWoutput <- RollingWind(states.fulldata, mths)
    presence <- RWoutput$presence
    presence.rw <- RWoutput$presence.rw
    presence.rw.fulldata <- presence.rw
    
    ## working back from current date to the last year period
    ## CREATE WINDOWS
    ms <- rev(seq(12,mths,12))
    ms <- ms[1:which(ms==rev(timewindows)[1])] #remove the last year
    ms.rw <- ms
    ms.rw[1] <- ms[1]-1
    
    ## initiate: output stored in a list of df by 6 month periods
    LRcoefsList <- list() ## store LR coeffs for truncated periods (length(ms)=13)
    length(LRcoefsList) <- length(ms)
      
    ## prep data for class algorithm (LR)
    for (r in 1:length(ms)){ #for each year in r
      end.period <- ms[r]-(timewindows[w]-1)
      states <- states.fulldata[end.period:ms[r],]
      presence.rw <- presence.rw.fulldata[end.period:ms.rw[r],]
      
      ### INTERVALS BETWEEN DETECTED CASES: gaps for each state individually 
      for(d in 1:n){
        state_cases <- states[,d]
        gaps <- gap_fc(state_cases, l=length(state_cases))
        state_gaps[[d]] <- gaps
      }
      names(state_gaps) <- state_names
      
      for (i in 1:n){ #for each state in n
        LRcoefs$cases_yearly[i] <- sum(states[,i])/(nrow(states)/12)
        LRcoefs$max_cases_monthly[i] <- max(states[,i])
        LRcoefs$cases_2years[i] <- ifelse(length(states[,i])>24,sum(states[(nrow(states)-Y2):nrow(states),i]), sum(states[,i]))
        LRcoefs$max_gaplength[i] <- max(state_gaps[[i]]) ## max gap length
        LRcoefs$mean_gaplength[i] <- mean(state_gaps[[i]]) ## mean gap length
        LRcoefs$t_since_last_case[i] <- state_gaps[[i]][length(state_gaps[[i]])] ## time since last case
        LRcoefs$t_nocase_prior_lastcase[i] <- ifelse(length(state_gaps[[i]])>1,
                                                     state_gaps[[i]][(length(state_gaps[[i]])-1)],0) ## with no case prior to last case
        
        ## are there cases present over consecutive months in last 2 yrs
        end <- length(states[,i])
        bincases <- ifelse(states[(end-Y2):end,i]>0,1,0) 
        cons <- rle(bincases)
        ind <- which(cons$length[cons$values == 1]>1)
        LRcoefs$consecutive_cases_2yr[i] <- ifelse(length(ind)>0,"Y","N") 
        
        state <- data.frame(counts=presence.rw[,i], months=1:nrow(presence.rw))    
        
        ## only fit if at least 2 cases and 1 zeros!
        if(sum(state$counts)>1 & sum(state$counts)!=length(state$counts)){
          m = LRmodel(data <- state) ## LR model
          LRcoefs$chsq.prob[i] <- m$chsq.prob ## check whether better than a null model
          LRcoefs$odds[i] <- m$odds  ## check whether increasing or decreasing trend
          LRcoefs$pval[i] <- m$pval
        }
        print(paste0("i",i))
      }
      LRcoefsList[[r]] <- LRcoefs
      print(r)
    } 
    
    ###Classify
    ## Add trend
    for(i in 1:length(ms)){
      LRcoefsList[[i]]$trend <- NA
      for (j in 1:nrow(LRcoefsList[[i]])){
        if(!is.na(LRcoefsList[[i]]$chsq.prob[j]) & LRcoefsList[[i]]$chsq.prob[j]<0.05){
          LRcoefsList[[i]]$trend[j] <- ifelse(LRcoefsList[[i]]$odds[j]>1,"increasing","decreasing")
        }else{
          LRcoefsList[[i]]$trend[j] <- "no trend"
        }
      }
    }
    
    ## import adjecency matrix
    adjMat <- read.csv(paste0("output/", cn, "/", cn, "_adjacency_matrix.csv"))
    
    ## run the algorithm
    criteria <- LRcoefsList
    for(j in 1:length(ms)){
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
          rm(conscas)
        }
      }
      
      ## Risk of incursions?: if neighbours with endemic/declining -> "Absent-Vulnerable"
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
        N <- as.numeric(criteria[[j]]$state[i]=="Chiapas" | criteria[[j]]$state[i]=="Mato.Grosso.do.Sul") + as.numeric(criteria[[j]]$phase[i]=="Absent")
        if (N==2){
          criteria[[j]]$phase[i] <- "Absent-Vulnerable"
        }
      }
    }
      
    ## unlist per period, store in matrix and and save ##
    classifications <- matrix(NA, nrow=length(state_names), ncol=length(ms))
    for(i in 1:length(ms)){
      # for each period i
      classifications[,i] <- criteria[[i]]$phase
    }
    classifications <- data.frame(classifications)
    colnames(classifications) <- paste0("yr",2015:2004)
    classifications <- cbind(state=state_names, classifications)
    
    f <- paste0("ClassFrameMS/output/",cn,"/", cn, "_classified_yearlyTW",timewindows[w]/12,"yr.csv") 
    write.csv(classifications,f, row.names=F)
  }
}

  



