### SIRVERA DATA: Timeseries (cases & gaps) for Mexico and Brazil
## This script produces series of output information and plots for case data from
## Mexico and Brazil from between Jan 2005 and Dec 2015
## output data:
## a) case binary-transformed data (absence/presence); and binary data expanded using the rolling window method,
## b) output statistics from logistic and poisson regression models (by state; for time periods of 2005-2010 and 2010-2015),
## plots:
## a) intervals between cases (periods of no cases recorded) by country and for each state,
## b) incidence and presence/absence data with fitted lines from poisson and logictic regressions respectively (by state; for time periods of 2005-2010 and 2010-2015),
## c) incidence data with fitted lines from poisson regressions and inserts of fitted lines from logistic regressions (by state; for time periods of 2005-2010 and 2010-2015)

rm(list=ls())
#setwd("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis")
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(zoo)
source("R/gap_fc.r")
source("R/LRmodel_fc.r")
source("R/PoissNBmodel_fc.r")
source("R/RollingWind_fc.r")

### MEXICO: WILDLIFE REMOVED ### 
## Directories setting & info
cn <- "Mexico"

fcases <- paste0("ClassFrameMS/output/",cn,"/", cn, "_MonthlyCases0515.csv")  
fstates <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_state0515.csv") 

# Read data - generated from: SIRVERA_Country.R
cases <- read.csv(fcases) # monthly detected cases 
states <- read.csv(fstates) # monthly detected cases by state 
wildvars <- read.csv("ClassFrameMS/data/Mexico_WildVariants.csv") # wildlife variants

# HARD CODE IMPORTANT VALUES (so not recalculating throughout functions etc)
mths  <-  nrow(states)
state_gaps <- list()
state_names <- colnames(states)[1:(ncol(states)-1)]
n  <-  length(state_names)
yrs  <-  length(unique(strftime(strptime(as.Date(as.yearmon(states$date)), format="%Y-%m-%d"),"%Y")))
Y2  <-  24 # PERIOD OF 2 YEARS

## remove wildlife variants if available
names <- gsub("[.]", " ", state_names)
for( i in 1:nrow(wildvars)){
  cind <- which(names==wildvars$State[i])
  rind <- which(states$date==as.character(wildvars$Date[i]))
  states[rind, cind] <-  states[rind, cind]-1
  print(states[rind, cind])
}

### INTERVALS BETWEEN DETECTED CASES
allgaps <- gap_fc(cases$cases, l=nrow(cases))

fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_monthly_Gaps.pdf")
pdf(fpdf, width=5.5, height=7)
par(mfrow = c(2,1),mar=c(4,5,2,3))
barplot(allgaps, xlab="", ylim=c(0,max(allgaps)),
        ylab="Months between cases", col="darkgrey", border="darkgrey", main="")
case_intervals = hist(allgaps, breaks=seq(-1,max(allgaps),1), plot=FALSE)
barplot(case_intervals$density[-1], xlab="Months between cases", ylab="Frequency",
        col="darkgrey", border="darkgrey", main="", names.arg = 1:max(allgaps))
dev.off()

## gaps for each state individually 
fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_Gaps_stateHist.pdf")
pdf(fpdf, width=9, height=5)
par(mfrow=c(4,7), cex=0.4, mar=c(5,3,2,1), mgp=c(1.5, 0.5, 0), tck=-0.03)
for(i in 1:n){
  state_cases <- states[,i]
  gaps <- gap_fc(state_cases, l=length(state_cases))
  state_gaps[[i]] <- gaps
  
  ## plot gaps for each state (if no cases at all then gaps at 133)
  hist(state_gaps[[i]], breaks=seq(-1,mths+1,1),ylim=c(0,max(hist(state_gaps[[i]],plot=F)$counts)+1), 
       xlab="No. of months between cases", ylab="Frequency",
       col="darkgrey", border="darkgrey", main=state_names[i])
}
dev.off()
names(state_gaps) <- state_names

## case presence: rolling window method
RWoutput <- RollingWind(states, mths)
presence <- RWoutput$presence
presence.rw <- RWoutput$presence.rw
fa <- paste0("ClassFrameMS/output/",cn, "/", cn, "_PresAbsen.csv")
fb <- paste0("ClassFrameMS/output/",cn, "/", cn, "_PresAbsen_rw.csv")
write.csv(presence,fa, row.names=F)
write.csv(presence.rw, fb, row.names=F)

### prepare time windows (FIRST AND SECOND LAST 5 YERAS) and work back from a) 2015, b) 2010
timewindows <- 5*12 # 5 yers of data to be fitted
ms <- c(mths, mths-timewindows) 
ms.rw <- ms; ms.rw[1] <- ms[1]-1

## initiate: output stored in a list of df 
LRcoefsList <- list() ## store LR coeffs for truncated periods (length(ms)=2)
PRcoefsList <- list() 
length(PRcoefsList) <- length(ms); length(LRcoefsList) <- length(ms)

mfittedLR <- list(); mfittedPR <- list()
length(mfittedLR) <- n; length(mfittedPR) <- n 
mfittedLRall <- list(mfittedLR, mfittedLR)
mfittedPRall <- list(mfittedPR, mfittedPR)

## plot settings
perd <- c("15", "10")
years  <-  unique(strftime(strptime(as.Date(as.yearmon(states$date)), format="%Y-%m-%d"),"%Y"))
nextyr <- as.numeric(years[length(years)])+1  
years <- list(2011:2016, 2006:2011)
ys <- seq(0.01,60.01,(12)); msp <- seq(0.01,60.01,(1))

## prep data and fit regression models
states.fulldata <- states 
presence.rw.fulldata <- presence.rw

for (r in 1:length(ms)){ #for each 5yr period
  end.period <- ms[r]-(timewindows-1) # substracted from start period
  states <- states.fulldata[end.period:ms[r],]
  presence.rw <- presence.rw.fulldata[end.period:ms.rw[r],]
  
  ### INTERVALS BETWEEN DETECTED CASES: gaps for each state individually 
  for(d in 1:n){
    state_cases <- states[,d]
    gaps <- gap_fc(state_cases, l=length(state_cases))
    state_gaps[[d]] <- gaps
  }
  names(state_gaps) <- state_names
  
  ### CASE DETECTION - Logistic Regression
  LRcoefs <- data.frame(state = state_names,
                        chsq.prob = rep(NA, n),
                        goodfit = rep(NA, n),
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
  
  fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_CasesLR_states", perd[r], ".pdf")
  pdf(fpdf, width=9, height=5)
  par(mfrow=c(7,5), cex=0.4, mar=c(2.35,2.3,0.8,0), mgp=c(1.3, 0.5, 0), tck=-0.03)
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
    
    ## plot
    plot(state$months, state$counts, ylab="Detection (0,1)", xlab=state_names[i],col="darkgrey",
         main="", ylim=c(0,1), xlim=c(1,timewindows),axes=F)
    axis(2,at=seq(0,1,1), lwd=0.2, tck=-0.02)
    axis(1, at = ys, labels=years[[r]], lwd=0.2)
    axis(1, at = msp,labels=F,tck=-0.01)
    
    ## only fit if at least 2 cases and 1 zeros!
    if(sum(state$counts)>1 & sum(state$counts)!=length(state$counts)){
      m = LRmodel(data <- state) ## LR model
      mfittedLRall[[r]][[i]] <- m$fitted
      LRcoefs$goodfit[i] <- m$goodfit ## check whether better than a null model
      LRcoefs$chsq.prob[i] <- m$chsq.prob 
      LRcoefs$odds[i] <- m$odds  ## check whether increasing or decreasing trend
      LRcoefs$pval[i] <- m$pval
      lines(state$months, m$fitted, col="red") ## add predicted
    }
    print(paste0("i",i))
  }
  dev.off()
  LRcoefsList[[r]] <- LRcoefs
  
  ### CASE INCIDENCE - Poisson Regression
  PRcoefs <- data.frame(state=state_names,
                        chsq.probPR = rep(NA, n),
                        goodfitPR = rep(NA, n),
                        pvalPR=rep(NA,n),
                        coeffPR=rep(NA, n))
  
  fpdf2 <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_CasesPR_states", perd[r], ".pdf")
  pdf(fpdf2, width=9, height=5)
  par(mfrow=c(7,5), cex=0.4, mar=c(2.35,2.3,0.8,0), mgp=c(1.3, 0.5, 0), tck=-0.03)
  for (i in 1:n){
    state <- data.frame(incidence=states[,i], months=1:nrow(states))    
    
    ## plot
    plot(state$months, state$incidence, ylab="Incidence", xlab=state_names[i],col="darkgrey",
         main="", ylim=c(0, 26), xlim=c(1,timewindows),axes=F)
    axis(2,at=seq(0,30,5), lwd=0.2, tck=-0.02)
    axis(1, at = ys, labels=years[[r]], lwd=0.2)
    axis(1, at = msp,labels=F,tck=-0.01)
    
    ## only fit if at least one positive observation
    if(sum(state$incidence)>1){
      m  <-  PRmodel(data <- state) 
      PRcoefs$goodfitPR[i] <- m$goodfit
      PRcoefs$chsq.probPR[i] <- m$chsq.prob 
      mfittedPRall[[r]][[i]] <- m$fitted
      PRcoefs$coeffPR[i] <- m$coeff  
      PRcoefs$pvalPR[i] <- m$pval
      lines(state$months, m$fitted, col="red") ## add predicted
    }
  }
  dev.off()
  PRcoefsList[[r]] <- PRcoefs
  print(r)
}

## unlist regression model output and save separately per 5yr period
LRcoefs15 <- LRcoefsList[[1]]; LRcoefs10 <- LRcoefsList[[2]]
head(LRcoefs15, 5); head(LRcoefs10, 5)

PRcoefs15 <- PRcoefsList[[1]]; PRcoefs10 <- PRcoefsList[[2]]
head(PRcoefs15, 5); head(PRcoefs10, 5)

fcoeffa <- paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesLRcoeffs_states15.csv")
fcoeffb <- paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesLRcoeffs_states10.csv")
write.csv(LRcoefs15,fcoeffa, row.names=F)
write.csv(LRcoefs10,fcoeffb, row.names=F)

fcoeffc <- paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesPRcoeffs_states15.csv")
fcoeffd <- paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesPRcoeffs_states10.csv")
write.csv(PRcoefs15,fcoeffc, row.names=F)
write.csv(PRcoefs10,fcoeffd, row.names=F)

### plot MS regression figure ##
states <- states.fulldata[73:132,]
presence.rw <- presence.rw.fulldata[73:131,]
years <- 2011:2016
ys <- seq(0.01,60.01,(12)); msp <- seq(0.01,60.01,(1))

# parameters for location of insets depending on how many states
x1 <- rep(seq(0.04,1,0.2),7)
x2 <- x1+0.04
y1 <- rep(rev(seq(0.09,1,0.14)), each = 5)
y2 <- y1+0.03

plr <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_case-incidence-PR_LRinserts1015TEST.pdf")
pdf(plr, width=9, height=5)
par(mfrow=c(7,5), cex=0.4, mar=c(2.2,2.3,0,0.4), mgp=c(1.3, 0.5, 0), tck=-0.03)
for (i in 1:n){
  plot(states[,i], ylab="Cases", xlab=colnames(states)[i],col="black",type="l",
       main="", ylim=c(0,26), xlim=c(1,timewindows),axes=F)
 
  axis(2,at=seq(0,30,5), lwd=0.2, tck=-0.02)
  axis(1, at = ys, labels=years, lwd=0.2)
  axis(1, at = msp,labels=F,tck=-0.01)
  
  if(!is.null(mfittedPRall[[1]][[i]])){
    lines(1:60, mfittedPRall[[1]][[i]], col="blue", lty=2)
  }
}

for (i in 1:n){
  # insert LR plots
  par(new=TRUE, mfrow=c(1,1), plt=c(x1[i], x2[i], y1[i], y2[i]), cex=0.4, mgp = c(0.7,0.005,0), tck=-0.01)
  plot(1:59, presence.rw[,i], xlim=c(1, 59), ylim=c(0,1),
       xlab=" ", ylab="", pch=16, cex=.3, col="darkgrey",xaxt = "n")
  
  if(!is.null(mfittedLRall[[1]][[i]])){
    lines(1:59, mfittedLRall[[1]][[i]], col="red")
  }
}
dev.off()

###----------------------------------------------------------------------------------------------###
### BRAZIL ### 
cn <- "Brazil"
fcases <- paste0("ClassFrameMS/output/",cn,"/", cn, "_MonthlyCases0515.csv")  
fstates <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_state0515.csv") 

# Read data - generated from: SIRVERA_Country.R
cases <- read.csv(fcases) # monthly detected cases 
states <- read.csv(fstates) # monthly detected cases by state 
wildvars <- read.csv("ClassFrameMS/data/Brazil_WildVariants.csv") # wildlife variants

# HARD CODE IMPORTANT VALUES (so not recalculating throughout functions etc)
mths  <-  nrow(states)
state_gaps <- list()
state_names <- colnames(states)[1:(ncol(states)-1)]
n  <-  length(state_names)
yrs  <-  length(unique(strftime(strptime(as.Date(as.yearmon(states$date)), format="%Y-%m-%d"),"%Y")))
Y2  <-  24 # PERIOD OF 2 YEARS

## remove wildlife variants if available
names <- gsub("[.]", " ", state_names)
for( i in 1:nrow(wildvars)){
  cind <- which(names==wildvars$State[i])
  rind <- which(states$date==as.character(wildvars$Date[i]))
  states[rind, cind] <-  states[rind, cind]-1
  print(states[rind, cind])
}

### INTERVALS BETWEEN DETECTED CASES
allgaps <- gap_fc(cases$cases, l=nrow(cases))

fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_monthly_Gaps.pdf")
pdf(fpdf, width=5.5, height=7)
par(mfrow = c(2,1),mar=c(4,5,2,3))
barplot(allgaps, xlab="", ylim=c(0,max(allgaps)),
        ylab="Months between cases", col="darkgrey", border="darkgrey", main="")
case_intervals = hist(allgaps, breaks=seq(-1,max(allgaps),1), plot=FALSE)
barplot(case_intervals$density[-1], xlab="Months between cases", ylab="Frequency",
        col="darkgrey", border="darkgrey", main="", names.arg = 1:max(allgaps))
dev.off()

## gaps for each state individually 
fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_Gaps_stateHist.pdf")
pdf(fpdf, width=9, height=5)
par(mfrow=c(4,7), cex=0.4, mar=c(5,3,2,1), mgp=c(1.5, 0.5, 0), tck=-0.03)
for(i in 1:n){
  state_cases <- states[,i]
  gaps <- gap_fc(state_cases, l=length(state_cases))
  state_gaps[[i]] <- gaps
  
  ## plot gaps for each state (if no cases at all then gaps at 133)
  hist(state_gaps[[i]], breaks=seq(-1,mths+1,1),ylim=c(0,max(hist(state_gaps[[i]],plot=F)$counts)+1), 
       xlab="No. of months between cases", ylab="Frequency",
       col="darkgrey", border="darkgrey", main=state_names[i])
}
dev.off()
names(state_gaps) <- state_names

## case presence: rolling window method
RWoutput <- RollingWind(states, mths)
presence <- RWoutput$presence
presence.rw <- RWoutput$presence.rw
fa <- paste0("ClassFrameMS/output/",cn, "/", cn, "_PresAbsen.csv")
fb <- paste0("ClassFrameMS/output/",cn, "/", cn, "_PresAbsen_rw.csv")
write.csv(presence,fa, row.names=F)
write.csv(presence.rw, fb, row.names=F)

### prepare time windows (FIRST AND SECOND LAST 5 YERAS) and work back from a) 2015, b) 2010
timewindows <- 5*12 # 5 yers of data to be fitted
ms <- c(mths, mths-timewindows) 
ms.rw <- ms; ms.rw[1] <- ms[1]-1

## initiate: output stored in a list of df 
LRcoefsList <- list() ## store LR coeffs for truncated periods (length(ms)=2)
PRcoefsList <- list() 
length(PRcoefsList) <- length(ms); length(LRcoefsList) <- length(ms)

mfittedLR <- list(); mfittedPR <- list()
length(mfittedLR) <- n; length(mfittedPR) <- n 
mfittedLRall <- list(mfittedLR, mfittedLR)
mfittedPRall <- list(mfittedPR, mfittedPR)

## plot settings
perd <- c("15", "10")
years  <-  unique(strftime(strptime(as.Date(as.yearmon(states$date)), format="%Y-%m-%d"),"%Y"))
nextyr <- as.numeric(years[length(years)])+1  
years <- list(2010:2015, 2005:2010)
ys <- seq(0.01,60.01,(12)); msp <- seq(0.01,60.01,(1))

## prep data and fit regression models
states.fulldata <- states 
presence.rw.fulldata <- presence.rw

for (r in 1:length(ms)){ #for each 5yr period
  end.period <- ms[r]-(timewindows-1) # substracted from start period
  states <- states.fulldata[end.period:ms[r],]
  presence.rw <- presence.rw.fulldata[end.period:ms.rw[r],]
  
  ### INTERVALS BETWEEN DETECTED CASES: gaps for each state individually 
  for(d in 1:n){
    state_cases <- states[,d]
    gaps <- gap_fc(state_cases, l=length(state_cases))
    state_gaps[[d]] <- gaps
  }
  names(state_gaps) <- state_names
  
  ### CASE DETECTION - Logistic Regression
  LRcoefs <- data.frame(state = state_names,
                        chsq.prob = rep(NA, n),
                        goodfit = rep(NA, n),
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
  
  fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_CasesLR_states", perd[r], ".pdf")
  pdf(fpdf, width=9, height=5)
  par(mfrow=c(7,4), cex=0.4, mar=c(2.35,2.3,0.8,0), mgp=c(1.3, 0.5, 0), tck=-0.03)
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
    
    ## plot
    plot(state$months, state$counts, ylab="Detection (0,1)", xlab=state_names[i],col="darkgrey",
         main="", ylim=c(0,1), xlim=c(1,timewindows),axes=F)
    axis(2,at=seq(0,1,1), lwd=0.2, tck=-0.02)
    axis(1, at = ys, labels=years[[r]], lwd=0.2)
    axis(1, at = msp,labels=F,tck=-0.01)
    
    ## only fit if at least 2 cases and 1 zeros!
    if(sum(state$counts)>1 & sum(state$counts)!=length(state$counts)){
      m = LRmodel(data <- state) ## LR model
      mfittedLRall[[r]][[i]] <- m$fitted
      LRcoefs$goodfit[i] <- m$goodfit ## check whether better than a null model
      LRcoefs$chsq.prob[i] <- m$chsq.prob 
      LRcoefs$odds[i] <- m$odds  ## check whether increasing or decreasing trend
      LRcoefs$pval[i] <- m$pval
      lines(state$months, m$fitted, col="red") ## add predicted
    }
    print(paste0("i",i))
  }
  dev.off()
  LRcoefsList[[r]] <- LRcoefs
  
  ### CASE INCIDENCE - Poisson Regression
  PRcoefs <- data.frame(state=state_names,
                        chsq.probPR = rep(NA, n),
                        goodfitPR = rep(NA, n),
                        pvalPR=rep(NA,n),
                        coeffPR=rep(NA, n))
  
  fpdf2 <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_CasesPR_states", perd[r], ".pdf")
  pdf(fpdf2, width=9, height=5)
  par(mfrow=c(7,4), cex=0.4, mar=c(2.35,2.3,0.8,0), mgp=c(1.3, 0.5, 0), tck=-0.03)
  for (i in 1:n){
    state <- data.frame(incidence=states[,i], months=1:nrow(states))    
    
    ## plot
    plot(state$months, state$incidence, ylab="Incidence", xlab=state_names[i],col="darkgrey",
         main="", ylim=c(0, 36), xlim=c(1,timewindows),axes=F)
    axis(2,at=seq(0,40,5), lwd=0.2, tck=-0.02)
    axis(1, at = ys, labels=years[[r]], lwd=0.2)
    axis(1, at = msp,labels=F,tck=-0.01)
    
    ## only fit if at least one positive observation
    if(sum(state$incidence)>1){
      m  <-  PRmodel(data <- state) 
      PRcoefs$goodfitPR[i] <- m$goodfit
      PRcoefs$chsq.probPR[i] <- m$chsq.prob 
      mfittedPRall[[r]][[i]] <- m$fitted
      PRcoefs$coeffPR[i] <- m$coeff  
      PRcoefs$pvalPR[i] <- m$pval
      lines(state$months, m$fitted, col="red") ## add predicted
    }
  }
  dev.off()
  PRcoefsList[[r]] <- PRcoefs
  print(r)
}

## unlist regression model output and save separately per 5yr period
LRcoefs15 <- LRcoefsList[[1]]; LRcoefs10 <- LRcoefsList[[2]]
head(LRcoefs15, 5); head(LRcoefs10, 5)

PRcoefs15 <- PRcoefsList[[1]]; PRcoefs10 <- PRcoefsList[[2]]
head(PRcoefs15, 5); head(PRcoefs10, 5)

fcoeffa <- paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesLRcoeffs_states15.csv")
fcoeffb <- paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesLRcoeffs_states10.csv")
write.csv(LRcoefs15,fcoeffa, row.names=F)
write.csv(LRcoefs10,fcoeffb, row.names=F)

fcoeffc <- paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesPRcoeffs_states15.csv")
fcoeffd <- paste0("ClassFrameMS/output/", cn, "/", cn, "_CasesPRcoeffs_states10.csv")
write.csv(PRcoefs15,fcoeffc, row.names=F)
write.csv(PRcoefs10,fcoeffd, row.names=F)

### plot MS regression figure 
states <- states.fulldata[73:132,]
presence.rw <- presence.rw.fulldata[73:131,]
years <- 2011:2016
ys <- seq(0.01,60.01,(12)); msp <- seq(0.01,60.01,(1))

# parameters for location of insets depending on how many states
x1 <- rep(seq(0.05,1,0.25),7) ## Brazil
x2 <- x1+0.04
y1 <- rep(rev(seq(0.09,1,0.14)), each = 4)
y2 <- y1+0.03

plr <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_case-incidence-PR_LRinserts1015.pdf")
pdf(plr, width=9, height=5)
par(mfrow=c(7,4), cex=0.4, mar=c(2.2,2.3,0,0.3), mgp=c(1.3, 0.5, 0), tck=-0.03)
for (i in 1:n){
  plot(states[,i], ylab="Cases", xlab=colnames(states)[i],col="black",type="l",
       main="", ylim=c(0,36), xlim=c(1,timewindows),axes=F)
  
  axis(2,at=seq(0,30,5), lwd=0.2, tck=-0.02)
  axis(1, at = ys, labels=years, lwd=0.2)
  axis(1, at = msp,labels=F,tck=-0.01)
  
  if(!is.null(mfittedPRall[[1]][[i]])){
    lines(1:60, mfittedPRall[[1]][[i]], col="blue", lty=2)
  }
}

for (i in 1:n){
  # insert LR plots
  par(new=TRUE, mfrow=c(1,1), plt=c(x1[i], x2[i], y1[i], y2[i]), cex=0.4, mgp = c(0.7,0.005,0), tck=-0.01)
  plot(1:59, presence.rw[,i], xlim=c(1, 59), ylim=c(0,1),
       xlab=" ", ylab="", pch=16, cex=.3, col="darkgrey",xaxt = "n")
  
  if(!is.null(mfittedLRall[[1]][[i]])){
    lines(1:59, mfittedLRall[[1]][[i]], col="red")
  }
}
dev.off()
