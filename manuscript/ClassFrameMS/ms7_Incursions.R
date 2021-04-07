### SIRVERA DATA: Identification of Incursions (in Mexico and Brazil)
## 1. The first part of the script takes output statistics from logistic regression models and 
## corresponding rolling windows binary data from Mexico and Brazil and 
## a) identifies incursions and the states and dates of where and when those incursions occured 
## bwetween Jan 2005 and Dec 2015 based on critera decided as INCURSION ALGORITHM,
## b) plots indetified incursions against the rolling windows data by state (country per page)

rm(list=ls())
#setwd("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis")
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(zoo)
library(RColorBrewer)
library(gplots)
source("R/gap_fc.r")

## Directories setting & info
cn <- "Brazil" # SWITCH between Mexico or Brazil

fmcases <- paste0("ClassFrameMS/output/",cn,"/", cn, "_MonthlyCases0515.csv")  
fstates <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_state0515.csv") 
fRW <- paste0("ClassFrameMS/output/",cn, "/", cn, "_PresAbsen_rw.csv")

## import
states <- read.csv(fstates) # monthly detected cases by state 
RWdat <- read.csv(fRW) 

## remove wildlife variants
wildvars <- read.csv(paste0("ClassFrameMS/data/", cn, "_WildVariants.csv"))
names <- gsub("[.]", " ", colnames(states)[1:(ncol(states)-1)])
for( i in 1:nrow(wildvars)){
  cind <- which(names==wildvars$State[i])
  rind <- which(states$date==as.character(wildvars$Date[i]))
  states[rind, cind] <-  states[rind, cind]-1
}

### INTERVALS BETWEEN DETECTED CASES ###
state_names <- colnames(states)[1:(ncol(states)-1)]

## For states with data look at gaps: how many incursions?
l <- nrow(RWdat)
incursionsDF <- data.frame(matrix(0,nrow=length(state_names), ncol=20)) #N.B.!!ncol 15Mex, 20Bra
colnames(incursionsDF) <- c("states", "incursions", paste("cases per inc", 1:18)) #13, 18
incursionsDF[,1] <- state_names

datf <- data.frame(matrix(NA, ncol=8))
colnames(datf) <- c("state", "month", "date", "mean_gaplength", "min_gaplength", "last_gaplength", 
                    "zero_lenght","cases_2yr")

for(i in 1:length(state_names)){ #32 fro MEx; 27 for Bra
  cases <- states[,i]
  dates <- states$date
  RWcases <- RWdat[,i]
  
  if(sum(cases)>0){
    ## how many incursions
    ## no consecutive months, 2 months between cases
    incursionsDF[i,3] <- gapRW_fc(RWcases, l) #number of incursions 
    #iDateDF[i,3] <- gapRW_fc(RWcases, l)
    
    ## and how many reports per each incursion event
    zeros <- which(RWcases == 0)
    case_dates <- c(-1,rep((1:l)[-zeros], RWcases[-zeros]), l+1) 
    gaps <- diff(case_dates)[which(diff(case_dates)>0)]-1 # how many zeros per each segment
    
    NZ <- which(RWcases>0) #cases
    gg <- which(gaps>0) #no cases
    
    for(j in 2:length(gg)){
      inds <- NZ[gg[j-1]:gg[j]]
      if(is.na(sum(inds))==T){
        inds <- na.omit(inds)
      }
      incursionsDF[i,j+2] <- sum(cases[inds])
      #iDateDF[i,j+2] <- inds[1]  #37 if j=3
      
      ## look at mean and variance of case 2yrs prior
      month <- ifelse(length(inds)>1, inds[2], inds[1]) # month of inc
      date <- as.character(dates[month])
      mt <- month-1
      if(mt<24){
        cases2yr <- cases[1:mt]
      }else{
        cases2yr <- cases[(mt-23):mt]
      }
      #cases2yr <- ifelse(month<24, c(cases[1:month]), c(cases[(month-23):month])) # case 2 yrs prior inc
      state <- state_names[i]
      cases_2yr <- sum(cases2yr)
      
      if(sum(cases2yr)>0){
        gaps_2yr <- gap_fc(cases2yr, l=length(cases2yr))
        mean_gaplength <- mean(gaps_2yr) # mean gap length
        min_gaplength <- min(gaps_2yr) # min gap length
        zero_length <- length(which(cases2yr==0)) # no of zeros
        last_gaplength <- gaps_2yr[length(gaps_2yr)] #last gaplength
      }else{
        mean_gaplength <- 24 # mean gap length
        min_gaplength <- 24 # min gap length
        zero_length <- 24 # porportion of zeros
        last_gaplength <- 24 # last gaplength
      }
      
      combined <- c(state, month, date, mean_gaplength, min_gaplength, last_gaplength, zero_length,cases_2yr)
      if(length(which(is.na(datf[1,])))==8){
        datf[1,] <- combined
      }else{
        datf <- rbind(datf, combined)
      }   
    } 
  }
  print(i)
}

head(incursionsDF) # size of outbreaks out of incursions
head(datf) # info to identify incursions

## save
finc <- paste0("ClassFrameMS/output/",cn, "/", cn, "_IncursionOutbreaks.csv")
write.csv(incursionsDF,finc, row.names=F)

fine <- paste0("ClassFrameMS/output/",cn, "/", cn, "_IncursionIdentificationInfo.csv")
write.csv(datf,fine, row.names=F)

### INCURSION IDENTIFICATION ALGORITHM ###
criteria <- datf
criteria$incursion <- "Y"

for(i in 1: nrow(criteria)){ 
  ## not an incursion
  if(as.numeric(criteria$zero_lenght[i])<12){
    criteria$incursion[i] <- "N"
  }
  if(as.numeric(criteria$mean_gaplength[i])<2.5){
    criteria$incursion[i] <- "N"
  }
  if(as.numeric(criteria$last_gaplength[i])<=as.numeric(criteria$mean_gaplength[i])){
    criteria$incursion[i] <- "N"
  }
  
  # if(as.numeric(criteria$last_gaplength[i])<=as.numeric(criteria$mean_gaplength[i])){
  #   # the last gaplength has to be smaller or equal to the mean gaplength
  #   criteria$incursion[i] <- "Y"
  # }
  
  if(as.numeric(criteria$last_gaplength[i])>6){ ##!! make it >6 months (but not =)
    # ~ 50% prob that cases not present; mth prob of case 0.95^12 months = 0.54
    criteria$incursion[i] <- "Y"
  }
  
  print(i)
}
head(criteria)
table(criteria$incursion) 

### SOME SENSITIVITY TESTING HERE!!! ###
## run with "as.numeric(criteria$last_gaplength[i])<=as.numeric(criteria$mean_gaplength[i]))" only: BRA=74, MEX=42
# I think this is bc the mean gaplength can be quite small and all the conditions above qould be automatically overriden
## run with "as.numeric(criteria$last_gaplength[i])>6" only: BRA=54, MEX=32
## run with both and compare all thre scenarios: BRA=76, MEX=45
# Mexico: 32 (when last_gaplength>6); 33 (when last_gaplength>=6)
# Brazil: 60 (when last_gaplength>6); 62 (when last_gaplength>=6)

finf <- paste0("ClassFrameMS/output/",cn, "/", cn, "_IncursionClassification.csv")
write.csv(criteria,finf, row.names=F)

## plot incursion ts with newly identified "incursions"
incursions <- criteria[which(criteria$incursion=="Y"),]

fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_ClassifiedIncursions_ts.pdf")
pdf(fpdf, width=9, height=5)
par(mfrow=c(4,5), cex=0.4, mar=c(5,3,2,1), mgp=c(1.5, 0.5, 0), tck=-0.03)
for(i in 1:length(state_names)){
  state_cases <- states[,i]
  state_RW <- c(RWdat[,i],0)
  main <- colnames(states)[i]
  
  ## binary transformed + RW
  barplot(state_RW, ylim=c(0,(max(state_cases)+1)),main=main, cex.main=0.8,
          ylab="Months between cases", xlab="Months (Jan 05 - Dec 15)", col="red", border="red")
  
  ## all cases
  par(new=TRUE); barplot(state_cases,ylim=c(0,(max(state_cases)+1)),main="",ylab="", xlab="",
                         col="darkgrey", border="darkgrey")
  
  ## add incursion
  state_inc <-  subset(incursions, state==colnames(states)[i])
  #points(x = state_inc$month, y = c(rep(1,3)), pch=17, col="black")
  
  par(new=TRUE);plot(x = state_inc$month, y = c(rep(0,nrow(state_inc))), pch=17,
                     xlim=c(0,132),ylim=c(0,(max(state_cases)+1)), axes=F,
                     main="", ylab="", xlab="", col="black")
  
  ## add legend to the first figure
  if(i==1){
    legend(100, max(state_cases), pch=c(15,15),
           col=c("red","darkgrey"), c("RW binary cases", "Cases"), bty = "n")}
}
dev.off()

### HEATMAP OF INCURSIONS ###
## 2. The second part of the script takes identified incursion data in Mexico and Brazil and 
## produces a heat map for each contry 
## gridded as states by rows and years by columns,
## and coloured in orange if a single or in red if two incursion in the given year

head(incursions)
incursions$year <- substr(incursions$date,1,4)

grid.mat <- matrix(0, nrow=length(state_names), ncol=length(2005:2015))
colnames(grid.mat) <- 2005:2015; row.names(grid.mat) <- state_names

for(i in 1:nrow(incursions)){
  rowind <- which(row.names(grid.mat)==incursions$state[i])
  colind <- which(colnames(grid.mat)==incursions$year[i])
  grid.mat[rowind, colind] <- as.numeric(grid.mat[rowind, colind])+1
  print(i)
}
sum(grid.mat) #32Mex, 60Bra
row.names(grid.mat) <- gsub("[.]", " ", state_names)

## N.B. !! change colours for Mexico and Brazil
unique(c(grid.mat)) #according to if 1 or 2 incursions

# ## plot and save as png
# png(paste0("ClassFrameMS/figs/", cn, "/", cn, "_incursion-heatmap.png"),    # create png for the heat map        
#     width = 5*300,        # 5 x 300 pixels
#     height = 5*300,
#     res = 300)            # 300 pixels per inch

## plot and save as pdf
pdf(paste0("ClassFrameMS/figs/", cn, "/", cn, "_incursion-heatmap.pdf"),    # create pdf for the heat map        
    width = 5,height = 5)            

heatmap.2(grid.mat,
          # col= c("white", "orange"), #Mex
          col= c("white", "orange", "red"), #Bra
          colsep=1:ncol(grid.mat), # separate cells 
          rowsep=1:nrow(grid.mat),
          sepwidth=c(0.001,0.001),
          sepcolor="gray",  # by a black line   
          cexRow=0.8,
          cexCol= 0.85,
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          dendrogram="none",     # only draw a row dendrogram
          Colv="NA",
          Rowv=F,
          symm = F,
          key = F,
          lhei=c(1,6),
          lwid=c(0.01,0.1),
          mar=c(3,12))
dev.off()

