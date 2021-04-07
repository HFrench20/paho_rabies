#ALGORITHM FOR TREE CONSTRUCTION (LAC)
#function to find progenitors for incursions - each case corresponds to a row in the incursion table

#the function takes: 
#the incursion (the row in the incursion table and matches possition within cases.df table - index), 
#the dates of the progenitor cases, 
#the IDs of possible progenitors, 
#a distance matrix between cases 

# ## testing/checking
# possdatesO=cases.df$jd
# possIDsO=cases.df$ID
# distmatrixO=distmat
# incindex=incursions$incindex
# index <- 1

guessLAC=function(index, possdatesO, possIDsO, incindex, distmatrixO){ 
  case <- incindex[index]
  
  ## remove this inc from cases and distmat
  possdates <- possdatesO[-case]
  possIDs <- possIDsO[-case]
  distmatrix <- distmatrixO[,-case]
  
  ## intervals and sources
  SIs <- possdates[case]-possdates[possdates<=possdates[case] & possdates>possdates[case]-730]+1 #cannot be zero diff; add a day  
  sourcei <- which(possdates<=possdates[case] & possdates>possdates[case]-730) # but not longer than 2 yrs
  
  ## !!SI might be ok, but dist kernel to small for cross country ##
  SIprobs <- dgamma(SIs, shape=SIshape, rate=SIrate) #serial interval probabilities of possible sources
  sourceIDs <- possIDs[sourcei]		#identities of possible sources
  
  sourcedists <- distmatrix[case, sourcei] #determine distances of possible progenitors 
  # <100m is never gonna happen in LAC!
  #   distprobs <- ifelse(sourcedists<=100, #distance probabilities of poss sources (dgamma with distshape and rate)
  #                       pgamma(100, shape=distshape, rate=distrate)/100, #accounts for the truncation at 100m
  #                       dgamma(sourcedists+50, shape=distshape, rate=distrate))
  
  #distprobs=dgamma(sourcedists+50, shape=distshape, rate=distrate) # why adding 50?
  distprobs=dgamma(sourcedists, shape=distshape, rate=distrate) 
  
  sourceprobs=distprobs*SIprobs 		#overall probability of progenitors (product of spatial & temporal distance)
  unitprobs=sourceprobs/sum(sourceprobs, na.rm=T) #Scale probablities to one
  unitprobs[is.na(unitprobs)]=0		#BUT GET RID OF NAs!
  cumunitprobs=cumsum(unitprobs)		#and make them cumulative
  ravr=runif(1,0,1)	#pick a random variate (RV) for assigning the progenitor
  sourceID=which(cumunitprobs>ravr)[sample(1:length(which(cumunitprobs>ravr)), 1)]#find which probability matches the RV
  thesource=sourceIDs[sourceID]			#designate that as the source
  sourceprob=unitprobs[sourceID]		#keep track of the probability
  sourceLL=log(sourceprobs[sourceID]) #assign log likelihood of progenitor
  
  theSIprobs <- SIprobs[sourceID]
  thedistprobs <- distprobs[sourceID]
  
  #if (length(distprobs)>0) { 	#if there is at least 1 possible projenitor
  #		sourceprob=max(sourceprobs, na.rm=T) 
  #		sources=sourceIDs[which(sourceprobs==sourceprob)] #provide list of them
  #		source=sources[sample(length(sources),1)] # and randomly sample them so there is one source
  #	} else {source=sourceprob=NA} #otherwise assign source as NA
  
  c(thesource, sourceprob, sourceLL, theSIprobs, thedistprobs)
  #SIprobs, distprobs?
}

