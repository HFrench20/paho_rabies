### FC to calculate weighted distance to source states per each incursion state and period 
problematic_states = function(classdata, states, mat, mode="neighbours",...,distmat, area, pop, cases){
  ## mode: neighbours (default), weighted distance (WD), 
  ## weighted distance + area (WDA), weighted distance
  ## weighted distance + population counts (WDPC), weighted distance + population density (WDPD)
  ## weighted distance + cases prevalence (WDCP)
  
  ## additional params: distmat=distance matrix, area=coverage area, 
  ## pop=population density/ counts, cases=cases prevalence 
       
  pnb <- numeric(length(states))
  
  if(length(classdata)>0){
    problematic <- c("Endemic", "Declining")
    classdata$state <- gsub("[.]", " ", classdata$state)
    colnames(classdata)[2] <- "phase"
    
    ### mode 1: count problematic neighbours
    if(mode=="neighbours"){
      for(j in 1:length(states)){
        nbs <- states[which(mat[j,]==1)]
        phases <- classdata$phase[which(classdata$state %in% c(as.character(nbs)))]
        pnb[j] <- sum(phases %in% c(problematic))
      }  
    }
    
    ### mode 2: weighted distance to problematic states j (SUM e^-dij)
    if(mode=="WD"){
      for(j in 1:length(states)){
        pst <- which(classdata$phase %in% c(problematic))
        if(length(which(pst==j))>0){
          pst <- pst[-which(pst==j)]
        }
        if(length(pst)>0){
          st <- j
          distances <- distmat[st, pst]
          pnb[j] <- sum(exp(-distances)) 
        }else{
          pnb[j] <- 0 
        }
      }
    }
    
    ### mode 3: weighted distance to problematic states scaled by area in states j (SUM (e^-dij) * Aj)
    if(mode=="WDA"){
      for(j in 1:length(states)){
        pst <- which(classdata$phase %in% c(problematic))
        if(length(which(pst==j))>0){
          pst <- pst[-which(pst==j)]
        }
        if(length(pst)>0){
          st <- j
          distances <- distmat[st, pst]
          A <- area[pst]
          pnb[j] <- sum(exp(-distances)*A)
        }else{
          pnb[j] <- 0 
        }
      }
    }
    
    ### mode 4: weighted distance inlcuding population counts in states j (SUM (e^-dij) * PCj)
    if(mode=="WDPC"){
      for(j in 1:length(states)){
        pst <- which(classdata$phase %in% c(problematic))
        if(length(which(pst==j))>0){
          pst <- pst[-which(pst==j)]
        }
        if(length(pst)>0){
          st <- j
          distances <- distmat[st, pst]
          PC <- pop[pst]
          pnb[j] <- sum(exp(-distances)*PC)
        }else{
          pnb[j] <- 0
        }
      }
    } 
    
    ### mode 5: weighted distance inlcuding population density in states j (SUM (e^-dij) * PDj)
    if(mode=="WDPD"){
      for(j in 1:length(states)){
        pst <- which(classdata$phase %in% c(problematic))
        if(length(which(pst==j))>0){
          pst <- pst[-which(pst==j)]
        }
        if(length(pst)>0){
          st <- j
          distances <- distmat[st, pst]
          PD <- pop[pst]
          pnb[j] <- sum(exp(-distances)*PD)
        }else{
          pnb[j] <- 0
        }
      }
    } 
    
    ### mode 6: weighted distance inlcuding case prevalence in states j (SUM (e^-dij) * CPj)
    if(mode=="WDCP"){
      for(j in 1:length(states)){
        pst <- which(classdata$phase %in% c(problematic)) 
        if(length(which(pst==j))>0){
          pst <- pst[-which(pst==j)]
        }
        if(length(pst)>0){
          st <- j
          distances <- distmat[st, pst]
          CP <- cases[pst] ## case prevalence from problemtic states
          pnb[j] <- sum(exp(-distances)*CP)
        }else{
          pnb[j] <- 0
        }
      }
    } 
  }else{
    ### mode 7: weighted distance inlcuding case prevalence in all states in Mexico (SUM (e^-dij) * CPall)
    if(mode=="WDCPall"){
      for(j in 1:length(states)){
        pst <- 1:length(states)
        pst <- pst[-j]
        st <- j
        distances <- distmat[st, pst]
        CP <- cases[pst] # case prevalence from all states
        pnb[j] <- sum(exp(-distances)*CP)
      }
    } 
  }
  
  pnb
}

  