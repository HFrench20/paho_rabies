# Script for recreating a susceptible time series
sprop = function(times, vmonth, finalTime, coverage, coverage_sd){
    sp = rep(1, length(times)+1) # susceptible proportion
    vp = seq(vmonth, length(sp), 12) # vaccination pulse month (e.g. 9 = september)
    vc = c(0, rnorm(length(vp), mean = coverage, sd=coverage_sd)) # coverage in each campaign - with no vc at start

    # Wrong distribution used for coverage - have to make sure do not exceed 1 or go below 0!!!
    vc = replace(vc, which(vc>1), 0.9)
    vc = replace(vc, which(vc<0), 0)
      
    tsv = rep(1:12, finalTime+2) # time since vacc
    lag = 1:(13-vmonth) # incorporate the timing of the first vaccination campaign
    tsv = c(tsv[-lag], lag) 
    campaign = rep(1:(finalTime+2), each = 12)[-lag]
    
    for(i in 2:length(sp)){ # Fill out the susceptible proportion
      sp[i] = 1-vc[campaign[i]] * exp(-br * tsv[i]) # but allows susceptible proportion to increase unrealistically
    }
    sp
  }



#NOW adjust numbers of campaigns conducted
sprop2 = function(times, vmonth, finalTime, coverage, coverage_sd, campaign_n){
  sp = rep(1, length(times)+1) # susceptible proportion
  vp = c(0, seq(vmonth, by=12, length.out=campaign_n))
  vc = c(0, rnorm(length(vp), mean = coverage, sd=coverage_sd)) # coverage in each campaign - with no vc at start
  
  # Wrong distribution used for coverage - have to make sure do not exceed 1 or go below 0!!!
  vc = replace(vc, which(vc>1), 0.9)
  vc = replace(vc, which(vc<0), 0)
  
  tsv = 0 # time since vacc and timeseries of times
  cn = 1
  for(i in 2:length(sp)){ # Fill out the susceptible proportion
    
    if(i < vp[cn+1]){ # if next campaign not started, iterate
      cn = cn 
      tsv = tsv+1}
    
    else{ # if campaign time, reset
      cn = cn+1
      tsv = 0}
    
    sp[i] = (1-vc[cn]) * exp(-br * tsv) 
  }
  sp
}



#sp = rep(1, length(times)+1) # susceptible proportion