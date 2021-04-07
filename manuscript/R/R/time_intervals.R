### Functions to divide data into time serials (and further process) ###

## FC to consolidate CASES per each interval (3, 6, 9 months) ##
consolidatecases_intervals = function(cases, interval, incursiongap=0){
  ## incursiongap: time (in months) prior when incursion study started divided by interval
  # Interval sequence
  l <- seq(0, nrow(cases), interval); l <- l[2:length(l)]
  states <- colnames(cases)[1:ncol(cases)-1]
  
  seasoncases <- matrix(0, ncol = ncol(cases)-1, nrow = length(l))
  
  # Sum up the cases
  for(i in 1:length(states)){
    state_data <- cases[,which(colnames(cases) == states[i])]
    for(j in 1:length(l)){
      index <- (l[j]-(interval-1)):l[j]
      seasoncases[j,i] <- sum(state_data[index])
    }
  }
  
  # Format dataframe
  seasoncases = data.frame(seasoncases)
  names(seasoncases) <- states
  seasoncases$period <- l/interval-incursiongap
  seasoncases$date = cases$date[l]
  seasoncases
}

## FC to match incursions with time intervals ##
findincursions_intervals = function(incursions, dates, interval){
  # Interval sequence
  l <- seq(0, length(dates), interval); l <- l[2:length(l)]
  ints <- numeric(length(dates))
  
  # assign intervals to all dates
  for(i in 1:length(l)){
    index <- (l[i]-(interval-1)):l[i]
    ints[index] <- l[i]/interval
  }
  
  intdates <- cbind(dates, ints)
  
  # find interval for all incursions
  incursions$interval <- NA
  for(i in 1:nrow(incursions)){
    index <- which(dates==as.character(incursions$date[i]))
    incursions$interval[i] <- ints[index] 
  }
  incursions
}

## FC to split a vector in interval and apply functions over each inerval ##
splitINintervals = function(x,interval,...,fc=0){
  ## INPUT: x=vector of values to be dividied into interval, interval=length of each interval
  ## OUTPUT: list if chunks of the original vector; if fc=FUN applies FUN over each chunk (e.g. fc=mean)
  
  n <- length(x)/interval
  interval_list <- split(x, factor(sort(rank(x)%%n)))
  if(class(fc)!="numeric"){mean_values <- sapply(interval_list, FUN=fc); return(mean_values)}
  else(return(interval_list))
}






