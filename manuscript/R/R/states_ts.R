# Function to take the SIRVERA data for a country and generate timeseries for each admin unit
states_ts = function(dates, states, data){

  cases <- matrix(0, ncol = length(states), nrow = length(dates))

  for(i in 1:length(states)){
    state_data <- data[which(as.character(data$UnidMaior) == states[i]),]
    for(j in 1:length(dates)){
      index <- which(state_data$date == dates[j])
      cases[j,i] <- sum(state_data$TotalCasos[index], na.rm=TRUE)
    }
  }

  # Format dataframe
  cases = data.frame(cases)
  names(cases) <- states
  cases$date = dates
  cases
}


