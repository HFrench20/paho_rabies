## SIRVERA DATA: CASE PRESENCE ROLLING WINDOW FUNCTION
## df of cases presence/absence (1/0) using rolling window method - assume case is latent for 1 month
RollingWind = function(states, mths){
  presence <- data.frame(matrix(NA, nrow=length(1:mths), ncol= ncol(states))) # one row per month, one column per state
  colnames(presence) <- c(colnames(states)) # give column names as states
  presence$date <- 1:(mths) # create additional column 1 to the number of months

  # create presence rolling window dataframe.
  presence.rw <- data.frame(matrix(NA, nrow=length(1:(mths-1)), ncol= ncol(states)))
  # the same as before except number of rows are equal to the number of rows in the time window -1)
  colnames(presence.rw) <- c(colnames(states))
  presence.rw$date <- 1:(mths-1)

  for (i in 1:(ncol(states)-1)){
    state <- states[,i] # column of data for a given state gives the number of cases per month (time series)
    
    # Convert incidence into just detected yes or no (detection) using the as.numeric command.
    # If there are cases, then give it a 1, if there are no cases, give it a 0.
    # Create list of yes/no detection for every month
    presence[,i] <- as.numeric(state>0) #assume last one is same as previous
    
    # -1 removes the first indexed place
    presence.rw[,i] <- as.numeric((presence[,i][-1] + presence[,i][-mths])>0)
    
    # Creates two vectors of the same length, one with the first month missing,
    # one with the last month missing.
    
    # These can be overlayed to create a list of 2's 1's and 0's that takes into
    # account the adjacent months. 
    
    # Then convert back to binary. 
    # Rolling window matrix = accounts for what happened the previous month. 
    
  }
  list(presence=presence, presence.rw=presence.rw)
}



# testing and detection in different states varys a lot 
# - people employed
# - motivation to find cases
# - current outbreak going on?

# just looking for trends in actual incidence can be misleading.
# instead:
# look at trends in presence:absence
#^ 
# look at the supplemental figure in the paper = comparison of trend inferred from 
# presence:absence vs trend inferrred from incidence. 

# balance between how long you look over as well, 5 years was the best. 
