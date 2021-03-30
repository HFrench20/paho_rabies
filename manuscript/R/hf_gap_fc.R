### SIRVERA DATA: INTERVALS BETWEEN DETECTED CASES
## gap function for cases

# if there are multiple 0's in a row = gaps. (using diff function)
# find the gaps and store them as a list.

# every state could have different numbers of gaps so we store them in an indexed list.
# every state has its own list of gaps. (see main function)

gap_fc= function(cases, l){
  zeros <- which(cases == 0)
  case_dates <- c(-1, rep((1:l)[-zeros], cases[-zeros]), l+1) #assume 1st report b4 start of observations?
  gaps <- diff(case_dates)[which(diff(case_dates)>0)]-1 # important function diff
  gaps
}

## gap funcion to RW data: incursions
gapRW_fc= function(RWcases, l){
  zeros <- which(RWcases == 0)
  case_dates <- c(-1,rep((1:l)[-zeros], RWcases[-zeros]), l+1)
  gaps <- diff(case_dates)[which(diff(case_dates)>0)]-1
  incursions <- length(which(gaps>0))-1
  incursions
}
