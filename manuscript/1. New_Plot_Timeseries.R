## Plot the Timeseries output of script 1
# Script prepares TS for plotting

# "lubridate package is your best friend to handle date format"

# check format with str(data)
# it is showing as charaacter
# date must be a full date? therefore add 01 for DD to make YYYY-MM-DD

rm(list=ls())
setwd("~/Code/paho_rabies_hf/manuscript")

# load libraries
library(TSstudio)
library(ggplot2)
library(ISOcodes)
library(dplyr)
library(zoo)

library(ggfortify)
library(tsbox)

# Load surveillance data with ISO codes (output of script 0.Add_ADM0_code.R )
dogs.ISO <- read.csv("data/SIRVERA_dogs16_ISO.csv")
## Extract unique ISO codes for list of countries I want to loop through.
countries <- c(unique(dogs.ISO$ADM0_ISO))


# practice 
cn <- "ARG"

for (l in 1:length(countries)){
  # subset one country of interest at a time.
  cn = countries[l]
  
  if (file.exists(paste0("output_new/", cn, "_ADM1_monthly_cases.csv"))){
    
    cn.df <- read.csv(paste0("output_new/", cn, "_ADM1_monthly_cases.csv"))
    head(cn.df)
    
    # Load the ADM1 state names GID_1 key for current country
    ADM1_key <- read.csv(paste0("data/cleaning/adm1_keys/", cn, "_adm1_key.csv"))
    
    
    ## ------------------- make adjustments ----------------------
    
    # change each row to a full date by adding -01, then convert whole column to date format
    for (i in 1:nrow(cn.df)) {
      cn.df[i, "date"] <- paste0(cn.df[i, "date"], "-01")
    }
    cn.df$date <- as.Date(cn.df$date) # convert whole column because row by row didn't work for some reason)
    head(cn.df)
    str(cn.df$date)
    
    # change each column GID_1 to corresponding state name
    col.names <- colnames(cn.df)
    col.gids <- head(col.names, -1)
    
    for (c in 1:length(col.gids)) {
      gid = col.gids[c]
      print(gid)
      
      # filter the row for the current gid and extract state name
      row.gid <- ADM1_key %>% filter(GID_1 == gid)
      state.name <- row.gid$NAME_1
      print(state.name)
      
      # change the column name
      colnames(cn.df)[colnames(cn.df) == gid] <- state.name
      # note: it generates an error when there is more than one entry in the key
      # "warning, number of items to replace is not a multiple of replacement lenght"
      # this seems to be ok because it just picks the first one it finds
      # which is the more official one I want to use anyway.
      
    }
    
    
    # another idea is to create a vector of the first that many rows of the key and hope for the best
    # RELIES ON ALL GID COLUMNS IN DF AND KEY BEING IN THE SAME ORDER!!
  #  all.names <- vector()
  #  all.names <- ADM1_key$NAME_1
  #  state.names <-all.names[1:cols]
  #  state.names <- c(state.names, "date")
    # and replace
  #  colnames(cn.df) <- state.names
    
    
    # filter the ISO codes table using the cn 3-letter ISO code
    row.ISO <- ISO_3166_1 %>% filter(Alpha_3 == cn)
    # assign the country name
    if (is.na(row.ISO$Common_name)) {cn.name <- row.ISO$Name} else {cn.name <- row.ISO$Common_name}
    
    
    # create plot and store as unique object to manually save...
    # it WAS working, but its now freaking out.
    # Error in ts_c() : argument is missing, with no default.
    # Maybe its relating to the extra dimensions? 
    plot.cn <- ts_plot(cn.df, 
                       title = paste("Monthly rabies cases in", cn.name),
                       Xtitle = "Time",
                       Ytitle = "Monthly cases",
                       line.mode = "markers", 
                       slider = FALSE,
    ) 
    plot.name <- paste0(cn, "_plot")
    assign(plot.name, plot.cn)
    
  } 
}


## attempting to automatically export
  # NOT WORKING YET! (gives corrupted file!)
  
  save.path <- paste0("plots_hf/", cn, "_TS_1995-2015_auto2.pdf")
  pdf(save.path)
  ts_plot(cn.df, 
          title = paste("Monthly rabies cases in", cn.name),
          Xtitle = "Time",
          Ytitle = "Monthly cases",
          line.mode = "markers")
  dev.off()  
  


#ggplot(cn.df) # doesn't work don't know why


