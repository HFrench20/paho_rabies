### SIRVERA DATA: Mexico & Brazil coloured classification timelines
## 1. the first part of the script produces plots of states in Mexico and Brazil between 2005 and 2015
## a) classified yearly (depicted by coloured backrounds),
## b) overlaid by cases timeseries and
## c) blue asteriks indicating incursions
## comparing how classification output differ depending on the legth of time windows of data fitted
## to logistic regression models (each time window per page; from 2 yrs to 10 yrs)

rm(list=ls())
#setwd("C:/Users/Kristyna/Dropbox/PAHOsurveillance/Analysis")
setwd("~/Dropbox/PAHOsurveillance/Analysis")

library(ggplot2)
source("R/multiplot.r")

countries <- c("Mexico", "Brazil")

for (l in 1:length(countries)){
  cn <- countries[l] #country of interest

  ## set directories and intial lists
  main.directory <- paste0("ClassFrameMS/output/",cn,"/", cn)
  all.windows <- list()
  timewindows <- 2:10
  for(i in 1: length(timewindows)){
    f <- paste0(main.directory, "_classified_yearlyTW",timewindows[i],"yr.csv")
    fstates <- read.csv(f)
    all.windows[[i]] <- fstates
  }
  names(all.windows) <- paste0("tw", timewindows, "yr")

  ## import case data
  fcases <- paste0(main.directory, "_monthly_cases_state0515.csv")
  cases <- read.csv(fcases) # monthly detected cases by state

  ## remove wildlife variants
  wildvars <- read.csv(paste0("ClassFrameMS/data/", cn, "_WildVariants.csv"))
  names <- gsub("[.]", " ", colnames(cases)[1:(ncol(cases)-1)])
  for(i in 1:nrow(wildvars)){
    cind <- which(names==wildvars$State[i])
    rind <- which(cases$date==as.character(wildvars$Date[i]))
    cases[rind, cind] <-  cases[rind, cind]-1
  }

  ## import incursion data and expand to cases like matrix
  incursions <- read.csv(paste0(main.directory, "_IncursionClassification.csv"))
  incursions <- incursions[which(incursions$incursion=="Y"),] #32, 54
  incursions$state <- gsub("[.]", " ", incursions$state)

  incs <- data.frame(matrix(NA, ncol=length(names), nrow=nrow(cases)))
  for(i in 1:nrow(incursions)){
    cind <- which(names==incursions$state[i])
    rind <- which(cases$date==as.character(incursions$date[i]))
    incs[rind, cind] <-  cases[rind, cind]
  }
  incs <- data.frame(incs, date=cases$date)

  ## redistribute TW data by state
  state_names <- gsub("[.]", " ", colnames(cases)[1:ncol(cases)-1])
  all.states <- list()
  for(i in 1:length(state_names)){
    temp.state <- matrix(NA, ncol=length((2005:2015)), nrow=length(timewindows)) # for each state
    for(j in 1:length(timewindows)){ # for each time window
      temp.state [j,] <- as.matrix(rev(all.windows[[j]][i,2:12]))
    }
    all.states[[i]] <- temp.state # TW by years; 9x11
  }
  names(all.states) <- state_names

  ## breaks for background rectangles
  st <- c(1,1:10*12); en <- 1:11*12
  rects <- data.frame(xstart = st, xend = en)
  yrs <- 2005:2015

  ## classification colours
  epicols <- data.frame(colours=c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F"),
                        class=c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic"))

  ## plot
  fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_Classification_ts.pdf")
  pdf(fpdf, width=8, height=5)

  for(s in 1: length(state_names)){ # for each state
    plots <- list()
    for (w in 1:length(timewindows)){ # for each TW

      ## prep colours
      phases <- all.states[[s]][w,]
      rows <- match(phases, epicols$class)
      cols <- as.character(epicols$colours[rows])

      p <- ggplot() + xlab("") + ylab("No. of cases") +
        geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = cols,
                  alpha = 0.5) +
        geom_line(data = cases, aes(as.numeric(date),cases[,s]), size=.5)
      p <- p + scale_x_continuous(breaks=seq(1,132,12), labels=yrs)
      if(sum(incs[,s], na.rm=T)>0){
        p <- p + geom_point(data = incs, aes(as.numeric(date),incs[,s]), na.rm = TRUE, shape=8, colour="blue")
      }

      plots[[w]] <- p + ggtitle(paste(state_names[s],"- TW", timewindows[w], "yrs")) +
        theme(axis.text=element_text(size=5), axis.title=element_text(size=5)) +
        theme(plot.title = element_text(size=7))
    }
    #grid.arrange(plots[[1]], plots[[2]],plots[[3]], ncol=3)
    multiplot(plotlist = plots, cols = 3)
  }
  dev.off()
}

### Mexico and Brazil: all states using 5 yr time window ###
## 2. the second part of the script produces plots of states in Mexico and Brazil between 2005 and 2015
## a) classified yearly (depicted by coloured backrounds) using the time window of 5yrs,
## b) overlaid by cases timeseries and
## c) blue atrices indicating incursions (each country per page)

cn <- "Mexico" # choose Brazil or Mexico
timewindows <- 5

## TW and case data
f <- paste0("ClassFrameMS/output/",cn,"/", cn, "_classified_yearlyTW",timewindows,"yr.csv")
fivewindow <- read.csv(f)

fcases <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_state0515.csv")
cases <- read.csv(fcases)
state_names <- gsub("[.]", " ", colnames(cases)[1:ncol(cases)-1])

## remove wild variants 
wildvars <- read.csv(paste0("ClassFrameMS/data/", cn, "_WildVariants.csv"))
names <- gsub("[.]", " ", colnames(cases)[1:(ncol(cases)-1)])
for( i in 1:nrow(wildvars)){
  cind <- which(names==wildvars$State[i])
  rind <- which(cases$date==as.character(wildvars$Date[i]))
  cases[rind, cind] <-  cases[rind, cind]-1
}

## incursions
incursions <- read.csv(paste0("ClassFrameMS/output/",cn,"/", cn, "_IncursionClassification.csv"))
incursions <- incursions[which(incursions$incursion=="Y"),] #32, 54
incursions$state <- gsub("[.]", " ", incursions$state)

incs <- data.frame(matrix(NA, ncol=length(names), nrow=nrow(cases)))
for(i in 1:nrow(incursions)){
  cind <- which(names==incursions$state[i])
  rind <- which(cases$date==as.character(incursions$date[i]))
  incs[rind, cind] <-  cases[rind, cind]
}
incs <- data.frame(incs, date=cases$date)

## breaks for background rectangles
st <- c(1,1:10*12); en <- 1:11*12
rects <- data.frame(xstart = st, xend = en)
yrs <- 2005:2016

## classification colours
epicols <- data.frame(colours=c("white","#FDFCD6","lightsalmon2","orange","red"),
                      class=c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic"))

# ## plot: this somehow doesn't work
# fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_Classification_ts5TW.pdf")
# pdf(fpdf, width=9, height=5)
#
# plots <- list()
# for(s in 1:length(state_names)){ # for each state
#   ## prep colours
#   phases <- as.matrix(rev(fivewindow[s,])[2:12])
#   rows <- match(phases, epicols$class)
#   cols <- as.character(epicols$colours[rows])
#   cases$num.mnth <- as.numeric(cases$date)
#
#     p <- ggplot() + xlab("") + ylab("No. of cases") +
#       geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = cols,
#                 alpha = 0.5) +
#       geom_line(data = cases, aes(as.numeric(date),cases[,s]), size=.5)
#     p <- p + scale_x_continuous(breaks=c(1,1:10*12,132), labels=yrs)
#
#     if(sum(incs[,s], na.rm=T)>0){
#      p <- p + geom_point(data = incs, aes(as.numeric(date),incs[,s]), na.rm = TRUE, shape=8, colour="blue")
#     }
#
#     p <- p + ggtitle(state_names[s]) +
#       theme(axis.text=element_text(size=2), axis.title=element_text(size=2)) +
#       theme(plot.title = element_text(size=7))
#
#     print(p)
#     plots[[s]] <- p
# }

## plot
myplots <- list()  # new empty list
for(s in 1:length(state_names))
  local({
    s <- s

    ## prep colours
    phases <- as.matrix(rev(fivewindow[s,])[2:12])
    rows <- match(phases, epicols$class)
    cols <- as.character(epicols$colours[rows])
    cases$num.mnth <- as.numeric(cases$date)

    p <- ggplot() + xlab("") + ylab("No. of cases") +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = 0, ymax = 40), fill = cols,
                alpha = 0.5) +
      geom_line(data = cases, aes(as.numeric(date),cases[,s]), size=.5)
    p <- p + scale_x_continuous(breaks=c(1,1:10*12,132), labels=yrs) # ymax=30 Mex, 40 Bra

    # if(sum(incs[,s], na.rm=T)>0){
    #   p <- p + geom_point(data = incs, aes(as.numeric(date),incs[,s]), na.rm = TRUE, shape=8, colour="blue")
    # }

    p1 <- p + ggtitle(state_names[s]) +
      theme(axis.text=element_text(size=2), axis.title=element_text(size=2)) +
      theme(plot.title = element_text(size=5))

    print(p1)
    myplots[[s]] <<- p1  # add each plot into plot list
  })

fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_Classification_ts5TW_noinc.pdf")
pdf(fpdf, width=9, height=5)
multiplot(plotlist = myplots, cols = 7)
dev.off()

### Mexico: problematic states ###
## 3. the third part of the script produces plots of states in Mexico whose classification differs
## widely depending on what time window of data fitted was used. These states were
## a) classified yearly (depicted by coloured backrounds, for 2005-2015),
## b) overlaid by cases timeseries and
## c) blue atrices indicating incursions (each state per page)

cn <- "Mexico"

## set directories and intial lists
main.directory <- paste0("ClassFrameMS/output/",cn,"/", cn)
all.windows <- list()
timewindows <- 4:6
for(i in 1: length(timewindows)){
  f <- paste0(main.directory, "_classified_yearlyTW",timewindows[i],"yr.csv")
  fstates <- read.csv(f)
  all.windows[[i]] <- fstates
}
names(all.windows) <- paste0("tw", timewindows, "yr")

## import case data
fcases <- paste0(main.directory, "_monthly_cases_state0515.csv")
cases <- read.csv(fcases)

## remove wild variants
wildvars <- read.csv(paste0("ClassFrameMS/data/", cn, "_WildVariants.csv"))
names <- gsub("[.]", " ", colnames(cases)[1:(ncol(cases)-1)])
for( i in 1:nrow(wildvars)){
  cind <- which(names==wildvars$State[i])
  rind <- which(cases$date==as.character(wildvars$Date[i]))
  cases[rind, cind] <-  cases[rind, cind]-1
}

## import incursion data and expand to cases like matrix
incursions <- read.csv(paste0(main.directory, "_IncursionClassification.csv"))
incursions <- incursions[which(incursions$incursion=="Y"),] #32
incursions$state <- gsub("[.]", " ", incursions$state)

incs <- data.frame(matrix(NA, ncol=length(names), nrow=nrow(cases)))
for(i in 1:nrow(incursions)){
  cind <- which(names==incursions$state[i])
  rind <- which(cases$date==as.character(incursions$date[i]))
  incs[rind, cind] <-  cases[rind, cind]
}
incs <- data.frame(incs, date=cases$date)

## redistribute TW data by state
state_names <- gsub("[.]", " ", colnames(cases)[1:ncol(cases)-1])
all.states <- list()
for(i in 1:length(state_names)){
  temp.state <- matrix(NA, ncol=length((2005:2015)), nrow=length(timewindows)) # for each state
  for(j in 1:length(timewindows)){ # for each time window
    temp.state [j,] <- as.matrix(rev(all.windows[[j]][i,2:12]))
  }
  all.states[[i]] <- temp.state # TW by years; 9x11
}
names(all.states) <- state_names

## breaks for background rectangles
st <- c(1,1:10*12); en <- 1:11*12
rects <- data.frame(xstart = st, xend = en)
yrs <- 2005:2015

## classification colours
epicols <- data.frame(colours=c("white","#FDFCD6","lightsalmon2","orange","red"),
                      class=c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic"))

## plot
problem.states <- c("Hidalgo", "Mexico", "Oaxaca", "Yucatan","Veracruz de Ignacio de la Llave")

fpdf <- paste0("ClassFrameMS/figs/",cn, "/",cn,"_ProblematicClassification_ts.pdf")
pdf(fpdf, width=4, height=6)

for(n in 1: length(problem.states)){ # for each state
  s <- which(problem.states[n]==as.character(state_names))
  plots <- list()
  for (w in 1:length(timewindows)){ # for each TW
    ## prep colours
    phases <- all.states[[s]][w,]
    rows <- match(phases, epicols$class)
    cols <- as.character(epicols$colours[rows])

    p <- ggplot() + xlab("") + ylab("No. of cases") +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = cols,
                alpha = 0.5) +
      geom_line(data = cases, aes(as.numeric(date),cases[,s]), size=.5)
    p <- p + scale_x_continuous(breaks=seq(1,132,12), labels=yrs)
    if(sum(incs[,s], na.rm=T)>0){
      p <- p + geom_point(data = incs, aes(as.numeric(date),incs[,s]), na.rm = TRUE, shape=8, colour="blue")
    }

    plots[[w]] <- p + ggtitle(paste(state_names[s],"- TW", timewindows[w], "yrs")) +
      theme(axis.text=element_text(size=5), axis.title=element_text(size=5)) +
      theme(plot.title = element_text(size=7))
  }
  multiplot(plotlist = plots, cols = 1)
}
dev.off()


