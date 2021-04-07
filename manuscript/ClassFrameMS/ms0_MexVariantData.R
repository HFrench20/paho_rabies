### MEXICO: Variant Data
rm(list=ls())
setwd("~/Dropbox/PAHOsurveillance/Analysis")

## import
# cases
cn <- "Mexico"
fstates <- paste0("ClassFrameMS/output/",cn,"/", cn, "_monthly_cases_state0515.csv")
states <- read.csv(fstates)

# variants
vars <- read.csv("ClassFrameMS/data/Mex_StateVariants.csv")
table(vars$Variant)

## clean variant data
vars$Variant <- toupper(vars$Variant)
vars$Variant[vars$Variant=="PERRO"] <- "dog"
vars$Variant[vars$Variant=="SILVESTRE"] <- "wild"
vars[vars$Variant=="wild",]

wildvar <- vars[which(vars$Variant=="Silvestre"),]

# save
write.csv(wildvar, "ClassFrameMS/data/Mex_WildVariants.csv", row.names=F)
write.csv(vars, "ClassFrameMS/data/Mex_StateVariants.csv", row.names=F)

