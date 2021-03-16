## Commands to test that 0.Add_ADM1_code_v2.R is working correctly. 


## ------------- Part 1: Testing the process for a single country ---------------------------

# Define a single cn value and run bits of the for loop in 0.Add_ADM1_code_v2 to test:
# e.g
cn = "MEX"

# Create a vector with the unique GID_1 codes in:
unique.gids <- unique(key.cn$GID_1)

# Subset the country of interest
test.cn <- filter(dogs.ISO.state, ADM0_ISO == cn)

# compare the sums for the filtered country and the non-filtered:
# note the number of Sin Informacion will be for all countries. 

total.sum <- 0
for (code in unique.gids) {
  sum.code <- length(which(test.cn$GID_1 == code))
  #print(paste(code, "occurs", sum.code, "times"))
  total.sum <- total.sum + sum.code
  print(paste(code, ": new total sum is", total.sum))
}
nrow(test.cn)

total.sum <- 0
for (code in unique.gids) {
  sum.code <- length(which(dogs.ISO.state$GID_1 == code))
  #print(paste(code, "occurs", sum.code, "times"))
  total.sum <- total.sum + sum.code
  print(paste(code, ": occurs", sum.code, "times, new total sum is", total.sum))
}
total.rows <- nrow(dogs.ISO.state)
print(paste("total rows in dogs.ISO.state= ", total.rows))
sum.na <- sum(is.na(dogs.ISO.state$GID_1))
test.total.rows <- sum.na + total.sum
print(paste0("sum.na (", sum.na, ") + total.sum merged (", total.sum, ") = ", test.total.rows))


## ------------ Part 2: Testing the final output (all countries) -------------------------------

# Run the script 0.Add_ADM1_code_v2 as normal. Then test:

# Check all the numbers add up, in total and for a couple of test countries.
# Check that some of the updated Key additions are correct.


# Check there is the same number of total rows and columns as expected after merge:
length(stacking.df)
nrow(stacking.df)
length(dogs.ISO)
nrow(dogs.ISO)

# Filter subsets of rows for selected countries to test in more depth:
start.DOM <- filter(dogs.ISO, ADM0_ISO == "DOM")
test.DOM <- filter(stacking.df, ADM0_ISO == "DOM")

# Compare number of rows for this given country
nrow(start.DOM)
nrow(test.DOM)
nrow(filter(start.DOM, UnidMaior == "Sin Informacion"))
nrow(filter(test.DOM, GID_1 == "Sin Informacion"))

# Compare the unique values from before and after the merge for a given country:
unique(start.DOM$UnidMaior)
unique(test.DOM$GID_1)

# Check there is the same number of rows before and after the merge for a given Name:Key pair.
nrow(filter(start.DOM, UnidMaior == "Elias Pina"))
nrow(filter(test.DOM, GID_1 == "DOM.12_1"))

# Check that all of the DOM.12_1's are what I expect (no surprises)
filter(test.DOM, GID_1 == "DOM.12_1")

# Check the other values of DOM.12_1 which were not in the surveillance data have not been added in somehow:
filter(test.DOM, UnidMaior == "Elias Pina")
filter(test.DOM, UnidMaior == "La Estrelleta")

## ------------- end. 

