rm(list=ls())
library(stringi)
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

reports <- lapply(strsplit(input, " "), as.integer)

is.safe <- function (report) {
    diffs <- diff(report)
    adiffs <- abs(diffs)
    return (uniqueN(sign(diffs)) == 1 && sign(diffs[1]) != 0 && all(adiffs >= 1 & adiffs <= 3))
}

# part 1
print(sum(unlist(lapply(reports, is.safe))))

# part 2 - for-loops are so dirty but so much easier to write lol
# I am sure there is a smart way to do this but I don't know what it is
nsafe <- 0
for (i in 1:length(reports)) {
  original.report <- reports[[i]]
  
  for (j in 0:length(original.report)) {
    report <- original.report
    # skip this bit for part 1
    if (j > 0) report <- original.report[-j]
    
    if (is.safe(report)) {
      nsafe <- nsafe + 1
      break
    }
  }
}
print(nsafe)
