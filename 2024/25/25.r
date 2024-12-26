rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

lines <- input
keys <- array(0, dim=c(5, 0))
locks <- array(0, dim=c(5, 0))

i <- 1
while (i < length(lines)) {
  thing <- lines[i + (0:6)]
  thing <- lines2matrix(thing)
  repr <- colSums(thing == "#") - 1
  if (thing[1,1] == "#" & thing[7, 1] == ".") {  # lock
    locks <- cbind(locks, repr)
  } else if (thing[1,1] == "." & thing[7,1] == "#") { # key
    keys <- cbind(keys, repr)
  } else {
    stop()
  }
  i <- i + 8
}
message(sprintf("%i locks and %i keys", ncol(locks), ncol(keys)))

fits <- apply(locks, MARGIN=2, function (lock) colSums(lock + keys <= 5) == 5)
print(sum(fits))
