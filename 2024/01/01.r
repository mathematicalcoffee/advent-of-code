rm(list=ls())
library(stringi)
library(data.table)
source("../handy.R")
x <- read.table("input.txt", header=FALSE)
first <- x$V1
sec <- x$V2

# part 1
print(
  sum(abs(sort(first) - sort(sec)))
)

# part 2 - totally inefficient but convenient (tabulate makes an arr of len 1:max(input) with the # occurrences of that int...)
print(
  sum(seq_len(max(first)) * tabulate(first) * tabulate(sec)[1:max(first)], na.rm=TRUE)
)

# maybe a more efficient version (str indices suck tho)
f1 <- table(first)
f2 <- table(sec)
print(
  sum(as.integer(names(f1)) * f1 * f2[names(f1)], na.rm=TRUE)
)
