rm(list=ls())
library(stringi)
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()
input <- paste(input, collapse="") # <-- derp. multiple lines. I assumed it reset each line.

sum_mul <- function (text) {
  x <- stri_match_all(
    text,
    regex="mul\\(([0-9]+),([0-9]+)\\)"
  )
  
  return(sum(unlist(lapply(x,
         function (xx)
        sum(as.integer(xx[,2]) * as.integer(xx[,3]), na.rm=TRUE)
   ))))
  
}
print(sum_mul(ex))
print(sum_mul(input))

# PART 2
ex <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
exclude_donts <- function (text) {
  x <- stri_match_all(text, regex="(^|don't\\(\\)|do\\(\\))(.*?)(?=don't\\(\\)|do\\(\\)|$)")[[1]]
  keep <- x[x[, 2] != "don't()", , drop=FALSE]
  return(paste(keep[,3], collapse=""))
}

print(sum_mul(exclude_donts(ex)))
print(sum_mul(exclude_donts(input)))
