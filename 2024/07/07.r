rm(list=ls())
library(data.table)
source("../../handy.R")
options(digits=20)
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_input <- function(lines) {
  # as.integer is too small...and R doesn't have native int64? well, it _says_ it can hold large ints in doubles exactly...
  return(lapply(stri_split_regex(pattern=": | ", lines), as.double))
}

recursive.fun <- function(test_value, numbers, ops, current_value, next.i, op, n) {
  if (missing(next.i) || missing(current_value) || missing(op)) {
    # assumptions all numbers > 0 (no 0s, all ops monotonic non-decreasing then)
    stopifnot(all(numbers > 0))
    next.i <- 1
    new_value <- numbers[1]
    n <- length(numbers)
  } else {
    new_value <- op(current_value, numbers[next.i])
  }
  if (next.i == n) {
    return(new_value == test_value)
  } else if (new_value > test_value) {
   # * and + and concat are all monotonic non-decreasing on +ve ints
   return(FALSE)
  } else {
    for (op in ops) {
      if (recursive.fun(
          current_value=new_value,
          next.i=next.i + 1,
          op=op,
          n=n,
          test_value=test_value,
          numbers=numbers,
          ops=ops
      )) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
} 

# ---------

eqns <- parse_input(input)
test_values <- sapply(eqns, `[`, i=1)

# part 1
OPS1 <- c(`*`, `+`)
valid1 <- sapply(eqns, function (eqn) recursive.fun(eqn[1], tail(eqn, -1), OPS1))
message(sum(test_values[valid1]))

# part 2 - only check the non-valid from part 1
concat <- function (x, y) {
  x*10^(floor(log10(y)) + 1) + y
}
OPS2 <- c(OPS1, concat)

valid2 <- sapply(
  seq_along(eqns),
  function (i) {
    if (valid1[i]) return(valid1[i])
    return(recursive.fun(eqns[[i]][1], tail(eqns[[i]], -1), OPS2))
  }
)
message(sum(test_values[valid2]))
