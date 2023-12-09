rm(list = ls())
# read data
y <- read.table("input.txt", header = F, sep = " ")
y <- array(as.numeric(unlist(y)), dim = dim(y))
# smash it with the hammer that is polynomial regression coz why not.
x <- seq_len(ncol(y))
predicted <- apply(
  y,
  1,
  function(yy) {
    predict(
      lm(yy ~ poly(x, degree = ncol(y) - 1)), # fit a model
      newdata = list(x = c(ncol(y) + 1, 0)) # get predictions
    )
  }
)
print(round(rowSums(predicted))) # the rounding just so R doesn't use scientific

# --------------------------------------------------------------------------- #
# or the way they probably wanted us to do it
predict.next <- function(coef, retval = 0) {
  if (all(coef == 0)) {
    return(retval)
  } else {
    return(predict.next(diff(coef), retval + coef[length(coef)]))
  }
}
print(sum(apply(y, 1, predict.next))) # next value
print(sum(apply(y[, rev(seq_len(ncol(y)))], 1, predict.next))) # previous value
