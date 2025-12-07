rm(list=ls())
library(data.table)
library(testthat)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_inputs <- function(str_input) {
  split.i <- which(str_input == "")
  return(
    list(
      fresh.ranges=matrix(
        as.numeric(unlist(stri_split_fixed(head(str_input, split.i - 1), pattern="-", n=2))),
        ncol=2,
        byrow=TRUE
      ),
      available=as.numeric(tail(str_input, -split.i))
    )
  )
} 

are_fresh <- function(ingredients, ranges) {
  vapply(
    ingredients,
    function (id) sum(id >= ranges[,1] & id <= ranges[,2]) > 0,
    FUN.VALUE=FALSE
  )
}

part1 <- function (str_input) {
  bits <- parse_inputs(str_input)
  sum(are_fresh(bits$available, bits$fresh.ranges))
}

expect_equal(part1(ex), 3)
part1(input)

# --------- part2 ----------
# damn I feel so smart (but it's probs got bugs)
dedupe.ranges <- function(ranges) {
  # plan: sort the starts and ends in one giant list, but
  #  calculate a 'parity': a +1 if you hit a start, a -1 if you hit an end.
  #  when parity == 0 you are out of the overlapping set.
  starts <- ranges[, 1]
  ends <- ranges[, 2]
  
  parities <- rep(c(1, -1), c(length(starts), length(ends)))
  points.of.interest <- c(starts, ends)

  ordered.POIs <- order(points.of.interest)
  sorted.POIs <- points.of.interest[ordered.POIs]
  
  end.is <- which(cumsum(parities[ordered.POIs]) == 0)
  start.is <- c(1, head(end.is + 1, -1))
  return(
    cbind(
      sorted.POIs[start.is],
      sorted.POIs[end.is]
    )
  )
}  

# 1--5
#    5--8
expect_equal(
  dedupe.ranges(
    rbind(
      c(1,5),
      c(5,8)
    )
  ),
  cbind(1, 8)
)
  
# 1---5
#  2--5
#   3----8
#         9---10
#                 13-15
# should give me 1-10 and 13-15
# (not fussed about whether 1-8 + 9-10 or 1-10)
expect_equal(
  dedupe.ranges(
    rbind(
      c(1,5),
      c(2,5),
      c(3,8),
      c(9,10),
      c(13,15)
    )[sample(5), ] # shuffle order
  ),
  rbind(
    c(1, 8),
    c(9, 10),
    c(13, 15)
  )
)

expect_equal(
  dedupe.ranges(
    rbind(
      c(1, 3),
      c(2, 4)
    )
  ),
  cbind(1, 4)
)

expect_equal(
  dedupe.ranges(
    rbind(
      c(1, 3),
      c(2, 4),
      c(4, 8),
      c(2, 5)
    )
  ),
  cbind(1,8)
)

part2 <- function (str_input) {
  bits <- parse_inputs(str_input)
  deduped.ranges <- dedupe.ranges(bits$fresh.ranges)
  sum(deduped.ranges[, 2] - deduped.ranges[, 1] + 1)
}
expect_equal(part2(ex), 14)
part2(input)
