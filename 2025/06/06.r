rm(list=ls())
library(data.table)
library(testthat)
source("../../handy.R")
COL <- 2
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_inputs <- function (input_str) {
  problems <- read.table(text=paste(input_str, collapse="\n"))
  nrows <- length(input_str)
  nums <- unname(lapply(as.list(problems[1:(nrows - 1), ]), as.numeric))
  ops <- stri_split(regex=" +", stri_trim(tail(input_str,1)))[[1]]
  return(
    list(
      nums=nums,
      ops=ops
    )
  )
}
do.problems <- function(bits) {
  vapply(
    seq_along(bits$ops),
    function (i) {
      op <- bits$ops[i]
      nums <- bits$nums[[i]]
      if (op == "*") {
        return(prod(nums))
      } else if (op == "+") {
        return(sum(nums))
      } else {
        stop(paste0("unrecognised operation '", op, "'"))
      }
    },
    FUN.VALUE=0
  )
} 

part1 <- function (input_str) {
  bits <- parse_inputs(input_str)
  ans <- do.problems(bits)
  sum(ans)
}

local({
  bits <- parse_inputs(ex)
  expect_equal(
    do.problems(bits),
    c(33210, 490, 4243455, 401)
  )
})

expect_equal(part1(ex), 4277556)
expect_equal(part1(input), 4805473544166)

# ----------- part 2 --------------
# LOL
parse_inputs <- function (input_str) {
  nums <- lines2matrix(head(input_str, -1), numeric=TRUE)
  ndigits <- nrow(nums)
  nums <- apply(
    nums[ndigits:1, ncol(nums):1],
    COL,
    function (digits) {
      digits <- na.omit(digits)
      sum(digits * 10^(0:(length(digits) - 1)))
    }
  )
  # chunk it up (brittle, relies on no 0s)
  nums <- unname(split(nums[nums != 0], cumsum(nums == 0)[nums != 0]))
  ops <- rev(stri_split(regex=" +", stri_trim(tail(input_str,1)))[[1]])
  return(
    list(
      nums=nums,
      ops=ops
    )
  )
}
local({
  bits <- parse_inputs(ex)
  expect_equal(
    do.problems(bits),
    c(1058, 3253600, 625, 8544)
  )
})

expect_equal(part1(ex), 3263827)
expect_equal(part1(input), 8907730960817)
