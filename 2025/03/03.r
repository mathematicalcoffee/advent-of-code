rm(list=ls())
library(data.table)
library(testthat)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()
# for apply(...)
ROW <- 1
COL <- 2


find.joltage <- function (bank) {
  n <- length(bank)
  first.digit.i <- which.max(head(bank, -1))
  second.digit <- max(tail(bank, -first.digit.i))
  bank[first.digit.i] * 10 + second.digit
}
part1 <- function (str_input) {
  batteries <- lines2matrix(str_input, numeric=TRUE)
  joltages <- apply(batteries, ROW, find.joltage)
  sum(joltages)
}

expect_equal(
  apply(lines2matrix(ex, numeric=TRUE), ROW, find.joltage),
  c(98, 89, 78, 92)
)
expect_equal(
  part1(ex), 357
)

part1(input)

# --------- part2 ----------------

find.joltage <- function (bank, n.batteries) {
  n <- length(bank)
  # inclusive
  lowest.i <- 1
  highest.i <- n - n.batteries + 1  # 1-based
  joltages <- rep(0, n.batteries)
  
  for (b in seq_len(n.batteries)) {
    next.digit.i <- which.max(bank[lowest.i:highest.i]) + lowest.i - 1
    joltages[b] <- bank[next.digit.i]
    
    lowest.i <- next.digit.i + 1
    highest.i <- highest.i + 1
  }
  
  sum(joltages * rev(10^seq(0, n.batteries - 1)))
}

part2 <- function (str_input, n.batteries) {
  batteries <- lines2matrix(str_input, numeric=TRUE)
  joltages <- apply(batteries, ROW, find.joltage, n.batteries)
  sum(joltages)
}

# part1
expect_equal(
  apply(
    lines2matrix(ex, numeric=TRUE),
    ROW,
    find.joltage,
    n.batteries=2
  ),
  c(98, 89, 78, 92)
)
expect_equal(
  part2(ex, n.batteries=2), 357
)
# part2
expect_equal(
  part2(ex, n.batteries=12), 3121910778619
)
part2(input, n.batteries=12)
