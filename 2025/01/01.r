rm(list=ls())
library(data.table)
library(testthat)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

x <- input
START <- 50
MOD <- 100

part1 <- function (x, START=50, MOD=100) {
  dir <- substring(x, 1, 1) # direction
  # number of clicks, -ve is left, +ve is right
  mag <- ifelse(dir == "L", -1, +1) * as.numeric(substring(x,2))
  return(sum((START + cumsum(mag)) %% MOD == 0))
}

expect_equal(
  part1(ex), 3
)
part1(input)

# Part 2
part2_so_smart_it_doesnt_work <- function (x, START=50, MOD=100) {
  # probably doesn't handle the case of START==0
  
  dir <- substring(x, 1, 1) # direction
  # number of clicks, -ve is left, +ve is right
  mag <- ifelse(dir == "L", -1, +1) * as.numeric(substring(x,2))
 
  # position (without modulo) including the start
  pos <- START + c(0, cumsum(mag))
  # which 'hundred' we are in
  which.hundred <- floor(pos / MOD)
  
  # this seems to double-count some things to do with starting/ending on 0
  # and I'm not smart enough to work it out
  sum(abs(diff(which.hundred)))

  end.on.zero <- pos %% MOD == 0
 
  N <- length(pos) 
  # spinning more than once
  sum(floor(abs(mag / MOD))) +
  # ending on a zero
  sum(end.on.zero) +
  # passing a zero (without ending or starting on it, already counted those)
  sum(diff(which.hundred) != 0 & !end.on.zero[-1] & !end.on.zero[-N])
}

# STOP TRYING TO BE SMART, BE A DUMMY :'(
# OF COURSE WORKED FIRST TIME :'(
part2 <- function (x, START=50, MOD=100) {
  count <- 0
  
  dir <- ifelse(substring(x, 1, 1) == "L", -1, 1) # direction
  nclicks <- as.numeric(substring(x,2))
  
  turn.dial <- function(pos, dir) { return(pos + dir) }
  
  curr.pos <- START 
  if (curr.pos %% MOD == 0) count <- count + 1
  for (instruction.i in seq_along(dir)) {
    increment <- dir[instruction.i]
    for (click.i in seq_len(nclicks[instruction.i])) {
      curr.pos <- turn.dial(curr.pos, increment)
      if (curr.pos %% MOD == 0) count <- count + 1
    }
  }
  
  return(count)
}

expect_equal(
  part2(ex), 6
)
# 50 -> 0
expect_equal(
  part2("L50"), 1
)
# 50 -> 0
expect_equal(
  part2("L150"), 2
)
# 50 -> 0 -> 0
expect_equal(
  part2(c("L50", "R100")), 2
)
# 50 -> 0 -> 50[+1 rev]
expect_equal(
  part2(c("L50", "R150")), 2
)

part2(input)


