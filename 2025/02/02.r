rm(list=ls())
library(data.table)
library(stringi)
library(testthat)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_input <- function (str) {
  matrix(
    as.numeric(unlist(stri_split_fixed(stri_split(paste(str, collapse=""), regex=",")[[1]], pattern="-", n=2))),
    ncol=2,
    byrow=TRUE
  )
}

# candidate_id X is valid if it is of the form X = z + 10^N * z = z(1 + 10^N),
#  where  N = floor(log_10(z) + 1)
#   for some z.
# given x, N must be something like log10(x)/2

# this returns "half" the number of digits of the candidate ID, rounded up.
find.n <- function (candidate_id) {
  return(ceiling(n_digits(candidate_id) / 2))
}
expect_equal(find.n(11), 1)   # 1 + (10^1)*1
expect_equal(find.n(1111), 2) # 11 + (10^2)*11
expect_equal(find.n(1010), 2) # 10 + (10^2)*10
expect_equal(find.n(100), 2)
expect_equal(find.n(1009), 2)
expect_equal(find.n(10090), 3)

find.divisor <- function (candidate_id) {
  # finds the appropriate (1 + 10^N) to divide by
  # N = half the number of digits of the candidate ID (arbitrary round up)
  return(1 + 10^(find.n(candidate_id)))
}

expect_equal(find.divisor(11), 11)
expect_equal(find.divisor(1212), 101)
expect_equal(find.divisor(123123), 1001)
# ??? not really sure.
expect_equal(find.divisor(100), 101)
expect_equal(find.divisor(1009), 101)

is.valid.id <- function (candidate_id) {
  if (candidate_id <= 0) {
    return(FALSE)
  }
  return(candidate_id %% find.divisor(candidate_id) == 0)
}

expect_true(is.valid.id(11))
expect_true(is.valid.id(22))
expect_true(is.valid.id(123123))

expect_false(is.valid.id(222))
expect_false(is.valid.id(9))
expect_false(is.valid.id(-19))
expect_false(is.valid.id(1231232))


generate.id <- function (z) {
  # candidate_id X is valid if it is of the form X = z + 10^N * z = z(1 + 10^N),
  #  where  N = number of digits in z
  N <- n_digits(z)
  return(z * (1 + 10^N))
}

smallest.z.for <- function (n) {
  # smallest number that is this many digits wide
  return(10^(n - 1))
}

smallest_id_geq_to <- function (x) {
  n <- find.n(x) # rounds up
  div <- 1 + 10^n
  
  smallest_z <- max(smallest.z.for(n), ceiling(x / div))
  
  return(c(smallest_z * div, smallest_z, n))
}
expect_equal(smallest_id_geq_to(11)[1], 11)
expect_equal(smallest_id_geq_to(12)[1], 22)
expect_equal(smallest_id_geq_to(123)[1], 1010)
expect_equal(smallest_id_geq_to(1234)[1], 1313)

valid_ids_between <- function (lower, upper) {
  # find smallest valid ID >= the lower
  bits <- smallest_id_geq_to(lower)
  smallest_z <- bits[2]
 
  valid <- c() 
  z <- smallest_z
  while ((id <- generate.id(z)) <= upper) {
    valid <- c(valid, id)
    z <- z + 1
  }
  return(valid)
}

numb_between <- function (lower, upper) {
  return(length(valid_ids_between(lower, upper)))
}

ranges <- parse_input(ex)
expect_equal(numb_between(11, 22), 2)
expect_equal(numb_between(95, 115), 1)
expect_equal(numb_between(998, 1012), 1)
expect_equal(numb_between(1188511880, 1188511890), 1)

expected <- c(2, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0)
expect_equal(
  mapply(numb_between, ranges[,1], ranges[,2]),
  expected
)

part1 <- function (str_input) {
  ranges <- parse_input(str_input)
  return(
    sum(
      unlist(
        mapply(
          valid_ids_between, ranges[,1], ranges[,2]
        )
      )
    )
  )
}  
expect_equal(part1(ex), 1227775554)
part1(input)
 
# --------------------------------------- 
# PART 2
# wow. I have no idea how to do this.
# OK, it turned out to be basically same as part1 but my brain is fried.


gen.divisor <- function(ndigits_z, nreps) {
  sum(10 ^ seq(0, length.out=nreps, by=ndigits_z))
}
generate.id <- function(z, nreps) {
  sum(z * gen.divisor(n_digits(z), nreps))
}
expect_equal(generate.id(1, 3), 111)
expect_equal(generate.id(12, 3), 121212)


smallest_z_geq_to <- function (x, nreps) {
  nd <- n_digits(x)
  ndigits_z <- ceiling(nd / nreps)
  divisor <- gen.divisor(ndigits_z, nreps)
  z <- max(smallest.z.for(ndigits_z), ceiling(x / divisor))
  return(z)
}

valid_ids_between <- function (lower, upper, nreps) {
  valid <- c()
  
  # find the smallest valid ID > upper for this number of repetitions
  z <- smallest_z_geq_to(lower, nreps)
  while ((id <- generate.id(z, nreps)) <= upper) {
    valid <- c(valid, id)
    z <- z + 1
  }
  return(valid)
}
all_valid_ids_between <- function (lower, upper) {
  unique(unlist(lapply(
    2:n_digits(upper),
    valid_ids_between,
    lower=lower,
    upper=upper
  )))
}
numb_between <- function (lower, upper) {
  return(length(all_valid_ids_between(lower, upper)))
}


expect_equal(numb_between(11, 22), 2)
expect_equal(numb_between(95, 115), 2)
expect_equal(numb_between(998, 1012), 2)
expect_equal(numb_between(1188511880, 1188511890), 1)

all_valid_ids_between(1188511880, 1188511890)

ranges <- parse_input(ex)
expected <- c(2, 2, 2, 1, 1, 0, 1, 1, 1, 1, 1)
expect_equal(
  mapply(numb_between, ranges[,1], ranges[,2]),
  expected
)

part2 <- function (str_input) {
  ranges <- parse_input(str_input)
  return(
    sum(
      unlist(
        mapply(
          all_valid_ids_between, ranges[,1], ranges[,2]
        )
      )
    )
  )
}  
expect_equal(part2(ex), 4174379265)
# 33832678369: too low - blah, my shortcut was shortcutting too early LOL
# 33832678380
part2(input)
