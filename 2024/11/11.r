rm(list=ls())
library(data.table)
source("../../handy.R")
#ex <- get_and_save_example_input()
input <- get_and_save_input()
ex <- "0 1 10 99 999"
ex2 <- "125 17"

parse_stones <- function (lines) {
  as.integer(stri_split_fixed(pattern=" ", lines)[[1]])
}


even_digits <- function (stone) {
  n_digits(stone) %% 2 == 0
}
n_digits <- function (stone) {
  if (stone == 0) return(1)
  if (stone < 0) return(n_digits(-stone))
  floor(log10(stone)) + 1
}
left_half <- function (stone, .n_digits) { # assumes n_digits is even
  if (missing(.n_digits)) n <- n_digits(stone) else n <- .n_digits
  stopifnot(n %% 2 == 0)
  return(floor(stone / 10^(n / 2)))
}
right_half <- function (stone, .n_digits) {
  if (missing(.n_digits)) n <- n_digits(stone) else n <- .n_digits
  stopifnot(n %% 2 == 0)
  return(stone %% 10^(n / 2))
}

# dumbdumb
blink <- function (stones) {
  new_stones <- NULL
  for (stone in stones) {
    if (stone == 0) {
      new_stones <- c(new_stones, 1)
    } else if (even_digits(stone)) {
      new_stones <- c(new_stones, left_half(stone), right_half(stone))
    } else {
      new_stones <- c(new_stones, stone * 2024)
    }
  }
  return(new_stones)
}


n_stones_spawned_memo <- function(stone, n_blinks_left) {
  if (n_blinks_left == 0) {
    return(1)
  }
  if (stone == 0) {
    return(
      n_stones_spawned_memo(1, n_blinks_left - 1)
    )
  } else if (even_digits(stone)) {
    return(
      n_stones_spawned_memo(left_half(stone), n_blinks_left - 1) +
        n_stones_spawned_memo(right_half(stone), n_blinks_left - 1)
    )
  } else {
    return(
      n_stones_spawned_memo(stone * 2024, n_blinks_left - 1)
    )
  }
}

to_cache <- function(stone, n_blinks_left, value) {
  key <- as.character(stone)
  if (is.null(cache[[key]]))
    cache[[key]] <- rep(NA, n_blinks_left)
  cache[[key]][n_blinks_left] <<- value
  return(value)
}
from_cache <- function(stone, n_blinks_left) {
  key <- as.character(stone)
  if (!is.null(cache[[key]]) && !is.na(ans <- cache[[key]][n_blinks_left])) {
    return(ans)
  }
  return(NULL)
}
calc_and_cache <- function (stone, n_blinks_left) {
  x <- n_stones_spawned_cache(stone, n_blinks_left)
  return(to_cache(stone, n_blinks_left, x))
}

# cache should be an ENVIRONMENT
n_stones_spawned_cache <- function(stone, n_blinks_left) {
  if (n_blinks_left == 0) {
    return(1)
  }
  if (!is.null(ans <- from_cache(stone, n_blinks_left)))
    return(ans)

  if (stone == 0) {
    return(
      calc_and_cache(1, n_blinks_left - 1)
    )
  } else if (even_digits(stone)) {
    return(
      calc_and_cache(left_half(stone), n_blinks_left - 1) +
        calc_and_cache(right_half(stone), n_blinks_left - 1)
    )
  } else {
    return(
      calc_and_cache(stone * 2024, n_blinks_left - 1)
    )
  }
}


# global cache is fine
n_stones_spawned_memo <- memoise(n_stones_spawned_memo)
total_n_stones_memo <- function (stones, n_blinks) {
  sum(sapply(stones, n_stones_spawned_memo, n_blinks_left=n_blinks))
}

cache <- list() # a global, yuck
total_n_stones_cache <- function (stones, n_blinks) {
  sum(sapply(stones, n_stones_spawned_cache, n_blinks_left=n_blinks))
}

library(memoise)
stones <- c(125, 17)
#print(blink(stones))
#print(total_n_stones_memo(stones, 25))
print(total_n_stones_cache(stones, 25))

stones <- parse_stones(input)
#print(total_n_stones_memo(stones, 25))
print(total_n_stones_cache(stones, 25))

#print(system.time(total_n_stones_memo(stones, 75))) # about 30s
print(system.time(total_n_stones_cache(stones, 75))) # about 20s? sweet.
