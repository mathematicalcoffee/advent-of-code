rm(list=ls())
library(data.table)
library(igraph)
library(testthat)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

num_accessible <- function(grid, max_nei=3, print_grid=FALSE) {
  # dumdum way - gogo bruteforceman
  # (I'm not entirely sure which part is so slow, the as.matrix or the expand.grid maybe)
  # this is super slow
  if (missing(print_grid))  {
    print_grid <- length(grid) < 15*15
  }
  accessible <- rep(FALSE, length(grid))
  
  dim <- dim(grid)
  for (i in 1:nrow(grid)) {
    for (j in 1:ncol(grid)) {
      if (grid[i,j] != "@") next
      nbh.ij <- expand.grid(i + (-1:1), j + (-1:1))
      nbh.ij <- as.matrix(nbh.ij[in_bounds(nbh.ij[, 1], nbh.ij[, 2], dim), , drop=FALSE])
      if (sum(grid[nbh.ij] == "@") <= (max_nei + 1)) { # the +1 includes itself
        accessible[ ij2i(i, j, dim) ] <- TRUE
      }
    }
  }
  grid[accessible] <- 'x'
  if (print_grid) {
    print_matrix(grid)
  }
  return(accessible)
}
part1 <- function(input_str, max_nei=3) {
  grid <- lines2matrix(input_str)
  return(sum(num_accessible(grid, max_nei)))
}
expect_equal(
  part1(ex[[1]]),
  13
)
expect_equal(
  part1(input),
  1508
)

# ----- PART 2 -----
part2 <- function (input_str) {
  total_removed <- 0
  grid <- lines2matrix(input_str)

  n.removed_this_iter <- Inf 
  while (n.removed_this_iter > 0) { 
    i.removed_this_iter <- num_accessible(grid, print_grid=FALSE)
    n.removed_this_iter <- sum(i.removed_this_iter)
    grid[i.removed_this_iter] <- '.'
    total_removed <- total_removed + n.removed_this_iter
  }
  return(total_removed)
}
expect_equal(
  part2(ex[[1]]),
  43
)
expect_equal(
  part2(input),
  8538
)

