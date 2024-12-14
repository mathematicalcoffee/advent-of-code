rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

x <- matrix(c(94, 22, 34, 67), byrow=TRUE, nrow=2)
prize <- c(8400, 5400)

parse_puzzles <- function (lines) {
  puzzles <- lapply(1:((length(lines) + 1)/4), function (i) lines[seq(from=(i - 1) * 4 + 1, length.out=3)])
}
puzzle_to_matrix <- function(puzzle, offs=0) {
  mat <- matrix(as.integer(
    do.call(rbind, stri_match_all(puzzle[1:2], regex="Button [AB]: X\\+([0-9]+), Y\\+([0-9]+)"))[, 2:3]
  ), nrow=2, byrow=T)
  ans <- as.integer(stri_match_all(puzzle[3], regex="X=([0-9]+), Y=([0-9]+)")[[1]][, 2:3]) + offs
  list(matrix=mat, answer=ans)
}


puzzles <- lapply(parse_puzzles(input), puzzle_to_matrix)
ans <- sapply(puzzles, function (x) solve(x$matrix, x$answer, tol =-1))
# calc error against rounded values
abserr <- abs(sapply(seq_along(puzzles), function (i) puzzles[[i]]$answer - puzzles[[i]]$matrix %*% round(ans[, i])))
solvable <- colSums(abserr) == 0
print(sprintf("n_solvable = %i", sum(solvable)))
print(sum(round(ans[, solvable]) * c(3,1)))

puzzles <- lapply(parse_puzzles(input), puzzle_to_matrix, offs=10000000000000)
# scale to get around magnitude problems
ans <- sapply(puzzles, function (x) round(solve(x$matrix, x$answer / 1e13, tol =-1) * 1e13))
# calc error against rounded values
abserr <- abs(sapply(seq_along(puzzles), function (i) puzzles[[i]]$answer - puzzles[[i]]$matrix %*% round(ans[, i])))
solvable <- colSums(abserr) == 0
print(sprintf("n_solvable = %i", sum(solvable)))
print(sum(round(ans[, solvable]) * c(3,1))) # FUCK I forgot the c(3,1) and spent an extra 1h50m on this using a symbolic maths package >:(

