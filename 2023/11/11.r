rm(list = ls())
library(stringi)
# input data ----
space <- stri_list2matrix(
  stri_split_boundaries(readLines("input.txt"), type = "character"),
  byrow = TRUE
)

expand_space_dumbdumb <- function(space) {
  # returns the `space` matrix with expanded rows and columns
  expand_space_columns <- function(space) {
    cols_to_double <- which(colSums(space == ".") == nrow(space))
    return(space[, sort(c(seq_len(ncol(space)), cols_to_double))])
  }
  return(t(expand_space_columns(t(expand_space_columns(space)))))
}

# 1. expand space
space_expanded <- expand_space_dumbdumb(space)
# 2. find galaxies
galaxies <- which(space_expanded == "#", arr.ind = TRUE)
# 3. calc distance
print(sum(dist(galaxies, method = "manhattan")))


expand_space_smrt <- function(space, mult) {
  # expands the coordinates of the galaxies without computing the new matrix
  # returns the coordinates of the galaxies post-expansion
  expand_space_coordinates_smrt <- function(space, cols_of_galaxies) {
    # add the number of expanded-columns that precede the galaxy columns
    cols_to_expand <- which(colSums(space == ".") == nrow(space))
    return(
      vapply(
        cols_of_galaxies,
        function(c) sum(cols_to_expand < c) * (mult - 1) + c,
        0
      )
    )
  }
  galaxies <- which(space == "#", arr.ind = TRUE)
  new_cols <- expand_space_coordinates_smrt(space, galaxies[, "col"])
  new_rows <- expand_space_coordinates_smrt(t(space), galaxies[, "row"])

  cbind(row = new_rows, col = new_cols)
}
# check on Q1...
og_galaxies <- which(space == "#", arr.ind = TRUE)
expected_galaxies <- which(space_expanded == "#", arr.ind = TRUE)
stopifnot(all(expected_galaxies == expand_space_smrt(space, 2)))
# answer on Q2
print(sum(dist(expand_space_smrt(space, 1e6), method = "manhattan")))
