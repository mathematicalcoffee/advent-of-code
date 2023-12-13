rm(list = ls())
library(stringi)
# input data ----

read_patterns <- function(filename) {
  lines <- readLines(filename)
  pattern_boundaries <- c(0, which(lines == ""), length(lines) + 1)
  n_patterns <- length(pattern_boundaries) - 1

  return(
    lapply(
      seq_len(n_patterns), function(pattern_i) {
        start_i <- pattern_boundaries[pattern_i] + 1
        end_i <- pattern_boundaries[pattern_i + 1] - 1
        return(stri_list2matrix(
          stri_split_boundaries(
            lines[start_i:end_i],
            type = "character"
          ),
          byrow = TRUE
        ))
      }
    )
  )
}

find_reflection_row <- function(pattern, ignore = Inf) {
  # return 0 if none
  for (i in 1:(nrow(pattern) - 1)) {
    n_to_check <- min(i, nrow(pattern) - i)
    if (all(
      pattern[seq(from = i, by = -1, length.out = n_to_check), ]
      ==
        pattern[seq(from = i + 1, by = 1, length.out = n_to_check), ]
    )) {
      if (i != ignore) {
        return(i)
      }
    }
  }
  return(0)
}
find_reflection_col <- function(pattern, ignore = Inf) {
  return(find_reflection_row(t(pattern), ignore))
}
find_reflection <- function(pattern, ignore = Inf) {
  # +ve if horizontal and -ve if vertical
  h <- find_reflection_row(pattern, ignore)
  if (h > 0) {
    return(h)
  }
  v <- find_reflection_col(pattern, -ignore)
  if (v > 0) {
    return(-v)
  }
  return(0)
}

find_reflection_with_smudge <- function(pattern, output_file = NULL) {
  og_reflection <- find_reflection(pattern)
  for (i in seq_len(nrow(pattern))) {
    for (j in seq_len(ncol(pattern))) {
      new_pattern <- copy(pattern)
      new_pattern[i, j] <- ifelse(pattern[i, j] == "#", ".", "#")
      x <- find_reflection(new_pattern, ignore = og_reflection)
      if (x != 0) {
        if (!is.null(output_file)) {
          write(
            t(new_pattern),
            file = output_file,
            append = TRUE,
            ncolumns = ncol(pattern),
            sep = ""
          )
          cat("\n", file = output_file, append = TRUE)
        }
        return(x)
      }
    }
  }
  return(0)
}

# -----------------------
patterns <- read_patterns("input.txt")

p1 <- sapply(patterns, find_reflection)
stopifnot(all(p1 != 0))
print(sum(100 * p1[p1 > 0]) + sum(abs(p1[p1 < 0])))
# 33520

output_file <- NULL
if (FALSE) {
  output_file <- "output.txt"
  if (!is.null(output_file)) {
    cat("", file = output_file, append = FALSE)
  }
}
p2 <- sapply(patterns, find_reflection_with_smudge, output_file = output_file)
stopifnot(all(p2 != 0))
stopifnot(all(p1 != p2))
# 34824
print(sum(100 * p2[p2 > 0]) + sum(abs(p2[p2 < 0])))
