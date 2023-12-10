rm(list = ls())
library(stringi)
# input data ----
map <- stri_list2matrix(
  stri_split_boundaries(readLines("input.txt"), type = "character"),
  byrow = TRUE
)

# helpers ----
all_directions <- c("N", "E", "S", "W")
direction_to_offset <- list(
  N = c(-1, 0),
  E = c(0, 1),
  S = c(1, 0),
  W = c(0, -1)
)
opposite_direction <- c(N = "S", E = "W", S = "N", W = "E")
# find valid directions we can go to FROM here
valid_directions_from <- list(
  L = c("N", "E"),
  J = c("N", "W"),
  `7` = c("S", "W"),
  `F` = c("S", "E"),
  `|` = c("N", "S"),
  `-` = c("E", "W")
)
can_go <- function(current_coords, direction) {
  # can we go from current coords in that direction?
  dest <- current_coords + direction_to_offset[[direction]]
  if (any(dest < 1) || any(dest > dim(map))) {
    return(FALSE)
  }
  dest_symbol <- map[dest]

  return(
    direction %in% opposite_direction[valid_directions_from[[dest_symbol]]]
  )
}
# helpful for drawing
output_symbol_map <- c(
  "-" = "-", "|" = "|", "F" = "┌", "J" = "┘", L = "└", `7` = "┐", "S" = "S"
)
corners <- c("┌", "┘", "└", "┐")

# ---- Q1 ----
start_coords <- which(map == "S", arr.ind = TRUE)
start_directions <- names(
  which(sapply(all_directions, can_go, current_coords = start_coords))
)

step <- 0
direction_from_start <- start_directions[1] # arbitrary
current_coords <- start_coords

# let's draw the loop in for shits n giggles (it looks cool and helps me see
#  where I've gone wrong)
map_no_garbage <- map
map_no_garbage[, ] <- "."

while (TRUE) {
  current_symbol <- map[current_coords]
  map_no_garbage[current_coords] <- output_symbol_map[current_symbol]
  if (current_symbol == "S" && step > 0) {
    # reached the start again, stop
    break
  }

  # work out next direction - exclude direction we just came from
  if (step == 0) {
    direction <- direction_from_start
  } else {
    direction <- setdiff(
      valid_directions_from[[current_symbol]],
      opposite_direction[direction]
    )
    stopifnot(length(direction) == 1)
  }
  current_coords <- current_coords + direction_to_offset[[direction]]
  step <- step + 1
}
print(step / 2)
writeLines(apply(map_no_garbage, 1, paste, collapse = ""), "output_q1.txt")

# ---- part 2 ---- #
# go from left to right on each line
# the first non-boundary space is "O", but every time you pass a vertical
#  boundary, flip parity (to "I" then to "O" then to ...)
# ┌┘ and └┐ count only as one vertical boundary despite taking up 2 chars.
# '-' in between said symbols don't count.
# ┌┐ and └┘ count as 2 vertical boundaries

# let's draw the output for sense-checking again.
map_in_out <- map_no_garbage
# must replace the S with whatever symbol it is supposed to be
start_symbol <- names(
  which(sapply(valid_directions_from, setequal, x = start_directions))
)
stopifnot(length(start_symbol) == 1)
map_in_out[start_coords] <- output_symbol_map[start_symbol]

in_out <- "O"
flip_in_out <- c(I = "O", O = "I")
last_vertical_wall <- "." # to remember the pairs
non_parity_flipping_pairs <- c("┌" = "┘", "└" = "┐")

for (row in seq_len(nrow(map_in_out))) {
  for (col in seq_len(ncol(map_in_out))) {
    symbol <- map_in_out[row, col]

    if (symbol == ".") {
      map_in_out[row, col] <- in_out
    } else if (symbol %in% c("|", corners)) {
      if (!isTRUE(non_parity_flipping_pairs[last_vertical_wall] == symbol)) {
        in_out <- flip_in_out[in_out] # flip parity
      }
      last_vertical_wall <- symbol
    }
  }
}

writeLines(apply(map_in_out, 1, paste, collapse = ""), "output_q2.txt")
print(sum(map_in_out == "I"))
