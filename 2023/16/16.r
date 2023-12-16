rm(list = ls())
source("../handy.R")
library(stringi)
library(data.table)

# R doesn't have proper enum type, it's tied to the data
# if I use `factor` it will mix up labels and values when indexing into arrays
EAST <- 1
NORTH <- 2
WEST <- 3
SOUTH <- 4

VERT_SPLITTER <- "|"
HORZ_SPLITTER <- "-"
RIGHT_CORNER <- "/" # leans right
LEFT_CORNER <- "\\" # leans left
STOP <- "S" # more convenient if I add a border of "stop" around

# direction we are travelling in when we hit it -> direction we need to go next
# AARGH - these MUST be in order of EAST, NORTH, WEST, SOUTH since we are using
#  ints not enums/factors (if we use factors, indexing into arrays later seems
#  to use the factor-label)
LEFT_CORNER_FROM_TO <- c(EAST = SOUTH, NORTH = WEST, WEST = NORTH, SOUTH = EAST)
RIGHT_CORNER_FROM_TO <- c(EAST = NORTH, NORTH = EAST, WEST = SOUTH, SOUTH = WEST)


augment_features <- function(features, dims) {
  # add border coordinates around
  return(
    rbind(
      features,
      # top edge
      data.table(
        row = rep(0, dims[2] + 1),
        col = seq(0, dims[2]),
        type = STOP
      ),
      # bottom edge
      data.table(
        row = rep(dims[1] + 1, dims[2] + 1),
        col = seq(1, dims[2] + 1),
        type = STOP
      ),
      # right edge
      data.table(
        row = seq(0, dims[1]),
        col = rep(dims[2] + 1, dims[1] + 1),
        type = STOP
      ),
      # left edge
      data.table(
        row = seq(1, dims[1] + 1),
        col = rep(0, dims[1] + 1),
        type = STOP
      )
    )
  )
}

find_next_feature <- function(from_row, from_col, direction, features) {
  # next feature of interest, not including where we are at
  # since from/to are always in bounds and `features` is augmented by
  #  out-of-bounds entries for edges, this always returns something
  # unfortunately, profiling says this is the slow part.
  if (direction == EAST) {
    return(features[col > from_col & row == from_row][which.min(col)])
  }
  if (direction == WEST) {
    return(features[col < from_col & row == from_row][which.max(col)])
  }
  if (direction == NORTH) {
    return(features[row < from_row & col == from_col][which.max(row)])
  }
  if (direction == SOUTH) {
    return(features[row > from_row & col == from_col][which.min(row)])
  }

  stop("find_next_feature: should not reach here")
}
.find_next_feature_i <- function(from_row, from_col, direction, features) {
  # next feature of interest, not including where we are at
  # since from/to are always in bounds and `features` is augmented by
  #  out-of-bounds entries for edges, this always returns something
  # unfortunately, profiling says this is the slow part.
  if (direction == EAST) {
    return(features[col > from_col & row == from_row, which = TRUE][1])
  }
  if (direction == WEST) {
    return(tail(features[col < from_col & row == from_row, which = TRUE], 1))
  }
  if (direction == NORTH) {
    return(tail(features[row < from_row & col == from_col, which = TRUE], 1))
  }
  if (direction == SOUTH) {
    return(features[row > from_row & col == from_col, which = TRUE][1])
  }

  stop("find_next_feature: should not reach here")
}
memoise_find_next_feature <- function(input) {
  cache <- array(0, dim = c(dim(input), 4))
  .inner <- function(from_row, from_col, direction, features) {
    i <- cache[from_row, from_col, direction]
    if (i == 0) {
      i <- .find_next_feature_i(from_row, from_col, direction, features)
      cache[from_row, from_col, direction] <<- i
    }
    return(features[i])
  }
  return(.inner)
}

get_next_directions <- function(current_char, current_direction) {
  # what directions to walk next given the current character & current_direction
  if (current_char == ".") {
    return(current_direction)
  } else if (current_char == VERT_SPLITTER) {
    if (current_direction %in% c(EAST, WEST)) {
      return(c(NORTH, SOUTH))
    } else {
      return(current_direction)
    }
  } else if (current_char == HORZ_SPLITTER) {
    if (current_direction %in% c(NORTH, SOUTH)) {
      return(c(EAST, WEST))
    } else {
      return(current_direction)
    }
  } else if (current_char == LEFT_CORNER) { # \
    return(LEFT_CORNER_FROM_TO[current_direction])
  } else if (current_char == RIGHT_CORNER) { # /
    return(RIGHT_CORNER_FROM_TO[current_direction])
  }
  stop("unknown character", current_char)
}

go <- function(from_row, from_col, direction, input) {
  .go <- function(from_row, from_col, direction) {
    # returns:: from_row, from_col, set of places visited?
    next_feature <- find_next_feature(from_row, from_col, direction, features)
    to_row <- next_feature$row
    to_col <- next_feature$col

    # did we go off the edge?
    off_the_edge <- (
      to_row > dims[1] || to_row == 0 || to_col > dims[2] || to_col == 0
    )

    # detect if we have already visited this node in this direction
    seen_this_film_before <- visited[from_row, from_col, direction] > 0

    # store from here to just-before the destination.
    if (!seen_this_film_before) {
      coords <- cbind(seq(from_row, to_row), seq(from_col, to_col), direction)
      coords <- head(coords, -1) # chop the destination off
      visited[coords] <<- visited[coords] + 1
    }

    if (FALSE) {
      print(
        paste(
          "from:", from_row, ",", from_col,
          c("east", "north", "west", "south")[direction],
          "to:", next_feature$row, next_feature$col, next_feature$type,
          ifelse(
            seen_this_film_before,
            "seen this film before [...and I didn't like the ending]",
            ""
          )
        )
      )
    }

    # work out what happens next
    if (seen_this_film_before || off_the_edge) {
      # do nothing
    } else {
      next_directions <- get_next_directions(next_feature$type, direction)
      for (next_direction in next_directions) {
        .go(to_row, to_col, next_direction)
      }
    }
  }

  dims <- dim(input)
  visited <- array(0, dim = c(dims, 4))

  features <- data.table(which(input != ".", arr.ind = TRUE))
  features[, type := input[cbind(row, col)]]
  features <- augment_features(features, dims)
  setkey(features, row, col) # must be sorted

  # edge case: first character is special, not so elegant
  first_directions <- get_next_directions(input[from_row, from_col], direction)
  for (dir in first_directions) {
    .go(from_row, from_col, dir)
  }
  return(visited)
}

go_1char <- function(from_row, from_col, direction, input) {
  .go_1char <- function(rowcoldir) {
    # this version just goes 1 char at a time and returns a list of the next
    #  direction. The problem is, if I call it recursively in itself I get a
    #  node stackoverflow, so instead I have to return and do the recursion in
    #  a loop (outside the function) instead
    at_row <- rowcoldir[1]
    at_col <- rowcoldir[2]
    direction <- rowcoldir[3]
    # did we go off the edge?
    off_the_edge <- (
      at_row > dims[1] || at_row == 0 || at_col > dims[2] || at_col == 0
    )
    seen_this_film_before <- FALSE

    # mark as visited
    current_char <- STOP
    if (!off_the_edge) {
      # detect if we have already visited this node in this direction
      seen_this_film_before <- visited[at_row, at_col, direction] > 0
      visited[at_row, at_col, direction] <<- visited[at_row, at_col, direction] + 1
      current_char <- input[at_row, at_col]
    }

    if (FALSE) {
      print(
        paste(
          "at:", at_row, ",", at_col, current_char,
          "having travelled", c("east", "north", "west", "south")[direction]
        )
      )
    }

    # work out what happens next
    going <- NULL
    if (seen_this_film_before || off_the_edge) {
      # do nothing
      return(NULL)
    } else {
      going <- get_next_directions(current_char, direction)
    }

    return(lapply(going, function(new_direction) {
      to_row <- at_row
      to_col <- at_col
      if (new_direction == NORTH) {
        to_row <- to_row - 1
      } else if (new_direction == SOUTH) {
        to_row <- to_row + 1
      } else if (new_direction == WEST) {
        to_col <- to_col - 1
      } else if (new_direction == EAST) {
        to_col <- to_col + 1
      }
      # if I go recursive, I get "node stack overflow"...
      #   .go_1char(to_row, to_col, new_direction)
      return(c(to_row, to_col, new_direction))
    }))
  }

  # -------
  # have to do it like this, R doesn't like the recursion
  # ** if I could separate the side-effects (`visited`) from the ret-val
  #    (next-directions) this would be a candidate for memoisation for part2
  dims <- dim(input)
  visited <- array(0, dim = c(dims, 4))

  next_steps <- list(c(from_row, from_col, direction))
  while (length(next_steps)) {
    (next_steps <- unlist(lapply(next_steps, .go_1char), recursive = FALSE))
  }

  return(visited)
}

print_visited <- function(visited, input) {
  heatmap <- apply(visited, c(1, 2), sum)
  heatmap[input != "."] <- 0

  output <- input
  output[visited[, , NORTH] == 1 & heatmap > 0] <- "^"
  output[visited[, , SOUTH] == 1 & heatmap > 0] <- "v"
  output[visited[, , EAST] == 1 & heatmap > 0] <- ">"
  output[visited[, , WEST] == 1 & heatmap > 0] <- "<"
  output[heatmap > 1] <- heatmap[heatmap > 1]

  print_matrix(output, with_coordinates = TRUE)
  return(invisible(output))
}

get_n_squares_energised <- function(visited) {
  return(sum(apply(visited, 1:2, sum) > 0))
}

# ----------------------------------------------------------------------------
input <- stri_list2matrix(
  stri_split_boundaries(
    readLines("input.txt"),
    type = "character"
  ),
  byrow = TRUE
)

# this version zooms all the way to the next feature of interest rather than
#  going 1 character at a time, but it's slower :(
# according to Rprof, `find_next_feature` is slow (the datatable lookup)
if (FALSE) {
  print(system.time({
    ret <- go(1, 1, EAST, input)
  }))
  print_visited(ret, input)
  print(get_n_squares_energised(ret))
}

# this version is "dumber": it goes one character at a time - and yet it's
#  MUCH faster...
print(system.time({
  ret2 <- go_1char(1, 1, EAST, input)
}))
print_visited(ret2, input)
print(get_n_squares_energised(ret2))
# 6906
# .go_1char got called 10,282 times

# --- part 2:: make start coords (datatable for convenient inspection later) ---
edges <- data.table(
  row = c(
    rep(1, ncol(input)), # top edge
    rep(nrow(input), ncol(input)), # bottom edge
    seq_len(nrow(input)), # left edge
    seq_len(nrow(input)) # right edge
  ),
  col = c(
    seq_len(ncol(input)), # top edge
    seq_len(ncol(input)), # bottom edge
    rep(1, nrow(input)), # left edge
    rep(ncol(input), nrow(input)) # right edge
  ),
  direction = rep(
    c(SOUTH, NORTH, EAST, WEST),
    c(ncol(input), ncol(input), nrow(input), nrow(input))
  )
)
# could memoise get_next_directions globally but can't be bothered.
edges[
  ,
  n_energised_1char :=
    get_n_squares_energised(go_1char(row, col, direction, input)),
  by = .(row, col, direction)
]
print(edges[which.max(n_energised_1char)])

# too slow. did attempt to memoise (with the package) find_next_features (which
#  according to profiling is the main culprit) but this did not help much.
if (FALSE) {
  find_next_feature <- memoise_find_next_feature(input)
  edges[
    ,
    n_energised_skipahead :=
      get_n_squares_energised(go(row, col, direction, input)),
    by = .(row, col, direction)
  ]
  # verify that the memoisation worked -- it did
  sum(environment(find_next_feature)$cache > 0)
  print(edges[which.max(n_energised_skpiahead)])
}
