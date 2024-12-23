rm(list=ls())
library(data.table)
source("../../handy.R")
library(igraph)
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_keyboard <- function (lines) {
  t(sapply(
    stri_match_all(
      grep("[0-9A\\^<v>]", lines, value=TRUE),
      regex=" ([ 0-9A\\^>v<]) "
    ),
    function (x) x[, 2]
  ))
}
keyboard_from_matrix <- function (mat) {
  n <- build_nodes(mat)
  n[, node_label := symbol]
  
  e <- build_orthogonal_edges(n, " ", directed=TRUE)
  e[from_row == to_row & from_col < to_col, direction := DIRECTION_LABELS[RIGHT]]
  e[from_row == to_row & from_col > to_col, direction := DIRECTION_LABELS[LEFT]]
  e[from_row < to_row & from_col == to_col, direction := DIRECTION_LABELS[DOWN]]
  e[from_row > to_row & from_col == to_col, direction := DIRECTION_LABELS[UP]]
  
  stopifnot(nrow(e[is.na(direction)]) == 0)
  
  setkey(n, node_id)
  e[, edge_label := sprintf("%s%s%s", n[J(from_node_id), symbol], direction, n[J(to_node_id), symbol])]
  g <- build_graph(n, e, directed=TRUE)
  E(g)$direction <- e$direction
  V(g)$name <- V(g)$label
  # plot_graph_on_grid(g)
  g
}
numeric_keyboard <- function () {
  mat <- parse_keyboard(ex[[1]])
  keyboard_from_matrix(mat)
}
directional_keyboard <- function () {
  mat <- parse_keyboard(ex[[2]])
  keyboard_from_matrix(mat)
}

# gives the directions for all shortest paths from `from` to `to` on the keyboard
# (assumes the directions are the edge names)
# just finds shortest path from `from` to `to` and extracts the names fo the
#  edges on the path
directions_for_path <- function(from, to, keyboard, append_A=FALSE) {
  o <- lapply(
    all_shortest_paths(
      keyboard,
      from=from,
      to=to
    )$epaths,
    function (ep) {
      E(keyboard)[ep]$direction
    }
  )
  if (append_A)
    o <- lapply(o, c, 'A')
  return(o)
  
}
type_on_keyboard <- function(sequence, keyboard){
  # return all possible shortest button press sequences for each character
  # this is a list of lists (outer list = sequence chars, inner lists = all
  #  shortest paths for those)
  # `sequence` is a list of characters
  # NOTE: it is possible to prune here, by discarding sequences where the same character
  #  occurs multiple time not in a run-length
  # e.g. >V>, >>V and V>> all get you to the same place, but >V> is guaranteed to be
  #  either the same length path if it's my keyboard, or longer if it's a robot's
  #  keyboard, because they have to move from V to > then back whereas tapping >>
  #  twice in a row is just two 'A's.
  # But I was not smart enough to implement the pruning
  setNames(
    lapply(
      seq_along(sequence),
      function (i) {
        directions_for_path(
            from=ifelse(i == 1, "A", sequence[i - 1]),
            to=sequence[i],
            keyboard,
            append_A=TRUE
        )
        # lapply(
        #   all_shortest_paths(
        #     keyboard,
        #     from=ifelse(i == 1, "A", sequence[i - 1]),
        #     to=sequence[i],
        #   )$epaths,
        #   function (ep) c(E(keyboard)[ep]$direction, "A")
        # )
      }
    ),
    sequence
  )
}

minlength.option <- function (options) {
  # return the element of the the list `options` with the minimum length.
  options[[which.min(lengths(options))]]
}
# This was horrible and involved 3 layers of lappy, except somehow the typing has
#  gone funny and there is an extra layer of nesting than I expected
type_sequence_part1_dumbdumb <- function (sequence_str, return.intermediate=FALSE) {
  num <- numeric_keyboard()
  dir <- directional_keyboard()
  x <- lapply(
    type_on_keyboard(chars(sequence_str), num),
    function (options_per_char) { # list[character vector]
      o <- lapply(
          lapply(options_per_char, type_on_keyboard, keyboard=dir), # list[list[character vector]]
          function (options_per_dir1) { # list[character vector]
            o <- lapply(
              lapply(options_per_dir1, lapply, type_on_keyboard, keyboard=dir),
              function (options_per_dir2) {
                # char vec
                minlength.option(
                  lapply(options_per_dir2, function (x) unlist(lapply(x, minlength.option), use.names=FALSE))
                )
              }
            )
          }
        )
      minlength.option(lapply(o, unlist, use.names=FALSE))
    }
  )
  unlist(x, use.names=FALSE)
}

complexity_part1 <- function (sequence_str, button_presses) {
  if (missing(button_presses))
    button_presses <- type_sequence_part1_dumbdumb(sequence_str)
  length(button_presses) * as.numeric(stri_extract(regex="^[0-9]+", sequence_str))
}

# PART 1 EXAMPLES
layer3.ans <- chars("<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A")
message(o <- type_sequence_part1_dumbdumb("029A", return.intermediate = TRUE))
stopifnot(length(o) == length(layer3.ans))
message(o <- type_sequence_part1_dumbdumb("029A"))
stopifnot(complexity_part1("029A") == 68 * 29)
stopifnot(complexity_part1("980A") == 60 * 980)
stopifnot(complexity_part1("179A") == 68 * 179)
stopifnot(complexity_part1("456A") == 64 * 456)
stopifnot(complexity_part1("379A") == 64 * 379)
# SLOW
ans <- sapply(input, complexity_part1)
sum(ans)

# ------------------ PART 2 ---------------------------- #
# The idea here is to compute the min # button presses I need to do, in order to
# move between any pair of keys on the dirpad that is just after to the numpad.
# Assuming `n` robot d-pads.
# makes a 5x5 matrix costs[from, to] = cost of going from the `from` button to the
#   `to`. e.g. the 2-dpad (part 1) cost matrix is 
# >  < ^ v A
# > 1  9 9 8 4
# < 5  1 7 4 8
# ^ 7  9 1 6 4
# v 4  8 4 1 7
# A 6 10 8 9 1
# So to go from an 'A' to a '<' on the dirpad just after the numpad, this
#  means I need to press buttons 6 times (it doesn't tell you what the sequence is).
#
# Then for any numeric code, just work out all shortest directional sequences to
#  type it on the dirpad just after and sum on the cost matrix.
# E.g. ^ < < A  is the costs[^, <] + costs[<, <] + costs[<, A].
#
# it memoises into a cache.
# the cache is [dpad sequence][n keypads left], but the dpad sequence is only ever
#  to get from one dpad key to another, so is at most 4 long
#  (e.g. the 'A' key to the '<' key is < < v A)
# meaning there are not too many unique keys.

# Useful constants ---
NUM <- numeric_keyboard()
DIR <- directional_keyboard()
DIRPAD_BUTTONS <- c(DIRECTION_LABELS, "A")

# Cache directions to and from keys of the dirpad.
# BEST_PATHS_A[[from]][[to]] = list of paths of shortest length + an 'A'
# TODO: could have pruned out e.g. >v> (>>v or v>> guaranteed better) but didn't
# Also - added an 'A' on the end because every time I wanted this I wanted an
#  'A' on the end.
BEST_PATHS_A <- setNames(replicate(5, list(), simplify=FALSE), DIRPAD_BUTTONS)
BEST_PATHS_A <- setNames(replicate(5, BEST_PATHS_A, simplify=FALSE), DIRPAD_BUTTONS)
for (fromkey in DIRPAD_BUTTONS)
  for (tokey in DIRPAD_BUTTONS)
    BEST_PATHS_A[[fromkey]][[tokey]] <- directions_for_path(fromkey, tokey, DIR, append_A=TRUE)

# --- Cache --- #
# the cache is [dpad sequence][n keypads left], but the dpad sequence is only ever
#  to get from one dpad key to another, so is at most 4 long
#  (e.g. the 'A' key to the '<' key is < < v A)
# meaning there are not too many unique keys.
keyf <- function (sequence) {
  # yuck
  paste(sequence, collapse="")
}
from_cache <- function (sequence, n_keyboards_left, cache) {
  key <- keyf(sequence)
  if (is.null(cache[[key]])) {
    cache[[key]] <- rep(NA, n_keyboards_left)
    return(NULL)
  } else {
    if (is.na(v <- cache[[key]][n_keyboards_left]))
      return(NULL)
    return(v)
  }
}
to_cache <- function(sequence, n_keyboards_left, cache, value) {
  key <- keyf(sequence)
  cache[[key]][n_keyboards_left] <- value
  value
}

# --- Recursive (go through the layers of keyboards) --- #
.cost_for <- function (sequence, n_keyboards_left, cache) {
  # This returns the # button presses required to type the given dirpad sequence
  #  through `n` dpads.
  if (n_keyboards_left == 0)  # reached the end, yay
    return(length(sequence))
  
  if (!is.null(v <- from_cache(sequence, n_keyboards_left, cache)))
    return(v)
  
  # otherwise, we need costs to move to each key in the sequence on the next
  #  dpad down.
  .cost <- 0
  for (i in seq_along(sequence)) {
    fromkey <- ifelse(i == 1, "A", sequence[i - 1])
    tokey <- sequence[i]
    options <- BEST_PATHS_A[[fromkey]][[tokey]]
    .cost <- .cost + min(
      sapply(options, .cost_for, n_keyboards_left = n_keyboards_left - 1, cache=cache)
    )
  }
  return(to_cache(sequence, n_keyboards_left, cache, .cost))
}

calc_dirpad_cost_matrix <- function (num_robot_keyboards, cache) {
  # build cost matrix to go from any key to any other key on the dirpad controls
  #  the numpad (top-level function)
  # for each <from, to> on the dirpad above the numpad, store the number of
  #  button presses I need to make to make the `num_robot_keyboards` robots
  #  do their thing.
  costs <- matrix(
    Inf,
    nrow=length(DIRPAD_BUTTONS),
    ncol=length(DIRPAD_BUTTONS),
    dimnames=list(DIRPAD_BUTTONS, DIRPAD_BUTTONS)
  )
 
  for (fromkey in DIRPAD_BUTTONS) {
    for (tokey in DIRPAD_BUTTONS) {
      if (fromkey == tokey) {
        costs[fromkey, tokey] <- 1 # just click 'A'
        next
      }
      options <- BEST_PATHS_A[[fromkey]][[tokey]]
      costs[fromkey, tokey] <- min(sapply(options, .cost_for, n_keyboards_left=num_robot_keyboards - 1, cache=cache))
    }
  }
  costs
}

dirpad_cost_lookup <- function (dirpad_sequence, costs) {
  # given the cost matrix and a dirpad sequence (to move on the numpad), work
  #  out the number of button presses required to execute it.
  # it just looks up the <from, to>s in the cost matrix and sums them e.g.
  # E.g. ^ < < A  is the costs[^, <] + costs[<, <] + costs[<, A].
  sum(
    costs[
      cbind(
        c("A", head(dirpad_sequence, -1)),
        dirpad_sequence
      )
    ]
  )
}


# ---- end-level functions that operate on the numpad ---- #
n_button_presses <- function (sequence_str, costs) {
  # final function - # of button presses to type "029A" on the numpad.
  # works out the dpad sequence to type on the numpad then looks them up in the
  #  cost matrix
  test <- type_on_keyboard(chars(sequence_str), NUM)
  sum(
    sapply(
      sapply(test, sapply, dirpad_cost_lookup, costs=costs),
      min
    )
  )
}
complexity <- function (sequence_str, costs) {
  n_button_presses(sequence_str, costs) * as.numeric(stri_extract(regex="^[0-9]+", sequence_str))
}



# Re-do part 1
cache <- new.env()
costs <- calc_dirpad_cost_matrix(2, cache)
stopifnot(complexity("029A", costs) == 68 * 29)
stopifnot(complexity("980A", costs) == 60 * 980)
stopifnot(complexity("179A", costs) == 68 * 179)
stopifnot(complexity("456A", costs) == 64 * 456)
stopifnot(complexity("379A", costs) == 64 * 379)

ans <- sapply(input, complexity, costs=costs)
stopifnot(sum(ans) == 176452)
print("part 1")
print(ans)

# Part 2 with the new thing
cache <- new.env()
costs <- calc_dirpad_cost_matrix(25, cache)
ans <- sapply(input, complexity, costs=costs)
print("part 2")
print(sum(ans))
# 218309335714068 oh my god it was right
