rm(list = ls())
library(stringi)
library(DescTools) # for LCM
inputs <- readLines("input.txt")
directions <- stri_split_boundaries(inputs[1], type = "character")[[1]]
bits <- stri_match_all(
  tail(inputs[inputs != ""], -1),
  regex = "^([0-9A-Z]+) *= *\\(([0-9A-Z]+), *([0-9A-Z]+)\\)"
)

map <- setNames(lapply(
  bits,
  function(x) {
    list(
      L = x[, 3],
      R = x[, 4],
      is.start = grepl("..A", x[, 2]),
      is.end = grepl("..Z", x[, 2])
    )
  }
), vapply(bits, "[[", j = 2, "default"))

get_cycle_length <- function(start_node) {
  step_number <- 0
  at <- start_node
  while (!map[[at]]$is.end) {
    direction <- directions[step_number %% length(directions) + 1]
    at <- map[[at]][[direction]]
    step_number <- step_number + 1
    # print(at)
  }
  return(step_number)
}
print(get_cycle_length("AAA"))

# Q2 AAARGH TURNS OUT THIS SOLUTION WORKED
# I have a more generic solution below and wasted like 2h coding it up
starts <- grep("..A", names(map), value = TRUE)
print(LCM(sapply(starts, get_cycle_length)), digits = 20)

# ------ Q2
# calc indices at which I hit a Z until I verify I'm in an endless loop
# this only happens when we visit a node we've seen before, at the **same**
#  directions-index
# e.g. you might hit Zs
#  steps 100 [+100], 150 [+50], 170 [+20], 220 [+50], 240 [+20], ...
# then you repeat the +50/+20 ad-lib :: means we are (+100, {+50, +20}*)
# do the same for all start-nodes and then work out the min index where they
#  coincide

get_all_steps_that_visit_zs_until_infinite_loop <- function(start_node) {
  # been[direction_index, node_name] means I've been here at that direction
  #  index before, and we are in an infinite loop
  been <- array(
    rep(FALSE, length(map), length(directions)),
    dim = c(length(directions), length(map))
  )
  colnames(been) <- names(map)

  step_numbers_of_zs <- NULL
  direction_index <- 1
  step_number <- 0
  from <- start_node

  # damn, but I really suck at writing loops and not mucking up my
  #  counters/stop conditions/last/current variables
  while (TRUE) {
    direction <- directions[direction_index]

    if (been[direction_index, from]) {
      # infinite loop, let's bail
      return(step_numbers_of_zs)
    }
    been[direction_index, from] <- TRUE

    to <- map[[from]][[direction]]
    if (map[[to]]$is.end) {
      step_numbers_of_zs <- c(step_numbers_of_zs, step_number + 1)
    }
    # print(paste(direction, " from ", from, "->", to))
    from <- to
    step_number <- step_number + 1
    direction_index <- ifelse(
      direction_index + 1 > length(directions), 1, direction_index + 1
    )
  }
  return(step_numbers_of_zs)
}

zs <- sapply(starts, get_all_steps_that_visit_zs_until_infinite_loop)
zs # FUCK ME THEY'RE ALL LENGTH 1 MY Q1 ANSWER WOULD HAVE WORKED
print(LCM(zs), digits = 20)
