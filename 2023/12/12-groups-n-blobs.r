rm(list = ls())
library(stringi)
source("simplify.r")
TESTS <- TRUE
# IDEA::
# "blob" = a group of #s or ?s
# "group" = a run-length of springs we need to fit (1,1,3 = 3 groups that are 1 1 and 3)
# how_many_ways(groups, blobs)
# for each blob
#   let max_n_groups be the max number of groups (from the front) that can fit in the blob
#   try to fit n_groups=0...max_n_groups into the blob
#       n_ways = how_many_ways(fit first n_groups groups, first blob)
#                * how_many_ways(fit rest of groups, next blobs)
#

#  ----- io ----- #
make_record <- function(chars, groups) {
  if (length(chars) == 1 && nchar(chars) > 1)
    chars <- split_springs(chars)
  
  is_dot_rle <- rle(chars == ".")
  ends <- cumsum(is_dot_rle$lengths)
  starts <- c(1, ends + 1) # too long but doesn't matter
  blob_is <- which(is_dot_rle$values == FALSE)
  blobs <- lapply(blob_is, function (blob_i) {
    chars[seq(starts[blob_i], length.out=is_dot_rle$lengths[blob_i])]
  })
  
  list(
    chars = chars,
    groups = groups,
    blobs = blobs
  )
}
process_record <- function(record) {
  return(
    make_record(record[[1]], as.numeric(stri_split_fixed(record[[2]], ",")[[1]]))
  )
}
unfold_record <- function(record, n) {
  # if you end in a gear and this == the last set of #, the next character MUST be a non-gear
  separating_character <- "?"
  return(make_record(
    head(rep(c(record$chars, separating_character), n), -1),
    rep(record$groups, n)
  ))
}
split_springs <- function(line) {
  return(stri_split_boundaries(line, type = "character")[[1]])
}

# ---------------------------- recursive shit ------------------------------- #
count_springs_per_blob <- function (blobs) {
  sapply(blobs, function (blob) sum(blob == "#"))
}
# TODO: does memoisation work on a list or do I need to work on indices
n_ways <- function(groups, blobs, confirmed_springs_per_blob=NULL) {
  # how many ways can I fit this many groups into these blobs?
  # groups: list of ints
  # blobs: list of list of chars [for now] - remaining blobs
  n <- 0

  # probably can pre-cache this with the blobs
  if (is.null(confirmed_springs_per_blob))
    confirmed_springs_per_blob <- count_springs_per_blob(blobs)
  # various stop conditions
  if (sum(confirmed_springs_per_blob) > sum(groups)) {
    # there are more confirmed springs than the number left to fit
    return(0)
  }
  if (length(groups) == 0) {
    # we've consumed all the groups and there are NOT more confirmed springs
    #   than the number left to fit (the previous check)
    return(1)
  }
  # too many springs to fit into the remaining blob-length, even without spaces
  if (sum(groups) > sum(lengths(blobs))) {
    return(0)
  }

  # sub-case: # ways to fit groups into one blob
  if (length(blobs) == 1) {
    return(n_ways_to_fit_groups_into_one_blob(groups, blobs[[1]]))
  }

  # otherwise there is more than one blob. pick the first blob and
  #  try putting blobs in it.
  min_feasible_groups_in_blob <- ifelse(confirmed_springs_per_blob[1] > 0, 1, 0)
  max_feasible_groups_in_blob <- max_n_groups_that_could_fit_in_blob(groups, length(blobs[[1]]))
  if (min_feasible_groups_in_blob > max_feasible_groups_in_blob) {
    return(0)
  }
  for (n_groups_to_put in seq(min_feasible_groups_in_blob, max_feasible_groups_in_blob)) {
    if (n_groups_to_put > 0) {
      # put n_groups_to_put into the first blob and multiply by the # of ways to put the rest in the rest of the blobs
      nnn <- n_ways_to_fit_groups_into_one_blob(groups[1:n_groups_to_put], blobs[[1]])
      if (nnn > 0) {
        n <- n + nnn * n_ways(groups[-(1:n_groups_to_put)], blobs[-1], confirmed_springs_per_blob[-1])
      }
    } else {
      # put the same groups into the remaining blobs
      n <- n + n_ways(groups, blobs[-1], confirmed_springs_per_blob[-1])
    }
  }
  # something about: if the blob is 100% #s then we can force it.

  return(n)
}

min_chars_required_for <- function(groups, include_spacer = FALSE) {
  return(
    sum(groups) + length(groups) - as.integer(!include_spacer)
  )
}
split_blob_at <- function(blob, first_blob_ends_at) {
  # blob: list of chars
  # first_blob_ends_at: first blob ends here (inclusive)
  # returns: list of first new-blob, second new-blob, minus a character in the middle for the dot
  # - 1 because of the dot
  # **OR** no validation
  stopifnot(first_blob_ends_at > 0 & first_blob_ends_at < length(blob) - 1)
  list(
    head(blob, first_blob_ends_at),
    tail(blob, -(first_blob_ends_at + 1))
  )
}
n_ways_to_fit_groups_into_one_blob <- function(groups, blob) {
  # groups: list of ints
  # blob: list of chars
  # assumptions: all groups fit in the blob, length-wise anyway.
  # the blob is # and ?s
  # assumptions
  n <- 0
  if (length(groups) == 1) {
    n <- n + n_ways_to_fit_one_group_into_one_blob(groups[1], blob)
  } else {
    # put the first group in somewhere and recurse
    # = number of ways to put the blob in the first X chars
    #   * number of ways to put the rest of the blobs into the last X chars
    # (split the blob)
    # UGH: number of ways to put the blob in the first X chars relates somehow
    #     to the number of ways to put it in the first X-1 chars
    # this end does not include the spacing dot.
    first_blob_ends_at <- groups[1]
    max_feasible_blob_end <- length(blob) - min_chars_required_for(groups[-1], include_spacer = TRUE)
    # this code is very much like the n_ways_to_fit_one_group_into_one_blob, should they be combined?
    spring_is <- which(blob == "#")
    if (length(spring_is)) {
      # you must place the first blob by the first spring!
      max_feasible_blob_end <- min(
        min(spring_is) + groups[1] - 1,
        max_feasible_blob_end
      )
    } else {
      # there is probably something smart and combinatorial that can be done here
    }

    while (first_blob_ends_at <= max_feasible_blob_end) {
      # you can only split on a '?'
      if (blob[first_blob_ends_at + 1] == "?") {
        split_blobs <- split_blob_at(blob, first_blob_ends_at)
        n <- n + n_ways_to_fit_groups_into_one_blob(groups[-1], split_blobs[[2]])
      }
      first_blob_ends_at <- first_blob_ends_at + 1
    }
  }
  return(n)
}

max_n_groups_that_could_fit_in_blob <- function(groups, blob_len) {
  # as it says on the tin, BUT it doesn't consider #s vs ?s in the blobs
  # consume groups in order with a spacer in between until we run out of space
  #  in the blob.
  n_groups <- 0
  consumed <- -1 # the -1 is because the first group needs no spacer
  group_i <- 1
  while (TRUE) {
    consumed <- consumed + groups[group_i] + 1 # the +1 is the spacer
    n_groups <- n_groups + 1
    group_i <- group_i + 1
    if (consumed > blob_len) {
      return(n_groups - 1)
    }
    if (consumed == blob_len) {
      return(n_groups)
    }
    if (group_i > length(groups)) {
      return(n_groups)
    }
  }
}

n_ways_to_fit_one_group_into_one_blob <- function(group_length, blob) {
  # group: integer
  # blob: chars, all # or ?
  # assumption: this will be the ONLY group in the blob.
  blob_len <- length(blob)
  if (group_length > blob_len) {
    # this group couldn't fit in the blob
    return(0)
  }
  if (group_length == blob_len) {
    return(1)
  }
  spring_is <- which(blob == "#") # definite springs
  if (length(spring_is) > group_length) {
    # too many springs observed for this group
    return(0)
  }
  if (length(spring_is) == 0) {
    # they are all question-marks, no constraints
    return(length(blob) - group_length + 1)
  }

  # otherwise it's a mix. start with all possible i's it can fit, but you
  #  can't pass the first confirmed spring, and need to make sure there are no
  #  other springs in the blob
  latest_start_i <- min(
    length(blob) - group_length + 1,
    min(spring_is)
  )
  if (latest_start_i < 1) {
    return(0)
  }
  last_spring_i <- max(spring_is)

  # potential starts are from 1 to latest_start_i
  # matching potential ends are start_i + spring_length - 1
  # we know (by construction) there are no springs before start_i
  # a start_i is valid if there are no springs after the corresponding end_i
  # i.e. end >= last_spring
  # => start + group_length - 1 >= last_spring
  # => start >= (last_spring - group_length + 1)
  # this means that [last_spring - group_length + 1] starts are valid
  earliest_start_i <- max(1, last_spring_i - group_length + 1)
  if (earliest_start_i > latest_start_i) {
    return(0)
  } else {
    return(latest_start_i - earliest_start_i + 1)
  }
}

# ----------------------------------------------------------------------------

if (TESTS) {
  stopifnot(min_chars_required_for(c(1, 1, 3)) == 7)
  stopifnot(min_chars_required_for(c(1)) == 1)
  stopifnot(min_chars_required_for(c(1), include_space = TRUE) == 2)
  
  stopifnot(max_n_groups_that_could_fit_in_blob(c(4, 1), 3) == 0)
  stopifnot(max_n_groups_that_could_fit_in_blob(c(1, 1), 3) == 2)
  stopifnot(max_n_groups_that_could_fit_in_blob(c(1, 1), 10) == 2)
  stopifnot(max_n_groups_that_could_fit_in_blob(c(3, 2), 6) == 2)
  stopifnot(max_n_groups_that_could_fit_in_blob(c(3, 2, 1, 1, 4), 6) == 2)
  stopifnot(max_n_groups_that_could_fit_in_blob(c(3, 2, 1, 1, 4), 10) == 4)

  stopifnot(n_ways_to_fit_one_group_into_one_blob(2, split_springs("####")) == 0)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(2, split_springs("????")) == 3)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(6, split_springs("????")) == 0)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(4, split_springs("###?")) == 1)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(2, split_springs("??#?")) == 2)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(2, split_springs("??#??")) == 2)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(3, split_springs("??#?#")) == 1)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(3, split_springs("#????")) == 1)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(3, split_springs("?#???")) == 2)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(3, split_springs("?#??#")) == 0)
  stopifnot(n_ways_to_fit_one_group_into_one_blob(3, split_springs("#??#?")) == 0)
  
  stopifnot(n_ways_to_fit_groups_into_one_blob(c(1, 1), split_springs("???")) == 1)
  stopifnot(n_ways_to_fit_groups_into_one_blob(c(1, 1), split_springs("??#")) == 1)
  stopifnot(n_ways_to_fit_groups_into_one_blob(c(1, 1), split_springs("?#?")) == 0)
  stopifnot(n_ways_to_fit_groups_into_one_blob(c(1, 1), split_springs("?#??")) == 1)
  stopifnot(n_ways_to_fit_groups_into_one_blob(c(1, 1), split_springs("?##")) == 0)
  # MORE...
  stopifnot(n_ways_to_fit_groups_into_one_blob(c(2, 3), split_springs("?#??##?")) == 3)
  # ##..###
  # .##.###
  # ##.###.
  
  stopifnot(n_ways_to_fit_groups_into_one_blob(c(3, 2, 1), split_springs("?###????????")) == 10)
  
  stopifnot(n_ways(c(2, 1), list(split_springs("#??"), split_springs("#"))) == 1)
  stopifnot(n_ways(c(1), list(split_springs("#??"), split_springs("#"))) == 0)
  
}

# ----------------------------------------------------------------------------

# -- part 1 -- #
# we could brute-force it rather than being smart....
# input data ----
input_file <- "input.txt"
raw <- stri_split_fixed(readLines(input_file), " ", n = 2)
records <- lapply(raw, process_record)

find_n_ways <- function (record) { return(n_ways(record$groups, record$blobs)) }
q1 <- sapply(records, find_n_ways)


if (TESTS) {
  if (input_file == "input-small.txt") {
    Q1_example_answers <- c(1,4,1,1,4,10)
    stopifnot(all(Q1_example_answers == q1))
  } else {
    Q1_bruteforce_answers <- local({
      trashenv <- new.env()
      load("Q1.rda", env=trashenv)
      trashenv$Q1
    })
    stopifnot(all(Q1_bruteforce_answers == q1))
    which(Q1_bruteforce_answers != q1)
    print(sum(q1))
    
    # i <- 24
    # blobs <- records[[i]]$blobs
    # groups <- records[[i]]$groups
    # Q1_bruteforce_answers[i]
    # q1[i]
  }
}



# non-memoised
#    user  system elapsed 
#   13.06    0.42   43.08 
# after caching springs_per_blob
# user  system elapsed 
# 11.78    0.35   29.03
# after memoising the recursive functions naively
# user  system elapsed 
# 4.71    0.19   11.43 
# after adding records_simplified
# user  system elapsed 
# 0.03    0.00    0.03 

library(memoise)
find_n_ways_memoised <- function(record) {
  n_ways <- memoise(n_ways)
  max_n_groups_that_could_fit_in_blob <- memoise(max_n_groups_that_could_fit_in_blob)
  n_ways_to_fit_groups_into_one_blob <- memoise(n_ways_to_fit_groups_into_one_blob)
  n_ways_to_fit_one_group_into_one_blob <- memoise(n_ways_to_fit_one_group_into_one_blob)
  split_blob_at <- memoise(split_blob_at)
  n_ways(record$groups, record$blobs)
}
records_unfolded <- lapply(records, unfold_record, n = 5)
records_simplified <- lapply(records_unfolded, put_dots_around_known_springs)
# print(system.time({
#   q2 <<- sapply(records_simplified, find_n_ways_memoised)
# }))
output <- "output.txt"
already_done <- read.table(output, header=F)$V1
for (i in setdiff(seq_along(records_unfolded), already_done)) {
  o <- find_n_ways_memoised(records_simplified[[i]])
  cat(i, " ", o, "\n", sep="", file=output, append=TRUE)
}

stop()
if (TESTS) {
  q2 <- sapply(records_simplified, find_n_ways)
  if (input_file == "input-small.txt") {
      Q2_example_answers <- c(1,16384,1,16,2500,506250)
      stopifnot(all(Q2_example_answers == q2))
      which(Q2_example_answers != q2)
      print(sum(q2))
      # i <- 1
      # blobs <- records_unfolded[[i]]$blobs
      # groups <- records_unfolded[[i]]$groups
      # Q2_example_answers[i]
      # q2[i]
  }
}
