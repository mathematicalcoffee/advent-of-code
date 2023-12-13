rm(list = ls())
library(stringi)
source("simplify.r")
TESTS <- TRUE
# IDEA::
# "group" = a run-length of springs we need to fit (1,1,3 = 3 groups that are 1 1 and 3)
# go 1 character at a time.
# keep track of: what group I'm up to, how many springs in the group I've allocated
#   and the current character
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
n_ways <- function(
    current_char, current_char_i, current_group_i, n_allocated_in_group,
    groups, chars
) {
  # print(paste("n_ways(",paste(current_char, current_char_i, current_group_i, n_allocated_in_group, sep=", "),")", sep=""))
  n <- 0
  next_char <- chars[current_char_i + 1]
  current_group_len <- groups[current_group_i]
  if (is.na(current_group_len)) current_group_len <- 0
  
  # stop conditions
  if (current_char_i > length(chars)) {
    # end of string. we are legal if we used up all the groups
    if (current_group_i >= length(groups) && n_allocated_in_group == current_group_len) {
      return(1)
    }
    return(0)
  }
  if (current_char == "#") {
    if (n_allocated_in_group == current_group_len) {
      # we have too many in the group, this is illegal
      return(0)
    } else if (current_group_i > length(groups)) {
      # we have no groups left, this is illegal
      return(0)
    } else {
      # consume one more of the group
      n <- n + n_ways(
        next_char, current_char_i + 1, current_group_i, n_allocated_in_group + 1, groups, chars
      )
    }
  } else if (current_char == ".") {
    # must end the current group
    if (n_allocated_in_group > 0) {
      if (n_allocated_in_group < current_group_len) {
        # we are mid-way through a group we did not finish, this is illegal
        return(0)
      } else {
        # we finished the group, advanced the counter
        n <- n + n_ways(
          next_char, current_char_i + 1, current_group_i + 1, 0, groups, chars
        )
      }
    } else {
      # have not yet entered a group, keep going but don't advance the counter
      n <- n + n_ways(
        next_char, current_char_i + 1, current_group_i, 0, groups, chars
      )
    }
  } else {
    # re-play it as a '#' or as a '.'
    n <- n + n_ways(".", current_char_i, current_group_i, n_allocated_in_group, groups, chars)
    n <- n + n_ways("#", current_char_i, current_group_i, n_allocated_in_group, groups, chars)
  }
  return(n)
}


# ----------------------------------------------------------------------------

# -- part 1 -- #
# we could brute-force it rather than being smart....
# input data ----
input_file <- "input-small.txt"
raw <- stri_split_fixed(readLines(input_file), " ", n = 2)
records <- lapply(raw, process_record)

find_n_ways <- function (record) {
  n_ways <- memoise(n_ways, omit_args=c('groups', 'chars'))
  return(
    n_ways(record$chars[1], 1, 1, 0, record$groups, record$chars)
  )
}

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
  }
}

records_unfolded <- lapply(records, unfold_record, n = 5)
records_simplified <- lapply(records_unfolded, put_dots_around_known_springs)
# on simplified records :::
# print(system.time({
#   q2 <<- sapply(records_simplified, find_n_ways)
# }))
# non-memoised
# user  system elapsed 
# 4.34    0.07   30.44 
# memoised
# user  system elapsed 
# 7.13    0.03   26.57 
# does not look good. something off with the memoisation
# (vs the 11s of 12-blobs-and-groups)

output <- "output.txt"
already_done <- read.table(output, header=F)$V1
for (i in setdiff(seq_along(records_unfolded), already_done)) {
  o <- find_n_ways(records_simplified[[i]])
  cat(i, " ", o, "\n", sep="", file=output, append=TRUE)
}
