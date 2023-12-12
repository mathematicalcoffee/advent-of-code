rm(list = ls())
library(stringi)
# input data ----
raw_input <- stri_split_fixed(readLines("input.txt"), " ", n=2)
DO_Q_1 <- FALSE


# calc_num_arrangements <- function (springs, rles)
make_record <- function(chars, rle, obs_rle=NULL) {
  if (is.null(obs_rle)) obs_rle <- get_rle(chars)
  list(
    chars=chars,
    expected_rle=rle,
    observed_rle=obs_rle
  )
}
get_rle <- function (record) {
  obs_rle <- rle(record)
  return(data.table(
    starts=1 + c(0, head(cumsum(obs_rle$lengths), -1)),
    length=obs_rle$lengths,
    char=obs_rle$values
  ))
}
split_springs <- function (line) {
  return(stri_split_boundaries(line, type="character")[[1]])
}
process_record <- function (record) {
  return(
   make_record(split_springs(record[[1]]), as.numeric(stri_split_fixed(record[[2]], ",")[[1]]))
  )
}

find_broken_rles <- function (springs) {
  rles <- rle(springs == "#")
  return(rles$lengths[rles$values])
}
is_arrangement_compatible <- function (springs, desired_rles) {
  broken_springs <- find_broken_rles(springs)
  return(length(broken_springs) == length(desired_rles) && all(broken_springs == desired_rles))
}
num_springs <- function (springs) { return(sum(springs == "#"))}

estimate_bruteforce <- function (record) {
  n_spring_deficit <- sum(record$expected_rle) - num_springs(record$chars)
  unknown_is <- which(record$chars == "?")
  return(choose(length(unknown_is), n_spring_deficit))
}

num_compatible_arrangements_bruteforce <- function (record) {
  # choose <deficit> of the spaces to be '?'
  n_spring_deficit <- sum(record$expected_rle) - num_springs(record$chars)
  
  unknown_is <- which(record$chars == "?")
  potential_spring_positions <- combn(unknown_is, n_spring_deficit)
  sum(apply(potential_spring_positions, 2, function (spring_is) {
    spring_candidate <- record$chars
    spring_candidate[spring_is] <- "#"
    is_arrangement_compatible(spring_candidate, record$expected_rle)
  }))
}

generate_potential_spring_positions_dumb <- function(observed_record, expected_broken_springs) {
  n_spring_deficit <- sum(expected_broken_springs) - num_springs(observed_record)
  unknown_is <- which(observed_record == "?")
  return(combn(unknown_is, n_spring_deficit))
}


# -- part 2 -- #
# fuck lol I knew it
unfold <- function (record, n) {
  # if you end in a gear and this == the last set of #, the next character MUST be a non-gear
  separating_character <- "?"
  return(make_record(
    head(rep(c(record$chars, separating_character), n), -1),
    rep(record$expected_rle, n)
  ))
}

# we can "lock in" certain RLEs I think.
# and this also locks in others.
# there is a way to generate Q2 from Q1. We are only inserting ONE '?' in between, so this limits
#   how the springs can be.

# ---------------

# we need to actually lock in things that are forced

# TODO
# - fill in as many # as we can like nonograms
# - if we have ???? xxxxxx and 4, we can fill it in
# - if we have ?## and 3, we can fill it in ()
remove_known_springs_of_exact_length <- function (record) {
  # match exact ##### to the run-length when there is no other option and fill dots around it.
  # remove them from the RLE and replace with a "."
  obs_rle <- record$observed_rle
 
  return_rle <- record$expected_rle
  sorted_rle <- sort(unique(return_rle), decreasing=TRUE)
  freq_rle <- table(return_rle)
  
  return_chars <- record$chars
 
  # match exact springs from longest to shortest run-length
  keep_going <- TRUE
  sorted_rle_i <- 0
  while (keep_going) {
    sorted_rle_i <- sorted_rle_i + 1
    matches <- obs_rle[char == "#" & length == sorted_rle[sorted_rle_i], nomatch=NULL]
    if (nrow(matches) == 0) {
      keep_going <- FALSE
    } else {
      # we need the same number of matches as we expect or there is too much ambiguity
      # (unless they are all the same, but that is hard)
      if (sum(return_rle == sorted_rle[sorted_rle_i]) == nrow(matches)) {
        return_chars[matches[starts > 1, starts - 1]] <- "."
        return_chars[matches[starts + length < length(return_chars), starts + length]] <- "."
        # don't need to fill in springs - they are already springs
        # fill out the dots tho
        # for (match_i in 1:nrow(matches)) {
        #   return_chars[
        #     matches[match_i, seq(max(1, starts - 1), min(length(record$chars), starts+length))]
        #   ] <- "."
        # }
      }  else {
        keep_going <- FALSE
      }
    }
  }
  # remove all the rle's we matched
  return(
    make_record(
      return_chars,
      record$expected_rle
      # record$expected_rle[!record$expected_rle %in% sorted_rle[seq_len(max(0, sorted_rle_i - 1))]]
    )
  )
}

remove_known_springs_from_start <- function (record) {
  ### remove springs until the first question_mark
  # e.g. .#.#..????.## 1,1,1,2 -> 1, 2 (cannot go past the `?`) and "..????.##"
  # and just strip the known ones off the start of the string
  # adjusts only the expected-rle
  
  if (record$observed_rle[char == "?", .N] == 0) {
    # no question-marks
    return(record)
  }
  
  # we count how many '#'-blobs before and NON-ADJACENT to the first '?'
  # remove these from the start
  obs_rle <- record$observed_rle
  stop_at <- obs_rle[char == "?", min(starts)]
  matches <- obs_rle[starts < stop_at & char == "#" & shift(char, -1) == "."]
  if (nrow(matches)) {
    # assumption
    stopifnot(matches[, length] == record$expected_rle[seq_len(nrow(matches))])
    chars <- tail(record$chars, -matches[which.max(starts), starts + length - 1])
    
    return (
      make_record(
        chars,
        tail(record$expected_rle, -nrow(matches))
      )
    )
  }
  return(record)
}
remove_known_springs_from_end <- function (record) {
  reverse_record <- function (record) {
    return(make_record(rev(record$chars), rev(record$expected_rle)))
  }
  return(reverse_record(remove_known_springs_from_start(reverse_record(record))))
}


simplify_record <- function (record) {
  MAX_ITER <- 1 # just doesn't work if you repeat it because of me reducin the expected_rle???
  iter <- 1
  keep_going <- TRUE

  prev_chars <- rep("X", length(record$chars))

  while (iter <= MAX_ITER && any(record$chars != prev_chars)) {
    prev_chars <- record$chars
    record <- remove_known_springs_from_start(record)
    record <- remove_known_springs_from_end(record)
    record <- remove_known_springs_of_exact_length(record) # <-- the only one that actually modifies the string???
    iter <- iter + 1
  }
  # print(iter)  # never manaed to iterate more than once?
  return(record)
}


# library(memoise) # don't thin kthis helps with how I've done it
# ----------------------------------------------------------------
# ensure that `chars` is of length `n_gears + 2` (1 char before 1 char after)
# but the substring may be more expensive than the indexing!!!
# better to compare to the RLE 
can_gears_go_at <- function (n_gears, start_i, chars) {
  # doesn't check we don't precede a '#'
  return (
    (n_gears <= length(chars))
    &&
    !any(chars[seq_len(n_gears) + start_i - 1] == ".")
    &&
    (start_i + n_gears > length(chars) || chars[start_i + n_gears] != "#")
    &&
    (start_i == 1 || chars[start_i - 1] != "#")
  )
}
can_gears_go_at_og <- function (n_gears, start_i, chars) {
  # doesn't check we don't precede a '#'
  return (
    (n_gears <= length(chars))
    &&
    !any(chars[seq_len(n_gears) + start_i - 1] == ".")
    &&
    (start_i + n_gears > length(chars) || chars[start_i + n_gears] != "#")
    &&
    (start_i == 1 || chars[start_i - 1] != "#")
  )
}

valid_start_is <- function (n_gears, chars, last_n_chars_cant_use=0) {
  # assume chars[1] is a #
  # yep we can be smarter about this with rle but too bad
  # chars is a mix of anything
  # last_n_chars_cant_use means you are not allowed to exceed those ones
  # what is can we start n_gears at?
  # at worst, 1:length(chars) - n_gears
  # but:
  # - there can be no gears before this
  # - gears must be surrounded by dots
  # - must entirely cover the gears
  # - must have space for the rest of the stuff
  # all must be legal
  stopifnot(n_gears > 0)
  if (n_gears > length(chars)) {
    return(integer(0))
  }
  non_dot_is <- which(chars != ".")
  if (length(non_dot_is) == 0) {
    return(integer(0))
  }
  
  max_start <- length(chars) - last_n_chars_cant_use - n_gears + 1
  if (max_start < 1) return(integer(0)) # can't put these gears and have enough space for the rest
  
  gear_is <- which(chars == "#") 
  # must place the gear by the first confirmed gear!
  if (length(gear_is)) max_start <- min(max_start, min(gear_is))
  
  min_start <- min(non_dot_is)
  if (min_start > max_start) return(integer(0))
  candidates <- intersect(non_dot_is, seq(min_start, max_start))
  
  
  valid_starts <- candidates[sapply(candidates, can_gears_go_at, n_gears=n_gears, chars=chars)]
  return(valid_starts)
}


how_many_ways <- function (rles, chars) {
  n_ways <- 0
  if (length(rles) == 0) {
    # we've consumed all the gears. make sure there are no more gears left in chars
    if (any(chars == "#")) {
      return (0)
    } else {
      return(1)
    }
  }
  
  # find out where the first set of gears can go
  n_gears <- rles[1]
  
  # need to leave this many characters at the end
  characters_needed_for_rest <- sum(rles[-1]) + length(rles) - 1 # the spacing dots
  valid_starts <- valid_start_is(n_gears, chars, characters_needed_for_rest)
  
  if (length(valid_starts) == 0) {
    # there are no valid places to put the gears
    return(0)
  } else if (length(rles) == 1) {
    # this is the last set of gears
    # need to ensure we consumed all the gears in the remainder of the string
    # assumed that valid_starts are sorted ascending
    for (i in seq_along(valid_starts)) {
      rest_of_chars <- tail(chars, -(valid_starts[i] + n_gears))
      if (all(rest_of_chars != "#")) {
        # this and everyone after will be valid
        return(length(valid_starts) - i + 1)
      }
    }
    return(0)    
  } else {
    # lock in the valid start and work out the rest
    for (start in valid_starts) {
      # place the gears there and then go on
      rest_of_rles <- rles[-1]
      # chop off the first start + n_gears+1 characters (gears and a dot)
      rest_of_chars <- tail(chars, -(start + n_gears))
      n_ways <- n_ways + how_many_ways(rest_of_rles, rest_of_chars)
    }
  }
  return(n_ways)
}

# 7260
records <- lapply(raw_input, process_record)

# -- part 1 -- #
# we could brute-force it rather than being smart....
if (DO_Q_1) {
  Q1 <- sapply(records, num_compatible_arrangements_bruteforce)
  # save(Q1, file="Q1.rda")
  # number of possibilities to bruteforce LOL
  # sum(sapply(records, estimate_bruteforce))
}
# 7260

library(memoise)
og_valid_start_is <- valid_start_is
og_can_gears_go_at <- can_gears_go_at
smort <- function (r) {
  xx <- memoise(how_many_ways)
  valid_start_is <- memoise(og_valid_start_is)
  can_gears_go_at <- memoise(og_can_gears_go_at)
  return(xx(r$expected_rle, r$chars))
}

output <- "output.txt"
records_unfolded <- lapply(records, unfold, n=5)
records_simplified <- lapply(records_unfolded, simplify_record)
already_done <- read.table(output, header=F)$V1
for (i in sample(setdiff(seq_along(records_unfolded), already_done))) {
  o <- smort(records_simplified[[i]])
  cat(i, " ", o, "\n", sep="", file=output, append=TRUE)
}

# n_estimate <- sapply(records_unfolded, estimate_bruteforce)
# sum(n_estimate)
# # LOL 1.5e+27
# max(n_estimate)
# # 1.3e+27

# load(file='Q1.rda')
# which(Q1 != smort_a)
