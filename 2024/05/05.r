rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

ex <- ex[[1]]

parse_inputs <- function (lines) {
  blank <- which(lines == "")
  orders <- stri_split_fixed(pattern="|", lines[1:(blank - 1)])
  order_dict <- list()
  for (order_pair in orders) {
    before <- order_pair[1]
    order_dict[[before]] <- c(order_dict[[before]], order_pair[2])
  }
  updates <- stri_split_fixed(lines[(blank + 1):length(lines)], pattern=",")
  return(list(
    order_rules=order_dict,
    updates=updates
  ))
}

find_valid_updates <- function (bits) {
  valid_updates <- sapply(bits$updates, is_valid, order_rules=bits$order_rules)
  return(valid_updates)
}
is_valid <- function (pages, order_rules) {
  for (page_i in 2:length(pages)) {
    seen <- head(pages, page_i - 1)
    focus <- pages[page_i]
    should_not_preceed <- order_rules[[focus]]
    if (any(seen %in% should_not_preceed)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

middle_numbers <- function (thelist) {
  vapply(
    thelist,
    function (x) {
      as.integer(x[ceiling(length(x) / 2)])
    },
    0
  )
}

fix_invalid <- function (pages, order_rules) {
  # find the broken ones and move them
  
  for (page_i in 1:length(pages)) {
    seen <- head(pages, page_i - 1)
    focus <- pages[page_i]
    should_not_preceed <- order_rules[[focus]]
    if (any(seen %in% should_not_preceed)) {
      # need to move the focus before the earliest
      move_before_i <- min(which(seen %in% should_not_preceed))
      
      pages <- c(
        head(pages, move_before_i - 1),
        focus,
        tail(pages[-page_i], length(pages) - move_before_i)
      )
    }
  }
  return(pages)
  
}

#bits <- parse_inputs(ex)
bits <- parse_inputs(input)
valid.m <- find_valid_updates(bits)
valid <- bits$updates[valid.m]
invalid <- bits$updates[!valid.m]

message(sprintf("total: %i, valid: %i", length(bits$updates), length(valid)))
message(sum(middle_numbers(valid)))

# part 2
fixed <- lapply(
  invalid, fix_invalid, order_rules=bits$order_rules
)
message(sum(middle_numbers(fixed)))
