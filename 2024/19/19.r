rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

to_cache <- function (cache, remaining, i, value) {
  # message(sprintf("to_cache(%s, patterni=%i) = %.0f", remaining, i, value))
  if (is.null(cache[[remaining]])) {
    cache[[remaining]] <- rep(-1, 500) # should be length(patterns)
  }
  cache[[remaining]][i] <- value
  value
}
from_cache <- function(cache, remaining, i) {
  if (is.null(cache[[remaining]])) return(NULL)
  if (cache[[remaining]][i] < 0) return(NULL)
  return(cache[[remaining]][i])
}
n_ways <- function(remaining, patterns, cache, .patterni=NULL) {
  #message(sprintf("n_ways(%s, .pattern=%s)", remaining, .pattern))
  if (missing(cache)) cache <- new.env()
  
  if (missing(.patterni)) {# first iter
    o <- 0
    for (.p in seq_along(patterns))
      o <- o + to_cache(cache, remaining, .p, n_ways(remaining, patterns, cache, .p))
    return(o)
  }
  
  v <- from_cache(cache, remaining, .patterni)
  if (!is.null(v)) return(v)
 
  .pattern <- patterns[.patterni] 
  pl <- nchar(.pattern)
  if (pl > nchar(remaining)) {
    return(0)
  }
  
  if (nchar(remaining) >= pl && substring(remaining, 1, pl) == .pattern) {
    remaining <- substring(remaining, pl + 1)
    if (remaining == "") {# success
      return(1)
    }
    o <- 0
    for (.p in seq_along(patterns))
      o <- o + to_cache(cache, remaining, .p, n_ways(remaining, patterns, cache, .p))
    return(o)
  }
  return(0)
}

lines <- input

patterns <- stri_split_fixed(pattern=", ", lines[1])[[1]]
terrible <- sprintf("^(%s)+$", paste(patterns, collapse="|"))
desired <- tail(lines, -2)

can_make <- grep(terrible, desired, value=TRUE)
print(length(can_make))

# PART 2
cache <- new.env()
x <- rep(-1, length(can_make))
names(x) <- can_make
# PROGRESS BAR - it's slow.
# 116s
  print(system.time({
for (design in can_make) {
  x[design] <- n_ways(design, patterns=patterns, cache=cache)
}
  }))
# 1041529704688380
print(sum(x))
