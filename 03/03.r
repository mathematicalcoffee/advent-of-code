# rm(list=ls())
library(stringi)
library(data.table)

lines <- readLines("./input.txt")

# get start/end row/column of each token in the schematic
# make sure that adjacent tokens & numbers are extracted as separate matches.
non_dot_regex <- "[0-9]+|[^.0-9]+"
coordinates <- stri_locate_all(
  regex = non_dot_regex, lines, omit_no_match = TRUE
)

tokens <- data.table(
  do.call(
    rbind,
    lapply(
      seq_along(coordinates),
      function(line_i) {
        matches <- coordinates[[line_i]]
        if (nrow(matches) == 0) {
          return(NULL)
        }
        data.table(
          row = rep(line_i, nrow(matches)),
          matches,
          token = stri_sub(lines[[line_i]], matches),
          raw = lines[[line_i]]
        )
      }
    )
  )
)
tokens[, token.numeric := suppressWarnings(as.numeric(token))]
tokens[, type := ifelse(is.na(token.numeric), "symbol", "number")]
tokens[, id := seq_len(.N)]

# to allow diagonally adjacent, we expand the symbols to be larger
symbols <- tokens[
  type == "symbol",
  .(
    symbol.id = id,
    symbol.row = row,
    symbol.start = start - 1,
    symbol.end = end + 1,
    symbol = token
  )
]
setkey(symbols, symbol.start, symbol.end)

numbers <- tokens[
  type == "number",
  .(
    number.id = id,
    number.row = row,
    number.start = start,
    number.end = end,
    number = token.numeric
  )
]
setkey(numbers, number.start, number.end)

# find overlaps by col then manually by row assuming each token is only on one
#  row this has every single overlap with distinct (number, symbol) pairs (any
#  given number/symbol can be in there multiple times). No overlaps = not in df.
df <- foverlaps(numbers, symbols)[abs(number.row - symbol.row) <= 1]

# sum of numbers in the engine schematic (uniqify by number ID)
print(df[, .SD[1], by = .(number.id)][, sum(number)])


### PART 2
# detect potential gears.
gear_candidates <- df[
  symbol == "*",
  .(
    n.adjacent.numbers = uniqueN(number.id),
    prod.adjacent.numbers = prod(number)
  ),
  by = .(symbol.id)
]

# extract gears
print(gear_candidates[n.adjacent.numbers == 2, sum(prod.adjacent.numbers)])
