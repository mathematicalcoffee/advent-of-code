library(stringi)
library(data.table)
input <- readLines("input.txt")
NUMBERS <- c(
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
)

## part 1
# this is fun but stupid
# ms=stri_match_all(
#   lines,
#   regex="^(?=[^0-9]*([0-9])).*?([0-9])[^0-9]*$",
# )

df <- data.table(
  input = input,
  first.digit = as.numeric(stri_extract_first(input, regex = "[1-9]")),
  last.digit = as.numeric(stri_extract_last(input, regex = "[1-9]"))
)
df[, sum(10 * first.digit + last.digit)]
# 54951

## part 2
## the question is not clear as to whether a word consumes the characters making
##  them unusable by the other characters
regex_forwards <- paste0("[1-9]|", paste(NUMBERS, collapse = "|"))
regex_backwards <- paste0("[1-9]|", paste(stri_reverse(NUMBERS), collapse = "|"))
df <- data.table(
  input = input,
  first.match = stri_extract_first(input, regex = regex_forwards),
  # imagine that, "extract last" doesn't extract last, `twone` -> extracts the
  #  two not the one. The regex being greedy
  last.match = stri_extract_first(stri_reverse(input), regex = regex_backwards)
)
convert_to_number <- function(x) {
  # converts input vector (expected to be a digit or the english word of a digit
  #  1-9) to numerics
  x_numeric <- suppressWarnings(as.numeric(x))
  # 1-based indices yeah
  return(
    ifelse(
      is.na(x_numeric),
      (match(x, c(NUMBERS, stri_reverse(NUMBERS))) - 1) %% 9 + 1,
      x_numeric
    )
  )
}
df[, first.digit := convert_to_number(first.match)]
df[, last.digit := convert_to_number(last.match)]
df[, sum(10 * first.digit + last.digit)]
