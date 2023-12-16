rm(list = ls())
library(stringi)
library(data.table)
# input data ----
x <- unlist(stri_split(readLines("input.txt"), fixed = ","))
chars <- stri_split_boundaries(x, type = "character")

HASH <- function(chars) {
  if (length(chars) == 1) {
    chars <- stri_split_boundaries(chars, type = "character")[[1]]
  }
  nums <- sapply(chars, utf8ToInt)
  Reduce(function(val, char) {
    (17 * (val + char)) %% 256
  }, nums, 0)
}

HASH(c("H", "A", "S", "H"))

sum(sapply(chars, HASH))


HASHMAP <- function(df) {
  boxes <- replicate(256, integer(0)) ## 1-based

  for (step in seq_len(nrow(df))) {
    instruction <- df[step]
    boxi <- instruction$box
    contents <- boxes[[boxi]] # these are lens IDs (step numbers) NOT labels
    contents_as_labels <- df[contents, label]

    if (instruction$operation == "=") {
      if (instruction$label %in% contents_as_labels) {
        contents[contents_as_labels == instruction$label] <- step
      } else {
        contents <- c(contents, step)
      }
    } else {
      if (length(contents)) {
        # remove instruction$label but keep the order
        contents <- contents[contents_as_labels != instruction$label]
      }
    }
    boxes[[boxi]] <- contents
  }
  return(boxes)
}

df <- rbindlist(
  lapply(stri_match_all(x, regex = "^(.+?)([=-])([0-9]+)?$"), data.table)
)
setnames(df, c("raw", "label", "operation", "focal"))
df[, focal := as.integer(focal)]
df[, hash := sapply(label, HASH)]
df[, step := seq_len(.N)]
df[, box := hash + 1]
setkey(df, step)
final_boxes <- HASHMAP(df)

sum(sapply(seq_along(final_boxes), function(bnplus1) {
  if (length(final_boxes[[bnplus1]])) {
    contents <- df[final_boxes[[bnplus1]], focal]
    return(sum(bnplus1 * contents * seq_along(contents)))
  }
  return(0)
}))
