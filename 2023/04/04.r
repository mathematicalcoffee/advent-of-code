rm(list = ls())
library(stringi)
library(data.table)
lines <- readLines("input.txt")

stripped_lines <- gsub("Card +[0-9]+: ", "", lines)
df <- rbindlist(
  lapply(
    seq_along(stripped_lines),
    function(game_i) {
      numbers <- stri_split(
        stri_split(stripped_lines[game_i], regex = " +\\| +")[[1]],
        regex = " +"
      )
      data.table(
        game = game_i,
        raw = lines[game_i],
        number = as.numeric(unlist(numbers)),
        type = rep(c("game", "guess"), lengths(numbers))
      )
    }
  )
)

matches <- df[
  ,
  .(
    n_matches = sum(
      .SD[type == "guess", number] %in% .SD[type == "game", number]
    )
  ),
  by = .(game)
][n_matches > 0]
matches[, score := 2^(n_matches - 1)]
print(matches[, sum(score)])


### PART 2
# what a brainfuck
# I swear there is a really sweet operation of data.table that does this.
#   that uses something like table(n_matches * (something))
copies <- rep(1, length(lines))
for (i in seq_len(nrow(matches))) {
  game_i <- matches[i, game]
  copies_won <- matches[i, seq_len(n_matches) + game]
  copies[copies_won] <- copies[copies_won] + copies[game_i]
}
print(sum(copies))
