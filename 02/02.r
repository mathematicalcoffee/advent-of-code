library(stringi)
library(data.table)

inputs <- readLines("input.txt")
# 12 red cubes, 13 green cubes, and 14 blue cubes
constraints <- data.table(
  colour = c("red", "green", "blue"),
  max.qty = c(12, 13, 14)
)

# produce a dataframe of observations (long-format)
# game | set | colour | qty
# extract a list of sets per game
sets <- stri_split(inputs, fixed = ";")
df <- rbindlist(unlist(lapply(
  seq_along(inputs), function(game_i) {
    # extract a list of observations in this set
    obs <- stri_match_all(
      sets[[game_i]], regex = "(?<qty>[0-9]+) (?<colour>blue|red|green)"
    )
    lapply(
      seq_along(obs),
      function(set_i) {
        data.table(
          game = game_i,
          set = set_i,
          obs[[set_i]][, c("qty", "colour"), drop = FALSE],
          raw_set = sets[[game_i]][[set_i]]
        )
      }
    )
  }
), recursive = FALSE))
df[, qty := as.numeric(qty)] # I can't believe you can do `"5" < 15` in R


### Q1:::

# compare to the constraints
df[constraints, on = .(colour)][,
  .(set.compliant = all(qty <= max.qty)),
  by = .(game, set)
][,
  .(game.compliant = all(set.compliant)),
  by = .(game)
][game.compliant == TRUE, sum(game)]


### Q2:::
# I LOVE DATA.TABLE
df[,
  .(min.required = max(qty)),
  by = .(game, colour)
][,
  .(power = prod(min.required)),
  by = .(game)
][, sum(power)]
