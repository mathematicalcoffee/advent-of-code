rm(list = ls())
library(stringi)
library(data.table)
# input data ----

# "rocks" = round rocks
# "stops" = square rocks (that stop the round rocks)

tilt <- function (direction, rocks, stops, dims) {
  # rocks / stops are df with row, col
  # sadly this is different but almost identical code for north/south/east/west
  #  because I've not implemented it well (as a dataframe and using rolling join
  #  rather than e.g. coordinates that can just be swapped for different dimensions)
  # but it's what I thought of first. I am trying to kill everything with data.table
  mult <- ifelse(direction %in% c("north", "west"), 1, -1) 
  roll <- mult * Inf
  if (direction %in% c("north", "south")) {
    setkey(rocks, col, row)
    setkey(stops, col, row)
    
    new_rocks <- stops[
      rocks, roll=roll
    ][,
      {
      x <- stop.row
      x[is.na(x)] <- ifelse(direction == "north", 0, dims[1] + 1)
      .(
        row=x + mult * seq_len(.N)
      )
      }, by=.(stop.row, col)
    ]
    return(new_rocks)
  } else if (direction %in% c("east", "west")) {
    # unfortunately this is the same code different
    setkey(rocks, row, col)
    setkey(stops, row, col)
    
    new_rocks <-  stops[
      rocks, roll=roll
    ][,
      {
      x <- stop.col
      x[is.na(x)] <- ifelse(direction == "west", 0, dims[2] + 1)
      .(
        col=x + mult * seq_len(.N)
      )
      }, by=.(stop.col, row)
    ]
    return(new_rocks)
    
  }
}
cycle <- function(rocks, stops, dims) {
  rocks <- tilt("north", rocks, stops, dims)
  rocks <- tilt("west", rocks, stops, dims)
  rocks <- tilt("south", rocks, stops, dims)
  rocks <- tilt("east", rocks, stops, dims)
}

# oof don't do this with a large n hey
# let's just cheese it, return the loads and hope they cycle after a while
# (seems like they should)
cycle_n <- function (rock, stops, dims, n) {
  load <- rep(-1, n)
  old_rocks <- rocks
  for (i in 1:n) {
    load[i] <- calc_load(rocks, dims)
    rocks <- cycle(rocks, stops, dims)
    if (all(old_rocks[, .(row, col)] == rocks[, .(row, col)])) {
      print("stopping early at", i)
      return(rocks)
    }
  }
  return(list(load=load, rocks=rocks))
}
df_to_dish <- function(rocks, stops, dims, print=TRUE) {
  # converts rock-coords & stop-coords to a pretty picture
  dish <- array(data=".", dim=dims)
  dish[as.matrix(rocks[, .(row, col)])] <- "O"
  dish[as.matrix(stops[, .(row, col)])] <- "#"
  if (print)
    write(t(dish), ncolumns=dims[2], sep="", file="")
    
  return(invisible(dish))
}
calc_load <- function(rocks, dims) {
  rocks[, sum(dims[1] - row + 1)]
}

# ------------------------------------------------------------------------------
input <- stri_list2matrix(
  stri_split_boundaries(
    readLines("input.txt"),
    type = "character"
  ),
  byrow = TRUE
)

df <- rbind(
  cbind(symbol="O", data.table(which(input == "O", arr.ind=T))),
  cbind(symbol="#", data.table(which(input == "#", arr.ind=T)))
)


# for each column, find spaces between the platforms
rocks <- df[symbol =="O"]
stops <- df[symbol =="#"]

NROW <- nrow(input)
stops[, stop.row := row]
stops[, stop.col := col]
# 108935 or 136
x <- tilt("north", rocks, stops, dim(input))
print(calc_load(x, dim(input)))

# 
# x <- tilt("south", rocks, stops, dim(input))
# df_to_dish(x, stops, dim(input))
# 
# x <- tilt("east", rocks, stops, dim(input))
# df_to_dish(x, stops, dim(input))
#
# x <- tilt("west", rocks, stops, dim(input))
# df_to_dish(x, stops, dim(input))

# x <- cycle(rocks, stops, dim(input))
# df_to_dish(x, stops, dim(input))

# L O L
# o <- cycle_n(rocks, stops, dim(input), 500)
# save(o, file="Q2.rda")
plot(o$load) # FUCK YEAH, THEY'RE CYCLIC

plot(o$load[102:136])
plot(o$load[137:(137+34)])

cycle_offset <- 101
cycle_len <- 35
cycle <- o$load[seq(cycle_offset + 1, length.out=cycle_len)]
plot(cycle) # yep

n <- 1000000000
zero_based_cycle_i <- (n - cycle_offset) %% cycle_len
# ta da
print(cycle[zero_based_cycle_i + 1])
