rm(list = ls())
source("../handy.R")
library(stringi)
library(data.table)
library(igraph)

NORTH <- 1
EAST <- 2
SOUTH <- 3
WEST <- 4
INITIAL_DIRECTION <- 5

VALID_NEXT_DIRECTIONS <- list(
  # order got to match up with enum-value
  NORTH=c(EAST, WEST),
  EAST=c(NORTH, SOUTH),
  SOUTH=c(EAST, WEST),
  WEST=c(NORTH, SOUTH),
  INITIAL_DIRECTION=c(NORTH, EAST, SOUTH, WEST)
)

heat_loss_vec <- function(x, n = 3L, fill=Inf) {
  # sum the heat loss by walking `n` steps. = sum of heatloss for next `n`
  #   elements, not including the element you start in
  # out[i] = sum(in[(i + 1):(i + n - 1)])
  #    1 2 3  4  5  6  7  8 9 10 with 2 steps
  # -> 5 7 9 11 13 15 17 19 NA NA
  #c(tail(cumsum(x) - cumsum(c(rep(0, n-1), head(x, -(n-1)))), -n + 1), rep(fill, n - 1))
  c(tail(cumsum(x) - cumsum(c(rep(0, n), head(x, -n))), -n), rep(fill, n))
}

populate_heat_loss <- function (input, step_range) {
  # heat_loss[i, j, n, direction] =
  # heat loss by starting at [i,j] and walking N steps in DIRECTION
  # offset by min(step_range) but 1-based
  offs <- min(step_range) - 1
  heat_loss <- array(Inf, c(dim(input), max(step_range) - min(step_range) + 1, 4))
  for (n_steps in step_range) {
    heat_loss[,, n_steps - offs, EAST] <- t(apply(input, 1, heat_loss_vec, n=n_steps))
    heat_loss[,ncol(input):1, n_steps - offs, WEST] <- t(apply(input[, ncol(input):1], 1, heat_loss_vec, n=n_steps))
    
    heat_loss[,, n_steps - offs, SOUTH] <- apply(input, 2, heat_loss_vec, n=n_steps)
    heat_loss[nrow(input):1,, n_steps - offs, NORTH] <- apply(input[nrow(input):1, ], 2, heat_loss_vec, n=n_steps)
  }
  return(heat_loss)
}
get_new_row <- function(at_row, direction, n_steps) {
     to_row <- at_row
     if (direction == NORTH) {
       to_row <- to_row - n_steps
     } else if (direction == SOUTH) {
       to_row <- to_row + n_steps
     }
     return(to_row)
}
get_new_col <- function(at_col, direction, n_steps) {
     to_col <- at_col
     if (direction == WEST) {
       to_col <- to_col - n_steps
     } else if (direction == EAST) {
       to_col <- to_col + n_steps
     }
     return(to_col)
}

# -----------------------------------
input_file <- "input.txt"
input <- read_matrix(input_file, numeric=TRUE)
step_range <- 1:3 # part 1
step_range <- 4:10
heat_loss <- populate_heat_loss(input, step_range=step_range)
output_file <- "Q1_part2.rda"

library(igraph)
library(Matrix)
if ((!file.exists(output_file) && input_file == "input.txt") || input_file == "input-small.txt") {
  len <- prod(dim(input)) * 2
  coords <- expand.grid(row=1:nrow(input), col=1:ncol(input))
  horizontal_node <- array(sprintf("%i_%i_h", coords[,'row'], coords[,'col']), dim=dim(input))
  vertical_node <- array(sprintf("%i_%i_v", coords[,'row'], coords[,'col']), dim=dim(input))
  node_names <- c(as.vector(horizontal_node), as.vector(vertical_node))
  # probably need to pre-allocate the non-sparse values
  weights <- sparseMatrix(i=integer(0), j=integer(0), x=0, dim=c(len, len), dimnames=list(node_names, node_names))
  # weights <- array(
  #   0,
  #   dim=c(len, len),
  #   dimnames=list(node_names, node_names)
  # )
  
  # node (i, j, direction) is at (i, j) and now will walk in direction
  # TODO this is taking ages, can we transform `heat_loss` instead
  offs <- min(step_range) - 1
  for (i in 1:nrow(input)) {
    for (j in 1:ncol(input)) {
      for (n_steps in step_range) {
        for (dir in c(NORTH, EAST, WEST, SOUTH)) {
          cost <- heat_loss[i, j, n_steps - offs, dir]
          if (is.infinite(cost))
            next
          
          to_row <- get_new_row(i, dir, n_steps)
          to_col <- get_new_col(j, dir, n_steps)
          if (dir %in% c(EAST, WEST)) {
            from_node <- horizontal_node[i, j]
            to_node <- vertical_node[to_row, to_col]
          } else if (dir %in% c(NORTH, SOUTH)) {
            from_node <- vertical_node[i, j]
            to_node <- horizontal_node[to_row, to_col]
          }
          weights[from_node, to_node] <- cost
        }
      }
    }
  }
  
  g <- graph_from_adjacency_matrix(weights, weighted=TRUE, mode='directed', diag=FALSE)
  if (input_file == "input.txt")
    save(g, weights, heat_loss, horizontal_node, vertical_node, file=output_file)
} else {
  load(output_file)
}
#E(g)[.from("5_5_h")]
#E(g)[.to("5_5_h")]

# this doesn't seem quite right
dims <- dim(input)
dist <- distances(
  g,
  v=V(g)[c(horizontal_node[1,1], vertical_node[1,1])],
  to=V(g)[c(horizontal_node[dims[1], dims[2]], vertical_node[dims[1], dims[2]])],
  mode='out'
)

# ps <- shortest_paths(g, from="1_1_h", to=V(g)[c("13_13_h", "13_13_v")], output='epath')
# sum(E(g)[ps$epath[[1]]]$weight)
# sum(E(g)[ps$epath[[2]]]$weight)

