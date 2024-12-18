rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

to_coords <- function (lines) {
  x <- do.call(rbind, lapply(stri_split_fixed(pattern=",", lines), as.integer))
  colnames(x) <- c("x", "y")
  x
}
coords_to_map <- function (coords, dims) {
  map <- array(".", dim=dims)
  map[coords] <- "#"
  map
}

# X, Y
# dims <- c(7, 7)
# cxy <- to_coords(ex[[1]])
# nbytes <- 12


dims <- c(71, 71)
cxy <- to_coords(input)
nbytes <- 1024

cij <- cxy[, c('y', 'x')]
colnames(cij) <- c('row', 'col')
cij <- cij + 1 # the y is inverted too

map <- coords_to_map(cij[1:nbytes, ], dims)
print_matrix(map)


library(igraph)
n <- build_nodes(map)
e <- build_orthogonal_edges(n, "#")
g <- build_graph(n, e)
if (F) {
  plot_graph_on_grid(g, edge.size=NA)
}


start <- n[row == 1 & col == 1, node_id]
end <- n[row == dims[1] & col == dims[2], node_id]

distances(g, v=start, to=end)

p <- shortest_paths(g, from=start, to=end)
byte.i <- nbytes + 1
orig.g <- g
while (TRUE) {
  new_byte_id <- n[row == cij[byte.i, 'row'] & col == cij[byte.i, 'col'], node_id]
  V(g)[new_byte_id]$symbol <- '#'
  es_to_remove <- incident_edges(g, new_byte_id, mode="all")
  
  g <- delete_edges(g, es_to_remove[[1]])
  d <- distances(g, v=start, to=end, mode="all")
  if (is.infinite(d))
    break
  byte.i <- byte.i + 1
}
plot_graph_on_grid(g, edge.size=0, edge.arrow.size=0, edge.color=NA)
# 64, 54
print(cxy[byte.i, ])
