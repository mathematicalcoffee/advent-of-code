rm(list=ls())
library(data.table)
library(igraph)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

WALL <- "#"
END <- "E"
START <- "S"

is_walkable <- function (fromnode, tonode) {
  # don't walk from the start; don't walk through walls
  return(
    fromnode$symbol != WALL &
    tonode$symbol != WALL &
    fromnode$symbol != END
  )
}
is_valid_edge <- function (fromnode, tonode) {
  # don't walk through walls
  is_walkable(fromnode, tonode) &
  # don't walk to the start node from elsewhere (but you can rotate)
  !(fromnode$symbol != "S" & tonode$symbol == "S") &
  (
    # walking straight
    (is_node_in_direction(fromnode, tonode) & fromnode$facing.direction == tonode$facing.direction)
    |
    # OR 90-degree turns on the spot
    (fromnode$linear_i == tonode$linear_i & is_turn(fromnode$facing.direction, tonode$facing.direction, "both"))
  )
}

map <- lines2matrix(input)
nodes <- build_nodes_with_direction(map)
# TODO: making the edges is quite slow, perhaps because of how I am doing it in data.table
edges <- build_edges(nodes[symbol != '#'], is_valid_edge)

edges[nodes, from_linear_i := linear_i, on=.(from_node_id=node_id)]
edges[nodes, to_linear_i := linear_i, on=.(to_node_id=node_id)]
edges[, cost := ifelse(from_linear_i == to_linear_i, 1000, 1)]
g <- build_graph(nodes, edges)
start.node <- nodes[from.symbol == 'S' & leaving.direction == RIGHT, node_id]
end.nodes <- nodes[from.symbol == 'E', node_id]

# ----- PART 1 -----
d <- distances(g, v=start.node, to=end.nodes, mode="out")
print(paste("min path cost is", min(d)))

# ----- PART 2 -----
ps <- suppressWarnings(all_shortest_paths(g, from=start.node, t=end.nodes, mode="out"))
wght <- sapply(ps$epaths, function (pth) sum(E(g)$weight[pth]))
keep <- wght == min(wght) # there are 2 approach directions in here
visited <- V(g)[unlist(ps$vpaths[keep])]
print(paste(uniqueN(visited$linear_i), "places to sit"))
# out <- map
# out[visited$linear_i] <- 'O'
# print_matrix(out)

