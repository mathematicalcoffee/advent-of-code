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
o <- build_grid_with_turn_directions(
  map, impassibles="#",
  additional_edge_filter = function (fromnode, tonode) {
    # don't walk from the end node anywhere (including rotating)
    fromnode$symbol != "E" &
      # don't walk to the start node from elsewhere (but you can rotate)
      !(fromnode$symbol != "S" & tonode$symbol == "S")
  }
)
nodes <- o$nodes
edges <- o$edges
edges[, cost := ifelse(from_facing != to_facing, 1000, 1)]
g <- build_graph(nodes, edges, directed=TRUE)
# plot_graph_on_grid(g, edge.arrow.size=0.2)

start.node <- nodes[symbol == 'S' & facing == RIGHT, node_id]
end.nodes <- nodes[symbol == 'E', node_id]

# ----- PART 1 -----
d <- distances(g, v=start.node, to=end.nodes, mode="out")
p <- shortest_paths(g, from=start.node, t=end.nodes, mode="out")
V(g)[p$vpath[[4]]]$label

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

