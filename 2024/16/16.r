rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

WALL <- "#"
END <- "E"
START <- "S"

is_opposite <- function (dir1, dir2) {
  return(
    dir1 == UP & dir2 == DOWN |
    dir1 == DOWN & dir2 == UP |
    dir1 == LEFT & dir2 == RIGHT |
    dir1 == RIGHT & dir2 == LEFT
  )
}

library(igraph)

build_nodes <- function (map) {
  nodes <- adjacency_df(dim(map))
  nodes[, from.symbol := map[as.matrix(nodes[, .(from.row, from.col)])]]
  nodes[, to.symbol := map[as.matrix(nodes[, .(to.row, to.col)])]]
  nodes <- nodes[from.symbol != WALL & to.symbol != WALL]
  
  nodes[from.row == to.row & to.col > from.col, leaving.direction := RIGHT]
  nodes[from.row == to.row & to.col < from.col, leaving.direction := LEFT]
  nodes[from.col == to.col & to.row > from.row, leaving.direction := DOWN]
  nodes[from.col == to.col & to.row < from.row, leaving.direction := UP]
  nodes[, linear_i := ij2i(from.row, from.col, dim(map))]
  nodes[, node_label := sprintf("%i,%i leaving %s", from.row, from.col, leaving.direction)]
  nodes[, node_id := .I]
  nodes <- nodes[order(node_id)]
  return(nodes)
}
build_edges <- function (nodes) {
  # then connect pairs of these
  adj <- merge(nodes, nodes, suffixes=c(".from", ".to"),
        by.x=c("to.row", "to.col"), by.y=c("from.row", "from.col"),
        allow.cartesian=TRUE)
  adj <- adj[!is_opposite(leaving.direction.from, leaving.direction.to)]
  adj[, cost := ifelse(leaving.direction.from == leaving.direction.to, 1, 1001)]
  
  # BAH: start node is allowed to rotate
  start.node <- nodes[from.symbol == START & leaving.direction == RIGHT, node_id]
  end.nodes <- nodes[from.symbol == END, node_id]
  turn.north.from.start <- nodes[from.symbol == START & leaving.direction == UP, node_id]
  adj <- rbind(
    adj,
    data.table(node_id.from=start.node, node_id.to=turn.north.from.start, cost=1000),
    fill=TRUE
  )
  adj[node_id.to %in% end.nodes, cost := 1] # not a turn
  adj[, edge_id := .I]
  adj
}

make_graph <- function (nodes, edges) {
  g <- graph_from_edgelist(as.matrix(edges[, .(node_id.from, node_id.to)]), directed=TRUE)
  V(g)$label <- nodes$node_label
  V(g)$i <- nodes$from.row
  V(g)$j <- nodes$from.col
  V(g)$linear_i <- nodes$linear_i
  E(g)$weight <- edges$cost
  return(g)
}

map <- lines2matrix(input)
nodes <- build_nodes(map)
edges <- build_edges(nodes)
g <- make_graph(nodes, edges)

start.node <- nodes[from.symbol == 'S' & leaving.direction == RIGHT, node_id]
end.nodes <- nodes[from.symbol == 'E', node_id]

d <- distances(
  g,
  v=start.node,
  to=end.nodes,
  mode="out"
)
print(paste("min path cost is", min(d)))

# PART 2
ps <- suppressWarnings(all_shortest_paths(g, from=start.node, t=end.nodes, mode="out"))
wght <- sapply(ps$epaths, function (pth) sum(E(g)$weight[pth]))
keep <- wght == min(wght) # there are 2 approach directions in here
visited <- V(g)[unlist(ps$vpaths[keep])]

out <- map
out[visited$linear_i] <- 'O'
# print_matrix(out)
print(paste(uniqueN(visited$linear_i), "places to sit"))
