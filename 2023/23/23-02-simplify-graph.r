rm(list = ls())
library(igraph)
library(stringi)
source("../handy.R")

# idea - remove all the useless nodes in the graph to just the junctions
#  and set the weighting of the edges to the distance between the nodes
# the useless nodes have degree 2.
# (we were being lazy in part 1)
# Unfortunately, it was still infeasible.

input_file <- "input.txt"
input <- read_matrix(input_file)

dims <- dim(input)
g <- make_lattice(dimvector = dims, directed = FALSE)
coords <- layout_on_grid(g, width = dims[2])
coords <- coords[, 2:1]
coords[, 2] <- -coords[, 2]

start_i <- which(row(input) == 1 & input == ".")
end_i <- which(row(input) == dims[1] & input == ".")
V(g)[start_i]$label <- "S"
V(g)[end_i]$label <- "E"

dropi <- input == "#"
g <- g - which(dropi)
coords <- coords[!dropi, ]

# Now - simplify `g` and drop everything with nnei == 2 (connecting along)
keep <- which(degree(g) != 2)
V(g)$og_i <- as.integer(V(g))

# can simplify if you can find a path between me and you without any of the
#  other junction nodes
adj_mat <- array(0, dim = c(length(keep), length(keep)))
for (starti in seq_along(keep)) {
  for (endi in starti:length(keep)) {
    if (starti == endi) next

    # ugh, the -vertex changes the indices of the start and end
    g2 <- g - keep[-c(starti, endi)]

    p <- all_simple_paths(
      g2,
      from = which(V(g2)$og_i == keep[starti]),
      to = which(V(g2)$og_i == keep[endi])
    )
    if (length(p) == 1) {
      # weight
      adj_mat[starti, endi] <- length(p[[1]]) - 1
      adj_mat[endi, starti] <- length(p[[1]]) - 1
    }
    if (length(p) > 1) { # can simplify somehow
      stop()
    }
  }
}

gs <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE)
V(gs)[1]$label <- "S"
V(gs)[length(keep)]$label <- "E"
coords_simp <- coords[keep, ]

vertices_of_interest <- NULL
png(filename = "day23-simplified-graph.png", width = 1200, height = 1200)
plot(
  g,
  layout = coords,
  vertex.size = 1,
  vertex.color = ifelse(V(g) %in% vertices_of_interest, "red", "grey"),
  vertex.frame.color = NA,
  edge.arrow.size = .1,
)
plot(
  gs,
  layout = coords_simp[, ],
  vertex.size = 5,
  vertex.color = ifelse(V(gs) %in% vertices_of_interest, "red", "black"),
  vertex.frame.color = NA,
  vertex.label.color = "black",
  edge.color = "black",
  edge.width = 1,
  edge.label = E(gs)$weight,
  add = TRUE
)
dev.off()

stop()
# this is not feasible for my actual data (in R's igraph anyway)
ps <- all_simple_paths(
  gs,
  from = which(V(gs)$label == "S"),
  to = which(V(gs)$label == "E")
)
# need the WEIGHT along those paths
dist <- sapply(
  ps,
  function(path) {
    es <- head(tail(rep(path, each = 2), -1), -1)
    sum(E(gs)[get.edge.ids(gs, es)]$weight)
  }
)
print(max(dist))
