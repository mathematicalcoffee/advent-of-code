rm(list = ls())
library(igraph)
library(Rcpp)

# In a final act of desperation, we try to go to a faster language :(
# Frustratingly, this finished in several seconds (vs the R versions not
#  finishing after several hours) and is basically the same code.
# I could have had my answer in hours earlier.
sourceCpp("./23-04-bruteforce-with-rcpp.cpp") # get_longest_path

g <- readRDS("simplified-graph.rds")

nv <- length(V(g))

visited <- rep(FALSE, nv)
start_i <- which(V(g)$label == "S")
end_i <- which(V(g)$label == "E")
dist_mat <- as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
visited <- rep(FALSE, nv)
visited[start_i] <- TRUE

# FUCK ME R is 1-based but Cpp is 0-based
# print(get_longest_path_length(start_i - 1, end_i - 1, visited, dist_mat, nv))

#
#  plots here.
path <- rep(0, nv + 1)
path[start_i + 1] <- 1
ans <- get_longest_path(start_i - 1, end_i - 1, 1, dist_mat, nv, path)
print(ans[1]) # longest path

# --------------------------- plot it ---------------------------- #
# draw it
path <- ans[-1]
n_not_visited <- sum(path == 0)
path <- order(path)[seq(1 + n_not_visited, length(path))]
path_edges <- E(g, path = path)
edge_in_path <- E(g) %in% path_edges

cache <- new.env()
load("plot.rda", envir = cache)
og <- cache$g
dir <- rep(">", sum(edge_in_path))

# draw the direction on the graph, why was that so hard?
g2 <- as.directed(g, mode = "arbitrary")
.ends <- ends(g, path_edges)
to_flip <- list()
for (i in 1:(length(path) - 1)) {
  fromi <- path[i]
  toi <- path[i + 1]
  e <- E(g2)[.from(toi) & .to(fromi)]
  if (length(e)) {
    # flip the parity
    to_flip <- c(to_flip, e)
  }
}
g2 <- reverse_edges(g2, eids = to_flip)

png(filename = "day23-part2.png", width = 1200, height = 1200)
plot(
  og,
  layout = cache$coords,
  vertex.size = 1,
  vertex.color = "grey",
  vertex.frame.color = NA,
  edge.arrow.size = .1,
)
plot(
  g2,
  layout = cache$coords_simp,
  vertex.size = 2,
  vertex.color = ifelse(V(g2) %in% path, "red", "grey"),
  vertex.frame.color = NA,
  vertex.label.cex = 2,
  edge.color = ifelse(edge_in_path, "red", "black"),
  edge.label.color = ifelse(edge_in_path, "black", "#666666"),
  edge.label.cex = 2,
  edge.width = ifelse(edge_in_path, 2, 1),
  edge.lty = ifelse(edge_in_path, "solid", "dashed"),
  edge.label = E(g2)$weight,
  edge.arrow.mode = ifelse(edge_in_path, ">", "-"),
  edge.arrow.size = 2,
  add = TRUE
)
dev.off()

stop()

png(filename = "day23-part2-maze.png", width = 1200, height = 1200)
# even cooler - can we convert this path back to the original graph
og_edge_in_path <- rep(FALSE, length(E(og)))
for (eid in which(edge_in_path)) {
  if (!is.null(E(g)[eid]$original_path)) {
    eids <- E(g)[eid]$original_path[[1]]
    og_edge_in_path[eids] <- TRUE
  }
}

og_v_in_path <- V(og) %in% unique(as.vector(ends(og, which(og_edge_in_path))))
plot(
  og,
  layout = cache$coords,
  vertex.size = 1,
  vertex.color = ifelse(og_v_in_path, "red", "grey"),
  vertex.label.cex = 2,
  edge.color = ifelse(og_edge_in_path, "red", "grey"),
  vertex.frame.color = NA
)
dev.off()
