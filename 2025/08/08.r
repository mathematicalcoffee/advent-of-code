rm(list=ls())
library(data.table)
library(stats)
library(testthat)
library(igraph)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()



# colnames(boxes) <- c("x", "y", "z")
# boxes[, id := 1:.N]
# boxes[, label := sprintf("%i,%i,%i", x, y, z)]
N_CONNECTIONS <- 10

part1 <- function (input_str, n_connections=10, top_n=3) {
  boxes <- parse_input(input_str)
  edges <- get_edgelist_by_distance(boxes)
  g <- graph_from_edgelist(edges[1:n_connections, 1:2], directed=FALSE)
  plot(g, vertex.label=NA, vertex.size=3, edge.width=2, edge.color='black')
  return(prod(sort(components(g)$csize, decreasing=TRUE)[1:top_n]))
}

parse_input <- function (input_str) {
  boxes <- read.csv(text=input_str, header=FALSE)
  boxes
}
get_edgelist_by_distance <- function (coords) {
  n <- nrow(coords)
  dd <- dist(coords, method="euclidean", diag=FALSE)
  # sorted by distance
  connection_ijs <- i2distij(order(dd, decreasing=FALSE), n)
  colnames(connection_ijs) <- c('from', 'to')
  return(cbind(connection_ijs, 'dist'=sort(dd)))
}
i2distij <- function (i, n) {
  # black magic! https://atrebas.github.io/post/2021-01-17-index_to_lower_triangular_subscripts/
  # i2ij, but on a col-wise lower-triangle matrix that excludes the diagonal ?!
  kp <- n * (n - 1) / 2 - i
  p  <- floor((sqrt(1 + 8 * kp) - 1) / 2)
  row  <- n - (kp - p * (p + 1) / 2)
  col  <- n - 1 - p
  cbind(row=row, col=col)
}

expect_equal(part1(ex, n_connections=10), 40)
part1(input, n_connections=1000)

# ------

# Totally diff approach for part2 (I feel like constructing the graph iteratively
#  just to call is_connected() on it with 1 extra edge at a time is going to be
#  really inefficient??)

in_same_cluster <- function (cluster1, cluster2) {
  cluster1 == cluster2 && has_cluster(cluster1)
}
neither_in_cluster <- function (cluster1, cluster2) {
  !has_cluster(cluster1) && !has_cluster(cluster2)
}
in_different_clusters <- function (cluster1, cluster2) {
  cluster1 != cluster2 && has_cluster(cluster1) && has_cluster(cluster2)
}
has_cluster <- function (cluster) {
  return(cluster != 0)
}

num_edges_until_fully_connected <- function(boxes, edges) {
  n <- nrow(boxes)
  cluster_membership <- rep(0, n)
  n_components <- n
  c.i <- 1 # cluster number (arbitrary, just needs to be different for each new one)
  n.connections <- 0
  while (n_components > 1) {
    n.connections <- n.connections + 1
    box1 <- edges[n.connections, 1]
    box2 <- edges[n.connections, 2]
    cluster1 <- cluster_membership[box1]
    cluster2 <- cluster_membership[box2]
    if (in_same_cluster(cluster1, cluster2)) {
      # pass - no reduction in # of components
    } else if (neither_in_cluster(cluster1, cluster2)) {
      # add both components to new cluster
      n_components = n_components - 1
      cluster_membership[box1] <- c.i
      cluster_membership[box2] <- c.i
      c.i <- c.i + 1
    } else if (in_different_clusters(cluster1, cluster2)) {
      # merge clusters
      n_components = n_components - 1
      cluster_membership[cluster_membership == cluster1] <- cluster2
      cluster_membership[box2] <- cluster2
    } else if (has_cluster(cluster1)) {
      # add unconnected node to existing cluster
      n_components = n_components - 1
      cluster_membership[box2] <- cluster1
    } else if (has_cluster(cluster2)) {
      # add unconnected node to existing cluster
      n_components = n_components - 1
      cluster_membership[box1] <- cluster2
    }
  }
  return(n.connections)
}

part2 <- function (input_str) {
  boxes <- parse_input(input_str)
  edges <- get_edgelist_by_distance(boxes)
  edge.i <- num_edges_until_fully_connected(boxes, edges)
  prod(boxes[edges[edge.i, 1:2], 1])
}

expect_equal(part2(ex), 25272)
part2(input)
