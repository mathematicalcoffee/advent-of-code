rm(list=ls())
library(igraph)
library(Rcpp)

# In a final act of desperation, we try to go to a faster language :(
# Frustratingly, this finished in several seconds (vs the R versions not
#  finishing after several hours) and is basically the same code.
# I could have had my answer in hours earlier.
sourceCpp("./23-04-bruteforce-with-rcpp.cpp")  # get_longest_path

g <- readRDS("simplified-graph.rds")

nv <- length(V(g))

visited <- rep(FALSE, nv)
start_i <- which(V(g)$label == "S")
end_i <- which(V(g)$label == "E")
dist_mat <- as_adjacency_matrix(g, attr="weight", sparse=FALSE)
visited <- rep(FALSE, nv)
visited[start_i] <- TRUE

# FUCK ME R is 1-based but Cpp is 0-based
print(get_longest_path(start_i - 1, end_i - 1, visited, dist_mat, nv))

# Unfortunately the code does not return the actual longest path, so no cool
#  plots here.
