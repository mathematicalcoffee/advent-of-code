rm(list = ls())
library(igraph)

# Aim - brute-force-search the longest path. The graph has only about 34
#  nodes with degree > 1 so it should be a small(ish) space.
# Two versions - one recursive appending to a `visited` array (yuck) and
#  one non-recursive with a stack pattern (but idk if the constant appending
#  /popping of the list is that efficient in R)
# Could do a version with a binary vector of `visited` but either way,
#  these ones took AGES and NEVER STOPPED (ran extremely fast on sample code)
# I am sure this is the right algorithm and I've made all the simplifications
#  I had to (reducing the graph to the simplest structure), and not sure if I'm
#  just hitting up against R limitations.

get_longest_path <- function(node, finish_node, g, visited) {
  if (node == finish_node) {
    return(0)
  }

  weight <- 0
  for (nbh in neighbors(g, node)) {
    if (!(nbh %in% visited)) {
      weight <- max(
        weight,
        E(g)[.from(node) & .to(nbh)]$weight +
          get_longest_path(nbh, finish_node, g, c(visited, nbh))
      )
    }
  }
  return(weight)
}

get_longest_path_stack <- function(start, end, g) {
  visited <- c(start)
  stack <- list(list(at = start, dist = 0, visited = visited))
  max_hike <- 0
  dist_mat <- as_adjacency_matrix(g, attr = "weight")

  while (length(stack)) {
    args <- stack[[1]]
    stack <- stack[-1]
    if (args$at == end) {
      max_hike <- max(max_hike, args$dist)
    }
    for (nbh in neighbors(g, args$at)) {
      if (!(nbh %in% args$visited)) {
        stack <- c(
          stack,
          list(
            list(
              at = nbh,
              dist = args$dist + dist_mat[args$at, nbh],
              visited = c(args$visited, nbh)
            )
          )
        )
      }
    }
  }
  return(max_hike)
}

g <- readRDS("simplified-graph.rds")
start_i <- which(V(g)$label == "S")
end_i <- which(V(g)$label == "E")

# INFEASIBLE - killed it after several hours
if (FALSE) {
  print(get_longest_path(start_i, end_i, g, c()))
  print(get_longest_path_stack(start_i, end_i, g))
}
