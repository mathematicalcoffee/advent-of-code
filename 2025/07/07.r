rm(list=ls())
library(data.table)
library(igraph)
library(testthat)
source("../../handy.R")
ex <- get_and_save_example_input()[[1]]
input <- get_and_save_input()

EMPTY <- '.'
START <- 'S'
SPLIT <- '^'
END <- 'E'

build_nodes <- function (grid) {
  dims <- dim(grid)
  grid[nrow(grid), ] <- END
  nodes <- data.table(
      which(grid != EMPTY, arr.ind=TRUE)
    )
  nodes[, symbol := grid[grid != EMPTY]]
  nodes <- nodes[order(row)]
  nodes[, node_id := 1:.N]
  nodes[, linear_i := ij2i(row, col, dims)]
  nodes[, node_label := sprintf("%i,%i", row, col)]
  nodes <- nodes[order(node_id)]
  setkey(nodes, node_id)
  nodes
}

build_edges <- function(nodes) {
  froms <- c()
  tos <- c()
  for (me.i in 1:nrow(nodes)) {
    me.row <- nodes[me.i, row]
    me.col <- nodes[me.i, col]
    if (nodes[me.i, symbol] == START) {
      # directly down
      dest <- nodes[col == me.col & symbol == SPLIT][which.min(row)]
      stopifnot(nrow(dest) == 1)
    } else if (nodes[me.i, symbol] == SPLIT) {
      # all splitters this splitter could hit.
      dest <- nodes[
        abs(col - me.col) == 1 & row > me.row & symbol %in% c(END, SPLIT)
      ][, .SD[which.min(row)], by=.(col)]
      stopifnot(nrow(dest) <= 2)
    } else {
      next
    }
    if (nrow(dest)) {
      froms <- c(froms, rep(me.i, nrow(dest)))
      tos <- c(tos, dest[, node_id])
    }
  }
  edges <- data.table(
    from_node_id=froms,
    to_node_id=tos
  )
  edges[, edge_id := 1:.N]
  edges[, edge_label := sprintf("%s -> %s", nodes[J(from_node_id), node_label], nodes[J(to_node_id), node_label])]
  edges
}


parse_input <- function (input_str) {
  grid <- lines2matrix(input_str)
  nodes <- build_nodes(grid)
  edges <- build_edges(nodes)
  g <- build_graph(nodes, edges, directed=TRUE)
  g
}

part1 <- function (g) {
  plot_graph_on_grid(g, edge.arrow.size=0, vertex.size=1)
  subcomponent_from_S <- subcomponent(g, V(g)[symbol == START], mode="out")
  sum(subcomponent_from_S$symbol == SPLIT)
}
ex.g <- parse_input(ex)
input.g <- parse_input(input)
expect_equal(part1(ex.g), 21)
expect_equal(part1(input.g), 1615)

# ---- part2 ----
n_paths <- function(g) {
  V(g)$nways <- 0
  
  V(g)[symbol == START]$nways <- 1 # seed.
  children <- V(g)[.outnei(V(g)[symbol == START])]
  
  # i <- 0
  while (length(children)) {
    # i <- i + 1
    for (child.v in children) {
      parent.vs <- V(g)[.innei(child.v)]
      # I feel like there's some sort of bug with visiting a child twice
      V(g)[child.v]$nways <- sum(V(g)[parent.vs]$nways)
    }
    children <- V(g)[.outnei(children)]
    # plot(g, layout=cbind(V(g)$col, -V(g)$row), vertex.size=1, vertex.shape="none", vertex.label=V(g)$nways, edge.label=NA, edge.arrow.size=0)
    # title(i)
  }
  # BAH - too many
  sum(V(g)[symbol == 'E']$nways)
}


part2 <- function (g) {
  n_paths(g)
  # This one is too slow / OOMs on my real data
  # sub.g <- subcomponent(g, V(g)[symbol == START], mode="out")
  # ends <- sub.g[symbol == END]
  # length(all_simple_paths(g, from=V(g)[symbol == START], to=ends, mode="out"))
}

expect_equal(part2(ex.g), 40)
# OUT OF MEMORY :(
# I don't actually care about the paths themselves, I just want the NUMBER of paths.
# too high:259118195229404719744086  oops was double-counting children that are visited on diff iterations (v = v + parent$nways vs v = sum(parent$nways))
expect_equal(
  part2(input.g),
  43560947406326
)

g <- input.g
cols <- sample(c("orangered", "lightskyblue", "white", "yellow", "mediumorchid"), length(V(g)), replace=TRUE)
cols[V(g)$symbol == START] <- "yellow"
cols[V(g)$symbol == END] <- "#654321"
par(bg="#0f0f23")
plot_graph_on_grid(
  g,
  edge.arrow.size=0,
  vertex.size=1,
  vertex.label.cex=ifelse(V(g)$symbol == "S", 1.5, 1),
  edge.color='forestgreen',
  vertex.label.color=cols,
)
dev.print(png, filename='day7.png', width=1000, height=1000)
