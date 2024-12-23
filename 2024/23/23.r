rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

library(igraph)

# sets of 3 computers where each computer is connected to the other 2 (but potentially others too)

edges <- input
g <- graph_from_edgelist(do.call(rbind, strsplit(edges, "-")), directed=FALSE)
# plot(g)
bits <- cliques(g, min=3, max=3)
paste(length(Filter(function (cluster) any(grepl("^t", cluster$name)), bits)))

x <- largest_cliques(g)
paste(collapse(sort(x[[1]]$name), sep=","))
