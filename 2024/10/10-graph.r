# did this version later with a graph. it's overkill but fun.
rm(list=ls())
library(igraph)
library(data.table)
library(ggplot2)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()
map <- lines2matrix(input, numeric=TRUE)


df <- adjacency_df(dim(map))
# data to work out edges etc
df[, from.id := ij2i(from.row, from.col, dim(map))]
df[, to.id := ij2i(to.row, to.col, dim(map))]
df[, from.height := map[cbind(from.row, from.col)]]
df[, to.height := map[cbind(to.row, to.col)]]
# make graph

adj_list <- lapply(
  1:length(map), 
  function (i) {
    df[from.id == i & to.height - from.height == 1, to.id]
  }
)
g <- graph_from_adj_list(
  adj_list,
  mode="out",
)
V(g)$label <- as.vector(map)

trailheads <- which(map == 0)
peaks <- which(map == 9)

paths <- distances( # rows = source, cols = dest
  g,
  v=trailheads,
  to=peaks,
  mode="out" # directed
)
# how many peaks can each trailhead reach?
print(sum(rowSums(!is.infinite(paths))))

# how many ways to reach a peak from each trailhead?
print(sum(sapply(
  lapply(trailheads, all_simple_paths, graph=g, to=peaks, mode="out"),
  length
)))

# ----- cool picture -----
plot(
  g,
  layout=layout_on_grid(g), width=dim(map)[2],
  vertex.size=ifelse(length(g)<100, 10, 2) * ifelse(V(g)$label %in% c(0, 9), 1.5, 1),
  vertex.color=c(`0`="lightgreen", `9`="salmon")[as.character(V(g)$label)],
  vertex.label=NA,
  edge.arrow.size=ifelse(length(g) < 100, 0.5, .1)
)
ggplot(
  unique(df[, .(row=from.row, col=from.col, height=from.height)]),
  aes(x=col, y=row, z=height)
) +
  #geom_contour_filled(show.legend=FALSE) +
  geom_contour(col='black') +
  matrix_style_base
