rm(list = ls())
library(stringi)
library(igraph)
source("../handy.R")
LEFT <- "<"
RIGHT <- ">"
DOWN <- "v"
UP <- "^"
SLOPES <- c(LEFT, RIGHT, DOWN, UP)


# make a graph, find all simple paths, get the one with max length.
# the v < > are just directed

input_file <- "input.txt"
input <- read_matrix(input_file)

dims <- dim(input)
g <- make_lattice(dimvector = dims, directed = FALSE)
coords <- layout_on_grid(g, width = dims[2])
# oopsies, Ithink we're transposed in the ij2i impl

start_i <- which(row(input) == 1 & input == ".")
end_i <- which(row(input) == dims[1] & input == ".")
V(g)[which(input == "#")]$label <- ""
V(g)[start_i]$label <- "S"
V(g)[end_i]$label <- "E"

slope_i <- which(input %in% SLOPES)
V(g)[slope_i]$label <- input[slope_i]

# remove the edges for the #s (and prob the vertices too)
# note - just remove the edges for now so the indices are the same
ii <- which(input == "#")
g <- delete_edges(g, unlist(incident_edges(g, ii)))

# set weights
gd <- as.directed(g, mode = "mutual")

# deal with slopes
for (i in slope_i) {
  nei <- NULL
  slope <- input[i]
  ij <- i2ij(i, dims)
  # col-major
  if (slope == LEFT) {
    nei <- i - dims[1]
  } else if (slope == RIGHT) {
    nei <- i + dims[1]
  } else if (slope == DOWN) {
    nei <- i + 1
  } else if (slope == UP) {
    nei <- i - 1
  } else {
    stop("woops")
  }
  # kill the edges from nei to me
  E(gd)[.from(nei) & .to(i)]
  gd <- delete_edges(gd, E(gd)[.from(nei) & .to(i)])
}

ps <- all_simple_paths(gd, from = start_i, to = end_i)
lengths <- sapply(ps, length)
print(max(lengths) - 1) # oopsies, this returns vertices not edges

longest.path <- ps[[which.max(lengths)]]
longest.path.edges <- E(gd, path = longest.path)

# obligatory picture. coords got a bit transposed or something
png(filename = "day23-part1.png", width = 1000, height = 1000)
coords <- coords[, 2:1]
coords[, 2] <- -coords[, 2]
plot(
  gd,
  layout = coords,
  vertex.size = 1,
  vertex.color = ifelse(input == "#", NA,
                 ifelse(V(g) %in% longest.path, "red", "grey")),
  vertex.frame.color = NA,
  edge.arrow.size = .1,
  edge.color = ifelse(E(gd) %in% longest.path.edges, "red", "grey")
)
dev.off()
