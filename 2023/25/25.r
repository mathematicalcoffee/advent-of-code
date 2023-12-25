rm(list = ls())
source("../handy.R")
library(stringi)
library(igraph)

input_file <- "input.txt"
lines <- stri_split(readLines(input_file), regex = "[ :]+")
g <- make_graph(
  unlist(
    lapply(
      lines,
      function(x) as.vector(rbind(x[1], x[-1]))
    )
  ),
  directed = FALSE
)
E(g)$label <- apply(ends(g, E(g)), 1, paste, collapse = "-")
E(g)$name <- apply(ends(g, E(g)), 1, paste, collapse = "-")
nv <- length(V(g))
png(filename = "day25.png", width = 1000, height = 1000)
plot(
  g,
  vertex.color = "grey",
  vertex.size = 3,
  vertex.frame.color = NA,
  vertex.label = NA
)
dev.off()
# Very lazy, proof by picture (I had intended to do this properly but meh
#  it's Christmas, and if the default plot algorithm happens to lay things out
#  so that they're obvious, well then that only teaches the importance of
#  visualising your data before doing anything, statistician's creed lol)
g2 <- delete_edges(g, c("htb-bbg", "htj-pcc", "dlk-pjj"))
print(prod(components(g2)$csize))
