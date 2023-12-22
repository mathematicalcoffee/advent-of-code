rm(list = ls())
source("../handy.R")
library(ggplot2)
library(stringi)
library(igraph)

# Make a graph (undirected), find all nodes reached by path 6
# I think whatever way we do it it's slow, you are guaranteed that the
#  number of nodes visited increases each time (even if you just restrict
#  to boundary nodes)

# make sure n is small
tile <- function(input, n) {
  out <- do.call(cbind, replicate(n, input, simplify=FALSE))
  out <- do.call(rbind, replicate(n, out, simplify=FALSE))
 
  # remove all the S but the middle one 
  out[out == "S"] <- "."
  m <- ceiling(dim(out) / 2)
  out[m, m] <- "S"
  return(out)
}

make_graph <- function (input) {
  g <- make_lattice(
    dimvector=dim(input),
    directed=FALSE
  )
  V(g)$label <- as.vector(V(g))
  ii <- which(input == "#")
  # delete edges connecting to the '#' (don't delete the vertices because it
  #  makes the IDs not match the coordinates any more and it's confusing)
  g <- delete_edges(g, unlist(incident_edges(g, ii)))
  V(g)[which(input == "#")]$label <- ""
  V(g)[which(input == "S")]$label <- "S"
  return(g)
}

# Idea:
# * store the vertices visited in `n` steps as a binary vector (T/F) rather than
#   and every-increasing set
# * but when taking additional steps, only calculate the /new/ nodes reached and
#   only from nodes we have not expanded from before 
n_nodes_visited <- function (start_i, g, n_steps, return_nodes=FALSE) {
  # returns the x/y of steps/n nodes. if return_nodes=TRUE instead returns the
  #  node IDs visited on the nth step
  n_nodes <- length(V(g))
  visited_this_iter <- rep(F, n_nodes)
  all_visited <- visited_this_iter[]
  visited_this_iter[start_i] <- TRUE
  odds <- (seq_len(n_nodes) %% 2 == 1)
  evens <- !odds
  nodes <- which(visited_this_iter)
  n_visited <- rep(-1, n_steps)
  
  for (i in 1:n_steps) {
    visited_this_iter[] <- FALSE
    for (node in nodes) {
      v <- neighbors(g, node)
      visited_this_iter[v] <- TRUE
    }
    i_this_iter <- which(visited_this_iter)
    nodes <- setdiff(i_this_iter, which(all_visited))
    all_visited[i_this_iter] <- TRUE
   
    # it alternates between odds and evens 
    # yes this is the right way around
    if (i %% 2 == 1) {
      n_visited[i] <- sum(all_visited[evens]) # inclusive of inner
    } else {
      n_visited[i] <- sum(all_visited[odds]) # inclusive of inner
    }
  }
  if (return_nodes) {
    if (n_steps %% 2 == 1) {
      return(intersect(which(all_visited), which(evens)))
    } else {
      return(intersect(which(all_visited), which(odds)))
    }
  } else {
    return(n_visited)
  }
} 

# ---------- part 1 ----------- #

input_file <- "input.txt"
input <- read_matrix(input_file)

g <- make_graph(input)
p1 <- n_nodes_visited(which(input == "S"), g, 64, return_nodes = TRUE)
print(length(p1))

# obligatory
# png("day22-graph.png", width=1000, height=1000)
# cute!
plot(
  g,
  layout=layout_on_grid(g, width=ncol(input)),
  # vertex.color=ifelse(input == "S", "red", ifelse(input == ".", "grey", NA)),
  vertex.color=ifelse(seq_along(V(g)) %in% p1, "red", ifelse(input == "#", NA, "grey")),
  vertex.frame.color=NA,
  vertex.label=NA,
  vertex.size=ifelse(input_file=="input-small.txt", 10, 1)
)
# dev.off()



# ---------- part 2 ----------- #
# Look for patterns.
# damn, that was slow
if (FALSE) {
  input <- read_matrix(input_file)
  input <- tile(input, 5)
  g <- make_graph(input)
  
  
  n <- floor(dim(input)[1]/2)
  p2 <- n_nodes_visited(which(input == "S"), g, n)
  #saveRDS(p2, file="visited.rds")
  # p2 <- n_nodes_visited(which(input == "S"), g, n, return_nodes=TRUE)
  # saveRDS(p2, file="visited-nodes.rds")

  # obligatory - damn, if I'd seen this earlier I think I would have done this
  #  "properly"
  png(filename="map-part-2.png", width=131*5*2, height=131*5*2)
  par(mar=c(0,0,0,0))
  map <- ifelse(input == "#", 0, 1)
  map[p2] <- 2
  image(map, col=c("black", "white", "red"), xaxt="n", yaxt="n", useRaster=TRUE)
  dev.off()
 
}

y <- readRDS("visited.rds")
n <- length(y)
x <- 1:n
PART2 <- list(x=26501365)

# HMMMMMMMM
plot(x, y, xlab="nsteps", ylab="nvisited")

# let's try to fit a quadratic
m <- lm(y ~ poly(x, 2))
plot(resid(m))  # residuals too high, don't like it

# Theory 2 - only odds? (or only evens but we have an odd number of steps)
odds <- seq(1, length(y), by=2)
yo <- y[odds]
xo <- x[odds]
mo <- lm(yo ~ poly(xo, 2))
plot(resid(mo))
summary(mo)
# again - residuals too funny. BUT IT's CYCLIC SOMEHOW

# Theory 3 - to the EDGES
xx <- c(65, 65 + 131, 65 + 131 * 2)
yy <- y[xx]
m_wtf <- lm(yy ~ poly(xx, 2))
yy - predict(m_wtf, newdata=list(xx=xx)) # I mean, 3 points, polynomial, ofc.
print(predict(m_wtf, newdata=list(xx=26501365)), digits=20)
# 618261433219146.375 

# why the fuck does this work lol, that's disgusting

# ----------------------------------------
xy <- readRDS("visited-nodes.rds")
dims <- c(131, 131) * 5
xy <- i2ij(xy, dims)
# partition into tiles

xy <- cbind(
  xy,
  tile_i=((xy[, 1] - 1) %/% 131) + 1,
  tile_j=((xy[, 2] - 1) %/% 131) + 1
)
