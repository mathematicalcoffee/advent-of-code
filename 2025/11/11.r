rm(list=ls())
library(data.table)
library(testthat)
library(igraph)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_inputs <- function (input_str, special=c('you', 'svr', 'out')) {
  edgelist <- do.call(rbind,
                      lapply(
                        stri_split(input_str, regex=":"),
                        function (bits) {
                          cbind(bits[1], stri_split(stri_trim(bits[2]), regex=" ")[[1]])
                        }
                      ))
  g <- graph_from_edgelist(edgelist)
  plot(
    g,
    vertex.size=1,
    edge.arrow.size=0.1,
    vertex.label.color=ifelse(V(g)$name %in% special, "blue", NA),
    vertex.color=ifelse(V(g)$name %in% special, "cyan", "gold"),
    vertex.label=ifelse(V(g)$name %in% c(special), V(g)$name, NA)
  )
  g
}

g <- parse_inputs(ex)
length(all_simple_paths(g, from="you", to="out"))

g <- parse_inputs(input)
length(all_simple_paths(g, from="you", to="out"))

ex2 <- stri_split(regex="\n", 'svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out')[[1]]

# ---------------------------------------------- #
# The graph looks like there are about 5 'bottlenecks': clusters of vertices
#  such that every path from svr must pass through at least one of the
#  vertices in each cluster.
# I thought we could produce a simplified graph of just the src, dest, mandatory
#  points, and bottleneck nodes, such that you join these nodes if you can get
#  from one to the other on the original graph without passing through any of the
#  others, and you weight the edge by the number of ways to get from one to
#  the other.
# By including the mandatory nodes in the simplified graph you force all paths to
#  go through.
# Then on this simplified graph, you find all ways to get from start to finish,
#  multiply along the edges, and add between the paths.
# But for some reason this produces way too many paths :(
# I have verified that:
# A) my clusters of bottleneck nodes are indeed bottlenecks: removing a cluster
#    disconnects the graph
# B) there is no way to get between any node of a cluster to any other node of
#    the same cluster: so no redundancy/double-counting
# But I must have made a mistake of understanding and still be double-counting
#  as my answers are too high.

crit <- c("dac", "fft")
special <- c(crit, 'svr', 'out')
g <- parse_inputs(input, special=special)
degs <- sort(degree(g), decreasing=TRUE)
# candidates (bottlenecks + the mandatories)
bottleneck_node_names <- unique(c(special, names(degs[degs > 10])))

dist <- distances(g, v=bottleneck_node_names, to=bottleneck_node_names, mode="out")
# there are 5 bottlenecks
# guesses based on sort(dist['svr', ])
bottleneck_membership <- sort(dist['svr', ]) #AHA they are all at precisely the same distance
# manual massaging..these ones are not bottlnecks but are of high degree...
bottleneck_membership <- bottleneck_membership[bottleneck_membership != 1]
bottleneck_nums <- unique(bottleneck_membership)

bb <- lapply(bottleneck_nums, function (i) names(which(bottleneck_membership == i)))
# I might have too many of the bottleneck points - reduce them down until they
#  are a minimal set
igs <- lapply(bb, function (nodes) { induced_subgraph(g, nodes) })
keepers <- unlist(sapply(igs, function (ig) names(which(degree(ig, mode="in") == 0))))
bottleneck_membership <- bottleneck_membership[keepers]
bottleneck_node_names <- names(bottleneck_membership)
# make tripply sure none of my bottleneck nodes can get to any within the same tier
stopifnot(all(
  sapply(bb, function (nodes) {
  all(unique(as.vector(distances(g, v=nodes, to=nodes, mode="out"))) %in% c(0, "Inf"))
})
))

# verify that I have at least a superset of the critical points
# removing each cluster would disconnect the graph
stopifnot(all(sapply(
  setdiff(bottleneck_nums, bottleneck_membership[special]),
  function (dest_bottleneck_i) {
    is_connected(g - names(which(bottleneck_membership == dest_bottleneck_i)))
  }
) == FALSE))

plot(
    g,
    vertex.size=1,
    edge.arrow.size=0.1,
    vertex.label.color=ifelse(V(g)$name %in% special, "blue", ifelse(V(g)$name %in% bottleneck_node_names, "darkgreen", NA)),
    vertex.color=ifelse(V(g)$name %in% special, "cyan", ifelse(V(g)$name %in% bottleneck_node_names, "green", "gold")),
    vertex.label=ifelse(V(g)$name %in% c(special), V(g)$name, NA),
    layout=(crd <- layout_nicely(g))
  )

conn <- matrix(0, nrow=length(bottleneck_membership), ncol=length(bottleneck_membership))
rownames(conn) <- colnames(conn) <- names(bottleneck_membership)
src_nodes <- 'svr'
for (dest_bottleneck_i in tail(bottleneck_nums, -1)) {
  dest_nodes <- names(which(bottleneck_membership == dest_bottleneck_i))
  
  for (src_node in src_nodes) {
    for (dest_node in dest_nodes) {
      # it's way faster if you remove all the subsequent bottlenecks of the graph to disconnect it
      #  because I know that subsequent bottlenecks never come back, I guess the alg doesn't
      vertices_to_keep <- c(src_node, dest_node)
      ps <- all_simple_paths(
        g - setdiff(names(bottleneck_membership), vertices_to_keep),
        src_node,
        dest_node
      )
      conn[src_node, dest_node] <- length(ps)
    }
  }
  
  src_nodes <- dest_nodes
  src_bottleneck_i <- dest_bottleneck_i
}

# simplified graph
g2 <- graph_from_adjacency_matrix(
  conn,
  mode='directed',
  weighted=TRUE
)
plot(
  g2,
  vertex.size=3,
  edge.arrow.size=0.1,
  edge.color="black",
  vertex.label=NA,
  vertex.label.color=ifelse(V(g2)$name %in% special, "blue", NA),
  vertex.label.cex=ifelse(V(g2)$name %in% special, 2, 1),
  #vertex.color=pal_brewer(type="seq")(length(bottleneck_nums))[c(1, cumsum(diff(bottleneck_membership) > 0) + 1)],
  vertex.color=ifelse(V(g2)$name %in% special, "cyan", ifelse(V(g2)$name %in% bottleneck_node_names, "green", "gold")),
  layout=crd[match(names(V(g2)), names(V(g))), ],
  add=TRUE
)
# another cool one
plot(
  g2,
  vertex.size=3,
  edge.arrow.size=0.2,
  edge.arrow.width=1,
  vertex.shape="none",
  vertex.label.color=ifelse(V(g2)$name %in% special, "blue", NA),
  vertex.label.cex=ifelse(V(g2)$name %in% special, 2, 1),
  edge.width=scales::rescale(sqrt(E(g2)$weight), to=c(1, 5)),
  #edge.label=E(g2)$weight,
  #edge.label.color="#444444",
  layout=layout_as_tree
)


ps <- all_simple_paths(g2, from='svr', to='out')
numways <- sapply(
  ps,
  function (p) {
    prod(E(g2)[p]$weight)
  }
)
sum(numways)
# too HIGH
# 649602546020736000
# now removed vertices that are 'redundant'
# too HIGH still
# 619803347872800000

# ------------------------------------------------------------------------------- #
# WHAT THE FUCK - I just remembered that I wrote this function in day 4.
# and ofcourse it works straight out of the box. (it isn't the fastest but
#  it's not too bad).
n_paths <- function(g, FROM, TO) {
  # number of ways to get from FROM to TO in a directed acyclic graph
  # the idea is that the number of ways to the next node is the sum of
  #  the ways to get to all of its parents
  V(g)$nways <- 0
 
  V(g)[name == FROM]$nways <- 1 # seed.
  children <- V(g)[.outnei(V(g)[name == FROM])]
  
  while (length(children)) {
    for (child.v in children) {
      parent.vs <- V(g)[.innei(child.v)]
      V(g)[child.v]$nways <- sum(V(g)[parent.vs]$nways)
    }
    children <- V(g)[.outnei(children)]
  }
  sum(V(g)[name == TO]$nways)
}
n_paths(g, 'svr', 'fft')*n_paths(g, 'fft', 'dac')*n_paths(g, 'dac', 'out')
390108778818526

n_paths(g, 'svr', 'fft') # 3011
n_paths(g, 'fft', 'dac') # 5983522
n_paths(g, 'dac', 'out') # 21653

# my simplified graph is just wrong somehow.
n_paths2 <- function (g, FROM, TO) {
  ps <- all_simple_paths(g2, from=FROM, to=TO)
  sum(sapply(ps, function (p) { prod(E(g2, path=p)$weight) } ))
}

# these are wrong!!
n_paths2(g2, 'svr', 'fft') # 1043104
n_paths2(g2, 'fft', 'dac') # 175084000
n_paths2(g2, 'dac', 'out') # 543000

# OH MY GOD I was indexing my edges with a vertex sequence and it let me do it
#  and I was extracting the wrong edges
# it was right all along!!!!
# AHHHHHHHH
# (but I still wish I'd remembered about day7 because that would have been
#  out of the box)
n_paths2(g2, 'svr', 'out')
