rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

initial_values <- function (lines) {
  # cache <- c()
  cache <- new.env()
  first <- lines[1:(which(lines=="") - 1)]
  for (iv in strsplit(first, ":")) {
    # cache[iv[1]] <- as.integer(iv[2]) > 0
    cache[[iv[1]]] <- as.integer(iv[2]) > 0
  }
  cache
}
parse_instructions <- function(lines) {
  o <- do.call(rbind, stri_match_all(regex="^(.+?) (AND|XOR|OR) (.+?) -> (.+?)$", lines))[, -1]
  colnames(o) <- c('in1', 'op', 'in2', 'out')
  o
}
evaluate_instruction <- function (instruction, state) {
  inputs <- unlist(mget(instruction[, c('in1', 'in2')], state, ifnotfound=NA), use.names=F)
  if (!anyNA(inputs)) {
    op <- instruction[, 'op']
    state[[instruction[, 'out']]] <- .evaluate_instruction(op, inputs[1], inputs[2])
    return(TRUE)
  }
  return(FALSE)
}
.evaluate_instruction <- function(op, in1, in2) {
  if (op == "AND")
    return(in1 & in2)
  else if (op == "OR")
    return(in1 | in2)
  else if (op == "XOR")
    return(xor(in1, in2)) # not piecewise
  else
    stop("UNKNOWN OPERATION")
}

get_outputs <- function (state) {
  zs <- grep('^z', ls(state), value=TRUE)
  nz <- length(zs)
  sum(
    2 ^ (seq_len(nz) - 1) * unlist(mget(sort(zs), state))
  )
}

evaluate_all_instructions <- function (instructions, state, maxn=10000) {
  # by inspection it seems to be max-ctr of 9851? 
  i <- 1
  n <- nrow(instructions)
  instructions_left <- rep(TRUE, n)
  ctr <- 0
  while (any(instructions_left) && ctr < maxn) {
    instruction <- instructions[i, , drop=FALSE]
    instructions_left[i] <- !evaluate_instruction(instruction, state)
    i <- (i %% n ) + 1
    ctr <- ctr + 1
  }
  if (any(instructions_left)) return(NA)
}

lines <- input
state <- initial_values(lines)
instructions <- parse_instructions(tail(lines, -(which(lines == ""))))
evaluate_all_instructions(instructions, state)
print(get_outputs(state))
# has_state <- rep(T, length(ls(state)))
# names(has_state) <- ls(state)


# ---- part2 ---
# I have 312 wires LEL
# we have a 46-bit int
# output wires are
# R I P

binvec2int <- function (binvec) {
  # left-most is 1 right-most is 2^64
  sum(
    2 ^ (seq_along(binvec) - 1) * binvec
  )
}
int2binvec <- function (x) {
  # intToBits. but can be a double
  # boy I hope that's right
  floor( (x %% (2^(1:64))) / (2^(0:63)) )
}
add_numbers <- function(x, y, instructions, verbose=FALSE) {
  # x,y have 45 bits and z has 46
  # doesn't work for 64bits and we need a 46-bit...
  xb <- int2binvec(x)
  yb <- int2binvec(y)
  # little-endian
  state <- new.env()
  for (i in 0:44) { # TODO: only support up to 32
    xval <- ifelse(i > (length(xb) - 1), 0, xb[i + 1]) > 0
    yval <- ifelse(i > (length(yb) - 1), 0, yb[i + 1]) > 0
    assign(sprintf("x%02i", i), xval, envir=state)
    assign(sprintf("y%02i", i), yval, envir=state)
  }
  evaluate_all_instructions(instructions, state)
  out <- get_outputs(state)
  correct <- x + y == out
  if (!correct && verbose) {
    message(sprintf("x       : %s", collapse(xb, sep="")))
    message(sprintf("y       : %s", collapse(yb, sep="")))
    message(sprintf("z [calc]: %s", collapse(int2binvec(out), sep="")))
    message(sprintf("z  [act]: %s", collapse(int2binvec(x + y), sep="")))
    message(sprintf("%.0f + %0.f = %.0f (%s)", x, y, out, ifelse(out == x + y, "CORRECT", "WRONG")))
  }
  return(out)
}

lines <- input
instructions <- parse_instructions(tail(lines, -(which(lines == ""))))

add_numbers(binvec2int(rep(1, 45)), binvec2int(rep(1, 45)), instructions)
# OK when I feed in heaps of 1111111 (the max number)
#    the 30th and 34th bit are not giving the correct answer

x <- rep(0, 45)
x[30] <- 1
add_numbers(binvec2int(x), 0, instructions)

add_numbers(binvec2int(rep(T, 32)), binvec2int(rep(T, 32)), instructions)

# so it's one of the gates in the chain for the z30

library(igraph)
graph_from_instructions <- function (instructions) {
  wire_names <- sort(unique(as.vector(instructions[, c("in1", "in2", "out")])))
  node_labels <- c(
    instructions[, "op"],
    # chuck a node on each input and also a node on each output
    grep("^x", wire_names, value=TRUE),
    grep("^y", wire_names, value=TRUE),
    grep("^z", wire_names, value=TRUE)
  )
  g <- make_empty_graph(n=length(node_labels))
  V(g)$label <- node_labels
  
  find_tail_of <- function (wire, instructions) {
    nodei <- which(instructions[, "out"] == wire, arr.ind=TRUE)
    stopifnot(length(nodei) == 1) 
    return(nodei)
  }

  edges <- cbind(from=integer(), to=integer())
  edge_names <- c()
  for (i in 1:nrow(instructions)) {
    head_node <- i
    for (wire in instructions[i, c("in1", "in2")]) {
      edge_names <- c(edge_names, wire)
      if (grepl("^[xyz]", wire)) {
        tail_node <- which(node_labels == wire)
      } else {
        tail_node <- find_tail_of(wire, instructions)
      }
      stopifnot(!is.na(tail_node))
      edges <- rbind(edges, c(from=tail_node, to=head_node))
    }
  }
  # add all zs
  .tmp <- grep("^z", instructions[, "out"])
  edges <- rbind(
    edges,
    cbind(
      from=.tmp,
      to=match(instructions[.tmp, "out"], node_labels)
    )
  )
  edge_names <- c(edge_names, instructions[.tmp, "out"])
  
  g <- add_edges(g, as.vector(t(edges)), directed=TRUE)
  E(g)$label <- edge_names
  g
}
# LOL
g <- graph_from_instructions(instructions)

plot(
  g, edge.arrow.size=0,
  # vertex.size=5, vertex.size2=2, vertex.shape='rectangle',
  vertex.size=1, vertex.shape='square', vertex.label=NA,
  vertex.color=c(c(AND='blue', OR='green', XOR='red')[instructions[, "op"]], rep('grey', 45 + 45 + 46)),
  edge.label.color=ifelse(E(g)$label == "jkb", "red", "black"),
  layout=layout_as_tree(g)
)

stupid <- function (from_node) {
  affected_nodes <- neighbourhood(g, order=100, nodes=from_node)[[1]]
  plot(
    g, edge.arrow.size=0,
    # vertex.size=5, vertex.size2=2, vertex.shape='rectangle',
    vertex.size=1, vertex.shape='square', vertex.label=NA,
    vertex.color=c(c(AND='blue', OR='green', XOR='red')[instructions[, "op"]], rep('grey', 45 + 45 + 46)),
    edge.label.color=ifelse(E(g)$label == "jkb", "red", "black"),
    layout=layout_as_tree(g)
  )
  
}

# there is an OR in the wrong spot and a greyd ot in the wrong spot
# jkb is somehow wrong

# degree(g)

# I think they should be coming from xor
# z12   z29   z37   z45 
# "OR" "AND" "AND"  "OR" 

xs <- V(g)[grepl("^x", label)]
ys <- V(g)[grepl("^y", label)]
xors <- V(g)[label == "XOR"]
zs <- V(g)[grepl("^z", label)]
dd <- degree(g)
dd[xs]
dd[ys]
dd[zs]
dd[xors]

# -----------------
powers_wrong <- NULL
for (pow in 0:44) {
  y <- 2^pow
  o <- add_numbers(0, y, instructions)
  if (o != y)
    powers_wrong <- c(powers_wrong, pow)
}
# x       : 0000000000000000000000000000000000000000000000000000000000000000
# y       : 0000000000001000000000000000000000000000000000000000000000000000
# z [calc]: 0000000000000100000000000000000000000000000000000000000000000000
# z  [act]: 0000000000001000000000000000000000000000000000000000000000000000
# 0 + 4096 = 8192 (WRONG)
# x       : 0000000000000000000000000000000000000000000000000000000000000000
# y       : 0000000000000000000000000000010000000000000000000000000000000000
# z [calc]: 0000000000000000000000000000001000000000000000000000000000000000
# z  [act]: 0000000000000000000000000000010000000000000000000000000000000000
# 0 + 536870912 = 1073741824 (WRONG)
# x       : 0000000000000000000000000000000000000000000000000000000000000000
# y       : 0000000000000000000000000000000001000000000000000000000000000000
# z [calc]: 0000000000000000000000000000000000100000000000000000000000000000
# z  [act]: 0000000000000000000000000000000001000000000000000000000000000000
# 0 + 8589934592 = 17179869184 (WRONG)
# x       : 0000000000000000000000000000000000000000000000000000000000000000
# y       : 0000000000000000000000000000000000000100000000000000000000000000
# z [calc]: 0000000000000000000000000000000000000010000000000000000000000000
# z  [act]: 0000000000000000000000000000000000000100000000000000000000000000
# 0 + 137438953472 = 274877906944 (WRONG)
# > powers_wrong
# [1] 12 29 33 37

# looks like they should mostly be XORs but are not

# do any zs have an output?
precedent_zs <- sapply(adjacent_vertices(g, zs, mode="all"), function (nbh) nbh$label)
names(precedent_zs) <- zs$label
# I had noted that the 30th and 34th bit didn't look right
precedent_zs[precedent_zs != "XOR"]
# z12   z29   z37   z45 
# "OR" "AND" "AND"  "OR" 

# OK so this plus the previous suggest that
# * swap z12 with an XOR
# * swap z29 with an XOR
# * swap z37 with an XOR
# * something in the 33 chain is incorrect
# let's start with z12... / 2^12
# OK. 2^12 has the 13th bit as a 1
# here are the next 2 in line
n1 <- V(g)[neighbors(g, V(g)[label == "x13"])] # XOR, AND
n2 <- V(g)[neighbors(g, n1)]
n3 <- V(g)[neighbors(g, n2)]

stupid <- function (zn) {
  g2 <- make_neighborhood_graph(g, order=100, nodes=V(g)[label == zn], mode="in")[[1]]
  plot(
       g2,
       vertex.color=ifelse(grepl("^x", V(g2)$label), "grey", ifelse(grepl("^y", V(g2)$label), "lightblue", ifelse(grepl("^z", V(g2)$label), "lightgreen", "yellow"))),
        layout=layout_as_tree(g2)
  )
}
# stupid("z03")

# OK let's swap the Z12 edge to come out of every other Xor and see if that works LOL
swap <- function(instructions, out1, out2) {
  instructions2 <- instructions
  i1 <- which(instructions2[, "out"] == out1)
  i2 <- which(instructions2[, "out"] == out2)
  instructions2[i1, "out"] <- out2
  instructions2[i2, "out"] <-  out1
  instructions2
}

# these come out of an XOR and I know z12 should come out of an XOR
zlabs <- function (x) { sprintf("z%02i", x) }
find_xors_to_swap_with <- function (pow, instructions, g) {
  # find all other 'out' to swap with that are not in the network of a Z with power less than me
  # and that are out of a 'xor'
  try_swapping_instructions <- which(instructions[, "op"] == "XOR")
  # ugh it's instructions not nodes/edges that we want to swap. we only care if it's the OUTPUT of an XOR instruction
  not_these_nodes <- unique(unlist(neighborhood(g, order=100, nodes=V(g)[label %in% zlabs(seq_len(pow) - 1)], mode="in")))  # skip the networks of working Zs (which are up to `pow`) -> assumption
  try_swapping_instructions <- setdiff(try_swapping_instructions, not_these_nodes)
  zedge <- zlabs(pow)
  successful <- NULL
  pb <- txtProgressBar(max=length(try_swapping_instructions), style=3)
  # ASSUMPTION: it must be one that REDUCES The # of wrong powers
  for (i in seq_along(try_swapping_instructions)) {
    xor_edge <- instructions[try_swapping_instructions[i], "out"]
    if (xor_edge != zedge) {
      o <- add_numbers(0, 2^pow, swap(instructions, zedge, xor_edge))
      if (!is.na(o) && o == 2^pow) {
        successful <- c(successful, xor_edge)
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(successful)
}

find_wrong_powers <- function (instructions, from=0) {
  powers_wrong <- NULL
  for (pow in from:44) {
    y <- 2^pow
    o <- add_numbers(0, y, instructions)
    if (o != y)
      powers_wrong <- c(powers_wrong, pow)
  }
  powers_wrong
}

.precedents <- function(g, powers) {
  unique(unlist(neighborhood(g, order=100, nodes=V(g)[label %in% zlabs(powers)], mode='in')))
}
swap_within_stack <- function (pow, instructions, g) {
  # find something in the z`pow` network and swap within
  # bah - this doesn't work if the node you needed was not within lol.
  
  # things in the `pow` network not in the `pow-1` network and exclude the x/y/z nodes
  swap1 <- intersect(
    1:nrow(instructions), # exclude the x/y/zs
    # contributes to 2^pow but not any lower powers
    setdiff( .precedents(g, pow), .precedents(g, pow - 1) )
  )
  
  
  swaps <- t(combn(swap1, 2))
  
  pb <- txtProgressBar(max=nrow(swaps), style=3)
  successful <- NULL
  # ASSUMPTION: it must be one that REDUCES The # of wrong powers
  for (i in seq_len(nrow(swaps))) {
    edge1 <- instructions[swaps[i, 1], "out"]
    edge2 <- instructions[swaps[i, 2], "out"]
    o <- add_numbers(0, 2^pow, swap(instructions, edge1, edge2))
    if (!is.na(o) && o == 2^pow) {
      successful <- rbind(successful, swaps[i, ])
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(successful)
}


# ---- BLAH
# > powers_wrong
# [1] 12 29 33 37
# do any zs have an output?
precedent_zs <- sapply(adjacent_vertices(g, zs, mode="all"), function (nbh) nbh$label)
names(precedent_zs) <- zs$label
# I had noted that the 30th and 34th bit didn't look right
precedent_zs[precedent_zs != "XOR"]
# z12   z29   z37   z45 
# "OR" "AND" "AND"  "OR" 


# 12 and 29 are definitely wrong. then there's something wrong in 33 but it's
#  not the z-edge. the 37-z-edge is also wrong.
# possible <- find_xors_to_swap_with(12, instructions, g)
# # only `fgc` makes things better, everything else makes things worse
# print(x <- setNames(lapply(possible, function (edge) { 
#   find_wrong_powers(swap(instructions, zlabs(12), edge), from=13)
# }), possible))
instructions1 <- swap(instructions, 'z12', 'fgc')
# g <- graph_from_instructions(instructions1)


# possible <- find_xors_to_swap_with(29, instructions1, g)
# print(x <- setNames(lapply(possible, function (edge) {
#   find_wrong_powers(swap(instructions1, zlabs(29), edge), from=30)
# }), possible))
instructions2 <- swap(instructions1, 'z29', 'mtj')
# g <- graph_from_instructions(instructions2)

# possible <- find_xors_to_swap_with(37, instructions2, g)
# print(x <- setNames(lapply(possible, function (edge) {
#   find_wrong_powers(swap(instructions2, zlabs(37), edge), from=38)
# }), possible))
instructions3 <- swap(instructions2, 'z37', 'dtv')
g <- graph_from_instructions(instructions3)
# find_wrong_powers(instructions3) # just 33

# need 32 & 98 in there
candidates <- V(g)[ setdiff(.precedents(g,33), .precedents(g, 1:32)) ]
E(g)[.from(candidates)]$label

g33 <- induced_subgraph(g, setdiff(.precedents(g,33), .precedents(g, 1:32)))
plot(g33, layout=layout_on_grid)

instructions4 <- swap(instructions3, 'vvm', 'dgr')
find_wrong_powers(instructions4)
# welp boo.
# possible <- swap_within_stack(34, instructions3, g)
# add_numbers(2^33, 0, instructions3, verbose=TRUE)

