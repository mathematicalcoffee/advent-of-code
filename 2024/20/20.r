rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_input <- function (lines) {
  
}

library(igraph)
#map <- lines2matrix(ex[[1]])
map <- lines2matrix(input)

# --- step 1: find the shortest path/time --- #
n <- build_nodes(map)
e <- build_orthogonal_edges(n, "#")
g <- build_graph(n, e, directed=FALSE)
plot_graph_on_grid(g)

S <- V(g)[V(g)$symbol == 'S']
E <- V(g)[V(g)$symbol == 'E']
shortest <- shortest_paths(g, from=S, to=E)
spath <- shortest$vpath[[1]]
shortest_time <- length(spath) - 1
print(paste("shortest path is", shortest_time, "picoseconds"))

# ------- part 1 original ----------
# ORIGINAL: loop through walls and kill them and try the shortest path.
# v slow.
if (FALSE) {
  # need nodes with '#" such that 2 neighbours are on the shortest path
  offs <- get_coords_reachable_in_n_steps(1)
  next_to_path <- n[
    n[
      J(shortest$vpath[[1]]),
      .(row = offs[,1] + row,
        col = offs[, 2] + col),
      by=.(source_row=row, source_col=col, source_node_id=node_id)
    ],
    on=.(row, col)
  ]
  candidates <- next_to_path[, .N, by=.(node_id, symbol)][N > 1 & symbol == '#']
  # 44 cheats
  times <- rep(Inf, nrow(candidates))
  for (i in 1:nrow(candidates)) {
    cheat_node <- n[J(candidates[i])]
    cheat_edges <- next_to_path[node_id == cheat_node$node_id]
    
    g2 <- add_edges(g, as.vector(cheat_edges[, rbind(source_node_id, node_id)]))
    S <- V(g2)[V(g2)$symbol == 'S']
    E <- V(g2)[V(g2)$symbol == 'E']
    d <- distances(g2, v=S, to=E)
    times[i] <- min(d)
  }
  time_saved <- shortest_time - times
  print(table(time_saved))
  
  print(sum(times <= shortest_time - 100))
}

# ---------- PART 2 ---------- #
# works for part 1 too
MAX_CHEAT_LEN <- 20

# To save time on a cheat, you need to cheat from and to the shortest path.
# Since a cheat is identified by its (start, end), we only need to search for
#  start/ends along the shortest path.
# For a (start, end) pair, the time taken to cheat, is
#
# time from S to `start` +
# taxi-distance between start and end +    (which can be at most MAX_CHEAT_LEN)
# time from `end` to E
#
# The time from S to `start` is the # of steps it is along the shortest path.
# (vertex i along the shortest path)
time_to_start <- 0:shortest_time
# The time from `end` to E is the # of steps until we reach the end (on the
#  shortest path). (vertex i along the shortest path)
time_to_end <- shortest_time:0

# distance from each thing on the path to the other
v_n <- n[J(spath)]
v_n[, i := .I]
final_times <- NULL
for (si in 1:(nrow(v_n) - 1)) {
  start_node <- v_n[si]
  end_vi <- v_n[
    i > si & (abs(row - start_node$row) + abs(col - start_node$col)) <= MAX_CHEAT_LEN,
    which=T
  ]
  # taxi-distance between start and end +    (which can be at most MAX_CHEAT_LEN)
  cheat_len <- v_n[end_vi, abs(row - start_node$row) + abs(col - start_node$col)]
  
  final_times <-c(final_times, (si - 1) + cheat_len + (shortest_time - end_vi + 1))
}
time_saved <- shortest_time - final_times
# print(table(time_saved))
print(sum(time_saved >= 100))
