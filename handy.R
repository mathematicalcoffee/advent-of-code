library(xml2)
library(httr)
library(stringi)

INPUT_URL_FORMAT <- "https://adventofcode.com/2024/day/%i/input"
QUESTION_URL_FORMAT <- "https://adventofcode.com/2024/day/%i"
COOKIE <- readLines("../AOC_SESSION_TOKEN")[1] # brittle...
options(digits=20) # printing long ints

collapse <- function (..., sep=", ") {
  paste(..., collapse=sep)
}
chars <- function (sequence) {
  # split a string to individual characters
  strsplit(sequence, "")[[1]]
}

i2ij <- function (i, dims) {
  # col-major
  return(cbind(
    row=((i-1) %% dims[2]) + 1,
    col=((i - 1) %/% dims[2]) + 1
    ))
}

ij2i <- function(i, j, dims) {
  # col-major
  (j - 1) * dims[1] + i
}

in_bounds <- function (i, j, dims) {
  return(
    i >= 1 &
    i <= dims[1] &
    j >= 1 &
    j <= dims[2] 
  )
}

# read a file into a matrix of 1 char each
read_matrix <- function (filename, numeric=FALSE) {
  return(lines2matrix(readLines(filename), numeric=numeric))
}

lines2matrix <- function (lines, numeric=FALSE) {
  bits <- stri_split_boundaries(lines, type="character")
  if (numeric) bits <- lapply(bits, as.numeric)
  return(do.call(rbind, bits))
}
# pretty-print a matrix as-is to file (if provided) or screen ("")
print_matrix <- function (mat, with_coordinates=FALSE, file="", ...) {
  # A printer for debugging, it's pretty damn cute
  #             1   
  #    1234567890   
  #  1 >|<<<\.... 1
  #  2 |2-.\^.... 2
  #  3 .v...|->>> 3
  #  4 .v...v^.|. 4
  #  5 .v...v^... 5
  #  6 .v...v^..\ 6
  #  7 .v../2\\.. 7
  #  8 2-2-/vv|.. 8
  #  9 .|<<<2-|.\ 9
  # 10 .v//.|.v.. 10
  #    1234567890   
  #             1   
  if (is.null(file) || length(file) == 0 || is.na(file))
    file <- ""
  if (with_coordinates)
    mat <- .add_coordinates(mat)
  write(t(mat), file=file, ncolumns=ncol(mat), sep="", ...)
  return(invisible(mat))
}

.add_coordinates <- function (mat) {
  dims <- dim(mat)
  
  # add col coordinates on the top and bottom, we'll write them vertical
  # e.g.
  #                   1
  # 1 2 3 4 5 6 7 8 9 0 1 2 3
  order <- floor(log10(dims[2]) + 1)
  col_coords <- list()
  cols <- seq_len(dims[2])
  for (i in seq(order, 1)) {
    divisor <- 10^(i - 1)
    labels <- (cols %/% divisor) %% 10 # extract the `i`th digit
    labels[cols %% divisor != 0] <- " "
    col_coords <- c(col_coords, list(labels))
  }
  col_coords <- do.call(rbind, col_coords)
  
  mat_with_coords <- rbind(
    col_coords,
    mat,
    col_coords[nrow(col_coords):1, ]  # upside down so the digits are close
  )
  
  # add row coordinates on the left and right
  order <- floor(log10(nrow(mat)) + 1)
  spacers <- rep(paste(rep(" ", order + 1), collapse=""), nrow(col_coords))
  rows <- as.character(seq_len(nrow(mat)))
  mat_with_coords <- cbind(
    c(spacers, paste0(format(rows, width=order, justify='r'), " "), spacers),
    mat_with_coords,
    c(spacers, paste0(" ", format(rows, width=order, justify='l')), spacers)
  )
  return(mat_with_coords)
}

# ---- rando utils for 
n_digits <- function (int) {
  # 999 -> 3 digits long
  if (int == 0) return(1)
  if (int < 0) return(n_digits(-int))
  floor(log10(int)) + 1
}
left_half <- function (int) { # assumes n_digits is even
  # left half of a number. 1234 -> 12
  n <- n_digits(int)
  stopifnot(n %% 2 == 0)
  return(floor(int / 10^(n / 2)))
}
right_half <- function (int) {
  # right half of a number. 1234 -> 34
  n <- n_digits(int)
  stopifnot(n %% 2 == 0)
  return(int %% 10^(n / 2))
}
concat_ints <- function (left, right) {
  # 12, 34 -> 1234
  left * 10^n_digits(right) + right
}

# input getters

get_example_input_data <- function (day) {
  # returns ALL example datas... list of vectors.
  
  # guess the first <code> block... no smarter than that.
  input_url <- sprintf(QUESTION_URL_FORMAT, as.integer(day))
  page <- read_html(url(input_url, headers=c(Cookie=sprintf("session=%s", COOKIE))))  # to get part 2 if relevant
  ex_data <- stri_split_fixed(stri_trim(xml_text(xml_find_all(page, "//pre/code"))), "\n")
  return(ex_data)
}

get_input_data <- function (day) {
  # must return a list
  input_url <- sprintf(INPUT_URL_FORMAT, as.integer(day))
  input <- readLines(
    con=url(input_url, headers=c(Cookie=sprintf("session=%s", COOKIE)))
  )
  return(list(input))
}

get_and_save_input_helper <- function (FUN, DEFAULT_FILENAME) {
  bound <- function (day=NULL, file=DEFAULT_FILENAME) {
    rds_file <- paste0(file, ".rds")
    # get and save
    if (file.exists(rds_file) && file.info(rds_file)$size > 0) {
      return(readRDS(rds_file))
    }
    if (is.null(day)) {
      # use the current dir
      day <- basename(getwd())
    }
    all_input_data <- FUN(day)
    multiple_inputs <- length(all_input_data) > 1
    
    # save files
    file_names <- sprintf("%s.txt", file)
    if (multiple_inputs) {
      message("**multiple input data found - will save each in its own text file\n")
      file_names <- sprintf("%s-%i.txt", file, seq_along(all_input_data))
    }
    for (i in seq_along(all_input_data)) {  # example data can have multiple
      save_file(all_input_data[[i]], file_names[i])
    }
    # if length 1 then unlist it for convenience
    if (!multiple_inputs)
      all_input_data <- all_input_data[[1]]
    
    saveRDS(all_input_data, file=rds_file)
   
    return(all_input_data)
  }
  return(bound)
}

save_file <- function (lines, file) {
  # save the vector of lines to a file
  # Print a message.
  # return the lines (invisibly)
  message(sprintf("saving input data to %s", file))
  to_print <- head(lines, 10)
  if (length(lines) > 10) to_print <- c(to_print, "...")
  message(paste(to_print, collapse="\n"))
  message("")
  writeLines(lines, file)
  return(invisible(lines))
}

get_and_save_example_input <- get_and_save_input_helper(get_example_input_data, "input-example")
get_and_save_input <- get_and_save_input_helper(get_input_data, "input")

is_wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) abs(x - round(x)) < tol


# -------- ggplot --------- #
library(ggplot2)
style_base <- list(theme_minimal())
matrix_style_base <- c(style_base, scale_y_reverse(), coord_fixed())

# ----- grid? ------ #
RIGHT <- 1
LEFT <- 2
UP <- 3
DOWN <- 4
ALL_DIRECTIONS <- c(RIGHT, LEFT, UP, DOWN)
DIRECTION_LABELS <- c(">", "<", "^", "v") # match the 1/2/3/4
# MUST match the 1/2/3/4 above
LEFT_TURN <- c(UP, DOWN, LEFT, RIGHT)
RIGHT_TURN <- c(DOWN, UP, RIGHT, LEFT)
U_TURN <- c(LEFT, RIGHT, DOWN, UP)

# dunno how to make this nice
new_coords_vector <- function (ij, direction) {
  # ij as a vector
  return(c(new_i(ij[1], direction), new_j(ij[2], direction)))
}
new_coords_matrix <- function (ij) {
  # ij as a matrix
  return(
    cbind(
      new_i(ij[, 1], direction),
      new_j(ij[, 2], direction)
    )
  )
}
new_i <- function (i, direction=c(LEFT, DOWN, RIGHT, UP)) {
  if (!all(direction %in% ALL_DIRECTIONS))
    stop("Unknown direction")
  return(
    ifelse(direction == UP, i - 1,
    ifelse(direction == DOWN, i + 1,
     i))
  )
}
new_j <- function (j, direction=c(LEFT, DOWN, RIGHT, UP)) {
  if (!all(direction %in% ALL_DIRECTIONS))
    stop("Unknown direction")
  return(
    ifelse(direction == LEFT, j - 1,
    ifelse(direction == RIGHT, j + 1,
     j))
  )
}

is_opposite_direction <- function (dir.from, dir.to) {
  return(U_TURN[dir.from] == dir.to)
}
is_turn <- function(dir.from, dir.to, direction=c("both", "left", "right")) {
  direction <- match.arg(direction)
  out <- FALSE
  if (direction %in% c("left", "both")) {
    out <- out | dir.to == LEFT_TURN[dir.from]
  }
  if (direction %in% c("right", "both")) {
    out <- out | dir.to == RIGHT_TURN[dir.from]
  }
  return(out)
}

is_in_direction <- function (from_i, from_j, to_i, to_j, direction) {
  # is it EACTLY 1 STEP in direction
  return(
    new_i(from_i, direction) == to_i &
    new_j(from_j, direction) == to_j
  )
}
is_orthogonal_adjacent <- function (from_i, from_j, to_i, to_j) {
  return(
    abs(from_i - to_i) + abs(from_j - to_j) == 1
  )
}
get_coords_reachable_in_steps <- function(min_n, max_n, from_i=0, from_j=0) {
  # from the from-row/from-col, get the coordinates that are reachable in between
  #  min_n to max_n steps, walking orthogonally.
  offsets = expand.grid(
    row=from_i + unique(c(0, +min_n:max_n, -min_n:max_n)),
    col=from_j + unique(c(0, +min_n:max_n, -min_n:max_n))
  )
  offsets[between(abs(offsets[, 'row'] - 0) + abs(offsets[, 'col'] - 0), min_n, max_n), ]
}
get_coords_reachable_in_n_steps <- function (n, from_i=0, from_j=0) {
  get_coords_reachable_in_steps(n, n, from_i, from_j)
}


# --------- graph ----------- #
# library(igraph)
adjacency_df <- function (dims) {
  # df with a connection from every IJ to its neighbour (for constructing adjacency lists)
  # might be easier/better to use make_lattice(dims, directed: bool) and add/remove vertices
  # can also make an edge-list from this
  df <- data.table(
    expand.grid(
      from.row=seq_len(dims[1]),
      from.col=seq_len(dims[2])
    )
  )
  df <- df[,
     .(
       to.row=c(from.row, from.row, from.row + 1, from.row - 1),
       to.col=c(from.col - 1, from.col + 1, from.col, from.col)
     ),
     by=.(from.row, from.col)
  ]
  df <- df[in_bounds(to.row, to.col, dims)]
  return(df)
}

# TODO: node costs.
build_nodes <- function (grid) {
  # datatable with 1 node per square on the grid, its symbol, and its coord
  dims <- dim(grid)
  nodes <- data.table(
    expand.grid(
      row=seq_len(dims[1]),
      col=seq_len(dims[2])
    )
  )
  nodes[, linear_i := ij2i(row, col, dims)]
  nodes[, symbol := grid[cbind(row, col)]]
  nodes[, node_label := sprintf("%i,%i", row, col)]
  nodes[, node_id := linear_i]
  nodes <- nodes[order(node_id)]
  return(nodes)
}

# TODO: this seems to be slow, whereas my original 2024/16.r is much faster.
build_edges <- function(nodes, is_valid_edge, ...) {
  # better performance if you exclude bad nodes from the input list.
  # datatable with 1 edge connecting nodes if the edge is valid
  # is_valid_edge: function (from_node_row, to_node_row) -> bool
  setkey(nodes, node_id)
  edges <- nodes[
    ,
    .(to_node_id = nodes[is_valid_edge(.SD, nodes, ...), node_id]),
    by=.(from_node_id=node_id)
  ]
  edges[, edge_id := .I]
  edges[, edge_label := sprintf("%s -> %s", nodes[J(from_node_id), node_label], nodes[J(to_node_id), node_label])]
  return(edges)
}
build_graph <- function (nodes, edges, directed) {
  g <- graph_from_edgelist(as.matrix(edges[, .(from_node_id, to_node_id)]), directed=directed)
  # Note - disconnected vertices are in the graph unless they are off the end
  # i.e. the # vertices is 1:max(vertice_id refereneced the edges)
  missing_nodes <- setdiff(nodes$node_id, V(g))
  if (length(missing_nodes))
    g <- add_vertices(g, length(missing_nodes)) 
  setkey(nodes, node_id)
  V(g)$label <- nodes[J(V(g)), node_label]
  V(g)$linear_i <- nodes[J(V(g)), linear_i]
  V(g)$row <- nodes[J(V(g)), row]
  V(g)$col <- nodes[J(V(g)), col]
  V(g)$symbol <- nodes[J(V(g)), symbol]
  E(g)$label <- edges$edge_label # <-- is this correct?
  if (!is.null(edges$cost))
    E(g)$weight <- edges$cost
  return(g)
}
plot_graph_on_grid <- function (g, vertex.size=5, edge.arrow.size=0.5, ...) {
  # plot a graph that came from one of these helpers
  # (expect attrs row, col, symbol)
  # and probably a $symbol
  plot(
    g,
    layout=cbind(V(g)$col, -V(g)$row),
    vertex.size=vertex.size,
    vertex.shape="none",
    vertex.label=V(g)$symbol,
    edge.label=NA,
    edge.arrow.size=edge.arrow.size,
    ...
  )
}
# like is_orthogonal_adjacent and is_in_direction but operating on nodes
is_node_adjacent <- function(fromnode, tonode) {
  is_orthogonal_adjacent(fromnode$row, fromnode$col, tonode$row, tonode$col)
}
is_node_in_direction <- function(fromnode, tonode) {
  # is it EACTLY 1 STEP in direction (implies adjacent)
  is_in_direction(fromnode$row, fromnode$col, tonode$row, tonode$col, fromnode$facing.direction)
}
build_orthogonal_edges <- function(nodes, impassibles, additional_edge_filter, min_node_distance=1, max_node_distance=1, directed=FALSE) {
  # this version uses specific knowledge of rows/cols to do its joins to try
  #  do it faster.
  offs <- get_coords_reachable_in_steps(min_node_distance, max_node_distance, from_i=0, from_j=0)
  if (!directed)
    offs <- offs[offs[, 'row'] >= 0 & offs[, 'col'] >= 0, ]  # only half the directions are needed
  setkey(nodes, row, col)
  if (!missing(impassibles))
    nodes <- nodes[!(symbol %in% impassibles)] # cut it down
  edges <- nodes[
    ,
    .(
      from_node_id=node_id,
      to_row=from_row + offs[, 'row'],
      to_col=from_col + offs[, 'col']
    ),
    by=.(from_row=row, from_col=col)
  ]
  edges[, to_node_id := nodes[.(to_row, to_col), node_id]]
  edges <- edges[!is.na(to_node_id)] # non-existent destination node
  setkey(nodes, node_id)
  if (!missing(additional_edge_filter))
    edges <- edges[additional_edge_filter(nodes[J(from_node_id)], nodes[J(to_node_id)])]
  edges[, edge_id := .I]
  edges[
    , edge_label := sprintf(
      "%s %s %s",
      nodes[J(from_node_id), node_label],
      ifelse(directed, "->", "-"),
      nodes[J(to_node_id), node_label]
    )
  ]
  edges
}
# Here we have a graph representing (i, j, direction I am facing).
# By default you can only walk to a square in the same direction you are facing.
# There are self-loops to representing turning that can be costed separately
# TODO: node costs.
build_nodes_with_direction <- function(grid) {
  nodes <- build_nodes(grid)
  nodes <- add_directions_to_nodes(nodes)
  return(nodes)
}
add_directions_to_nodes <- function (nodes) {
  nodes4 <- nodes[, .(facing=c(UP, DOWN, LEFT, RIGHT)), by=names(nodes)]
  nodes4[, node_label := sprintf("%i,%i%s", row, col, DIRECTION_LABELS[facing])]
  nodes4[, node_id := .I]
  setkeyv(nodes4, key(nodes))
  return(nodes4)
}
build_grid_with_turn_directions <- function(grid, impassibles, additional_edge_filter, min_node_distance=1, max_node_distance=1) {
  # returns nodes and edges (to make a graph) that represents a grid where direction is taken into account.
  # * walking forward and turning to face a different direction are explicitly represented
  #   (and can be costed separately)
  # * currently you CANNOT U-turn
  # * providing `impassibles` (list of symbols) means you're not allowed to walk in that direction
  # * min_node_distance/max_node_distance: a node will be connected to all nodes it can reach in these number of steps
  # NB this ONLY makes sense with directed=True
  # MIN/MAX NODE DISETANCE ARE IGNORED ATM AND ALWAYS 1
  nodes <- build_nodes(grid)
  # now copy-paste it for each direction and only join between directions from a node to itself (representing turning)
  # but maybe drop all the impassible nodes except the 'first' so you can draw a picture still
  if (missing(impassibles)) impassibles <- c()
  nodes4 <- add_directions_to_nodes(nodes)[facing == UP | !(symbol %in% impassibles)]
  nodes4[, node_id := .I]
  setkey(nodes4, node_id)

  edges <- build_orthogonal_edges(nodes, impassibles, min_node_distance=min_node_distance, max_node_distance=max_node_distance, directed=TRUE)
  edges[from_row == to_row & from_col < to_col, facing := RIGHT]
  edges[from_row == to_row & from_col > to_col, facing := LEFT]
  edges[from_col == to_col & from_row < to_row, facing := DOWN]
  edges[from_col == to_col & from_row > to_row, facing := UP]
  edges <- rbind(
    edges[, .(from_row, from_col, from_node_id, from_facing=facing, to_row, to_col, to_facing=facing)],
    nodes4[  # turning left and right
      !(symbol %in% impassibles),
      .(from_row=row, from_col=col, from_node_id=node_id, from_facing=facing,
        to_row=row, to_col=col,
        # this will recycle everything else
        to_facing=c(LEFT_TURN[facing], RIGHT_TURN[facing]))
    ]
  )
  edges[nodes4, to_node_id := node_id, on=.(to_row=row, to_col=col, to_facing=facing)]
  edges <- edges[!is.na(to_node_id)] # walking to out of bounds, to impassible, ...
  if (!missing(additional_edge_filter))
    edges <- edges[additional_edge_filter(nodes4[J(from_node_id)], nodes4[J(to_node_id)])]
  edges[, edge_id := .I]
  edges[
    , edge_label := sprintf(
      "%s -> %s",
      nodes4[J(from_node_id), node_label],
      nodes4[J(to_node_id), node_label]
    )
  ]
  edges <- edges[, .(from_node_id, from_row, from_col, from_facing, to_node_id, to_row, to_col, to_facing, edge_id, edge_label)]
  return(list(nodes=nodes4, edges=edges))
}

if (FALSE) {
# EXAMPLE (grid-like data with '#' as a wall)
map <- lines2matrix(stri_split_fixed(pattern="\n", stri_trim("
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"))[[1]])

# ---- simple (no explicit turning)
n <- build_nodes(map)
e <- build_orthogonal_edges(n, "#")
g <- build_graph(n, e, directed=FALSE)
plot_graph_on_grid(g)

# ---- more difficult (direction taken into account)
o <- build_grid_with_turn_directions(
  map, "#",
  additional_edge_filter = function (fromnode, tonode) {
    # don't walk from the end node anywhere (including rotating)
    fromnode$symbol != "E" &
    # don't walk to the start node from elsewhere (but you can rotate)
    !(fromnode$symbol != "S" & tonode$symbol == "S")
  }
)
o$edges[, cost := ifelse(from_node_id == to_node_id, 1000, 1)]
g <- build_graph(o$nodes, o$edges, directed=TRUE)
plot_graph_on_grid(g, edge.arrow.size=0.2)

}

# ------ int64 --------
# you need bit64 for this.
# it does various operations making use of the bit64 package/int64 type, BUT
#  endeavours to return doubles where possible (since lots of R functions muck
#  up with int64)
int2str <- function (x) {
  # numeric -> string of 1s and 0s of 64 characters
  as.bitstring(as.integer64(x))
}
bin2int <- function (x) {
  # binary string -> double, can handle int64
  as.double(as.integer64.bitstring(gsub(" ", "", x)))
}
stupid_xor <- function (x, y) {
  # x, y: double, double -> double
  # wow, the bit64 packages doesn't have bitwise xor, and the bitops package
  #  doesn't support int64. This is the stupidest thing I have ever done.
  # returns a double (but has to go via int64)
  xx <- substring(as.bitstring(as.integer64(x)), seq_len(64), seq_len(64))
  yy <- substring(as.bitstring(as.integer64(y)), seq_len(64), seq_len(64))
  return(as.double(as.integer64.bitstring(paste(ifelse(xx==yy, "0", "1"), collapse=""))))
}
stupid_bitwShiftL <- function(x, pow) {
  # i, pow: double, numeric -> double
  as.double(x) * 2^pow
}
