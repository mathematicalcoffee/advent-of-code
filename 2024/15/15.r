rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

ME <- "@"
BOULDER <- "O"
BL <- "["
BR <- "]"
BLANK <- "."
WALL <- "#"
LEFT <- "<"
RIGHT <- ">"
UP <- "^"
DOWN <- "v"
PUSHABLE <- c(BOULDER, BL, BR)
parse_bits <- function (lines) {
  blank <- which(lines == "")
  map <- lines2matrix(lines[seq_len(blank - 1)])
  directions <- stri_split_boundaries(paste(lines[seq(blank + 1, length(lines))], collapse=""), type="character")[[1]]
  list(map=map, directions=directions)
}

gps <- function (i, j) {
  (i - 1)*100 + (j - 1)
}
  
new_coords <- function (ij, direction=c(LEFT, DOWN, RIGHT, UP)) {
  # ij as a vector
  direction <- match.arg(direction)
  if (direction == LEFT)
    return(ij + c(0, -1))
  if (direction == DOWN)
    return(ij + c(1, 0))
  if (direction == RIGHT)
    return(ij + c(0, 1))
  if (direction == UP)
    return(ij + c(-1, 0))
  stop("unknown direction")
}
new_coords_matrix <- function (ij, direction=c(LEFT, DOWN, RIGHT, UP)) {
  # ij as a matrix
  direction <- match.arg(direction)
  if (direction == LEFT) {
    ij[,2] <- ij[, 2] - 1
  } else if (direction == DOWN) {
    ij[,1] <- ij[, 1] + 1
  } else if (direction == RIGHT) {
    ij[,2] <- ij[, 2] + 1
  } else if (direction == UP) {
    ij[,1] <- ij[, 1] - 1
  } else {
    stop("unknown direction")
  }
  return(ij)
}

walk <- function (ij, direction, map, dims) {
  new_ij <- new_coords(ij, direction)
  new_char <- map[new_ij]
  
  if (new_char == BLANK) {
    map[new_ij] <- ME
    map[ij] <- BLANK
  } else if (new_char == WALL) {
    new_ij <- ij  # don't move
  } else if (new_char %in% PUSHABLE) {
    # see if we can push
    out <- push(new_ij, direction, map, dims)
    if (isFALSE(out)) {
      new_ij <- ij  # don't move
    } else {
      map <- out
      map[ij] <- BLANK
      map[new_ij] <- ME
    }
  } else {
    stop(sprintf("Unknown character %s at %i, %i", new_char, ij[1], ij[2]))
  }
  return(list(ij=new_ij, map=map))
}

push_working_part1 <- function(boulder_ij, direction, map, dims) {
  from_range <- NULL
  to_range <- NULL
  # push boulder at boulder_ij in direction
  if (direction == LEFT) {
    wall_js <- which(map[boulder_ij[1], 1:boulder_ij[2]] == WALL)
    offs <- max(wall_js)
    blank_js <- which(map[boulder_ij[1], offs:boulder_ij[2]] == BLANK)
    to_j <- suppressWarnings(max(blank_js))
    if (!is.infinite(to_j)) {
      to_j <- to_j + offs - 1
      from_range <- cbind(boulder_ij[1], (to_j + 1):boulder_ij[2])
      to_range <- cbind(boulder_ij[1], from_range[, 2] - 1)
    }
  }
  if (direction == RIGHT) {
    wall_js <- which(map[boulder_ij[1], boulder_ij[2]:dims[2]] == WALL) + boulder_ij[2] - 1
    blank_js <- which(map[boulder_ij[1], boulder_ij[2]:min(wall_js)] == BLANK)
    to_j <- suppressWarnings(min(blank_js))
    if (!is.infinite(to_j)) {
      to_j <- to_j + boulder_ij[2] - 1
      from_range <- cbind(boulder_ij[1], boulder_ij[2]:(to_j - 1))
      to_range <- cbind(boulder_ij[1], from_range[, 2] + 1)
    }
  }
  if (direction == UP) {
    wall_is <- which(map[1:boulder_ij[1], boulder_ij[2]] == WALL)
    offs <- max(wall_is)
    blank_is <- which(map[offs:boulder_ij[1], boulder_ij[2]] == BLANK)
    to_i <- suppressWarnings(max(blank_is))
    if (!is.infinite(to_i)) {
      to_i <- to_i + offs - 1
      from_range <- cbind((to_i + 1):boulder_ij[1], boulder_ij[2])
      to_range <- cbind(from_range[, 1] - 1, boulder_ij[2])
    }
  }
  if (direction == DOWN) {
    wall_is <- which(map[boulder_ij[1]:dims[1], boulder_ij[2]] == WALL) + boulder_ij[1]- 1
    blank_is <- which(map[boulder_ij[1]:min(wall_is), boulder_ij[2]] == BLANK)
    to_i <- suppressWarnings(min(blank_is))
    if (!is.infinite(to_i)) {
      to_i <- to_i + boulder_ij[1] - 1
      from_range <- cbind(boulder_ij[1]:(to_i - 1), boulder_ij[2])
      to_range <- cbind(from_range[, 1] + 1, boulder_ij[2])
    }
  }
  
  if (!is.null(from_range)) {
    old <- map[from_range]
    to_range[map[to_range] == BL, , drop=F]
    map[from_range] <- BLANK
    map[to_range] <- old
    return(map)
  }
  return(FALSE)
}


get_boulder_ijs <- function(boulder_ij, map) {
  # expand out the coordinates which much be a matrix, to get all the bits of
  # the boxes
  chars <- map[boulder_ij]
  lb <- chars == BL
  rb <- chars == BR
  return(
    rbind(
      boulder_ij,
      cbind(row=boulder_ij[lb, 1], col=boulder_ij[lb, 2] + 1),
      cbind(row=boulder_ij[rb, 1], col=boulder_ij[rb, 2] - 1)
    )
  )
}

push <- function(boulder_ij, direction, map, dims) {
  from_range <- NULL
  to_range <- NULL
  # push boulder at boulder_ij in direction
  # should change my left/right to match the up/down and the signature boulder_ij
  #  to be ij of the pusher, but spent too long already.
  if (direction == LEFT) {
    wall_js <- which(map[boulder_ij[1], 1:boulder_ij[2]] == WALL)
    offs <- max(wall_js)
    blank_js <- which(map[boulder_ij[1], offs:boulder_ij[2]] == BLANK)
    to_j <- suppressWarnings(max(blank_js))
    if (!is.infinite(to_j)) {
      to_j <- to_j + offs - 1
      from_range <- cbind(boulder_ij[1], (to_j + 1):boulder_ij[2])
      to_range <- cbind(boulder_ij[1], from_range[, 2] - 1)
    }
  } else if (direction == RIGHT) {
    wall_js <- which(map[boulder_ij[1], boulder_ij[2]:dims[2]] == WALL) + boulder_ij[2] - 1
    blank_js <- which(map[boulder_ij[1], boulder_ij[2]:min(wall_js)] == BLANK)
    to_j <- suppressWarnings(min(blank_js))
    if (!is.infinite(to_j)) {
      to_j <- to_j + boulder_ij[2] - 1
      from_range <- cbind(boulder_ij[1], boulder_ij[2]:(to_j - 1))
      to_range <- cbind(boulder_ij[1], from_range[, 2] + 1)
    }
  } else if (direction %in% c(UP, DOWN)) {
    from_range <- push_recursive(boulder_ij + c(ifelse(direction == UP, 1, -1), 0), direction, map)
    if (!isFALSE(from_range)) {
      to_range <- cbind(from_range[, 1] + ifelse(direction == UP, -1, 1), from_range[, 2])
    } else { from_range <- NULL }
  } else {
    stop("pushing in unknown direction")
  }
  
  if (!is.null(from_range)) {
    old <- map[from_range]
    map[from_range] <- BLANK
    map[to_range] <- old
    return(map)
  }
  return(FALSE)
}
push_recursive <- function(push_from_ijs, direction, map, already_pushed_ijs=NULL) {
  # Can we push into all coordinates specified by into_ij ?
  #  which is an array/matrix not a vector
  # returns: FALSE if we can't, and otherwise all source coordinates.
  #  i.e. all coordinates from which we are pushing.
  into_ijs <- get_boulder_ijs(new_coords_matrix(push_from_ijs, direction), map)
  chars <- map[into_ijs]
  if (any(chars == WALL)) {
    return(FALSE)
  } else if (all(chars == BLANK)) {
    return(
      rbind(push_from_ijs, already_pushed_ijs)
    )
  } else if (any(chars %in% PUSHABLE)) {
    return(
      push_recursive(
        into_ijs[chars %in% PUSHABLE, , drop=FALSE],
        direction,
        map,
        rbind(push_from_ijs, already_pushed_ijs)
      )
    )
  }
  stop("dunno")
}


walk_n <- function (map, directions, verbose=FALSE) {
  start <- which(map == ME, arr.ind=TRUE)
  dims <- dim(map)
  o <- list(map=map, ij=start)
  for (dir in directions) {
    o <- walk(o$ij, dir, o$map, dims)
    if (verbose) {
      write(dir, file="")
      print_matrix(o$map)
      write("", file="")
    }
  }
  return(o$map)
}

enwiden_map <- function (map) {
  new_map <- array(BLANK, dim=dim(map) * c(1, 2))
 
  for (oi in 1:dim(map)[1]) {
    new_j <- 1
    for (oj in 1:dim(map)[2]) {
      if (map[oi, oj] == WALL)
        new_map[oi, new_j + 0:1] <- WALL
      if (map[oi, oj] == ME)
        new_map[oi, new_j + 0:1] <- c(ME, BLANK)
      if (map[oi, oj] == BOULDER)
        new_map[oi, new_j + 0:1] <- c(BL, BR)
      new_j <- new_j + 2
    }
  }
  return(new_map)
}

calc_ans <- function (map) {
  coords <- which(map == BOULDER | map == BL, arr.ind=TRUE)
  return(sum(gps(coords[, 'row'], coords[, 'col'])))
}

  
bits <- parse_bits(ex[[2]])
print_matrix(final_map <- walk_n(bits$map, bits$directions))
print(ans <- calc_ans(final_map))
stopifnot(ans == 2028)

bits <- parse_bits(ex[[1]])
print_matrix(final_map <- walk_n(bits$map, bits$directions))
print(ans <- calc_ans(final_map))
stopifnot(ans == 10092)

print("PART 1 INPUT DATA")
bits <- parse_bits(input)
print_matrix(final_map <- walk_n(bits$map, bits$directions))
print(ans <- calc_ans(final_map))
stopifnot(ans == 1505963)

expt2 <- "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######"
map <- enwiden_map(lines2matrix(stri_split_fixed(pattern="\n", expt2)[[1]]))
directions <- stri_split_boundaries(type="character", "<vv<<^^<<^^")[[1]]
print_matrix(map)
print_matrix(final_map <- walk_n(map, directions, verbose=FALSE))
print(ans <- calc_ans(final_map))
# don't know what the answer is
  
bits <- parse_bits(ex[[1]])
new_map <- enwiden_map(bits$map)
# print_matrix(final_map <- walk_n(new_map, bits$directions[1:50], verbose=F))
# print_matrix(final_map <- walk_n(final_map, bits$directions[51:100], verbose=F))
# print_matrix(final_map <- walk_n(final_map, bits$directions[101:150], verbose=F))
# print_matrix(final_map <- walk_n(final_map, bits$directions[151:200], verbose=F))
# print_matrix(final_map <- walk_n(final_map, bits$directions[201:250], verbose=F))
# print_matrix(final_map <- walk_n(final_map, bits$directions[251:300], verbose=F))
# print_matrix(final_map <- walk_n(final_map, bits$directions[301:350], verbose=T))
print_matrix(final_map <- walk_n(new_map, bits$directions))
print(ans <- calc_ans(final_map))
stopifnot(ans == 9021)

# wrong <- "####################
# ##[]..[]......[][]##
# ##[]...........[].##
# ##...........@[][]##
# ##..........[].[].##
# ##..##[]..[].[]...##
# ##...[]...[]..[]..##
# ##.....[]..[].[][]##
# ##........[]......##
# ####################"
# map <- lines2matrix(stri_split_fixed(pattern="\n", wrong)[[1]])
# direction <- DOWN
# print_matrix(push(cbind(5, 14), DOWN, map, dims(map)))
# boulder_ij <- cbind(5, 14)
# push_from_ijs <- cbind(4,14)

print("PART 2 INPUT DATA")
bits <- parse_bits(input)
new_map <- enwiden_map(bits$map)
print_matrix(final_map <- walk_n(new_map, bits$directions, verbose=F))
print(ans <- calc_ans(final_map))
# 1543141
