# this version tried to maintain a datatable of (row, col) for eac hfeature
# so that it was sparse, and do the operations a bit smarter (manipulating
#  the coordinates rather than moving characters in a matrix)
# But for some reason it is insufferably slow (it runs the 2 examples in part 1
#  but I gave up waiting for the input data). Perhaps because of data.table
# So it's not tested and was for part 1 (but had a `width` column to be ready
#  for part 2).
source("15.r")
walk_n_features <- function (map, directions, verbose=FALSE) {
  start <- which(map == ME, arr.ind=TRUE)
  dims <- dim(map)
  features <- calc_features(map)
  for (dir in directions) {
    features <- walk_features(dir, features)
    if (verbose) {
      map <- features_to_map(features, dims)
      write(dir, file="")
      print_matrix(map)
      write("", file="")
    }
  }
  
  return(features)
}
features_to_map <- function (features, dims) {
  map <- array("!", dims)
  map[as.matrix(features[, .(row, col)])] <- features$feature
  map
}
# a SMALL map
calc_features <- function (map) {
  dims <- dim(map)
  ij <- expand.grid(row=1:dims[1], col=1:dims[2])
  # ij <- which(map != BLANK, arr.ind=TRUE)
  features <- cbind(data.table(ij), feature=map[as.matrix(ij)], width=1)
  features
}

walk_features <- function (direction, features) {
  ij <- features[feature == ME, c(row, col)]
  new_ij <- new_coords(ij, direction) # the coords I am walking to.
  new_char <- features[row == new_ij[1] & col == new_ij[2], feature]
  features[row == ij[1] & col == ij[2], feature := BLANK]
  
  if (new_char == BLANK) {
    # sweet
  } else if (new_char == WALL) {
    new_ij <- ij  # don't move
  } else if (new_char == BOULDER) {
    # see if we can push
    successful_push <- push_features(new_ij[1], new_ij[2], direction, features)
    if (!successful_push) {
      new_ij <- ij  # don't move
    } else {
      # sweet, we walk, `push` has mutated the coords
    }
  }
  features[row == new_ij[1] & col == new_ij[2], feature := ME]
  return(features)
  stop(sprintf("Unknown character %s at %i, %i", new_char, ij[1], ij[2]))
}

push_features <- function(boulder_i, boulder_j, direction, features){
  moved <- FALSE
  
  # push boulder at boulder_ij in direction
  if (direction == LEFT) {
    # find spaces between the wall and me
    wall_j <- features[row == boulder_i & col < boulder_j & feature == WALL, max(col)]
    space_j <- features[row == boulder_i & col > wall_j & col <= boulder_j & feature == BLANK, suppressWarnings(max(col))]
    if (!is.infinite(space_j)) { # there is space:: push
      features[row == boulder_i & col > space_j & col <= boulder_j & feature == BOULDER, col := col - 1]
      features[row == boulder_i & col == space_j & feature == BLANK, col := boulder_j]
      moved <- TRUE
    }
  }
  if (direction == RIGHT) {
    wall_j <- features[row == boulder_i & col > boulder_j & feature == WALL, min(col)]
    space_j <- features[row == boulder_i & col >= boulder_j & col < wall_j & feature == BLANK, suppressWarnings(min(col))]
    
    if (!is.infinite(space_j)) { # there is space:: push
      features[row == boulder_i & col >= boulder_j & col < space_j & feature == BOULDER, col := col + 1]
      features[row == boulder_i & col == space_j & feature == BLANK, col := boulder_j]
      moved <- TRUE
    }
  }
  if (direction == UP) {
    wall_i <- features[col == boulder_j & row < boulder_i & feature == WALL, max(row)]
    space_i <- features[col == boulder_j & row > wall_i & row <= boulder_i & feature == BLANK, suppressWarnings(max(row))]
    if (!is.infinite(space_i)) { # there is space:: push
      features[col == boulder_j & row > space_i & row <= boulder_i & feature == BOULDER, row := row - 1]
      features[col == boulder_j & row == space_i & feature == BLANK, row := boulder_i]
      moved <- TRUE
    }
  }
  if (direction == DOWN) {
    wall_i <- features[col == boulder_j & row > boulder_i & feature == WALL, min(row)]
    space_i <- features[col == boulder_j & row >= boulder_i & row < wall_i & feature == BLANK, suppressWarnings(min(row))]
    if (!is.infinite(space_i)) { # there is space:: push
      features[col == boulder_j & row >= boulder_i & row < space_i & feature == BOULDER, row := row + 1]
      features[col == boulder_j & row == space_i & feature == BLANK, row := boulder_i]
      moved <- TRUE
    }
  }
  return(moved)
}
bits <- parse_bits(ex[[2]])
print_matrix(final_map <- features_to_map(walk_n_features(bits$map, bits$directions), dim(bits$map)))
print(calc_ans(final_map))

bits <- parse_bits(ex[[1]])
print_matrix(final_map <- features_to_map(walk_n_features(bits$map, bits$directions), dim(bits$map)))
print(calc_ans(final_map))

bits <- parse_bits(input)
print_matrix(final_map <- features_to_map(walk_n_features(bits$map, bits$directions), dim(bits$map)))
print(calc_ans(final_map))
