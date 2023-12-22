rm(list = ls())
source("../handy.R")
library(stringi)
ZBOTTOM <- 3
ZTOP <- 6
XJ <- c(1, 4)
YJ <- c(2, 5)
ZJ <- c(ZBOTTOM, ZTOP)
XYZ1 <- 1:3
XYZ2 <- 4:6

read_input <- function(input_file) {
  input <- readLines(input_file)
  coords <- t(
    sapply(
      stri_split(input, fixed = "~"),
      function(coords) {
        as.numeric(unlist(stri_split(coords, fixed = ",")))
      }
    )
  )
  colnames(coords) <- c("xstart", "ystart", "zstart", "xend", "yend", "zend")
  # rearrange coords so that zstart <= zend for each block
  to_rearrange <- which(coords[, 3] > coords[, 6])
  if (length(to_rearrange)) {
    tmp <- coords[to_rearrange, ]
    coords[to_rearrange, XYZ1] <- coords[to_rearrange, XYZ2]
    coords[to_rearrange, XYZ2] <- tmp[, XYZ1]
  }
  # make some blocknames...do it inefficiently
  nblocks <- nrow(coords)
  n <- ceiling(log(nblocks) / log(26))
  rownames(coords) <-
    apply(
      do.call(expand.grid, replicate(n, LETTERS, simplify = FALSE)),
      1,
      paste,
      collapse = ""
    )[seq_len(nblocks)]

  # rearrange the blocks lowest to highest Z
  coords <- coords[order(coords[, ZBOTTOM], decreasing = FALSE), ]

  return(coords)
}

overlaps_2d <- function(block1, block2) {
  # ignore Z, each is a set of 6 coords
  return(
    overlaps_1d(block1[XJ], block2[XJ]) &&
      overlaps_1d(block1[YJ], block2[YJ])
  )
}

overlaps_1d <- function(interval1, interval2) {
  return(
    between(interval1[1], interval2) ||
      between(interval2[1], interval1)
  )
}
between <- function(x, y1y2) { # inclusive
  return(x >= y1y2[1] & x <= y1y2[2])
}

overlaps_2d_matrix <- function(blocks) {
  # retval[i,j] = whether block i overlaps block j considering just x-y
  # I am not sure why but this is slower than I'd thought
  nblocks <- nrow(blocks)
  overlaps_matrix <- array(
    FALSE,
    dim = c(nblocks, nblocks),
    dimnames = list(rownames(blocks), rownames(blocks))
  )
  # return matrix[i,j] = block i overlaps block j (symmetric)
  for (b1 in seq_len(nblocks)) {
    for (b2 in b1:nrow(blocks)) {
      overlaps <- FALSE
      if (b1 == b2) overlaps <- TRUE
      overlaps <- overlaps_2d(blocks[b1, ], blocks[b2, ])

      overlaps_matrix[b1, b2] <- overlaps
      overlaps_matrix[b2, b1] <- overlaps
    }
  }
  return(overlaps_matrix)
}

fall <- function(blocks, overlap_matrix = NULL) {
  # apply gravity to all blocks.
  # start from the bottom block(s) and drop them as far as they will go
  # this is slow - is it re-allocating the vector?
  # NOTE: they do not necessarily end up in z order any more depending how
  #   far each one can fall
  if (is.null(overlap_matrix)) overlap_matrix <- overlaps_2d_matrix(blocks)
  nblocks <- nrow(blocks)
  blockis <- seq_len(nblocks)

  for (i in blockis) {
    # from the lowest block up, shift them as far down as they will go
    dz <- 0
    # blocks beneath me overlap me and have Z smaller than me (and we have
    #  sorted blocks by z ascending)
    beneath_me <- overlap_matrix[i, ] & blocks[, ZTOP] < blocks[i, ZBOTTOM]
    if (sum(beneath_me)) {
      dz <- blocks[i, ZBOTTOM] - max(blocks[beneath_me, ZTOP]) - 1
    } else {
      dz <- blocks[i, ZBOTTOM] - 0 - 1
    }

    # shift block down
    if (dz > 0) {
      blocks[i, ZJ] <- blocks[i, ZJ] - dz
    }
  }
  return(blocks)
}

supports_matrix <- function(blocks, overlap_matrix = NULL) {
  # does block1 support block2?
  # it supports block2 if
  # * their xys overlap
  # * block1's touches block2 (block1's high z is block2's low z - 1)
  # ASSUMPTIONS
  # blocks have fallen already
  # block1[i,j] is true if block i supports j (directional)
  if (is.null(overlap_matrix)) overlap_matrix <- overlaps_2d_matrix(blocks)
  nblocks <- nrow(blocks)
  supports <- array(
    FALSE,
    dim = c(nblocks, nblocks),
    dimnames = list(rownames(blocks), rownames(blocks))
  )
  zbottoms <- blocks[, ZBOTTOM]

  for (i in seq_len(nblocks)) {
    supports_me <- overlap_matrix[i, ] & zbottoms == blocks[i, ZTOP] + 1
    supports[i, supports_me] <- TRUE
  }

  return(supports)
}

prep_for_plotting <- function(blocks) {
  # fills a block with coordinates for plotting (and validates if there are
  #  mistakes
  xlim <- range(blocks[, XJ]) # 0-9
  ylim <- range(blocks[, YJ]) # 0-9
  zlim <- range(blocks[, ZJ]) # 1-114
  # fill in some slices for plotting...
  space <- array(0, dim = c(diff(xlim) + 1, diff(ylim) + 1, diff(zlim) + 1))
  for (i in seq_len(nrow(blocks))) {
    # assume everyone is long and skinny
    coords <- cbind(
      seq(blocks[i, 1], blocks[i, 4]),
      seq(blocks[i, 2], blocks[i, 5]),
      seq(blocks[i, 3], blocks[i, 6])
    )
    if (sum(space[coords]) > 0) {
      stop("blocks overlapping others")
    }
    space[coords] <- i
  }
  return(space)
}

# --- part 2 --- #
# something about this is slow, and I'm not entirely sure what - I thought the
#  binary vectors would make things good.
# there is probably something smarter with matrix multiplication
total_blocks_fallen_by_removing <- function(block, supports) {
  # remove a block and work out how many fall

  disintegrated <- rep(FALSE, nrow(supports)) # not including the initial block
  names(disintegrated) <- colnames(supports)
  disintegrate_next <- disintegrated[]
  disintegrate_next[block] <- TRUE
  on_ground <- colSums(supports) == 0
  while (sum(disintegrate_next) > 0) {
    # disintegrate me
    supports[disintegrate_next, ] <- FALSE
    disintegrated[disintegrate_next] <- TRUE

    # who has no supports (and is not on the ground)
    (disintegrate_next <- !disintegrated & colSums(supports) == 0 & !on_ground)
  }
  return(sum(disintegrated) - 1) # remove original
}


blocks_orig <- read_input("input.txt")
ovl <- overlaps_2d_matrix(blocks_orig) # this might be what is slow oops
blocks <- fall(blocks_orig, overlap_matrix = ovl)
supports <- supports_matrix(blocks, overlap_matrix = ovl) # also slow

# find things that are supported by only one block
# supports[i, j] :: i supports j, j is supported by i

# you can be disintegrated if you are not the sole support for any block
# these blocks are only supported by 1 block
supported_by_one <- which(colSums(supports) == 1)
is_sole_support <- rowSums(supports[, supported_by_one]) > 0
can_disintegrate <- !is_sole_support
print(sum(can_disintegrate))

# --- part 2 --- #
p2 <- sapply(
  rownames(blocks),
  total_blocks_fallen_by_removing,
  supports = supports
)
# would have thought that the blocks lower down would support more things
plot(blocks[, ZTOP], p2)
print(sum(p2))
