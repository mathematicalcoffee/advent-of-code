rm(list=ls())
library(data.table)
library(testthat)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()
X <- 1
Y <- 2

parse_input <- function (input_str) {
  coords <- as.matrix(read.csv(text=input_str, header=FALSE))
  colnames(coords) <- c("x", "y")
  coords
}
pairwise_areas <- function (coords) {
  # OK so it's like a distance matrix but with a custom metric
  dx <- dist(coords[, X, drop=FALSE], method="manhattan") + 1
  dy <- dist(coords[, Y, drop=FALSE], method="manhattan") + 1
  return(dx * dy)
}
part1 <- function (input_str) {
  coords <- parse_input(input_str)
  areas <- pairwise_areas(coords)
  max(areas)
}

expect_equal(part1(ex[[1]]), 50)
expect_equal(part1(input), 4760959496)

# --------------------- PART 2 -----------------------------
plot_rect <- function (coords, col, ...) {
  points(coords, col=col, pch=20, cex=2) # part 1
  rect(min(coords[, X]), min(coords[, Y]), max(coords[, X]), max(coords[, Y]), col=col, density=50, ...)
}

coords <- parse_input(input)
areas <- pairwise_areas(coords)
part1.corners <- i2distij(which.max(areas), nrow(coords))

# This is too hard, so let's draw a picture
# OOOOOOOOOOO the coords are in a circle and all of h-v nature!!!
plot(coords, pch=20, cex=0.5, asp=1)
lines(rbind(coords, coords[1, ]))
plot_rect(coords[part1.corners, ], "red")
title("Day 9 Part 1")

# OK so by inspection it surely has to have one of those inset points LOL
# Let's try find them
diffs <- diff(coords)
horz.i <- which.max(abs(diffs[,1]))
lower.i <- horz.i
upper.i <- horz.i - 1
areas <- as.matrix(areas) # just easier to work with than triangular numbers

# So .. eyeball 
bad_guess_search_bottom_half <- function (topright_corner.i, coords, areas) {
  topright_corner <- coords[topright_corner.i, ]
  # returns index into i
  min.y <- min(coords[
    coords[, X] >= topright_corner[X] & coords[, Y] <= topright_corner[Y], Y
  ])
  valid_others <- which( coords[, Y] >= min.y & coords[, Y] <= topright_corner[Y]  )
  
  # find max area among valid other points
  max.area <- max(areas[cbind(topright_corner.i, valid_others)])
  which(areas[topright_corner.i, ] == max.area)
}

plot(coords, pch=20, cex=0.5, asp=1)
plot_rect(
  coords[c(lower.i, bad_guess_search_bottom_half(lower.i, coords, areas)), ], "cyan"
)
# be lazy and flip it upside down for the other size
coords.lazy <- coords
coords.lazy[, Y] <- -coords.lazy[, Y]
plot_rect( # flip upside down
  coords[c(upper.i, bad_guess_search_bottom_half(upper.i, coords.lazy, areas)), ], "green"
)
lines(rbind(coords, coords[1, ]))
title("derp")  # not quite right

search_for_rectangle_top_half <- function(bottomright_corner.i, coords, areas) {
  # OK so we are going to go through all points in the top half of the semicircle
  #  and see if we can make a rectangle out of them with the given bottomright point
  # if any other dot is in our rectangle it's valid.
  fixed.x <- coords[bottomright_corner.i, X]
  fixed.y <- coords[bottomright_corner.i, Y]
  valid_others <- areas[1,] < 0
  for (i in 1:nrow(coords)) {
    candidate.x <- coords[i, X]
    candidate.y <- coords[i, Y]
    # our candidates need to be top-left corners
    if (candidate.y <= fixed.y) {
      next
    }
    if (candidate.x >= fixed.x) {
      next
    }
    
    # check for other points strictly inside
    if (any(
      coords[, X] > candidate.x &
      coords[, X] < fixed.x &
      coords[, Y] < candidate.y &
      coords[, Y] > fixed.y
      )) {
      next
    }
    valid_others[i] <- TRUE
  }
  max.area <- max(areas[cbind(bottomright_corner.i, which(valid_others))])
  which(areas[bottomright_corner.i, ] == max.area)
}
plot(coords, pch=20, cex=0.5, asp=1)
plot_rect(
  coords[c(upper.i, upper.candidate.i <- search_for_rectangle_top_half(upper.i, coords, areas)), ], "green"
)
plot_rect(
  coords[c(lower.i, lower.candidate.i <- search_for_rectangle_top_half(lower.i, coords.lazy, areas)), ], "cyan"
)
lines(rbind(coords, coords[1, ]))
title("Part 2 by visual inspection baby!")
title("writing code is too hard", cex.main=0.8, line=0.7)

expect_equal(
  max(
    areas[upper.i, upper.candidate.i],
    areas[lower.i, lower.candidate.i]
  ),
  1343576598
)
