rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

walk.maze <- function (maze, return.visited) {
  walk.maze.inner(
    which(maze == "^", arr.ind=TRUE),
    which(maze == "#", arr.ind=TRUE),
    dim(maze),
    return.visited
  )
}
walk.maze.inner <- function(start.position, obstacles, dims, return.visited) {
  # tracking the # walked squares is slow (at least how I've implemented it...)
  # so don't do it unless we have to
  walked <- NULL
  if (return.visited) {
    walked <- array(FALSE, dim=dims)
  }
  current.position <- start.position
  dir <- "up"
  finished <- FALSE
  seen <- array(FALSE, c(4, prod(dims)))
  rownames(seen) <- c("up", "right", "down", "left")
  
  while (!finished) {
    current.i <-ij2i(current.position[1, "row"], current.position[1, "col"], dims)
    if (seen[dir, current.i]) {
      break
    }
    seen[dir, current.i] <- TRUE
    
    axis <- ifelse(dir %in% c("up", "down"), 1, 2) # rows or cols
    
    if (dir %in% c("up", "left")) {
      final.coord <- suppressWarnings(
          max(obstacles[obstacles[, axis] < current.position[1, axis] & obstacles[, -axis] == current.position[1, -axis], axis])
      ) + 1
      if (is.infinite(final.coord)) {
        final.coord <- 1
        finished <- TRUE
      }
    } else {
      final.coord <- suppressWarnings(
        min(obstacles[obstacles[, axis] > current.position[1, axis] & obstacles[, -axis] == current.position[1, -axis], axis])
      ) - 1
      if (is.infinite(final.coord)) {
        final.coord <- dims[axis]
        finished <- TRUE
      }
    }
    
    if (return.visited) {
      if (axis == 1) {
        walked[ current.position[1, "row"]:final.coord, current.position[1, "col"] ] <- TRUE
      } else {
        walked[ current.position[1, "row"], current.position[1, "col"]:final.coord ] <- TRUE
      }
    }
  
    # walk  
    current.position[1, axis] <- final.coord
    
    # turn right
    dir <- c(up="right", right="down", down="left", left="up")[dir]
  }
  return(list(is.loop=!finished, walked=walked))
}

# part 1 (NB I am only handling starting with ^ but meh)
maze <- do.call(rbind, stri_split_boundaries(input, type="character"))
walked <- walk.maze(maze, return.visited=TRUE)$walked
print(sum(walked))

# part 2
induces.loop <- function (added.obstacle, maze) {
  walked <- walk.maze.inner(
    which(maze == "^", arr.ind=TRUE),
    rbind(which(maze == "#", arr.ind=TRUE), added.obstacle),
    dim(maze),
    return.visited=FALSE
  )
  return(walked$is.loop)
}

candidate.obstacles <- which(maze == ".", arr.ind=TRUE)
# Ben's smart way that I didn't think of
# candidate.obstacles <- which(walked & maze == ".", arr.ind=TRUE)

# wow that's slow
print(sum(
  vapply(1:nrow(candidate.obstacles), function (i) {
    #print(i)
    induces.loop(candidate.obstacles[i, ,drop=FALSE], maze)
  },
  FUN.VALUE=FALSE)
))
