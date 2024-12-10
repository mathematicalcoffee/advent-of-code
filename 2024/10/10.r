rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

# UGH this is the number of ways to get there, not the number of distinct
#  trailheads you can reach
# hey cool! that was part 2 LOL (I wrote this for part 1)
num.paths.to.reach <- function (curr.i, curr.j, prev.val, map, dims, target.val=9) {
  if (!in_bounds(curr.i, curr.j, dims))
    return(0) # OOB
  curr.val <- map[curr.i, curr.j]
  if ((curr.val - prev.val) != 1) # too steep
    return(0)
  if (curr.val == target.val) # can reach!
    return(1)
  
  return(
    num.paths.to.reach( # SOUTH
      curr.i, curr.j + 1, curr.val, map, dims
    ) +
    num.paths.to.reach( # NORTH
      curr.i, curr.j - 1, curr.val, map, dims
    ) +
    num.paths.to.reach( # WEST
      curr.i - 1, curr.j, curr.val, map, dims
    ) +
    num.paths.to.reach( # EAST
      curr.i + 1, curr.j, curr.val, map, dims
    )
  )
}

can.reach <- function(curr.i, curr.j, prev.val, map, dims, target.i, target.j) {
  if (!in_bounds(curr.i, curr.j, dims))
    return(FALSE) # OOB
  curr.val <- map[curr.i, curr.j]
  if ((curr.val - prev.val) != 1) # too steep
    return(FALSE)
  if (curr.i == target.i & curr.j == target.j) # can reach!
    return(TRUE)
 
  # if we can reach then just quit and don't explore the other directions 
  if (can.reach( # SOUTH
      curr.i, curr.j + 1, curr.val, map, dims, target.i, target.j
    )) return(TRUE)
  if(can.reach( # NORTH
      curr.i, curr.j - 1, curr.val, map, dims, target.i, target.j
    )) return(TRUE)
  if(can.reach( # WEST
      curr.i - 1, curr.j, curr.val, map, dims, target.i, target.j
    )) return(TRUE)
  if(can.reach( # EAST
      curr.i + 1, curr.j, curr.val, map, dims, target.i, target.j
    )) return(TRUE)
  return(FALSE)
}

# map <- lines2matrix(ex[[5]], numeric=TRUE)
map <- lines2matrix(input, numeric=TRUE)

trailheads <- which(map == 0, arr.ind=TRUE)
peaks <- which(map == 9, arr.ind=TRUE)
o <- sapply(
  1:nrow(trailheads),
  function (th.i) {
    sum(
      sapply(
        1:nrow(peaks),
        function (peak.i) {
          can.reach(
            trailheads[th.i, 1],
            trailheads[th.i, 2],
            -1, # prev.val
            map,
            dim(map),
            target.i=peaks[peak.i, 1],
            target.j=peaks[peak.i, 2]
          )
        }
      )
    )
  }
)
print(sum(o)) # part 1. for some reason much slower than part 2.
              # oh, because we keep re-taking invalid paths for new peaks.



o2 <- sapply(
  1:nrow(trailheads),
  function (th.i) {
    num.paths.to.reach(
      trailheads[th.i, 1],
      trailheads[th.i, 2],
      -1, # prev.val
      map,
      dim(map)
    )
  }
)
print(sum(o2)) # part 2.

# it would have been cool to construct a graph
