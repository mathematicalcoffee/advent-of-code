rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

to.grid <- function (x) {
  do.call(rbind, stri_split_boundaries(x, type="character"))
}

# only in one direction
find.words <- function (grid, word) {
  starts <- which(grid == word[1], arr.ind=T)
  dims <- dim(grid)
  nfound <- 0
  n <- length(word)
  
  for (i in 1:nrow(starts)) {
    start <- starts[i,]
      # horizontal forward
      if ((start["col"] + n - 1) > dims[2]) {
      } else {
        if (all(grid[start["row"], start["col"] + seq_len(n) - 1] == word)) {
          nfound <- nfound + 1
          message(sprintf("horizontal, %i %i", start["row"], start["col"]))
        }
        
        
        # SE (arbitrary)
        if ((start["row"] + n - 1) > dims[1]) {
        } else {
          if (all(grid[ij2i(start["row"] + seq_len(n) - 1, start["col"] + seq_len(n) - 1, dims)] == word)) {
            nfound <- nfound + 1
            message(sprintf("SE, %i %i", start["row"], start["col"]))
          }
        }
      }
  }
  message("")
  return(nfound)
}

word.search <- function (grid, word) {
  dims <- dim(grid)
  return(
  # E + SE
  find.words(grid, word)
  # W + NW
  + find.words(grid[dims[1]:1, dims[2]:1], word)
  # S + SW
  + find.words(t(grid)[, dims[1]:1], word)
  # N + NE
  + find.words(t(grid)[dims[2]:1, ], word)
  )
}

XMAS <- c("X","M","A","S")

message("Part 1 - example")
ex_grid <- to.grid(ex[[2]])
message(suppressMessages(word.search(ex_grid, XMAS)))
message("Part 2 - input data")
grid <- to.grid(input)
message(suppressMessages(word.search(grid, XMAS)))

# fuck me
x.mas <- c(
"
M.S
.A.
M.S
",
"
S.S
.A.
M.M
",
"
M.M
.A.
S.S
",
"
S.M
.A.
S.M
"
)
blocks <- lapply(
  x.mas,
  function (x) {
    do.call(rbind, stri_split_boundaries(stri_split_fixed(pattern="\n", stri_trim(x))[[1]], type="character"))
  }
)

locate.block <- function (grid, block) {
  nfound <- 0
  grid.size <- dim(grid)
  block.size <- dim(block)
  if(any(block.size > grid.size))
    return(0)
  
  block.coords <- which(block != ".", arr.ind=TRUE)
  block.i <- ij2i(block.coords[, "row"], block.coords[, "col"], grid.size)
  block.v <- block[block != "."]
  
  
  for (i in 1:(grid.size[1] - block.size[1] + 1)) {
    for (j in 1:(grid.size[2] - block.size[2] + 1)) {
      if(all(
        grid[ij2i(i, j, grid.size) + block.i - 1]
       == block.v)) {
        nfound <- nfound + 1
      }
    }
  }
  return(nfound)
}

message("Part 2 - example")
message(sum(unlist(lapply(blocks, locate.block, grid=ex_grid))))
message("Part 2 - input")
message(sum(unlist(lapply(blocks, locate.block, grid=grid))))
