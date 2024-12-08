rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

find.antinodes <- function (ddf) {
  ddf[, .(row=row + c(-1, 1) * diff(row), col=col + c(-1, 1) * diff(col))]
}

# I'm a dirty statistician and I love it
find.antinodes.part2 <- function (ddf, dims) {
  m <- lm(col ~ row, ddf)
  data.table(
    row=1:dims[1],
    col=predict(m, data.frame(row=1:dims[1]))
  )
}

grid <- lines2matrix(input)
dims <- dim(grid)
freqs <- unique(grid[ grid != "."])

antenna.coords <- which(grid != ".", arr.ind=TRUE)
df <- cbind(data.table(antenna.coords), freq=grid[antenna.coords])

antinodes <- df[,
  rbindlist(combn(.N, 2, function (x) { find.antinodes(.SD[x]) }, simplify=FALSE)),
  by=.(freq)
][row <= dims[1] & row > 0 & col <= dims[2] & col > 0]
print(uniqueN(antinodes, by=c("row", "col")))

antinodes2 <- df[,
  rbindlist(combn(.N, 2, function (i) { find.antinodes.part2(.SD[i], dims) }, simplify=FALSE)),
  by=.(freq)
][is_wholenumber(col)]
antinodes2[, col := round(col)]
antinodes2 <- antinodes2[row <= dims[1] & row > 0 & col <= dims[2] & col > 0]
print(uniqueN(antinodes2, by=c("row", "col")))

ans <- array(".", dim=dims)
for (freqq in unique(df$freq)) {
  ans[as.matrix(antinodes2[freq == freqq, .(row, col)])] <- "#"
}
print_matrix(ans)
