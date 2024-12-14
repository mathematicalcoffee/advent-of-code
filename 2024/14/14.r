rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_data <- function (lines) {
  return(
    data.table(do.call(rbind, stri_match_all(regex="p=(?<x>[0-9]+),(?<y>[0-9]+) v=(?<vx>-?[0-9]+),(?<vy>-?[0-9]+)", lines))[, -1])[, lapply(.SD, as.integer)]
  )
}

final_position <- function (df, nsteps, limxy) {
  return(df[,
      .(
        x=(x + vx * nsteps) %% limxy[1],
        y=(y + vy * nsteps) %% limxy[2],
        vx=vx,
        vy=vy
      )
  ])
}
freq_by_quarter <- function (df, dims) {
  mid <- floor(dims / 2)
  f[, qx := ifelse(x < mid[1] & y < mid[2], 'NW',
            ifelse(x < mid[1] & y > mid[2], 'SW',
            ifelse(x > mid[1] & y > mid[2], 'SE',
            ifelse(x > mid[1] & y < mid[2], 'NE',
            'unknown'))))]
  f[, .N, by=.(qx)]
}
draw_positions <- function (df, dims) {
  pic <- array(".", dim=rev(dims))
  freq <- df[, .N, by=.(x, y)]
  pic[as.matrix(freq[, .(y + 1, x + 1)])] <- freq$N
  print_matrix(pic)
  return(invisible(pic))
}
dat <- parse_data(ex[[1]])
dims <- c(11, 7) # x, y
f <- final_position(dat, 100, dims)
draw_positions(f, dims)
fr <- freq_by_quarter(f, dims)
print(fr[qx != "unknown", prod(N)])

# --- input ---
dat <- parse_data(input)
dims <- c(101, 103) # x, y
f <- final_position(dat, 100, dims)
# draw_positions(f, dims)
fr <- freq_by_quarter(f, dims)
print(fr[qx != "unknown", prod(N)])

# --- part 2 ---
# this is really dumb. it would sitll look like a christmas tree with 2-9s on
#  it, has the man never heard of ascii art?
df <- dat
for (ns in 1:100000000) {
  df <- final_position(df, 1, dims)
  if (all(df[, .N, by=.(x, y)]$N == 1)) { 
    draw_positions(df, dims)
    r <- readline(prompt=sprintf("%i: Enter to proceed, STOP to stop", ns))
    if (tolower(r) == "stop")
      break
  }
}
# 1286, 3541, 6587
pic <- draw_positions(df, dims)
write(t(pic), file="tree.txt", ncolumns=dim(pic)[2])
library(ggplot2)
ggplot(df, aes(x=x,y=y)) +
  geom_tile(fill='darkgreen') +
  theme_minimal() + coord_fixed() +
  scale_x_continuous(breaks=0:dims[1] - .5, minor_breaks=NULL, labels=NULL, name=NULL) +
  scale_y_reverse(breaks=0:dims[2] - .5, minor_breaks=NULL, labels=NULL, name=NULL) +
  ggtitle("day 14 sux")
ggsave("day14.png")
