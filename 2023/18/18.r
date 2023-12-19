rm(list = ls())
source("../handy.R")
library(data.table)
library(ggplot2)

# Idea: work out the (i, j) coordinates of each tile, being the centre-point,
#  but then convert to bounding-box coordinates (x-y) i.e. dilate the solid by
#  0.5 and calculate the actual area in euclidean space
#  using the wizardry known as Gauss' shoelace formula.

input_file <- "input.txt"
PART2 <- TRUE
plan <- fread(
  input_file,
  header = FALSE,
  sep = " ",
  col.names = c("direction", "length", "colour")
)

plan[, step := seq_len(.N)]
plan[, colour := substring(colour, 2, 8)]
setkey(plan, step)

if (PART2) {
  # part 2
  plan[, length := strtoi(substring(colour, 2, 6), base = 16)]
  plan[
    ,
    direction := c("R", "D", "L", "U")[as.numeric(substring(colour, 7, 7)) + 1]
  ]
}

plan[
  ,
  dj := ifelse(direction == "R", length, ifelse(direction == "L", -length, 0))
]
plan[
  ,
  di := ifelse(direction == "D", length, ifelse(direction == "U", -length, 0))
]
plan[, j := c(0, head(cumsum(dj), -1))]
plan[, i := c(0, head(cumsum(di), -1))]
plan[, prev_direction := c(direction[.N], tail(shift(direction), -1))]

# the y is still 0 at top and decreasing down like ij
plan[, `:=`(x = NA_real_, y = NA_real_)]
plan[prev_direction == "D" | direction == "D", x := j + 0.5] # on right side, dilate right
plan[prev_direction == "U" | direction == "U", x := j - 0.5] # on left side, dilate left
plan[prev_direction == "R" | direction == "R", y := i - 0.5] # on top side, dilate up
plan[prev_direction == "L" | direction == "L", y := i + 0.5] # on bottom side, dilate down

# check !
style_base <- list(scale_y_reverse(), coord_fixed(), theme_minimal())
print(
  ggplot(plan, aes(x = j, y = i)) +
    geom_polygon() +
    geom_path(aes(x = x, y = y)) +
    style_base
)

# cheese it.
# library(splancs)
# poly <- as.matrix(plan[, .(x, y)])
# get area
# print(areapl(poly), digits = 20)

# "Shoelace formula" - WOW
plan[, prev_y := c(y[.N], tail(shift(y), -1))]
plan[, next_y := c(head(shift(y, -1), -1), y[1])]
print(plan[, 1 / 2 * sum(x * (next_y - prev_y))], digits = 20)
# 42617947302920

# ----- colour it in and fill it with lava for fun ---- #
plan[, prev_x := c(x[.N], tail(shift(x), -1))]
midpoint <- function(x) {
  diff(range(x)) / 2 + min(x)
}

print(
  ggplot(plan, aes(x = j, y = i)) +
    scale_y_reverse() +
    coord_fixed() +
    geom_polygon(fill = "red") +
    geom_segment(
      aes(x = prev_x, y = prev_y, xend = x, yend = y),
      linewidth = 2,
      colour = plan$colour
    ) +
    annotate(
      "text",
      x = midpoint(plan$x), y = midpoint(plan$y),
      label = "ლ( `Д´ ლ) ",
      col = "white", fontface = "bold", size = 6, hjust = 0.9
    ) +
    theme_minimal()
)
# ggsave("day18.png")
