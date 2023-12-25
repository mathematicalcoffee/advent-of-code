rm(list = ls())
source("../handy.R")
library(stringi)
library(data.table)

input_file <- "input.txt"
if (input_file == "input-small.txt") {
  bounds <- c(7, 27)
} else {
  bounds <- c(200000000000000, 400000000000000)
}

df <- fread(
  text = gsub("@", ",", readLines(input_file)),
  col.names = c("x", "y", "z", "vx", "vy", "vz"),
  header = FALSE
)
df[, id := seq_len(.N)]

# shift everyone
# do I have to use the same scale in each dimension ?
dx <- scale(c(df$x, bounds), scale = TRUE, center = FALSE)
dy <- scale(c(df$y, bounds), scale = attr(dx, "scaled:scale"), center = FALSE)
dz <- scale(c(df$z, bounds), scale = attr(dx, "scaled:scale"), center = FALSE)
df[, og_x := x]
df[, og_y := y]
df[, og_z := z]
df[, x := head(dx, -2)]
df[, y := head(dy, -2)]
df[, z := head(dz, -2)]

bounds_x <- tail(dx, 2)
bounds_y <- tail(dy, 2)

# re-parameterise ax + by + c = 0
df[, mx := -vy]
df[, my := vx]
df[, c := -vx * y + vy * x]

setkey(df, id)
df[, dmy := 1]

stopifnot(all(df$vx != 0))
stopifnot(all(df$vy != 0))

# dummy table for cartesian product (compare every hailstone to every other)
res <- merge(
  df, df,
  by = "dmy", suffixes = c("1", "2"), allow.cartesian = TRUE
)[id1 < id2] # comparing A to B is the same as B to A
res <- res[
  ,
  tryCatch(
    {
      c(
        .SD,
        # my m/c are mx + my + c = 0, `solve` solves Ax = b (so b is -c)
        # we are solving A x = b where A/b = the 2 lines in question and x
        #  (the same x for both lines) is the intersection
        setNames(as.list(solve(
          matrix(c(mx1, mx2, my1, my2), nrow = 2, byrow = FALSE),
          -c(c1, c2)
        )), c("x", "y"))
      )
    },
    error = function(e) NULL
  ),
  by = .(i = 1:nrow(res)) # seq_len doesno't work
]
res[
  x %between% bounds_x &
    y %between% bounds_y &
    ifelse(sign(vx1) == 1, x > x1, x < x1) & # must be in hailstone 1's future
    ifelse(sign(vx2) == 1, x > x2, x < x2), # as well as hailstone 2's future
  print(.N)
]
# 27732

# ----------------------------------
unscale <- function(x, scalevec) {
  m <- attr(scalevec, "scaled:scale")
  if (is.null(m)) m <- 1
  c <- attr(scalevec, "scaled:center")
  if (is.null(c)) c <- 0
  return(m * x + c)
}

# Ugh, these are not linear in the unknowns. have unknown-vel * unknown-time of
#   intersection.
# t = (x0-x1)/(vx1-vx0)
# t = (y0-y1)/(yx1-yx0)
# t = (z0-z1)/(zx1-zx0)
# Different t for each set of lines
# so (x0 - x1)/(vx1 - vx0) = (y0-1)
#
# Parametrised intersections
# Line * with line 1.
# x0[*] + v[*,x] t[1] = x0[1] + v[1,x] t[1]
# y0[*] + v[*,y] t[1] = y0[1] + v[1,y] t[1]
# z0[*] + v[*,z] t[1] = z0[1] + v[1,z] t[1]
# 7 unknowns (x0/y0/z0/vx/vy/vz* and t1)
# Line * with line 2
# x0[*] + v[*,x] t[2] = x0[2] + v[2,x] t[2]
# y0[*] + v[*,y] t[2] = y0[2] + v[2,y] t[2]
# z0[*] + v[*,z] t[2] = z0[2] + v[2,z] t[2]
# +1 unknown (t2)
# Line * with line 3
# x0[*] + v[*,x] t[3] = x0[3] + v[3,x] t[3]
# y0[*] + v[*,y] t[3] = y0[3] + v[3,y] t[3]
# z0[*] + v[*,z] t[3] = z0[3] + v[3,z] t[3]
# +1 unknown (t3)
# So # equations = 3 per hailstone
# So # unknowns = 6 + 1 per hailstone
# need 3 hailstones minimum.
# And since it's AoC probably any 3 (non-parallel) will do and produce the same
#  unique answer.

# But how to solve these?? just invoke a solver?? they are not linear so
#  linear algebra will not work.
#
# Option 1 - calc the min-dist between 2 lines (magic-line and hailstone line)
#  and minimise the sum of squares of these for all hailstone lines (knowing
#  that in fact the true answer is 0)
# Advantage - no time variable (but it's AoC so I'm pretty sure that there will
#  only be one such line and it will just so happen to work out with the times)
# Disadvantage - no time variable, CONVERGENCE ISSUES (stopping too early,
#  we /know/ there is a 0 as an answer)
library(pracma) # to get actual crossproduct (R's crossprod isn't a euclidean
#                 cross-product, it's a statsy one)
# 2 lines a + tv and b + su (t,s scalar)
# min-dist is given by

norm2 <- function(x) sqrt(crossprod(x))
min_dist <- function(off1, coef1, off2, coef2) {
  cp <- cross(coef1, coef2)
  abs(((off1 - off2) %*% cp) / suppressWarnings(norm2(cp)))[1, 1]
}
# call suppressWarnings, getting problem in norm2 - the sqrt has negatives in
#  the off-diagonal, not that we care.
min_dist_vectorised <- function(off1, coef1, off2, coef2) {
  # rbind the things together
  cp <- t(cross(coef1, coef2))
  abs(diag((off1 - off2) %*% cp) / diag(suppressWarnings(norm2(cp))))
}

total_min_dist_from_hailstone_trajectories <- function(line, df, scaled = TRUE) {
  # cost function: min-dist between the lines (sum of)
  # line = (x0 y0 z0 vx vy vz)
  offs <- matrix(rep(line[1:3], each = nrow(df)), ncol = 3)
  coef <- matrix(rep(line[4:6], each = nrow(df)), ncol = 3)

  if (scaled) {
    df[
      ,
      sum(
        min_dist_vectorised(
          offs,
          coef,
          matrix(c(x, y, z), ncol = 3),
          matrix(c(vx, vy, vz), ncol = 3)
        )
      )
    ]
  } else {
    df[
      ,
      sum(
        min_dist_vectorised(
          offs,
          coef,
          matrix(c(og_x, og_y, og_z), ncol = 3),
          matrix(c(vx, vy, vz), ncol = 3)
        )
      )
    ]
  }
}
# validation - evaluating at known solution on sample data produces 0.
# even on sample data, this minimiser ends too early (best I got was a cost
# of 1e-6, but the answers were not near the real answers! solution space is
# weirdly shaped or not scaled properly in one of the dimensions) -
# I /know/ the min is 0
# the `z` is a max because you gotta start above or below all of them probably
init <- df[, c(mean(x), mean(y), max(z), mean(vx), mean(vy), mean(vz))]
suppressWarnings({
  print(
    o <<- fminunc(
      init,
      total_min_dist_from_hailstone_trajectories,
      df = df,
      tol = .Machine$double.eps,
      # if you want to wait for ages to still not get a good-enough answer
      # (tolerance 6.3e-6, Rvmminu convered)
      # maxiter=1e5,
      # maxfeval=1e5
    )
  )
})
get_answer <- function(pars, scale_x, scale_y, scale_z) {
  # scale_foo is from calling scale() on things and looks for the scaled:center
  #  and scaled:scale attrs
  c(
    unscale(pars[1], scale_x),
    unscale(pars[2], scale_y),
    unscale(pars[3], scale_z),
    pars[4],
    pars[5],
    pars[6]
  )
}
# 273601097170614.3
# 409011973747401.9
# 460644797289383.9
# -5.9, 29.5, 46 (no-where near my eventual answers)
sprintf(
  "%.1f",
  get_answer(o$par, dx, dy, dz)
)


# hmm fnscale=1 on sample data gives /almost/ good answers, but in general the
#  answers vary wildly with how I tweak the various fnscale/parscale/iter
#  parameters, and I'm not experienced enough to know what they should be.
#
# BFGS was the best of these (and fminunc uses quasi-newton too)
# NOTE: my scaled x/y/z and unscaled vx/vy/vz different magnitudes.
# Reports "convergence" at a f-val of 0.035 :/
suppressWarnings({
  print(o <<- optim(
    c(0, 0, 0, 1, 1, -1), # try a different initial value?
    total_min_dist_from_hailstone_trajectories,
    df = df,
    control = list(
      abstol = .Machine$double.eps, reltol = .Machine$double.eps, maxit = 1e3
    ),
    method = "BFGS"
  ))
})
# 177338673438191.8
# 890513341290425.5
# 1210471542321036.5
# 1.1, -5.5, -8.6
sprintf(
  "%.1f",
  get_answer(o$par, dx, dy, dz)
)

# OPTION 2 ------ equations with time
# minimise the distance between the rock and each hailstone at the "collision"
#  time (also solving for the time of collision)
# way more parameters (one time per hailstone + the original line parameters)

total_dist_between_rock_and_hailstones_at_collision <- function(par, df) {
  # x0 y0 z0 vx vy vz t1 t2 t3 ... tn
  # (n = number of lines, t = time of intersetion with that line)
  ts <- tail(par, -6)
  df[
    ,
    sum(
      ((x + vx * ts) - (par[1] + par[4] * ts))^2 +
        ((y + vy * ts) - (par[2] + par[5] * ts))^2 +
        ((z + vz * ts) - (par[3] + par[6] * ts))^2
    )
  ]
}

# RESULT - similar problems to the previous, couldn't converge, or converges
#  but at a very small function value that is nevertheless not 0.
# also takes ages

# this is 0, so we really do want an f-value of 0 at the reported minimum
# min_dist_from_hailstone_trajectories(c(24, 13, 10, -3, 1, 2), df)
# all of these just took ages to run and essentially ran out at very high fn-
#  values (the solution space is too large)
suppressWarnings({
  print(
    o <<- fminunc(
      c(0, 0, 0, 1, 1, -1, rep(10, nrow(df))),
      total_dist_between_rock_and_hailstones_at_collision,
      df = df
    )
  )
})
# wildly off, f-val of 23+
sprintf("%.1f", get_answer(o$par, dx, dy, dz))

# somehow initialising like this gives an f-value of 756million lol
suppressWarnings({
  print(
    o <<- optim(
      c(init, rep(10, nrow(df))), # arbitrary collision at 10s guess
      total_dist_between_rock_and_hailstones_at_collision,
      df = df,
      method = "BFGS"
    )
  )
})
sprintf("%.1f", get_answer(o$par, dx, dy, dz))

# ----------------- OPTION 3 - nonlinear system of equations ------------------
# so instead of solving a univariate equation we try to solve a system of
#  nonlinear equations (in that the unknowns are nonlinear)

fit_line_nonlinear <- function(par, df) {
  # x0 y0 z0 vx vy vz t1 t2 t3 ... tn
  # (n = number of lines, t = time of intersection with that line)
  # produces 3*n values,
  #  x_rock - x_hailstone, y_rock - y_hailstone, z_rock - z_hailstone
  #
  ts <- tail(par, -6)
  df[
    , c(
      ((x + vx * ts) - (par[1] + par[4] * ts))^2,
      ((y + vy * ts) - (par[2] + par[5] * ts))^2,
      ((z + vz * ts) - (par[3] + par[6] * ts))^2
    )
  ]
}
# verify 0 for sample data
fit_line_nonlinear(c(24, 13, 10, -3, 1, 2, 5, 3, 4, 6, 1), df)

# got down to 2.2e-09, maybe try again from here?
# (none of the unscaled answers are close to integer)
print(o <<- fsolve(
  fit_line_nonlinear,
  c(init, rep(1, nrow(df))),
  df = df,
  maxiter = 1000
))
sprintf("%.1f", get_answer(o$x, dx, dy, dz))

# try again from these
print(o <<- fsolve(
  fit_line_nonlinear,
  o$x,
  df = df,
  maxiter = 1000,
  tol = .Machine$double.eps
))
# got down to E-21 this time, much quicker
# (or maybe it was a few rounds of this)
sprintf("%.1f", get_answer(o$x, dx, dy, dz))
# 315844786844943.9
# 196475302013986.0
# 129300312660524.6
# -32.0
# 161.0
# 251.0
# no dice - I mean when all the original numbers are E+14 ofc all of this detail
#  is going to get lost
total_min_dist_from_hailstone_trajectories(
  c(315844786844944, 196475302013986, 129300312660525, -32, 161, 251),
  df,
  scaled = FALSE
)

# Maybe I can scale manually to a more friendly factor?
print(o <<- fsolve(
  fit_line_nonlinear,
  c(31584.48, 19647.53, 12930.03, -32, 161, 251, rep(1, nrow(df))),
  df = df[, .(x = og_x / 1e10, y = og_y / 1e10, z = og_z / 1e10, vx, vy, vz)],
  maxiter = 1000
))

# just reduce the # of equations? (should only need 3 hailstones)
df2 <- df[1:3]

# Manual scaling? and just reduce the # of equations
#  (should only need 3 hailstones)
print(o <<- fsolve(
  fit_line_nonlinear,
  c(1, 1, 1, 1, 1, 1, rep(1, nrow(df2))),
  df = df2[, .(x = og_x / 1e10, y = og_y / 1e10, z = og_z / 1e10, vx, vy, vz)],
  maxiter = 1000
))
# run this a couple times to see if things improve
print(o <<- fsolve(
  fit_line_nonlinear,
  o$x,
  df = df2[, .(x = og_x / 1e10, y = og_y / 1e10, z = og_z / 1e10, vx, vy, vz)],
  maxiter = 1000
))
# OK it tdidn't like it because it says there is a singular matrix, maybe the
#  parallel lines ? (they have a different `t` though).
# Let's just pick the first 3 hailstones? (You only need 3 and since it's AoC
#  we know there is a solution and it is unique)
for (i in 1:10) {
  print(o <<- fsolve(
    fit_line_nonlinear,
    o$x[1:9],
    df = df2[1:3, .(x = og_x / 1e10, y = og_y / 1e10, z = og_z / 1e10, vx, vy, vz)],
    maxiter = 1000,
    tol = .Machine$double.eps
  ))
}
# 315844328917588.0625
# 196475852174059.0625
# 129299668674521.01562
print(o$x[1] * 1e10, digits = 20)
print(o$x[2] * 1e10, digits = 20)
print(o$x[3] * 1e10, digits = 20)
print(o$x[4:6]) # -32 161 251
# 7e+16
total_min_dist_from_hailstone_trajectories(
  c(315844328917588, 196475852174059, 129299668674521, -32, 161, 251),
  df,
  scaled = FALSE
)
fit_line_nonlinear(
  c(315844328917588, 196475852174059, 129299668674521,
    -32, 161, 251, tail(o$x, -6)),
  df[1:3, .(x = og_x, y = og_y, z = og_z, vx, vy, vz)]
)
# OK, idk why but these both gave huge numbers, and yet are the correct answer,
#  maybe the times scale too - but the first one (time-agnostic) should have
#  worked
# I kind of wanted to verify my unscaled answers without having to know
#  the times because that was too much work.
# Suppose I could take my part1 answer but cbf

# **** OK FINAL DIRTY ATTEMPT because I've had enough and using a 3rd party
#  solver just isn't fun for AoC (and coding one myself seems ridiculous)
# - print some stuff to copypaste into sagemath/wolfram alpha
paste(df[1:3][,
  # the %s for integer64?!
  sprintf(
    # e.g. A + B t = 123 + 456 t (x/vx, y/vy, z/vz for each line)
    "%s + %.0f*%s = %s + %s*%s",
    c(og_x, og_y, og_z),
    c(vx, vy, vz),
    letters[.BY$i],
    LETTERS[1:3],
    LETTERS[4:6],
    letters[.BY$i]
  ),
  by = .(i = 1:3)
]$V1, collapse = ", ")
# 315844328917588
# 196475852174059
# 129299668674521
# wat, same answer as the previous
