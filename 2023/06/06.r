rm(list = ls())
lines <- readLines("input.txt")

# FUCK YEAH MATHS
# t such that -t^2 + Tt - d > 0
# T is max time, d is the min distance

# part 1
# max_times <- as.numeric(strsplit(strsplit(lines[1], " *: *")[[1]][2], " +")[[1]])
# min_distances <- as.numeric(strsplit(strsplit(lines[2], " *: *")[[1]][2], " +")[[1]])

# part 2
max_times <- as.numeric(
  gsub(" +", "", strsplit(lines[1], " *: *")[[1]][2])[[1]]
)
min_distances <- as.numeric(
  gsub(" +", "", strsplit(lines[2], " *: *")[[1]][2])[[1]]
)

dist.achieved <- function(t, max_time) max_time * t - t^2

roots <- lapply(
  seq_along(max_times), function(i) {
    polyroot(c(-min_distances[i], max_times[i], -1))
  }
)
# check for non-real solutions
# derp polynomials like this always got 2 real solutions.
stopifnot(!any(sapply(roots, function(rs) any(abs(Im(rs)) > 1e-6))))
# check for negatives but doubt it
stopifnot(!any(sapply(roots, function(rs) any(Re(rs) < 0))))

real_roots <- sapply(roots, Re)
# round to nearest seconds
real_roots[1, ] <- ceiling(real_roots[1, ])
real_roots[2, ] <- floor(real_roots[2, ])

# then check for > vs >= and bump up/down accordingly
lower <- mapply(dist.achieved, real_roots[1, ], max_times)
upper <- mapply(dist.achieved, real_roots[2, ], max_times)
real_roots[1, lower == min_distances] <- real_roots[1, lower == min_distances] + 1
real_roots[2, upper == min_distances] <- real_roots[2, upper == min_distances] - 1

real_roots[1, ] <- real_roots[1, ] + ifelse(lower == min_distances, 1, 0)
real_roots[2, ] <- real_roots[2, ] - ifelse(upper == min_distances, 1, 0)

# le answer
print(prod(real_roots[2, ] - real_roots[1, ] + 1))
