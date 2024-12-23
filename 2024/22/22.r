rm(list=ls())
library(data.table)
source("../../handy.R")
library(bit64)
ex <- get_and_save_example_input()
input <- get_and_save_input()

evolve <- function (secret) {
  # multiply by 64 & mix  = add 6 bits and xor with itself
  # NB 64 = 2^6
  secret <- mix(secret * 64, secret)
  # prune: keep first 24 bits
  secret <- prune(secret)
  # divide by 32 32 = 2^5 = chop off last 5 bits & xor with itself
  secret <- mix(floor(secret / 32), secret)
  # prune: keep first 24 bits
  secret <- prune(secret)
  # multiply by 2^11 (add 11 bits) and xor with itself
  secret <- mix(secret * 2048, secret)
  # # keep first 24 bits
  secret <- prune(secret)
  secret 
}

mix <- function(secret, value) {
  stupid_xor(secret, value)
}
prune <- function (secret) {
  # keep first 24 bits
  secret %% 16777216
}

twos <- sapply(1:32, function (pow) 2^pow)
evolve2 <- function (secret) {
  # multiply by 64 & mix  = add 6 bits and xor with itself
  # prune: keep first 24 bits
  # NB 64 = 2^6
  secret <- mix_mult_then_prune(secret, 6)
  
  # divide by 32 32 = 2^5 = chop off last 5 bits & xor with itself
  secret <- mix_div_then_prune(secret, 5)
  
  # multiply by 2^11 (add 11 bits) and xor with itself
  secret <- mix_mult_then_prune(secret, 11)
  secret 
}
mix_mult_then_prune <- function (secret, pow) {
  # ASSUMES INTEGER TYPE
  stopifnot(pow < 24) # something like that, it's signed int
  # rightpad with `pow` bits (mult by 2^pow)
  # but then only keep the first 24
  # means I may as well just retain bits 1 up to (24 - pow) and chuck `pow` 0s on the end
  # allowing me to stay in int32 space
  # R = truncating, L = padding
  bitwXor(
    bitwShiftL(
      retain_first_n_bits(secret, 24 - pow),
      pow
    ),
    secret
  )
}
mix_div_then_prune <- function (secret, pow) {
  # chop the last X digits off, xor with self, prune
  retain_first_n_bits(
    bitwXor(
      secret,
      bitwShiftR(secret, pow)
    ),
    24
  )
}
retain_first_n_bits <- function (secret, n) {
  # and it with a mask
  bitwAnd(secret, twos[n] - 1)
}

secret_n <- function (secret, n) {
  for (i in 1:n) {
    # print(secret)
    secret <- evolve2(secret)
  }
  secret
}


# ----
# PART 1: SLOW
# seeds <- as.numeric(input)
s <- 123
stopifnot((s <- evolve(s)) == 15887950)
stopifnot((s <- evolve(s)) == 16495136)
stopifnot((s <- evolve(s)) == 527345)
stopifnot((s <- evolve(s)) == 704524)

ex <- c(1, 10, 100, 2024)
stopifnot((o <- sapply(ex, secret_n, n=2000)) == c(8685429, 4700978, 15273692, 8667524))
sum(o)

input <- as.numeric(input)
if (F) { # SLOW
  o <- rep(NA_real_, length=length(seeds))
  for (i in seq_along(input))
    o[i] <- secret_n(input[i], 2000)
  print(sum(o))
}

# ----------- PART 2 ----------------- #
# just store, for all sets of 4-char in the target sequence, the price (per buyer)
# so cache[one,two,three,four,buyeri] = price for `buyeri` after the first occurence
#   of the diff sequence `one,two,three,four`
# then sum(cache[one,two,three,four, ]) is the # banans for that sequence
# I thought that a matrix would be the fastest way to store this but it seems
#  very very slow in R for some reason, whereas pasting the one/two/three/four
#  to a string and storing in an environment/cache is faster, which is quite
#  upsetting to discover.

price_from_secret <- function (secret) {
  secret %% 10
}
calc_prices <- function(seeds, nreps) {
  # stores the last digit
  nbuyers <- length(seeds)
  prices <- matrix(NA_real_, nrow=nreps + 1, ncol=nbuyers)
  
  for (buyer_i in seq_len(nbuyers)) {
    secret <- seeds[buyer_i]
    prices[1, buyer_i] <- price_from_secret(secret)
    
    for (secret_i in (seq_len(nreps) + 1)) {
      secret <- evolve2(secret)
      prices[secret_i, buyer_i] <- price_from_secret(secret)
    }
  }
  prices
}

FLOOR <- 10
keyf <- function(target_seq) collapse(target_seq)
get_price_lookup <- function(target_seq, buyeri, cache, .key) {
  if (missing(.key))
    .key <- keyf(target_seq)
  if (is.null(cache[[.key]]))
    return(NA) # Hmm.
  cache[[.key]][buyeri]
}
set_price_lookup_in_cache <- function(target_seq, buyeri, price, cache, nbuyers, .key) {
  if (missing(.key))
    .key <- keyf(target_seq)
  if (is.null(cache[[.key]])) cache[[.key]] <- rep(NA, nbuyers)
  cache[[.key]][buyeri] <- price
}

build_price_lookup <- function (prices, cache) {
  nbuyers <- ncol(prices)
  changes <- diff(prices)
  nchanges <- nrow(changes)
  if (missing(cache))
    cache <- new.env()
  # cache$price_lookup <- array(-1, dim=c(19, 19, 19, 19, nbuyers))
  target_len <- 4
  offs <- seq_len(target_len) - 1
  pb <- txtProgressBar(max=nchanges * nbuyers, style=3)
  pbc <- 0
  pbmod <- 1000
  
  for (buyer_i in seq_len(nbuyers)) {
    for (i in seq_len(nchanges - target_len + 1)) {
      target_seq <- changes[i + offs, buyer_i]
      # if we haven't seen this before then store the price
      key <- keyf(target_seq)
      if (is.na(get_price_lookup(target_seq, buyer_i, cache, .key=key))) {
        set_price_lookup_in_cache(target_seq, buyer_i, prices[i + target_len, buyer_i], cache, .key=key, nbuyers=nbuyers)
      }
      pbc <- pbc + 1
      if (pbc %% pbmod == 0)
        setTxtProgressBar(pb, pbc)
    }
  }
  cache
}

# ---- PART 2 EVAL ----
target_seq <- c(-2, 1, -1, 3)

# part 2
# example
seeds <- c(1:3, 2024)
# for reals (this is still a bit slow)
seeds <- input
prices <- calc_prices(seeds, 2000)
lut <- new.env()
build_price_lookup(prices, lut)
bananas <- sapply(mget(ls(lut), lut), sum, na.rm=TRUE)
print(sprintf("target sequence is: %s:: %i total", names(which.max(bananas)), max(bananas)))

