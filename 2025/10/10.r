rm(list=ls())
library(data.table)
library(testthat)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

parse_inputs <- function (input_str) {
  lapply(
    input_str,
    parse_machine
  )
}
parse_machine <- function (str) {
  # read buttons into a matrix
  # read desired, joltage into matrices
  # str=ex[[1]][1]
  bits <- stri_match(regex="^\\[(?<desired>[.#]+)\\] (?<buttons>.+?) \\{(?<joltage>.+?)\\}$", str)
  desired <- bits[, "desired"]
  desired <- c(`.`=0, `#`=1)[substring(desired, 1:nchar(desired), 1:nchar(desired))]
  nlights <- length(desired)
  joltage <- scan(text=bits[, "joltage"], sep=",", quiet=TRUE)
  
  buttons <- stri_split(bits[, "buttons"], regex=" ")[[1]]
  buttons <- substring(buttons, 2, nchar(buttons)-1)
  buttons <- vapply(buttons, function (pos) {
    out <- rep(0, nlights)
    out[scan(text=pos, sep=",", quiet=TRUE) + 1] <- 1
    out
  }, FUN.VALUE=rep(0, nlights), USE.NAMES=FALSE)
  list(
    buttons=buttons,
    desired=desired,
    joltage=joltage
  )
}

solve_lights <- function (machine) {
  # bit dumb. really I want to solve MIN(1^T %*% p) such that Buttons %*% p == lights (mod 2)
  # but idk how to do this in mod 2.
  # so instead we just make the realisation that at most you will turn each button on once
  #  (since doing it twice would undo what you just did)
  # so dumbly pick `n` buttons to press and see if you got the result you wanted lol.
  nbuttons <- ncol(machine$buttons)
  nlights <- length(machine$desired)
  
  for (npresses in 1:nbuttons) {
    # pick the buttons to press, up to `npresses`
    P <- combn(nbuttons, npresses, FUN = function (buttons_to_press) as.integer((1:nbuttons) %in% buttons_to_press))
    if (any(colSums(abs(machine$buttons %*% P %% 2 - machine$desired)) == 0)) {
      return(npresses)
    }
  }
  return(NA)
}

library(lpSolve)
solve_joltage <- function (machine) {
  # want min(sum(p)) such that machine$buttons %*% p == joltage
  nbuttons <- ncol(machine$buttons)
  return(
    lp(
      objective.in=rep(1, nbuttons),
      const.mat=machine$buttons,
      const.dir="=",
      const.rhs=machine$joltage,
      all.int=TRUE
    )$objval
  )
}

solve_lights <- function (machine) {
  # want min(sum(p)) such that machine$buttons %*% p == desired (mod 2)
  #
  # let q = (p, n)'
  # 0 <= p = # of presses <= 1 (1 per button)
  # n = the modulo offset (1 per light)
  #
  # min(sum(p)) such that
  #
  # ( BUTTONs  2*ID ) (p n)' = desired
  # so button %*% p + 2n = desired
  #
  # however it assumes all my integer values are >= 0 !!!
  # whereas my `n`s are not necessarily.
  # so I have to add a second set of ns, one positive, one negatve
  #   and only one can be filled
  nbuttons <- ncol(machine$buttons)
  nlights <- length(machine$desire)
  zeroes <- matrix(0, nbuttons, nlights)
  o <- lp(
    objective.in=rep(c(1, 0), c(nbuttons, nlights, nlights)),
    const.mat=rbind(
      cbind(diag(nbuttons), zeroes, zeroes),  # num press >= 0
      cbind(diag(nbuttons), zeroes, zeroes), # num presses <= 1
      # B %*% p + 2n == desired
      cbind(machine$buttons, 2 * diag(nlights))
    ),
    const.dir=rep(c(">=", "<=", "="), c(nbuttons, nbuttons, nlights)),
    const.rhs=c(
      rep(0, nbuttons),
      rep(1, nbuttons),
      machine$desired + 2000 # to hope that `n`s are > 0
    ),
    all.int=TRUE
  )
  return (round(o$objval))
}

machines <- parse_inputs(ex[[1]])
expect_equal(
  sum(sapply(machines, solve_lights)),
  7
)
expect_equal(
  sum(sapply(machines, solve_joltage)),
  33
)

machines <- parse_inputs(input)
expect_equal(
  sum(sapply(machines, solve_lights)),
  509
)
expect_equal(
  sum(sapply(machines, solve_joltage)),
  20083
)
