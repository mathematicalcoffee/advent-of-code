rm(list=ls())
library(data.table)
library(bit64)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()

# --- STUPID INT64 STUFFS BECAUSE R DOESNT SUPPORT IT --- #
int2str <- function (x) as.bitstring(as.integer64(x))
bin2int <- function (x) {
  as.integer64.bitstring(gsub(" ", "", x))
}
print_as_bits <- function (x) {
  # pad otu to divisble by 3 from 64
  print(gsub("(...)", "\\1 ", paste0("  ",int2str(x))))
}
stupidly_slow_xor <- function (x, y) {
  # R does not have bitwise operations for int64, since it doesn't have int64 properly.
  # so we'll cast to string and xor manually.
  # I can't believe this.
  # returns a double (but has to go via int64)
  # i.e bitwXor only handles int32s. and bitops doesn't handle int64s either.
  # and the bit64 package only has logical ops not bitwise ops
  xx <- substring(as.bitstring(as.integer64(x)), seq_len(64), seq_len(64))
  yy <- substring(as.bitstring(as.integer64(y)), seq_len(64), seq_len(64))
  return(as.double(as.integer64.bitstring(paste(ifelse(xx==yy, "0", "1"), collapse=""))))
}
stupid_bitwShiftL <- function(i, pow) {
  # returns a double
  as.double(i) * 2^pow
}

# --- puzzle --- #
parse_program <- function (lines) {
  bits <- do.call(rbind, stri_match_all(grep("Register", lines, value=TRUE), regex="Register (?<register>[A-Z]): (?<value>[0-9]+)"))
  register <- as.list(as.integer(bits[, "value"]))
  names(register) <- bits[, "register"]
  register$output <- NULL
 
  # some assumptions 1 line at the bottom 
  program <- as.integer(stri_split_fixed(pattern=",", stri_replace(tail(lines, 1), fixed="Program: ", replacement=""))[[1]])
  
  return(list(register=as.environment(register), program=program))
}
new_register <- function (A, B=0, C=0) {
  env <- new.env()
  env$A <- A
  env$B <- B
  env$C <- C
  env$output <- NULL
  return(env)
}

opnames <- c("adv", "bxl", "bst", "jnz", "Bxc", "out", "bdv", "cdv")
perform_op <- function(instr_pointer, program, register, verbose=FALSE) {
  op <- program[instr_pointer]
  operand <- program[instr_pointer + 1]
  new_instr_pointer <- instr_pointer + 2

  if (verbose) 
    message(sprintf("[%.0f]%s(%.0f)", op, opnames[op + 1], operand))
  if (op == 0) {
    setRegister("A", floor(register$A / (2^combo_operand(operand, register))), register, verbose)
  } else if (op == 1) {
    setRegister("B", stupidly_slow_xor(register$B, literal_operand(operand, register)), register, verbose)
  } else if (op == 2) {
    setRegister("B", combo_operand(operand, register) %% 8, register, verbose)
  } else if (op == 3) {
    if (register$A != 0) {
      new_instr_pointer <- literal_operand(operand, register) + 1 # R is 1-based
    }
  } else if (op == 4) {
    setRegister("B", stupidly_slow_xor(register$B, register$C), register, verbose)
  } else if (op == 5) {
    new_out <- combo_operand(operand, register) %% 8
    register$output <- c(register$output, new_out)
    if (!is.null(register$target_index)) { # part 2
      if (register$target_index > length(register$target) || new_out != register$target[register$target_index])
        return(Inf)
      ## otherwise: it matches, increment the counter
      register$target_index <- register$target_index + 1
    }
  } else if (op == 6) {
    setRegister("B", floor(register$A / (2^combo_operand(operand, register))), register, verbose)
  } else if (op == 7) {
    setRegister("C", floor(register$A / (2^combo_operand(operand, register))), register, verbose)
  }
  return(new_instr_pointer)
}
setRegister <- function(register_name, value, register, verbose=FALSE) {
  if (verbose) 
    message(sprintf(" setting register %s to %.0f", register_name, value))
  register[[register_name]] <- value
}
literal_operand <- function (op, register) {
  return(op)
}
combo_operand <- function(op, register) {
  if (op %in% 0:3)
    return(op)
  else if (op == 4) return(register$A)
  else if (op == 5) return(register$B)
  else if (op == 6) return(register$C)
  stop(paste("unknown combo op", op))
}

run_program <- function (program, register, verbose=FALSE) {
  if (verbose) message("---start---")
  instr_pointer <- 1
  while(instr_pointer <= length(program)) {
    if (instr_pointer == length(program))
      stop("mistake has been made, no operand")
    instr_pointer <- perform_op(instr_pointer, program, register, verbose=verbose)
  }
  if (verbose) message("---finished---")
  # outputs are in register
  return(register)
}

# ------------------ TESTS ---------------
o <- run_program(c(2,6), new_register(A=0,B=0,C=9))
stopifnot(o$B == 1)

o <- run_program(c(5,0,5,1,5,4), new_register(A=10,B=0,C=0))
stopifnot(o$output == 0:2)

o <- run_program(c(0,1,5,4,3,0), new_register(A=2024,B=0,C=0))
stopifnot(o$A == 0)
stopifnot(o$output == c(4,2,5,6,7,7,7,7,3,1,0))

o <- run_program(c(0,1,5,4,3,0), new_register(A=729, B=0, C=0))
stopifnot(paste(o$output, collapse=",") == "4,6,3,5,6,3,5,2,1,0")

# ------------ PART 1 -------------- #
bits <- parse_program(input)
o <- run_program(bits$program, bits$register) 
print("PART 1")
print(paste(o$output, collapse=",")) # 7,1,2,3,2,6,7,2,5.

# ------------ PART 2 -------------- #
# This relies on the fact that each block of 3 bits, adds 1 digit to the output.
# putting another block of 3 bits to the right of the bitwise representation
#   adds another output to the FRONT of the output.
# for example
# run_program(orig$program, new_register(A=as.integer(bin2int("101 110 000            "))))$output # 5 3 0
# run_program(orig$program, new_register(A=as.integer(bin2int("101 110 000 000        "))))$output # 5 5 3 0
# run_program(orig$program, new_register(A=as.integer(bin2int("101 110 000 000 101    "))))$output # 0 5 5 3 0
# run_program(orig$program, new_register(A=as.integer(bin2int("101 110 000 000 101 011"))))$output # 4 0 5 5 3 0
# Note: it's fluke that that at this point, 000 always gives a 5, 011 always gives a 0 etc
# I spent ages reverse-engineering the set of 3 that would produce a single output number
#  and it seemed you could paste them all together to construct sequences of choice, but
#  it turns out whenever you encounter a '4' in the output, not only can you not find a set of 3 that
#  will reproducibly produce a 4, but after you encounter the 4, the mapping changes and
#   is no longer consistent :(
# FWIW that's my cheatsheet, works until you hit a 4...damn.
#     0     1     2     3     4     5     6     7     8     9 
#  "101" "100" "111" "110"    NA "000" "011" "010" "101" "100" 
#
# But still - the fact remains that you add 3 digits (in binary) to the RHS of the output
#  and this will produce a new number in the front of the output.
# So when you brute-force search, you only need to search all permutations of 3 digits in binary.
# e.g. if number
#   aaa bbb ccc produces output 1 2 3,
# and you want to try make 1 2 3 9, then you only need to try
#   aaa bbb ccc DDD   for DDD in 000, 001, 010, 011, 100, 101, 110, 111
# so bitshift your current guess to the left by 3 and then only need to try
#  the 8 subsequent numbers!!!

# PART 2

tryFrom <- function(base, target, program) {
  guesses <- stupid_bitwShiftL(base, 3) + 0:7
  opts <-  lapply(
    guesses,
    function (a) run_program(program, new_register(A=a))$output
  )
  viable <- guesses[which(sapply(opts, function (out) all(out == target)))]
  return(viable)
}

find_A_for <- function (target, program, .highest.digit.matched=0, .curr.base=0) {
  if (.highest.digit.matched == length(target))
    return(.curr.base)
  guesses <- tryFrom(.curr.base, target=tail(target, .highest.digit.matched + 1), program=program)
  # have to try every guess beacuse sometimes the higher one gives the lower number
  return(
    unlist(
    lapply(
      guesses,
      find_A_for,
      program=program,
      target=target,
      .highest.digit.matched=.highest.digit.matched + 1
      # .curr.base takes the value in `guesses`
    )
  ))
}
bits <- parse_program(input)
opts <- find_A_for(target=bits$program, program=bits$program)
print("PART 2")
print(min(opts))

# [1] "5 -> 0"
# [1] "46 -> 3 0"
# [1] "368 -> 5 3 0"
# [1] "2944 -> 5 5 3 0"
# [1] "23557 -> 0 5 5 3 0"
# [1] "188459 -> 4 0 5 5 3 0"
# [1] "1507674 -> 4 4 0 5 5 3 0"
# [1] "12061399 -> 1 4 4 0 5 5 3 0"
# [1] "96491197 -> 3 1 4 4 0 5 5 3 0"
# [1] "771929582 -> 0 3 1 4 4 0 5 5 3 0"
# [1] "6175436656 -> 5 0 3 1 4 4 0 5 5 3 0"
# [1] "49403493250 -> 7 5 0 3 1 4 4 0 5 5 3 0"
# [1] "395227946004 -> 1 7 5 0 3 1 4 4 0 5 5 3 0"
