rm(list = ls())
# NO CLEAN CODE TODAY
source("../handy.R")
library(data.table)
library(stringi)
library(ggplot2)

HIGH <- TRUE
LOW <- FALSE
CONJUNCTION <- "&"
FLIPFLOP <- "%"
BROADCAST <- "broadcaster"
START <- "button" # button
OUTPUT <- "O"

# Today is super confusing to me for some reason. No nice code today,
#  spaghetti code it is!
# Example 1
# start state: [off, off, off, low] = A/B/C/inv
# one_step(BUTTON, LOW) -> [A, low] [B, low], [C, low]
# STEP: [button to broadcaster: low]
# STEP: OUT: [br to A: low] [br to B: low] [br to C: low]
# STEP: [A to B: high] [B to C: high] [C to inv: high]  [and A/B/C are ON]
# STEP: [inv to A: low]
# STEP: [A to B: low] [A is OFF]
# STEP: [B to C: low] [B is OFF]
# STEP: [C to inv: low] [C is OFF] [inv is low]
# STEP: [inv to A: high]
# end state: [off, off, off, low] = A/B/C/inv = start state
#
#
# Example 2
# start state: A/inv/B/con: off/low/off/low-low
# STEP: [button to broadcaster: low]
# STEP: [br to A: low]
# STEP: [A to inv: high] [A to con: high]      state [ON/low/off/low-low] (of the froms not the tos)
# STEP: [inv to B: low] [con to out: high]     state [on/HIGH/off/HIGH-low]
# STEP: [B -> con: high]                       state [on/high/ON/high-low]
# STEP: [con -> output: low]                   state [on/high/ON/high-HIGH]
#
# BUTTON AGAIN
# STEP: [button to broadcaster: low]           state [on/high/ON/high-high]
# STEP: [br to A: low]                         state [on/high/on/high-high]
# STEP: [A to inv: low] [A to con: low]        state [OFF/high/on/high-high] (of the froms not the tos)
# STEP: [inv to B: high] [con to out: high]    state [off/LOW/on/LOW-high]
# STEP: []                                     state [off/low/on/low-high]
#
# BUTTON AGAIN
# STEP: [button to broadcaster: low]           state [off/low/on/low-high]
# STEP: [br to A: low]                         state [off/low/on/low-high]
# STEP: [A to inv: high] [A to con: high]      state [ON/low/on/low-high] * state before the outputs and after prev step
# STEP: [inv to B: low] [con to out: low]      state [on/HIGH/on/HIGH-high]
# STEP: [B to con: low]                        state [on/high/OFF/high-high]
# STEP: [con to outup: high]                   state [on/high/off/high-LOW]
#
# BUTTON AGAIN
# STEP: [button to broadcaster: low]           state [on/high/off/high-low]
# STEP: [br to A: low]                         state unchanged
# STEP: [A to inv: low] [A to con: low]        state [OFF/low/off/high-low]
# STEP: [inv to B: high] [con to out: high]    state [off/LOW/off/LOW-low]



# ---- input readers ----
read_modules <- function(input_file) {
  # returns: list of lists
  input <- readLines(input_file)
  modules <- list()
  for (module in input) {
    bits <- stri_split(module, fixed = " -> ")[[1]]
    source <- stri_trim(bits[1])
    first_char <- substring(source, 1, 1)
    if (first_char %in% c(CONJUNCTION, FLIPFLOP)) {
      type <- first_char
      source <- substring(source, 2)
    } else if (source == "broadcaster") {
      type <- "broadcaster"
    }
    # appending is so bad but I'm past caring
    modules[[source]] <- list(
      name = source,
      type = type,
      to = stri_trim(stri_split(bits[2], fixed = ",")[[1]])
    )
  }
  modules_pointed_to <- unique(unlist(sapply(modules, "[[", i = "to")))
  if (length(setdiff(modules_pointed_to, names(modules)))) {
    for (name in setdiff(modules_pointed_to, names(modules))) {
      modules[[name]] <- list(name = name, type = OUTPUT)
    }
  }
  for (module in modules) {
    modules_connecting_to_me <- names(modules)[
      sapply(modules, function(m) module$name %in% m$to)
    ]
    modules[[module$name]]$into <- modules_connecting_to_me
  }

  # add a START module (just for printing)
  modules[[START]] <- list(name = START, type = START, to = BROADCAST)
  return(modules)
}

populate_initial_state <- function(modules) {
  # populate state, returns list[[module-name]]
  state <- new.env()
  for (name in names(modules)) {
    module <- modules[[name]]
    if (module$type == FLIPFLOP) {
      state[[name]] <- FALSE
    } else if (module$type == CONJUNCTION) {
      if (length(module$into) == 0) {
        stop(paste("No modules connecting to conjunction", name))
      }
      state[[name]] <- setNames(rep(FALSE, length(module$into)), module$into)
    }
  }

  state$equals <- function(other_state) {
    # naaasty - see if this state matches the other, we are faking it with an
    #  environment since they're mutable
    incomparables <- c("equals", "history")
    comparables <- setdiff(ls(state), incomparables)
    if (setequal(comparables, setdiff(ls(other_state), incomparables))) {
      return(
        isTRUE(
          all.equal(
            mget(comparables, envir = state),
            mget(comparables, envir = other_state)
          )
        )
      )
    }
    return(FALSE)
  }
  return(state)
}
print_state <- function(state, modules) {
  # yuck.
  message(
    paste(
      sort(vapply(modules, repr_module, state = state, "template")),
      collapse = "\n"
    )
  )
}

# --- printers ---- #
repr_pulse <- function(pulse, state, modules) {
  return(
    sprintf(
      "%s -%s-> %s",
      repr_module(modules[[pulse$from]], state),
      ifelse(pulse$type == HIGH, "high", "low"),
      repr_module(modules[[pulse$to]], state)
    )
  )
}
repr_module <- function(module, state = NULL) {
  if (module$type %in% c(BROADCAST, START)) {
    return(module$name)
  }
  if (module$type == OUTPUT || (is.null(state))) {
    return(sprintf("%s%s", module$type, module$name))
  }
  if (module$type %in% FLIPFLOP) {
    return(
      sprintf(
        "%s%s[%s]",
        module$type,
        module$name,
        ifelse(state[[module$name]], "ON", "OFF")
      )
    )
  }
  if (module$type == CONJUNCTION) {
    return(
      sprintf(
        "%s%s[%s]",
        module$type,
        module$name,
        paste(ifelse(state[[module$name]], "H", "L"), collapse = ",")
      )
    )
  }

  return(module$name)
}

print_iter <- function(pulses, state, modules, i = NULL) {
  if (!is.null(i)) {
    message(paste("ITER", i))
  }
  message(
    paste0(
      "\n\t",
      sapply(pulses, repr_pulse, state = state, modules = modules),
      collapse = ""
    )
  )
}


# --- logic ---- #
make_pulses <- function(from, tos, type) {
  lapply(tos, function(to) list(from = from, to = to, type = type))
}

process_pulse <- function(pulse, .state, modules) {
  # MUTATES::: variable `state` which MUST BE AN ENVIRONMENT
  # returns:: list of pulses that this pulse made

  new_state <- NULL
  out_pulses <- c()
  module_name <- pulse$to
  module <- modules[[module_name]]

  if (module$type == CONJUNCTION) {
    new_state <- .state[[module_name]]
    new_state[pulse$from] <- pulse$type
    out_pulses <- make_pulses(module_name, module$to, !all(new_state))
  } else if (module$type == FLIPFLOP) {
    if (pulse$type != HIGH) {
      new_state <- !.state[[module_name]]
      out_pulses <- make_pulses(module_name, module$to, new_state)
    }
  } else if (module$type == BROADCAST) {
    out_pulses <- make_pulses(module_name, module$to, pulse$type)
  } else if (module$type == START) {
    out_pulses <- make_pulses(module_name, module$to, LOW)
  } else if (module$type == OUTPUT) {
    # no outputs
  } else {
    stop("should not reach")
  }

  # MUTATE STATE
  if (!is.null(new_state)) {
    .state[[module_name]] <- new_state
  }

  return(out_pulses)
}

one_button_push <- function(modules, state, print_updates = FALSE) {
  MAX_ITER <- 100 # silly infinite-loop check, adjust as necessary
  counts <- c(LOW = 0, HIGH = 0)

  i <- 0
  pulses_to_process <- list(list(from = START, to = BROADCAST, type = LOW))
  if (print_updates) {
    print_iter(pulses_to_process, state, modules, i = i)
  }

  while (i < MAX_ITER && length(pulses_to_process) > 0) {
    i <- i + 1

    # check
    # OK apparently this is OK and you just emit one out-pulse per in-pulse and
    #  hope the order is OK
    # if (anyDuplicated(sapply(pulses_to_process, '[[', i='to'))) {
    #   message("multiple pulses to same dest")
    #   recover()
    # }
    pulse_types <- sapply(pulses_to_process, "[[", i = "type")
    counts["LOW"] <- counts["LOW"] + sum(!pulse_types)
    counts["HIGH"] <- counts["HIGH"] + sum(pulse_types)

    pulses_to_process <- unlist(
      lapply(pulses_to_process, process_pulse, .state = state, modules = modules),
      recursive = FALSE
    )
    if (print_updates && length(pulses_to_process) > 0) {
      # the printed state is before said pulses have been processed
      print_iter(pulses_to_process, state, modules, i = i)
    }
  }
  stopifnot(i < MAX_ITER)
  return(counts)
}

# ---------------------------------
get_button_cycle_length <- function(modules, n = 100, print_updates = FALSE) {
  # The original idea was to look for cycles in state and return the cycle
  #  length & counts, so you could then extrapolate it forward to large ns
  # Turns out on the real data, there are no cycles in feasible n (or else that
  #  would be the answer for part 2)
  # So I just commented it all out. No clean code today.
  i <- 0

  state <<- populate_initial_state(modules)
  initial_state <- populate_initial_state(modules)
  counts <- NULL
  # history INCLUDING initial (0th push)
  state_modules <- names(
    which(sapply(names(modules), function(n) !is.null(state[[n]])))
  ) # we track the state of these
  state$history <- array(
    NA,
    dim = c(n + 1, length(state_modules)), dimnames = list(NULL, state_modules)
  )

  while (i == 0 || i <= n) { # } && !state$equals(initial_state)) {  # <-- looking for cycles
    i <- i + 1
    state$history[i, ] <- vapply(mget(state_modules, envir = state), all, NA)
    if (i %% 10000 == 0) {
      history <- state$history[1:i, ]
      save(history, file = sprintf("%.0fiters.rda", i))
    }


    counts <- rbind(
      counts,
      one_button_push(modules, state, print_updates = print_updates)
    )
  }
  stopifnot(nrow(counts) == i)
  return(list(counts = counts, cycle_length = i))
}

press_button_n_times <- function(modules, n, print_updates = FALSE) {
  x <- get_button_cycle_length(modules, n, print_updates = print_updates)
  if (x$cycle_length == n) {
    return(colSums(x$counts))
  }

  # if we return to original state
  final <- (n %/% x$cycle_length) * colSums(x$counts)
  leftover <- n %% x$cycle_length
  if (leftover > 0) {
    final <- final +
      colSums(x$counts[seq(1, length.out = leftover), , drop = FALSE])
  }
  return(final)
}


# -------------- part 1 ------------
modules <- read_modules("input.txt")
# x <- press_button_n_times(modules, n=1000, print_updates=FALSE)
# print(x)
# print(
#   sprintf( # not sure how to disable scientific formatting in standard print
#     "%0.f",
#     prod(x)
#   )
# )

# -------------- part 2 ------------
# state <- new.env()
# x <- press_button_n_times(modules, 100000, print_updates=FALSE)
# There is no code here, just mucking around

library(igraph)
make_module_graph <- function(modules) {
  g <- make_empty_graph(n = 0, directed = TRUE)
  for (module in modules) {
    g <- add_vertices(
      g,
      nv = 1,
      name = module$name,
      type = module$type,
      label = repr_module(module)
    )
  }
  for (module in modules) {
    if (length(module$to)) {
      edges <- rbind(from = module$name, to = module$to)
      g <- add_edges(g, as.vector(edges))
    }
  }
  return(g)
}
coords <- structure(
  c(
    532, 887, 531, 766, 82, 521, 490, 87, 713, 869, 125,
    258, 650, 353, 742, 371, 47, 291, 699, 760, 495, 5, 395, 253,
    232, 792, 321, 266, 870, 263, 166, 640, 621, 430, 828, 161, 704,
    628, 707, 417, 923, 737, 733, 226, 947, 810, 947, 818, 26, 877,
    0, 317, 913, 796, 500, 633, 451, 850, 482, 545, 644, 530, 705,
    404, 293, 383, 301, 67, 186, 583, 191, 148, 589, 551, 513, 802,
    94, 594, 627, 637, 582, 192, 665, 663, 281, 5, 778, 732, 467,
    218, 77, 183, 540, 798, 424, 306, 112, 390, 422, 495, 176, 251,
    32, 98, 125, 245, 55, 617, 251, 210, 132, 257, 20, 137, 769,
    464, 563, 0, 224, 477
  ),
  dim = c(60L, 2L)
)
g <- make_module_graph(modules)
mtype <- sapply(modules, "[[", i = "type")
cols <- ifelse(mtype == CONJUNCTION, "yellow",
  ifelse(mtype %in% c(BROADCAST, OUTPUT), "red",
    NA
  )
)
# PLOT THE GRAPH
# png("graph-day20.png", width=900, height=900)
plot(g, layout = coords, vertex.size = 8, vertex.color = cols, edge.arrow.size = 0.75)
# dev.off()

# IN THAT ORDER
load("10000iters.rda")
# png("state-5000-iters.png", width=1000,height=1000)
par(mgp=c(1,1,1), mar=c(2,2,1,1))
image(history[1:5000, ], col = c("red", "seagreen"), xlab = "iter", ylab = "module", xaxt = "n", yaxt = "n")
# dev.off()

# png("state-5000-iters-subgraph.png", width=1000,height=1000)
modules_of_interest <- c("lb", "nj", "xx", "hb", "qk", "hs", "fp", "xb", "tl", "kg", "px", "tm")
par(mgp=c(1,1,1), mar=c(2,2,1,1))
image(history[1:5000, modules_of_interest], col = c("red", "seagreen"), xlab = "iter", ylab = "module", xaxt = "n", yaxt = "n")
# dev.off()

# -------------- scratchpad ------------------
# (data explorating leading to part 2)
# these need to have a LOW state to win.

modules$rx$into # &ql
modules$ql$into # &fh, &mf, &fz, &ss.
# we need ql to all get a H in order to send an L
sapply(modules[modules$ql$into], repr_module, state = state)
# so ALL of fh, mf, fz ss need to send an H
# meaning ALL of fh, mf, fz, ss must RECEIVE a low so that they can SEND a high
sapply(modules[modules$fh$into], repr_module, state = state)
sapply(modules[modules$mf$into], repr_module, state = state)
sapply(modules[modules$fz$into], repr_module, state = state)
sapply(modules[modules$ss$into], repr_module, state = state)
# they each only have one into which is a conjunction, meaning each of
# sn, hl, tf, lr MUST send an L, i.e. ALL must receive H
# they are each fed by 7 flip-flops. To send an H, the flip-flops must turn on
sapply(modules[modules$sn$into], repr_module, state = state)
sapply(modules[modules$hl$into], repr_module, state = state)
sapply(modules[modules$tf$into], repr_module, state = state)
sapply(modules[modules$lr$into], repr_module, state = state)

# the flip-flops feed into each other in a ring
# flip-flops only turn on if they receive a LOW
# the flip-flops only receive a LOW if they are fed a HIGH!

# I need all the flip-flops to turn ON.
unique(rle(history[, "lb"])$lengths)
# they're all the same until they're not for one round.
lapply(modules_of_interest, function(x) unique(rle(history[, x])$lengths))
which(rle(history[, "lb"])$lengths == 2)

# Basically each quadrant is independent so that makes the problem smaller
# iteration 3761 or so? (the 3762th index) - the false-true is a false-false
# that's not a power of 2...
# WHAT IS HAPPENING HERE - MISTAKE OR REAL
image(history[3700:3800, modules_of_interest])
image(history[3750:3770, modules_of_interest])

# suppose we wanted the first time lb and nj were ON at the same time
# lb has period 1, nj has period 2, the answer is 4 [they start OFF] (- 1 because state 0 is not a button press)
history[1:10, c("lb", "nj")]

# ditto lb, nj, xx (period 1,2,4)
history[1:10, c("lb", "nj", "xx")] # iter 8
# so it's 2^(nelements)
# **what is happening on iteration 3761 - is it a bug in my code? this is not a power of 2.
# HL can feed back in to lb. It's always sending a HIGH though. did it send a LOW then?
history[3761:3762, c(modules_of_interest, "mf")] # WHYYYY. lb ALWAYS gets a low-input. It must have gotten 2 on this instance.
#    the only other one can have come from HL !!!!! but then mf would have got it too and didn't
# TODO: play that step of state through. Get HL's actual state.
intToBin(3761)
# 111010110001
# if you look at the ins and outs of hl starting from tm clockwise:
# in in in OUT in OUT in in OUT OUT OUT BOTH (what's the BOTH count as)

# so it's c_i + n*4096   HOPEFULLY (c_i is around 3800)
# tf will be
#
#
m2 <- c("js", "dc", "dp", "xv", "rm", "hj", "bq", "gk", "hm", "rd", "xl", "gx") # lr
image(history[, m2])
which(rle(history[, "js"])$lengths != 1) # 3881
history[3881:3882, m2]
intToBin(3881)
# "111100101001"
# from gx anticlockwise
# I I I I O O I O I O O B    (in to lr)

# OK so the in/out going from the broadcaster-node around the flip-flips is from low-to-high
#   the binary number representing the reset.
# let's HOPE that after that it all repeats and I'm not out-by-one
m3 <- c("gr", "cb", "jg", "qn", "td", "zj", "vr", "hq", "kb", "mq", "fl", "gz")
# I I I O I I O I O O O BOTH
strtoi("111011010001", base = 2) # 3793
history[3793:3794, m3]

# for `sn`
# I I I I O O O O O I I B
strtoi("111100000111", base = 2) # 3847

ofs <- c(3761, 3881, 3793, 3847)


# grab state at a glitch-iter, the only way to go OFF OFF is to receive 2 lows
# this means hl must have fired!
m1 <- c("lb", "nj", "xx", "hb", "qk", "hs", "fp", "xb", "tl", "kg", "px", "tm")
history[3761:3762, m1] # i=3762 is the all-false
history[7522:7523, m1] # i=7523 is the all-false

all(history[1:3761, m1] == history[3762:7522, m1])
# OK IT REPEATS ITSELF and is ALL OFF at this point. That's the I I want
periods <- c(3761, 3881, 3793, 3847)
library(DescTools)
print(LCM(periods), digits = 20)
# damn, that worked? Though'd I'd need a -1 somewhere.
