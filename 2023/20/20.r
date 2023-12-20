rm(list = ls())
source("../handy.R")
library(data.table)
library(stringi)
library(ggplot2)

HIGH <- TRUE
LOW <- FALSE
ON <- TRUE
OFF <- FALSE
CONJUNCTION <- "&"
FLIPFLOP <- "%"
BROADCAST <- "broadcaster"
START <- "button"  # button
OUTPUT <- "O"
NOPULSE <- NA

# Module:
# in [nodes]
# out [nodes]


one_step <- function () {
  # give pulses in (type & node it is going from & into)
  # return pulses out (node-from & pulse type)
}
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


# I sorta want classes that can store their own state.
# what if I.....
# 
# * dataframe module-type (statea stateb) pulse-type -> output lookup
# * dataframe modules module_id - type - statea stateb
# need to know if the pulse sent is an A or a B


# ---- input readers ----
read_modules <- function(input_file) {
  # returns: list of lists
  input <- readLines(input_file)
  modules <- list()
  for (module in input) {
    bits <- stri_split(module, fixed=" -> ")[[1]]
    source <- stri_trim(bits[1])
    first_char <- substring(source, 1, 1)
    if (first_char %in% c(CONJUNCTION, FLIPFLOP)) {
      type <- first_char
      source <- substring(source, 2)
    } else if (source == 'broadcaster') {
      type <- "broadcaster"
    }
    # appending is so bad but I'm past caring
    modules[[source]] <- list(name=source, type=type, to=stri_trim(stri_split(bits[2], fixed=",")[[1]]))
  }
  modules_pointed_to <- unique(unlist(sapply(modules, '[[', i='to')))
  if (length(setdiff(modules_pointed_to, names(modules)))) {
    for (name in setdiff(modules_pointed_to, names(modules))) {
      modules[[name]] <- list(name=name, type=OUTPUT)
    }
  }
  
  # add a START module
  modules[[START]] <- list(name=START, type=START)
  return(modules)
}

populate_initial_state <- function (modules) {
  # populate state, returns list[[module-name]]
  state <- new.env()
  for (name in names(modules)) {
    module <- modules[[name]]
    if (module$type == FLIPFLOP) {
      state[[name]] <- FALSE
    } else if (module$type == CONJUNCTION) {
        modules_connecting_to_me <- names(modules)[sapply(modules, function (m) name %in% m$to)]
        if (length(modules_connecting_to_me) == 0)
          stop(paste("No modules connecting to conjunction", name))
        state[[name]] <- setNames(rep(FALSE, length(modules_connecting_to_me)), modules_connecting_to_me)
    }
  }
  
  state$equals <- function(other_state) {
    # naaasty
    incomparables <- c('equals')
    comparables <- setdiff(ls(state), incomparables)
    if (setequal(comparables, setdiff(ls(other_state), incomparables))) {
       return(isTRUE(all.equal(mget(comparables, envir=state), mget(comparables, envir=other_state))))
    }
    return(FALSE)
  }
  return(state)
}

# --- printers ---- #
repr_pulse <- function (pulse, state, modules) {
  return(
    sprintf(
      "%s -%s-> %s",
      repr_module(modules[[pulse$from]], state),
      ifelse(pulse$type == HIGH, "high", "low"),
      repr_module(modules[[pulse$to]], state)
    )
  )
}
repr_module <- function (module, state) {
  if (module$type %in% c(BROADCAST, START))
    return(module$name)
  if (module$type == OUTPUT)
    return(sprintf("%s%s", module$type, module$name))
  if (module$type %in% FLIPFLOP)
    return(sprintf("%s%s[%s]   ", module$type, module$name, ifelse(state[[module$name]], 'ON', 'OFF')))
  if (module$type == CONJUNCTION)
    return(sprintf("%s%s[%s]", module$type, module$name, paste(ifelse(state[[module$name]], 'H', 'L'), collapse=",")))
  
  return(module$name)
}

print_iter <- function(pulses, state, modules, i=NULL) {
  if (!is.null(i)) {
    message(paste("ITER", i))
  }
  message(
    paste0(
      "\n\t",
      sapply(pulses, repr_pulse, state=state, modules=modules),
      collapse=""
    )
  )
}


# --- logic ---- #
make_pulses <- function(from, tos, type) {
  lapply(tos, function (to) list(from=from, to=to, type=type))
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
  } else if (module$type == FLIPFLOP)  {
    if (pulse$type != HIGH) {
      new_state <- !.state[[module_name]]
      out_pulses <- make_pulses(module_name, module$to, new_state) # confusing `state` with `on/off` here but oh well
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
  if (!is.null(new_state))
    .state[[module_name]] <- new_state
  
  return(out_pulses)
}

one_button_push <- function(modules, state, print_updates=FALSE) {
  MAX_ITER <- 100  # silly infinite-loop check, adjust as necessary
  counts <- c(LOW=0, HIGH=0)
  
  i <- 0
  pulses_to_process <- list(list(from=START, to=BROADCAST, type=LOW))
  if (print_updates)
    print_iter(pulses_to_process, state, modules, i=i)
  
  while (i <= MAX_ITER && length(pulses_to_process) > 0) {
    i <- i + 1
    
    # check
    if (anyDuplicated(sapply(pulses_to_process, '[[', i='to'))) {
      message("multiple pulses to same dest")
      recover()
    }
    pulse_types <- sapply(pulses_to_process, '[[', i='type')
    counts['LOW'] <- counts['LOW'] + sum(!pulse_types)
    counts['HIGH'] <- counts['HIGH'] + sum(pulse_types)
    
    pulses_to_process <- unlist(lapply(pulses_to_process, process_pulse, .state=state, modules=modules), recursive=FALSE)
    if (print_updates && length(pulses_to_process) > 0)
      # the printed state is before said pulses have been processed
      print_iter(pulses_to_process, state, modules, i=i)
  }
  stopifnot(i < MAX_ITER)
  return(counts)
}

# ---------------------------------
get_button_cycle_length <- function (modules, print_updates = FALSE) {
  MAX_ITER <- 100 # please no
  i <- 0
  
  state <- populate_initial_state(modules)
  initial_state <- populate_initial_state(modules)
  counts <- NULL
  
  while (i == 0 || i <= MAX_ITER && !state$equals(initial_state)) {
    i <- i + 1
    counts <- rbind(counts, one_button_push(modules, state, print_updates = print_updates))
  }
  stopifnot(i < MAX_ITER)
  stopifnot(nrow(counts) == i)
  return(list(counts=counts, cycle_length=i))
}

counts_after_button_pushes <- function (n, counts, cycle_length) {
  # UGH
  final <- (n %/% cycle_length) * colSums(counts)
  leftover <- n %% cycle_length
  if (leftover > 0) {
    final <- final + colSums(counts[seq(1, length.out=leftover), , drop=FALSE])
  }
  return(final)
}

modules <- read_modules("input.txt")
x <- get_button_cycle_length(modules, print_updates=FALSE)
print(
  sprintf( # not sure how to disable scientific formatting in standard print
    "%0.f",
    prod(counts_after_button_pushes(1000, x$counts, x$cycle_length))
  )
)
