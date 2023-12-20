rm(list = ls())
source("../handy.R")
source("common.r")

# Part 2 only
# New idea
# recursive, the sole intent is to work out the number of parts that satisfy
#  the condition.
# or really I'm just generating "parts of interest" more intelligently than the
#  previous `expand.grid(all thresholds in all workflows)` by restricting this
#  to only split parts that would go through that particular workflow.
# It just so happens that in doing so we may as well send them through the
#  workflow so we end up throwing away all of the part 1 code :(
#
# drop the datatables & joining - just do one part at a time :[
# a part is now a named-vector c(x=, m=, a=, s=)

evaluate_instruction <- function(part, instruction) {
  # part = c(x,m,a,s)
  # instruction = list(property, op, thresh)
  # op = >, <, NA (means return TRUE)
  if (instruction$op == "<") {
    return(part[instruction$property] < instruction$thresh)
  } else {
    return(part[instruction$property] > instruction$thresh)
  }
}

pp <- function(part) {
  # pretty-printer for a part
  paste(sprintf("%s=%s", names(part), part), collapse = ",")
}

# --------------------------------------------
INPUT <- "input.txt"

# ---- massage data ---- #
input <- readLines(INPUT)
i <- which(input == "")[1]
workflows <- parse_workflows(head(input, i - 1))

# Read the parts but melt to long-form rather than wide-form
# part-id | property | value vs part-id | x | m | a | s
# the 'final' property is the catchall (the last instruction of the workflow) -
#  it's just a dummy but is needed when we join properties to workflow-steps
parts_df <- parse_parts(tail(input, -i))[, final := 0]
parts_df <- melt(parts_df, id.vars = "part_id", variable.name = "property")
workflows[is.na(property), property := "final"] # the last step of the workflow

# `min_value` is the property value to start this property from
# If the rule is (e.g.) "s < 1000" we need to make 2 new parts:
#  one with s=1, and one with s=1000
# Every part with s=[1, 1000) evaluates to True, and [1000, Inf) evaluates to
#  False
# But if the rule is "s > 1000" then the brackets are [0, 1001) and [1001, Inf)
workflows[op == "<", min_value := thresh]
workflows[op == ">", min_value := thresh + 1]

# -------------- logic ------------ #
# 1. a part really represents a range of parts, from its x/m/a/s upwards
#    until the max-values for each property (`maxs`)
# 2. send the part through step `stepn` of the workflow `wf` and:
#    * if it's A or R, return the number of parts accepted and stop
#    * if the step tells you to go to a workflow, do that (at step 1)
#    * if the step has an instruction (< or >) then create 2 new parts (one for
#      the TRUE and the FALSE condition) and send them on to the next relevant
#      step.
get_n_workflows <- function(part, maxs, wf, stepn) {
  # Recursive part 2
  # maxs, part = c(x/m/a/s)
  # the max is noninclusive
  instruction <- workflows[J(wf, stepn)]
  nextstep <- instruction$then
  n <- 0
  # print(paste(pp(part), "max", pp(maxs), "workflow", wf, "step", stepn))
  # print(instruction)

  # stop conditions
  if (wf == "A") {
    # parts[['A']] <<- rbind(parts[['A']], rbind(part, maxs))
    return(prod(maxs - part))
  } else if (wf == "R") {
    # parts[['R']] <<- rbind(parts[['R']], rbind(part, maxs))
    return(0)
  }

  # otherwise it's a wfn or an if/else
  if (instruction$property == "final") {
    # it's a final catchall in a workflow. go to next workflow.
    n <- get_n_workflows(part, maxs, nextstep, 1)
  } else {
    # it's an instruction, evaluate it
    # split the parts into 2
    new_part <- part[]
    new_part[instruction$property] <- instruction$min_value

    old_maxs <- maxs[]
    # maxs are non-inclusive
    old_maxs[instruction$property] <- instruction$min_value

    result <- evaluate_instruction(part, instruction)

    # part: c(x,m,a,s)
    # max: c(x,m,a,s)
    if (result == TRUE) {
      # True condition - the original part
      n <- n + get_n_workflows(part, old_maxs, nextstep, 1)
      # False condition - next step of same workflow - the new part
      n <- n + get_n_workflows(new_part, maxs, wf, stepn + 1)
    } else {
      # True condition - the original part
      n <- n + get_n_workflows(new_part, maxs, nextstep, 1)
      # False condition - next step of same workflow - the new part
      n <- n + get_n_workflows(part, old_maxs, wf, stepn + 1)
    }
  }
  return(n)
}


# parts <- list(A=list(), R=list())
setkey(workflows, workflow_name, step)
print(sprintf("%.0f", get_n_workflows(
  c(x = 1, m = 1, a = 1, s = 1),
  c(x = 4001, m = 4001, a = 4001, s = 4001),
  "in",
  1
)))
