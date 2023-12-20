rm(list = ls())
source("../handy.R")
source("common.r")

evaluate_instruction <- function (value, op, thresh) {
  ifelse(is.na(op), TRUE,
         ifelse(op == "<", value < thresh, value > thresh))
}

# --------------------------------------------
INPUT <- "input-small.txt"
PART_1 <- TRUE
PART_2 <- (INPUT == "input-small.txt") # this one not feasible for real input.

# ---- massage data ---- #
input <- readLines(INPUT)
i <- which(input == "")[1]
workflows <- parse_workflows(head(input, i - 1))

# Read the parts but melt to long-form rather than wide-form
# part-id | property | value vs part-id | x | m | a | s
# the 'final' property is the catchall (the last instruction of the workflow) -
#  it's just a dummy but is needed when we join properties to workflow-steps
parts_df <- parse_parts(tail(input, -i))[, final := 0]
parts_df <- melt(parts_df, id.vars='part_id', variable.name="property")
workflows[is.na(property), property:="final"]  # the last step of the workflow

# ------- logic -------- #
# ---- idea:: 
# Split the workflows to workflow - step - property - operation - threshold
# Evaluate each part against each workflow to say where it would end up under
#  that workflow (the first step that has a TRUE condition)
# Then use this to actually track parts through the workflows.
send_parts_through_workflow <- function (parts_df, workflows) {
  # send all the parts (in long-format) through the workflows (also in long
  # format). Return df of part-id | result (= A or R)
  workflows <- copy(workflows)
  parts_df <- copy(parts_df)
  # Evaluate each part against each step of each workflow
  # want foreach part, foreach workflowstep, do we satisfy the rule
  setkey(parts_df, property)
  setkey(workflows, property)
  result_of_part_per_workflow <- parts_df[workflows, allow.cartesian=TRUE]
  result_of_part_per_workflow[
    , wf_result := evaluate_instruction(value, op, thresh)
  ]
  
  # Reduce to the /final/ step of each part in each workflow
  #  (earliest TRUE step per part per workflow)
  result_of_part_per_workflow <- result_of_part_per_workflow[
    (wf_result == TRUE),
    .(result=then[which.min(step)]),
    by=.(part_id, workflow_name)
  ]
  # augment results with a part_id, workflow_name A, result=A, same for R
  result_of_part_per_workflow <- rbind(
    result_of_part_per_workflow,
    result_of_part_per_workflow[
      , CJ(part_id, workflow_name=c("A", "R"), unique=TRUE)
    ],
    fill=TRUE
  )
  result_of_part_per_workflow[is.na(result), result := workflow_name]
  setkey(result_of_part_per_workflow, part_id, workflow_name)
  
  # Iterate all the parts through the workflows simultaneously
  MAX_ITER <- 100  # because I will have mucked something up
  i <- 0
  final <- result_of_part_per_workflow[workflow_name == "in"]
  while (!all(final$result %in% c("A", "R")) && i <= MAX_ITER) {
    final <- result_of_part_per_workflow[final[, .(part_id, result)]]
    i <- i + 1
  }
  return(final)
}

if (PART_1) {
  final <- send_parts_through_workflow(parts_df, workflows)
  setkey(parts_df, part_id)
  setkey(final, part_id)
  print(
  parts_df[
    final[result == 'A'], sum(value)
  ]
  )
}

# ------- part 2 -------------
# brute force LOL
# Only certain x/m/a/s are interesting - those that represent a branch-point.
# We could generate all parts with all combos of branch-points, (arbitrarily
#  representing the part with properties in the range [value, next_value))
#  send them through the part1 code to see where they end up***
# Here each part represents a range of parts (every part in the range will take
#  the same workflow branches) and then we just calculate the sum of the range.
# **Not all combos of interesting x/m/a/s are required as some are only relevant
#  in some workflow branches, but this is lazy and easy and hopefully doesn't
#  create too many parts to compute.

if (PART_2) {
  # `min_value` is the property value to start this property from
  # If the rule is (e.g.) "s < 1000" we need to make 2 new parts:
  #  one with s=1, and one with s=1000
  # Every part with s=[1, 1000) evaluates to True, and [1000, Inf) evaluates to
  #  False
  # But if the rule is "s > 1000" then the brackets are [0, 1001) and [1001, Inf)
  workflows[op == '<', min_value := thresh]
  workflows[op == '>', min_value := thresh + 1]
  
  # generate all potentially interesting parts
  # must add a part (x=1, m=1, a=1, s=1)
  new_parts <- workflows[
    property != 'final',
    do.call(CJ, split(c(rep(1, 4), min_value), c('x','m','a','s', property)))
  ]
  # **on the real data, the above is intractable!! soemthing like 300^4**
  
  new_parts[, part_id:= seq_len(.N)]
  new_parts[, final := 0]  # needed for the join so the loop does not end
  new_parts <- new_parts[, .(part_id, x, m, a, s, final)]  # reorder for sanity
  # cast to long
  parts_df <- melt(new_parts, id.vars='part_id', variable.name='property')
  
  # find range from each property-value to the next one
  part_size <- unique(
    parts_df[property != 'final', .(property, value)]
    )[
      order(property, value),
      .(value, size=c(diff(value), 4001 - value[.N])),
      by=.(property)
    ][!is.na(size)]
  
  # join back to parts_df and multiply out to get the number of parts each part
  #  represents
  part_size <- part_size[
    parts_df[property != 'final'], on=.(property, value)
  ][, .(size=prod(size)), by=.(part_id)]
  stopifnot(part_size[, sum(size) == 4000 ^ 4])
  
  final <- send_parts_through_workflow(parts_df, workflows)
  
  setkey(part_size, part_id)
  setkey(final, part_id)
  print(part_size[final[result == 'A'], sum(size)], digits=20)
}  
