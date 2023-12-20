library(stringi)
library(data.table)

parse_workflows <- function(workflows) {
  # table name, step, property, op, threshold, result
  df <- data.table(
    do.call(
      rbind,
      stri_match_all(workflows, regex="^(?<name>[a-z]+)\\{(?<contents>.+)\\}$")
    )[, -1]
  )
  df <- df[,
     {
       out <- data.table(
         stri_match_all(
           regex="(?:(?<property>[xmas])(?<op>[<>])(?<thresh>[0-9]+):)?(?<then>[a-zA-Z]+)",
           contents
         )[[1]][, -1, drop=FALSE])
       cbind(step=seq_len(nrow(out)), out)
     },
     by=.(name, contents)
  ]
  setnames(df, "name", "workflow_name")
  df[,thresh := as.numeric(thresh)]
  df[, contents := NULL]
  return(df)
}


parse_parts <- function(parts) {
  # naaaasty
  # returns list of c(x/m/a/s) (lol, XMAS)
  parts <- sub('{', 'c(', parts, fixed=TRUE)
  parts <- sub('}', ')', parts, fixed=TRUE)
  
  return(
    # wide data-table
    data.table(
      do.call(rbind, lapply(lapply(parts, function (x) parse(text=x)), eval))
    )[, part_id := seq_len(.N)]
 # list of vectors
 #   lapply(lapply(parts, function (x) parse(text=x)), eval)
  )
}
