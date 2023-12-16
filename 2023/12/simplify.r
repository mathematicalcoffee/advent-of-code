# the functions in here are all around filling in as many question-marks as possible
put_dots_around_known_springs <- function (record) {
  raw_rle <- rle(record$chars == "#")
  raw_rle$ends <- cumsum(raw_rle$lengths)
  raw_rle$starts <- raw_rle$ends - raw_rle$lengths + 1
  springs_mask <- raw_rle$values == TRUE
  
  n_expected_springs <- sort(record$groups, decreasing=TRUE)
  return_chars <- record$chars
  
  for (n_confirmed_springs in n_expected_springs) {
    spring_is <- which(raw_rle$lengths == n_confirmed_springs & springs_mask)
    if (length(spring_is) == sum(n_expected_springs == n_confirmed_springs)) {
      # fill them in
      for (i in spring_is) {
        if (raw_rle$starts[i] > 1)
          return_chars[raw_rle$starts[i] - 1] <- "."
        if (raw_rle$ends[i] < length(return_chars))
          return_chars[raw_rle$ends[i] + 1] <- "."
      }
    } else {
      break
    }
  }
  return(
    make_record(
      return_chars,
      record$groups
    )
  )
}
 
# record <- make_record(split_springs("?###????????"), c(3,2,1))
# expected <- make_record(split_springs(".###.???????"), c(3,2,1))
# all.equal(put_dots_around_known_springs(record)$chars, expected$chars)
