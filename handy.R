i2ij <- function (i, dims) {
  # col-major
  return(cbind(
    row=((i-1) %% dims[2]) + 1,
    col=((i - 1) %/% dims[2]) + 1
    ))
}

ij2i <- function(i, j, dims) {
  # col-major
  (j - 1) * dims[1] + i
}

# read a file into a matrix of 1 char each
read_matrix <- function (filename, numeric=FALSE) {
  bits <- stri_split_boundaries(readLines(filename), type = "character")
  if (numeric) {
    bits <- lapply(bits, as.numeric)
    return(do.call(rbind, bits))
  } else {
    return(stri_list2matrix(bits, byrow = TRUE))
  }
}

# pretty-print a matrix as-is to file (if provided) or screen ("")
print_matrix <- function (mat, with_coordinates=FALSE, file="", ...) {
  # A printer for debugging, it's pretty damn cute
  #             1   
  #    1234567890   
  #  1 >|<<<\.... 1
  #  2 |2-.\^.... 2
  #  3 .v...|->>> 3
  #  4 .v...v^.|. 4
  #  5 .v...v^... 5
  #  6 .v...v^..\ 6
  #  7 .v../2\\.. 7
  #  8 2-2-/vv|.. 8
  #  9 .|<<<2-|.\ 9
  # 10 .v//.|.v.. 10
  #    1234567890   
  #             1   
  if (is.null(file) || length(file) == 0 || is.na(file))
    file <- ""
  if (with_coordinates)
    mat <- .add_coordinates(mat)
  write(t(mat), file=file, ncolumns=ncol(mat), sep="", ...)
  return(invisible(mat))
}

.add_coordinates <- function (mat) {
  dims <- dim(mat)
  
  # add col coordinates on the top and bottom, we'll write them vertical
  # e.g.
  #                   1
  # 1 2 3 4 5 6 7 8 9 0 1 2 3
  order <- floor(log10(dims[2]) + 1)
  col_coords <- list()
  cols <- seq_len(dims[2])
  for (i in seq(order, 1)) {
    divisor <- 10^(i - 1)
    labels <- (cols %/% divisor) %% 10 # extract the `i`th digit
    labels[cols %% divisor != 0] <- " "
    col_coords <- c(col_coords, list(labels))
  }
  col_coords <- do.call(rbind, col_coords)
  
  mat_with_coords <- rbind(
    col_coords,
    mat,
    col_coords[nrow(col_coords):1, ]  # upside down so the digits are close
  )
  
  # add row coordinates on the left and right
  order <- floor(log10(nrow(mat)) + 1)
  spacers <- rep(paste(rep(" ", order + 1), collapse=""), nrow(col_coords))
  rows <- as.character(seq_len(nrow(mat)))
  mat_with_coords <- cbind(
    c(spacers, paste0(format(rows, width=order, justify='r'), " "), spacers),
    mat_with_coords,
    c(spacers, paste0(" ", format(rows, width=order, justify='l')), spacers)
  )
  return(mat_with_coords)
}


URL_FORMAT <- "https://adventofcode.com/2024/day/%i/input"
COOKIE <- readLines("../AOC_SESSION_TOKEN")[1] # brittle...

get_and_save_input <- function (day=NULL, file="input.txt") {
  # get and save
  if (file.exists(file) && file.info(file)$size > 0) {
    return(readLines(file))
  }
  if (is.null(day)) {
    # use the current dir
    day <- basename(getwd())
  }
  message(sprintf("saving input file for day %s to %s", day, file))
  input_data <- get_input_data(day)
  writeLines(input_data, file)
  return(input_data)
  
}
get_input_data <- function (day) {
  input_url <- sprintf(URL_FORMAT, as.integer(day))
  input <- readLines(
    con=url(input_url, headers=c(Cookie=sprintf("session=%s", COOKIE)))
  )
}
