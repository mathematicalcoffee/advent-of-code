library(xml2)
library(httr)
library(stringi)

INPUT_URL_FORMAT <- "https://adventofcode.com/2024/day/%i/input"
QUESTION_URL_FORMAT <- "https://adventofcode.com/2024/day/%i"
COOKIE <- readLines("../AOC_SESSION_TOKEN")[1] # brittle...
options(digits=20) # printing long ints

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

in_bounds <- function (i, j, dims) {
  return(
    i >= 1 &
    i <= dims[1] &
    j >= 1 &
    j <= dims[2] 
  )
}

# read a file into a matrix of 1 char each
read_matrix <- function (filename, numeric=FALSE) {
  return(lines2matrix(readLines(filename), numeric=numeric))
}

lines2matrix <- function (lines, numeric=FALSE) {
  bits <- stri_split_boundaries(lines, type="character")
  if (numeric) bits <- lapply(bits, as.numeric)
  return(do.call(rbind, bits))
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

# ---- rando utils for 
n_digits <- function (int) {
  # 999 -> 3 digits long
  if (int == 0) return(1)
  if (int < 0) return(n_digits(-int))
  floor(log10(int)) + 1
}
left_half <- function (int) { # assumes n_digits is even
  # left half of a number. 1234 -> 12
  n <- n_digits(int)
  stopifnot(n %% 2 == 0)
  return(floor(int / 10^(n / 2)))
}
right_half <- function (int) {
  # right half of a number. 1234 -> 34
  n <- n_digits(int)
  stopifnot(n %% 2 == 0)
  return(int %% 10^(n / 2))
}
concat_ints <- function (left, right) {
  # 12, 34 -> 1234
  left * 10^n_digits(right) + right
}

# input getters

get_example_input_data <- function (day) {
  # returns ALL example datas... list of vectors.
  
  # guess the first <code> block... no smarter than that.
  input_url <- sprintf(QUESTION_URL_FORMAT, as.integer(day))
  page <- read_html(url(input_url, headers=c(Cookie=sprintf("session=%s", COOKIE))))  # to get part 2 if relevant
  ex_data <- stri_split_fixed(stri_trim(xml_text(xml_find_all(page, "//pre/code"))), "\n")
  return(ex_data)
}

get_input_data <- function (day) {
  # must return a list
  input_url <- sprintf(INPUT_URL_FORMAT, as.integer(day))
  input <- readLines(
    con=url(input_url, headers=c(Cookie=sprintf("session=%s", COOKIE)))
  )
  return(list(input))
}

get_and_save_input_helper <- function (FUN, DEFAULT_FILENAME) {
  bound <- function (day=NULL, file=DEFAULT_FILENAME) {
    rds_file <- paste0(file, ".rds")
    # get and save
    if (file.exists(rds_file) && file.info(rds_file)$size > 0) {
      return(readRDS(rds_file))
    }
    if (is.null(day)) {
      # use the current dir
      day <- basename(getwd())
    }
    all_input_data <- FUN(day)
    multiple_inputs <- length(all_input_data) > 1
    
    # save files
    file_names <- sprintf("%s.txt", file)
    if (multiple_inputs) {
      message("**multiple input data found - will save each in its own text file\n")
      file_names <- sprintf("%s-%i.txt", file, seq_along(all_input_data))
    }
    for (i in seq_along(all_input_data)) {  # example data can have multiple
      save_file(all_input_data[[i]], file_names[i])
    }
    # if length 1 then unlist it for convenience
    if (!multiple_inputs)
      all_input_data <- all_input_data[[1]]
    
    saveRDS(all_input_data, file=rds_file)
   
    return(all_input_data)
  }
  return(bound)
}

save_file <- function (lines, file) {
  # save the vector of lines to a file
  # Print a message.
  # return the lines (invisibly)
  message(sprintf("saving input data to %s", file))
  to_print <- head(lines, 10)
  if (length(lines) > 10) to_print <- c(to_print, "...")
  message(paste(to_print, collapse="\n"))
  message("")
  writeLines(lines, file)
  return(invisible(lines))
}

get_and_save_example_input <- get_and_save_input_helper(get_example_input_data, "input-example")
get_and_save_input <- get_and_save_input_helper(get_input_data, "input")

is_wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) abs(x - round(x)) < tol

# --------- graph ----------- #
# library(igraph)
adjacency_df <- function (dims) {
  # df with a connection from every IJ to its neighbour (for constructing adjacency lists)
  # might be easier/better to use make_lattice(dims, directed: bool) and add/remove vertices
  df <- data.table(
    expand.grid(
      from.row=seq_len(dims[1]),
      from.col=seq_len(dims[2])
    )
  )
  df <- df[,
     .(
       to.row=c(from.row, from.row, from.row + 1, from.row - 1),
       to.col=c(from.col - 1, from.col + 1, from.col, from.col)
     ),
     by=.(from.row, from.col)
  ]
  df <- df[in_bounds(to.row, to.col, dims)]
  return(df)
}


# -------- ggplot --------- #
library(ggplot2)
style_base <- list(theme_minimal())
matrix_style_base <- c(style_base, scale_y_reverse(), coord_fixed())
