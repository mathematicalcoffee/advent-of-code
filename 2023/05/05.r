rm(list = ls())
library(stringi)
library(data.table)
lines <- readLines("input.txt")
lines <- lines[lines != ""]

# ----- parse inputs ----- #
seeds <- as.numeric(stri_split(gsub("seeds: ", "", grep("^seeds:", lines, value=TRUE)), fixed=" ")[[1]])

map.df <- data.table()
for (line in tail(lines, -1)) {
  if (grepl("map", line)) {
    header <- line
    header.bits <- stri_match_all(header, regex="^(?<source>[a-zA-Z]+)-to-(?<destination>[a-zA-Z]+)")[[1]]
  } else {
    bits = as.numeric(stri_split(line, regex=" +")[[1]])
    source.start <- bits[2]
    transform <- bits[1] - bits[2] # offset to add to the source.i to get the dest
    len <- bits[3]
    
    map.df <- rbind(
      map.df,
      list(
        source.type = header.bits[, "source"],
        destination.type = header.bits[, "destination"],
        source.start = bits[2],
        source.end = bits[2] + bits[3] - 1,
        len=bits[3],
        destination.start = bits[1],
        destination.end = bits[1] + bits[3] - 1
      )
    )
  }
}
type.map.df <- unique(map.df[, .(source.type, destination.type)])
setkey(type.map.df, source.type)


map.df[, lookup.number := source.start] # dummy col (copy of source.start since I'll rolling-join on that)
setkey(map.df, source.type, lookup.number)


# ----- do the questions ----- #
get.destination.recursive <- function(lookup.numbers, lookup.type, map.df, type.map.df) {
  dest.type <- type.map.df[J(lookup.type), destination.type]
  dest.numbers <- map.df[
    J(lookup.type, lookup.numbers), roll=Inf
  ][,
    ifelse(!is.na(source.start) & lookup.number <= source.end, lookup.number - source.start + destination.start, lookup.number)
  ]
  if (is.na(dest.type)) {
    return(dest.numbers)
  } else {
    # message("")
    # message("dest.type: ", dest.type)
    # message(dest.type, ".numbers: ", paste0(dest.numbers, collapse=", "))
    return(get.destination.recursive(unique(dest.numbers), dest.type, map.df, type.map.df)) # <--
  }
}

# ----- get the answers ----- #
# PART 1
# 282277027
print(min(get.destination.recursive(seeds, "seed", map.df, type.map.df)))

# FUCK ME PART 2
# I should just partition the inputs into buckets by each A->B mapping and so
#  produce a seed -> location direct mapping
# but I am too dumb-dumb

# make some compromise between allocating the entire vector in memory and iterating 1 at a time
#  given the goodness of R vectorisation
starts <- seeds[seq(from=1, to=length(seeds), by=2)]
lengths <- seeds[seq(from=2, to=length(seeds), by=2)] # fuck me
ends <- starts + lengths - 1

min.loc <- Inf
batch.size <- 1e6
est.n.batches <- sum(ceiling(lengths / batch.size)) # FUCK ME
batch.i <- 1
pb <- txtProgressBar(min=0, max=est.n.batches, width=80)

for (i in seq_along(starts)) {
  range.start <- starts[i]
  range.end <- range.start
  while (range.start <= ends[i]) {
    # I shouldn't have to generate the numbers to know. I should pre-chunk into numbers that count
    # like intersect(seed.range,  map.seed.ranges) and then everyone else is a i->i
    range.end <- min(range.start + batch.size - 1, ends[i])
    batch.seeds <- seq(range.start, range.end)

    min.loc <- min(min.loc, get.destination.recursive(batch.seeds, "seed", map.df, type.map.df))
    #message(range.start, " -> ", range.end, ": ", min.loc)
    
    range.start <- range.end + 1
    batch.i <- batch.i + 1
    setTxtProgressBar(pb, batch.i)
  }
}
print(min.loc)
# 11554135
