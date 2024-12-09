rm(list=ls())
library(data.table)
source("../../handy.R")
ex <- get_and_save_example_input()
input <- get_and_save_input()


parse.diskmap <- function (line) {
  return(as.integer(stri_split_boundaries(line, type="character")[[1]]))
}
calc.checksum <- function (disk) {
  sum((seq_along(disk) - 1) * disk, na.rm=TRUE)
}
create.disk <- function(diskmap) {
  ids <- seq_len(ceiling(length(diskmap)/2)) - 1
  inverse.rle(
    list(
      lengths=diskmap,
      values=as.vector(rbind(ids, NA_integer_))[seq_along(diskmap)]
    )
  )
}
compact.disk <- function(disk) {
  blanks <- is.na(disk)
  
  n.notblank <- sum(!blanks)
  n.spaces.at.start <- sum(blanks[seq_len(n.notblank)])
  
  i.to.fill <- which(blanks)[seq_len(n.spaces.at.start)]
  # i.to.clear <- tail(which(!blanks), n.spaces.at.start)
  
  disk[i.to.fill] <- rev(tail(disk[!blanks], n.spaces.at.start))
  disk[seq(n.notblank + 1, length(disk))] <- NA
  return(disk)
}
print.disk <- function (disk) print(paste(ifelse(is.na(disk), ".", disk), collapse=""))

# slow
compact.disk.byblock <- function(disk) {
  BLANK <- -1
  rr <- base::rle(
    ifelse(is.na(disk), BLANK, disk)
  )
  blank.lens <- rr$lengths[rr$values == BLANK]
  blank.start.is <- c(1, cumsum(rr$lengths) + 1)[rr$values == BLANK]
  ids <- seq_len(max(disk, na.rm=TRUE))
  out <- disk
  
  for (id in rev(ids)) {
    # find first blank slot that fits that
    id.start.i <- min(which(out == id))
    idlen <- rr$lengths[rr$values == id]
    
    blank.i <- suppressWarnings(min(which(blank.lens >= idlen & blank.start.is < id.start.i)))
    if (is.infinite(blank.i)) {
      # cannot move this block
    } else {
      # move the block
      out[disk == id] <- NA
      out[blank.start.is[blank.i] + seq_len(idlen) - 1] <- id
      # re-calc the blank stats
      rr <- base::rle(
        ifelse(is.na(out), -1, out)
      )
      blank.lens <- rr$lengths[rr$values == -1]
      blank.start.is <- c(1, cumsum(rr$lengths) + 1)[rr$values == -1]
      
      # print.disk(out)
    }
  }
  return(out)
}

compact.disk.byblock.rle <- function(diskmap) {
  BLANK <- -1
  n <- length(diskmap)
  file_ids <- seq_len(ceiling(n/2)) - 1
  rdf <- data.table(
    id=seq_len(n), # internal id
    length=diskmap,
    file_id=as.vector(rbind(file_ids, BLANK))[seq_along(diskmap)] # file id
  )[length > 0]
  rdf[, start.i := c(1, cumsum(length[-.N]) + 1)]
  
  for (fid in rev(file_ids)) {
    # find first blank slot that fits that
    # these are copies
    idrow <- rdf[file_id == fid]
    blankrow <- rdf[order(start.i)][file_id == BLANK & start.i < idrow$start.i & length >= idrow$length][1]
    if (is.na(blankrow$start.i)) {
      # cannot move this block
    } else {
      # blank the old spot
      rdf[file_id == fid, file_id := BLANK]
      # move the ID to the new spot - decide if we are splitting or replacing
      rdf[id == blankrow$id, `:=`(file_id= fid, length=idrow$length)]
      if (idrow$length != blankrow$length) {
        # we need to add a new row for the remainder of the blank
        rdf <- rbind(
          rdf,
          data.table(
            id=nrow(rdf) + 1,
            start.i=blankrow$start.i + idrow$length,
            file_id=BLANK,
            length=blankrow$length - idrow$length
          )
        )[order(start.i)]
      }
      # merge adjacent blank spots
      # note - could have done previous 2 steps in one rather than appending a row
      #  and dropping a row.
      # TOO HARD BASKET.
      
      # print.disk(out)
    }
  }
  return(out)
}
print.disk.df <- function (rdf) {
  paste(rdf[, rep(ifelse(file_id == BLANK, ".", file_id), length)], collapse="")
}


diskmap <- parse.diskmap(ex[[1]])

# do the dumbdumb method -> make a vector and expand it out
disk <- create.disk(diskmap)
out <- compact.disk(disk)
print(calc.checksum(out))

out2 <- compact.disk.byblock(disk)
# out3 <- compact.disk.byblock.rle(diskmap)
print(calc.checksum(out2))
# print(calc.checksum(out3))
# 6420913943576
