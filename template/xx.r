rm(list=ls())
library(stringi)
library(data.table)
source("../../handy.R")
ex <- readLines("input-small.txt")
input <- get_and_save_input()