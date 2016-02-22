### Libraries----
library(data.table)

### import data----
network <- read.csv("http://moreno.ss.uci.edu/krackht.dat", header=TRUE, sep=",", skip= 7)
attributes <- read.csv("http://moreno.ss.uci.edu/krackht_att.dat", header=TRUE, sep=",", skip= 8)

clmns <- strsplit(as.numeric(network[1]),'') 
setDT(network[, paste0("clmns", 1:16) := tstrsplit(clmns, ",")])
