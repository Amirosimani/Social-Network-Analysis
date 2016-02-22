### Libraries----
library(data.table)
library(dplyr)

### import data----
network <- read.csv("http://moreno.ss.uci.edu/krackht.dat", header=T, sep=";", skip= 7)
network = setDT(network)[, tstrsplit(DATA., ' ')]
network = network[,V1:=NULL]

#splitting the network data to advice/friendship/report
advice <- slice(network, 1:21)
friendship <- slice(network, 22:42)
report <- slice(network, 43:63)

#importing attributes data
attributes <- read.csv("Kracht_attributes.csv", header=TRUE, sep=";")


### 