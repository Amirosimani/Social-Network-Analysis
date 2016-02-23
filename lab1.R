### 0.Libraries----
library(data.table)
library(dplyr)
library(igraph)

### 1.import data----
network <- read.csv("http://moreno.ss.uci.edu/krackht.dat", header=T, sep=";", skip= 7)
network = setDT(network)[, tstrsplit(DATA., ' ')]
network = network[,V1:=NULL]

#splitting the network data to advice/friendship/report
advice <- slice(network, 1:21)
friendship <- slice(network, 22:42)
report <- slice(network, 43:63)

#importing attributes data
attributes <- read.csv("Kracht_attributes.csv", header=TRUE, sep=";")
attributes$ID <- row.names(attributes)
attributes <- subset(attributes, select=c(ID,1:4))

### 2.Graph----
#. Create igraph object from this matrix
friendship.matrix <- data.matrix(friendship, rownames.force = NA)
friendship_graph <- graph.adjacency(friendship.matrix, mode = "directed", weighted=NULL)

# Attach atributes to the matrix
vertex_attr(friendship_graph) <- attributes

#. Create an edgelist data.frame from the graph
friendship_edges <- data.frame(get.edgelist(friendship_graph))
colnames(friendship_edges)  <- c('ID1', 'ID2')

# Attaching vertex attributes to the edge list to ID1 & ID2
friendship_edges_attributes <- merge(friendship_edges,
                                     setNames(attributes, paste0(names(attributes) ,'1')),
                                     by='ID1')

friendship_edges_attributes <- arrange(friendship_edges_attributes, ID2)

friendship_edges_attributes <- merge(friendship_edges_attributes,
                                     setNames(attributes, paste0(names(attributes) ,'2')),
                                     by='ID2')

#sorting back
friendship_edges_attributes <- arrange(friendship_edges_attributes, ID1)
friendship_edges_attributes <- subset(friendship_edges_attributes, select=c(ID1,1:10))
friendship_edges_attributes$ID1.1 <- NULL

### 3.Homophily----

#. Add columns to friendship_edges_attributes which inidcate if the department of
#  the first vertex in each edge is the same as that of the second vertex
friendship_edges_attributes$same_dept = ifelse(
  friendship_edges_attributes$Dept1==friendship_edges_attributes$Dept2, 1,
  0) # otherwise it's 0

#  Add columns  which inidcate if they have the same level
friendship_edges_attributes$same_level = ifelse(
  friendship_edges_attributes$Level1==friendship_edges_attributes$Level2, 1,
  0) # otherwise it's 0

#  Add columns  which inidcate if they are in the same age group (based on age SD)
friendship_edges_attributes$same_age = ifelse(
  (friendship_edges_attributes$Age1 < friendship_edges_attributes$Age2 + 5) & (friendship_edges_attributes$Age1 > friendship_edges_attributes$Age2 - 5),
   1,
  0) # otherwise it's 0

# Get the proportion of each ego's connections who have the same department, level, and age
ego_homophily_stats <- aggregate(friendship_edges_attributes[,c('same_dept', 'same_level','same_age')], by=list(ID1=friendship_edges_attributes$ID1), FUN=mean, na.rm=TRUE)

#merge back to attributes
attributes <- merge(attributes,ego_homophily_stats,  by.x="ID", by.y="ID1")

summary(lm(same_dept ~ Dept, attributes))
summary(lm(same_level ~ Level, attributes))
summary(lm(same_age ~ Age, attributes))

### 4.Homphily with degree----
# Add the degree of each vertex to the attributes
attributes <- merge(attributes, data.frame(ID=V(friendship_graph)$ID,
                             degree= degree(friendship_graph)),
                           by='ID')

summary(lm(same_dept ~ Dept+degree, attributes))
summary(lm(same_level ~ Level+degree, attributes))
summary(lm(same_age ~ Age+degree, attributes))

### 5. add transitivity----
attributes <- merge(attributes, data.frame(ID=V(friendship_graph)$ID, transitivity=transitivity(friendship_graph, type="local") ) , by='ID')

dyad.census(friendship_graph)
