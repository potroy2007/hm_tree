library(phytools)
library(ape)
library(data.tree)

tree <- read.newick(file = "test.tre")
tree <- as.Node(tree)
plot(tree)
