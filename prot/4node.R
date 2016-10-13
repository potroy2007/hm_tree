library(data.tree)
library(ape)
library(phytools)

df <- read.table("test_data.csv", header = TRUE,sep = ",")

columns = 3:6
for (i in columns){
    df[[i]][df[[i]] < 50] <- 0
    df[[i]][df[[i]] > 50] <- 1
}

trees <- read.newick(file = "4nodetrees.tre")
trees <- unique(trees)
trees <- lapply(trees, as.Node)

score <- function(tree){
    total <- 0
    z <- tree$Get("path")
    for (i in 2:length(z)){
        x <- z[[i]][length(z[[i]])-1]
        y <- z[[i]][length(z[[i]])]
        bool <- df[,x]== df[,y]
        total <- total + sum(bool == F)
    }
    total
}

scores <- lapply(trees, score)
best_tree <- trees[[which.min(scores)]]
plot(best_tree)
