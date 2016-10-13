library(data.tree)
library(ape)

df <- read.table("testdata.csv", header = TRUE,sep = ",")

columns = 2:5
for (i in columns){
    df[[i]][df[[i]] < 50] <- 0
    df[[i]][df[[i]] > 50] <- 1
}

trees <- read.tree(file = "trees.tre")
trees <- unique(trees)
trees_alt <- lapply(trees, as.Node)

score <- function(tree){
    total <- 0
    x <- tree$root$name
    for (i in 1:length(tree$children)){
        y <- tree$children[[i]]$name
        bool <- df[,x]== df[,y]
        total <- total + sum(bool == F)
    }
    total
}

scores <- lapply(trees_alt, score)
best_tree <- trees_alt[[which.min(scores)]]
plot(best_tree)
