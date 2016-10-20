ptm <- proc.time()

library(data.tree)
library(ape)
library(phytools)

make_matrix <- function(v, df){
    m <- matrix(0,length(v),length(v))
    dimnames(m) <- list(v,v)
    for (i in 1:ncol(m)){
        for (j in 1:nrow(m)){
            m[i,j] <- 1 - cor(df[,v[i]], df[,v[j]], method="spearman")
        }
    }
    m
}

score <- function(tree, matrix){
    total <- 0
    z <- tree$Get("path")
    for (i in 2:length(z)){
        x <- z[[i]][length(z[[i]])-1]
        y <- z[[i]][length(z[[i]])]
        total <- total + m[x,y]
    }
    pair <- c(total, tree)
}

df <- read.table("5node_data.csv", header = TRUE,sep = ",")

v <- c("MPP","CMP","GMP","MEP","EryA")
m <- make_matrix(v, df)

trees <- read.newick(file = "trees.tre")
trees <- lapply(trees, as.Node)

pairs <- lapply(trees, score)
pairs <- pairs[order(sapply(pairs, function(x) x[[1]]))]
lapply(pairs[1:5], function(x) x[[2]])

proc.time()-ptm