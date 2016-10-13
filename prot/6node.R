ptm <- proc.time()

library(data.tree)
library(ape)
library(phytools)

# Initialize matrix and store scores of all possible edges
make_matrix <- function(v, df){
    m <- matrix(0,length(v),length(v))
    dimnames(m) <- list(v,v)
    for (i in 1:ncol(m)){
        for (j in 1:nrow(m)){
            bool <- df[,v[i]]== df[,v[j]]
            m[i,j] = sum(bool == F)
        }
    }
    m
}

# Calculate score of each tree by looking up the matrix
score <- function(tree, matrix){
    total <- 0
    z <- tree$Get("path")
    for (i in 2:length(z)){
        x <- z[[i]][length(z[[i]])-1]
        y <- z[[i]][length(z[[i]])]
        total <- total + m[x,y]
    }
    total
}
# read in expression data csv file
df <- read.table("6node_data.csv", header = TRUE,sep = ",")

# convert expression data into binary numbers
columns = 3:8
for (i in columns){
    df[[i]][df[[i]] < 50] <- 0
    df[[i]][df[[i]] > 50] <- 1
}

v <- c("MPP","CLP","CMP","GMP","MEP","CD8")
m <- make_matrix(v, df)

# read in phylos, remove duplicates, and convert them to Data.tree
trees <- read.newick(file = "6nodetrees.tre")
trees <- unique(trees)
trees <- lapply(trees, as.Node)

# score each tree and plot the tree with the lowest score
scores <- lapply(trees, score)
best_tree <- trees[[which.min(scores)]]
plot(best_tree)

proc.time()-ptm