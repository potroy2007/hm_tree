library(igraph)

cell_types <- c("MPP","CMP","GMP","MEP","EryA")

edges <- c(0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
adj_matrix <- matrix(edges,length(cell_types),length(cell_types),byrow = T)
dimnames(adj_matrix) <- list(cell_types,cell_types)

tree <- graph.adjacency(adj_matrix)

make_matrix <- function(v, df){
    m <- matrix(0,length(v),length(v))
    dimnames(m) <- list(v,v)
    for (i in 1:ncol(m)){
        for (j in 1:nrow(m)){
            m[i,j] <- round(1-cor(df[,v[i]], df[,v[j]], method="pearson"),digit = 2)
        }
    }
    m
}

score <- function(tree){
    total <- 0
    z <- get.edgelist(tree)
    for (i in 1:nrow(z)){
        x <- z[i,1]
        y <- z[i,2]
        total <- total + m[x,y]
    }
    total
}

df <- read.table("5node_data.csv", header = TRUE,sep = ",")

v <- c("MPP","CMP","GMP","MEP","EryA")
m <- make_matrix(v, df)

s <- score(tree)
plot.igraph(tree)

