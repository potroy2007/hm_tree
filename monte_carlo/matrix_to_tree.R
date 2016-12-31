library(data.tree)

add.child <- function(node,matrix){ # recursive function
    
    children <- colnames(matrix)[which(matrix[node$name,]==1)]
    
    if (length(children)==0){
        return(NULL)
        }
    for (i in 1:length(children)){
        node$AddChild(children[i])
    }
    
    if (length(children)==2){
        add.child(node$children[[1]],matrix)
        add.child(node$children[[2]],matrix)
    }else{
        add.child(node$children[[1]],matrix)
    }
}

matrix.to.tree <- function(matrix){
    tree <- Node$new(rownames(matrix)[1])
    add.child(tree,matrix)
    tree
}


cell_types <- c("MPP","CMP","GMP","MEP","EryA")
edges <- c(0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
mx <- matrix(edges,length(cell_types),length(cell_types),byrow = T)
dimnames(mx) <- list(cell_types,cell_types)
plot(matrix.to.tree(mx))
