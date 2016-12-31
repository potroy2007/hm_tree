library(data.tree)

cor.matrix <- function(df){
    v <- colnames(df)
    m <- matrix(0,length(v),length(v),dimnames = list(v,v))
    sm <- matrix(0,length(v),length(v),dimnames = list(v,v))
    for (i in 1:ncol(m)){
        for (j in 1:nrow(m)){
            r <- cor(df[,v[i]], df[,v[j]], method="pearson")
            sm[i,j] <- 1-r
            if (r < 0){
                m[i,j] <- 1e-5
            }else{
                m[i,j] <- r
            }
        }
    }
    list(m,sm)
}

choose.root.node <- function(cor.matrix){
    nodes <- rownames(cor.matrix)
    sample(nodes,1)
}

add.edge_ <- function(chosen,rv){
    if (chosen != ""){
        rv <- rv[which(names(rv)!=chosen)]
    }
    
    if (length(rv)==0){
        p_vector <- 1
    }else if (length(rv)==1){
        p_vector <- rv/length(rv)
        p_vector <- c(p_vector, 1-sum(p_vector))
    }else{
        p_vector <- rv/(max(rv)*length(rv))
        p_vector <- c(p_vector, 1-sum(p_vector))
    }
    
    p <- runif(1)
    cumProb <- 0
    for (i in 1:length(p_vector)){
        cumProb <- cumProb + p_vector[i]
        if (p < cumProb){
            return(names(p_vector[i]))
        }
    }
}

add.edge <- function(node,tree,used,cor.m){
    
    rw <- cor.m[node,]
    rv <- rw[!names(rw) %in% used]
    chosen <- ""
    chosen <- add.edge_(chosen,rv)
    result <- c(chosen,add.edge_(chosen,rv))
    result <- result[which(result!="")]
    result <- sort(result)
    n_used <- c(used,result)
    if (length(result)!=0){
        n_tree <- cbind(tree,matrix(0,nrow(tree),length(result)))
        n_tree <- rbind(n_tree,matrix(0,length(result),ncol(n_tree)))
        dimnames(n_tree) <- list(n_used,n_used)
        n_tree[node,result] <- 1
    }else{
        n_tree <- tree
    }
    
    return(list(result,n_tree,n_used))
}

tree.gen <- function(cor.m){
    root <- choose.root.node(cor.m)
    checked <- root
    tree_m <- matrix(0,1,1,dimnames = list(checked,checked))
    
    to_check <- list(root)
    
    while (length(to_check)>0 && length(checked)<nrow(cor.m)){
        parent <- to_check[[1]]
        to_check <- to_check[-1] # dequeue
        
        output <- add.edge(parent,tree_m,checked,cor.m)
        
        children <- output[[1]]
        
        if (length(children)==2){
            s <- sample(0:1, 1, replace = T)
            if (s==1){
                children[c(1,2)] <- children[c(2,1)]
            }
        }
        
        to_check <- c(to_check,as.list(children)) #enqueue
        
        tree_m <- output[[2]]
        checked <- output[[3]]
    }
    tree_m
}

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

score <- function(tree.m,score.m){
    k <- arrayInd(which(tree.m==1), dim(tree.m))
    pairs <- apply(k,1,function(pair){
        row <- rownames(tree.m)[pair[1]]
        col <- colnames(tree.m)[pair[2]]
        c(row,col)
    })
    sum(apply(pairs,2,function(pair){
        score.m[pair[1],pair[2]]
    }))
}

##########################################################
df <- read.table("5node_data.csv", header = TRUE,sep = ",")
matrices <- cor.matrix(df)
m <- matrices[[1]]
sm <- matrices[[2]]

tms <- replicate(5000,expr = tree.gen(m))
y <- sapply(tms,function(m) sum(m)==4)
tms <- tms[y]
length(tms)

z <- unique(tms)
length(z)

dist <- sapply(z,function(unique_m){
    n <- sapply(tms,function(m)identical(m,unique_m))
    sum(n)
})

index <- which(dist==max(dist))
for (i in index){
    print(z[[i]])
    print(dist[[i]])
}

scores <- sapply(z,function(unique_m)score(unique_m,sm))
index2 <- which(scores==min(scores))
for (i in index2){ 
    print(z[[i]]) 
    print(dist[i])}

tm1 <- matrix(c(0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),nrow = 5,byrow = T)
dimnames(tm1) <- list(c("MPP","CMP","GMP","MEP","EryA"),c("MPP","CMP","GMP","MEP","EryA"))
k <- sapply(tms,function(m) identical(m,tm1))
sum(k)
sort(dist)