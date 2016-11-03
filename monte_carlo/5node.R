library(igraph)

check_single_line <- function(n,m){
    mp <- head(m,n-1) # subset matrix
    check <- apply(mp,1,function(x)sum(x)==1)
    all(check)
}

random_tree <- function(t){
    m <- matrix(0,t,t) # initialize t x t matrix
    n <- 1 # root node
    nx <- 2 # next node to be added to tree
    
    while(nx<t+1){
        p <- sample(0:2,1,replace = T) # p = 0.5
        
        if (n==1 && p==0){ # first row no childs
            break
        }else if (p==0 && sum(m[n-1,])==0){ # two straight rows with no childs
            break
        }else if (p!=0 && n==nx){
            break
        }else if (p!=0 && nx == t){ # add one edge to avoid going out of bounds
            m[n,nx]<-1
            nx <- nx+1
        }else if (p==2){ # add two edges
            m[n,c(nx,nx+1)] <- 1 
            nx <- nx+2
        }else if (p==1){ # add one edge
            m[n,nx]<-1
            nx <- nx+1
        }
        n <- n+1
    }
    list(m)
}

x <- replicate(5000,expr = random_tree(5))
y <- unlist(lapply(x,function(m) sum(m)==4))
x <- x[y]

z <- unique(x)
length(z)

m1 <- z[[1]]
m2 <- z[[2]]
m3 <- z[[3]]
m4 <- z[[4]]
m5 <- z[[5]]
m6 <- z[[6]]
m7 <- z[[7]]
m8 <- z[[8]]
m9 <- z[[9]]


identical_matrix <- function(x,m1){
    z <- sapply(x,function(m)identical(m,m1))
    sum(z==T)
}

x1 <- identical_matrix(x,m1)
x2 <- identical_matrix(x,m2)
x3 <- identical_matrix(x,m3)
x4 <- identical_matrix(x,m4)
x5 <- identical_matrix(x,m5)
x6 <- identical_matrix(x,m6)
x7 <- identical_matrix(x,m7)
x8 <- identical_matrix(x,m8)
x9 <- identical_matrix(x,m9)
tm <- c(x1,x2,x3,x4,x5,x6,x7,x8,x9)
tm/length(x)

tree <- graph.adjacency(m9)
plot(tree)
