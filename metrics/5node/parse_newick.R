library(igraph)

string <- "((E,D)C,B)A;"
string <- gsub("\\(|;",'',string,perl = T) # remove ( and ; from newick strings
cell_types <- strsplit(string,')|,')[[1]] # get cell types

m <- matrix(0,length(cell_types),length(cell_types),dimnames = list(cell_types,cell_types))

components <- strsplit(string,')')[[1]]
components <- lapply(components,function(x) strsplit(x,',')[[1]])

for (i in length(components):2){
    if (length(components[[i]])>1){
        m[components[[i]][1],components[[i-1]]]=1
    }
    else {
        m[components[[i]],components[[i-1]]]=1
    }
}
m
tree <- graph.adjacency(m)
plot(tree)




parse.newick <- function(str){
    string <- gsub("\\(|;",'',str,perl = T)
    components <- strsplit(string,')')[[1]]
    m <- matrix(0,length(components),length(components),dimnames = list(components,components))
    for (i in length(components):1){
        m[components[i],components[i-1]]=1
    }
    m
}