library(tidyverse)
library(vsp)
library(Matrix)
library(igraph)

el = read_csv("synonym_edges.csv", col_names = F) %>% as.matrix
colnames(el) = c("from","to")

# make an igraph...
g = graph_from_edgelist(el, directed = F)

# compute largest connected components:
cl = clusters(g)
lcc = which(cl$membership==which.max(cl$csize))

# take only the largest connected component, then create the adjacency Matrix
gl = induced.subgraph(g, lcc) %>% simplify()
A = as_adjacency_matrix(gl)

# PCA + Varimax!
fa = vsp(A, k = 50)

#  this selects the words that correspond to the 
#    largest 20 elements in each factor

tmp = apply(fa$Y,2, function(x) which(rank(-x)<21))

pdf(file = "subgraphs.pdf")
for(j in 1:50){
  induced.subgraph(gl, tmp[[j]]) %>% 
    plot.igraph(
      vertex.color = "white", 
      vertex.frame.color = grey(.1,.1), 
      edge.color = grey(.1,.1))
}
dev.off()