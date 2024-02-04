
# Load libraries ----------------------------------------------------------

library(igraph)
library(tidyverse)


# Load in data ------------------------------------------------------------

data <- read.csv("BCCA_objectmoves - ALL.csv")

# Separate nodes and edges ------------------------------------------------

nodes_list <- c(1:15, "shelf", "1A.1", "1B.1", "1B.2", "1B.3", "1C.1", "1C.2", "1C.3", 
                "1D.1", "1D.2", "1D.3", "2A.1", "2A.2", "2A.3", "2B.1", "2B.2", "2B.3",
                "2C.1", "2C.2", "2C.3", "2D.1", "DEACC", "COSTUMES", "CHRIS") 

nodes <- data.frame(nodes_list)
colnames(nodes) <- c("id")

for(i in 1:nrow(nodes)){
  if(nodes$id[i] %in% 1:15 | nodes$id[i] == "shelf"){
    nodes$type[i] <- "TRUE"
  } else {
    nodes$type[i] <- "FALSE"
  }
}

edges <- data %>% 
  select(box_original, box_new)
colnames(edges) <- c("from", "to")

edges_vector <- c()

for(i in 1:nrow(edges)){
  new_piece <- c(edges$from[i], edges$to[i])
  edges_vector <- c(edges_vector, new_piece)
}

# Make igraph object ------------------------------------------------------

types <- nodes$type
names(types) <- nodes$id

net <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)
is_bipartite(net)
plot(net)

net %>% 
  add_layout_(as_bipartite(types = types)) %>% 
  plot(vertex.shape = "csquare",
       vertex.size = 10,
       vertex.label.cex = 0.5)
