
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

L0 <- layout_as_bipartite(net)

# net %>% 
#   plot(layout=L0[,2:1],
#        vertex.shape = "rectangle",
#        vertex.size = 20,
#        vertex.size2 = 10,
#        vertex.label.cex = 0.5,
#        edge.arrow.size = 0.5,
#        edge.arrow.width = 0.2)


# Make second network that weights arrows ---------------------------------

edges_test <- edges

for(i in 1:nrow(edges_test)) {
  edges_test$path[i] <- paste(edges$from[i], edges$to[i], sep = ",")
}

# Add color values for nodes
for(i in 1:nrow(nodes)){
  if(nodes$id[i] == "DEACC" | nodes$id[i] == "CHRIS" | nodes$id[i] == "COSTUMES") {
    nodes$color[i] <- "#A5A5A5"
  } else if (nodes$id[i] %in% c(1:15, "shelf") ){
    nodes$color[i] <- "#DBDBDB"
  } else {
    nodes$color[i] <- "#F6F6F6"
  }
}


edges_weights <- edges_test %>% 
  count(path) %>% 
  separate(path, c("from", "to"), sep = ",") %>% 
  mutate(weight = n) %>% 
  select(-n)

for(i in 1:nrow(edges_weights)){
  if(edges_weights$weight[i] <= 2){
    edges_weights$color[i] <- "#F7D3D3"
  } else if(edges_weights$weight[i] <= 3){
    edges_weights$color[i] <- "#E6A4A4"
  } else if(edges_weights$weight[i] <= 4){
    edges_weights$color[i] <- "#CB6F70"
  } else if(edges_weights$weight[i] <= 6){
    edges_weights$color[i] <- "#9D3D3D"
  } else {
    edges_weights$color[i] <- "#5F1415"
  }
}

weighted <- graph_from_data_frame(d = edges_weights, vertices = nodes, directed = T)
L1 <- layout_as_bipartite(weighted)

E(weighted)$width <- E(weighted)$weight

reorder_igraph_nodes(weighted, sortAttributes = c("id"))

weighted %>% 
  plot(layout=L1[,2:1],
       vertex.shape = "rectangle",
       vertex.size = 30,
       vertex.size2 = 10,
       vertex.label.color = "#000000",
       vertex.color = nodes$color,
       vertex.label.cex = 0.5,
       vertex.label.family = "Helvetica",
       vertex.label.font = 2,
       edge.arrow.size = 0.3,
       edge.arrow.width = 0.3,
       edge.color = edges_weights$color,
       edge.curved = 0.2)


