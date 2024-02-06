
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

nodes_to_weights <- edges %>% 
  count(to)

nodes <- left_join(nodes, nodes_to_weights, by = c("id" = "to"))


# Add color values for nodes
for(i in 1:nrow(nodes)){
  if(nodes$id[i] == "DEACC" | nodes$id[i] == "CHRIS" | nodes$id[i] == "COSTUMES") {
    nodes$color[i] <- "#A5A5A5"
    nodes$text_color[i] <- "#000000"
  } else if (nodes$id[i] %in% c(1:15, "shelf") ){
    nodes$color[i] <- "#DBDBDB"
    nodes$text_color[i] <- "#000000"
  } else if(nodes$n[i] >= 10) {
    nodes$color[i] <- "#002F70"
    nodes$text_color[i] <- "#FFFFFF"
  } else if(nodes$n[i] >= 7) {
    nodes$color[i] <- "#265BAB"
    nodes$text_color[i] <- "#FFFFFF"
  } else if(nodes$n[i] >= 5){
    nodes$color[i] <- "#6889D0"
    nodes$text_color[i] <- "#000000"
  } else if(nodes$n[i] >= 3){
    nodes$color[i] <- "#A3B4E5"
    nodes$text_color[i] <- "#000000"
  } else {
    nodes$color[i] <- "#D3DBF4"
    nodes$text_color[i] <- "#000000"
  }
}


edges_weights <- edges_test %>% 
  count(path) %>% 
  separate(path, c("from", "to"), sep = ",") %>% 
  mutate(weight = n) %>% 
  select(-n)

for(i in 1:nrow(edges_weights)){
  if(edges_weights$weight[i] <= 1){
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

# this code will put nodes in the correct order but it messes other things up
# weighted_neworder <- permute(weighted, c(10, 5, 4, 13,14, 15, 11, 12, 7, 9, 2, 6, 16, 3, 8, 1, 21, 34, 
#                                          25, 32, 30, 37, 31, 33, 24, 38, 22, 36, 39, 23, 19, 26, 17, 
#                                          20, 29, 18, 28, 27, 35))

weighted %>% 
  plot(layout=L1[,2:1],
       vertex.shape = "rectangle",
       vertex.size = 30,
       vertex.size2 = 10,
       vertex.color = nodes$color,
       vertex.label.cex = 0.5,
       vertex.label.family = "Helvetica",
       vertex.label.font = 2,
       vertex.label.color = nodes$text_color,
       edge.arrow.size = 0.3,
       edge.arrow.width = 0.3,
       edge.color = edges_weights$color,
       edge.curved = 0.2)


# Removing noisy lines ----------------------------------------------------

edges_clean <- edges_weights %>% 
  filter(weight >= 2)

clean <- graph_from_data_frame(d = edges_clean, vertices = nodes, directed = T)
L1 <- layout_as_bipartite(clean)

E(clean)$width <- E(clean)$weight


clean %>% 
  plot(layout=L1[,2:1],
       vertex.shape = "rectangle",
       vertex.size = 30,
       vertex.size2 = 10,
       vertex.color = nodes$color,
       vertex.label.cex = 0.5,
       vertex.label.family = "Helvetica",
       vertex.label.font = 2,
       vertex.label.color = nodes$text_color,
       edge.arrow.size = 0.3,
       edge.arrow.width = 0.3,
       edge.color = edges_clean$color,
       edge.curved = 0.2)
