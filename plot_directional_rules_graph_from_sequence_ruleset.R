#rules relationship graph from a given rule set
# this code was previously found in the cspade_in_progress file
# but did not focus on rulesets 
library(arulesSequences)
library(dplyr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(ggtext)
library(forcats)
library(tidyr)
library(stringr)
library(BAMMtools)
library(graphics)
library(grid)
library(soilDB)
library(aqp)
library(mapplots) 

setwd("B:/modelNC")
load("ag_rules_list")
load("pub_rules_list")

Daniels.class <- 2


oneset <- as(ag_rules_list[[Daniels.class]], "data.frame")
oneset_df <- cbind(str_extract_all(oneset[,1], "[a-zA-Z.]+", simplify=TRUE), oneset[,-1])
colnames(oneset_df)[c(1,2)] <- c("source", "destination") 
g <- oneset_df[!oneset_df$source==oneset_df$destination,]
#################################################
soil.names.in.nodes <- unique(str_extract(paste(c(g$source,g$destination)),"[^\"{}]+"))
nodes.yeet <- rep("NULL", length(soil.names.in.nodes))
for(name in 1:length(soil.names.in.nodes)){
  tryCatch(nodes.yeet[name] <-fetchOSD(soil.names.in.nodes[name])$subgroup, error=function(e) e)
}

colorcount <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))

pie.colors <- colorcount(max(as.numeric(factor(nodes.yeet))))
lookup <- data.frame(cbind(as.numeric(factor(nodes.yeet)),nodes.yeet,soil.names.in.nodes))
lookup[!duplicated(lookup[,1]),2]

lookup$colors <- sapply(lookup[,1],function(x) pie.colors[as.numeric(x)])
Subgroups <- nodes.yeet

scale.mid <-unboringed_oneset_df

################################################
g <- g %>% as_tbl_graph(edges=g[,1:3])


ggraph(g, layout = 'fr') + 
  #geom_node_voronoi(aes(fill = as.factor(community)), alpha = .4) + 
  geom_edge_link2(aes(color=lift),
                  #color = "#5851DB",
                  edge_width = .1,
                  arrow = arrow(angle = 15,length = unit(5, 'mm'), type="closed"),
                  start_cap = circle(3, 'mm'),
                  end_cap = circle(3, 'mm'))+
  geom_node_point(aes(fill = Subgroups), size = 5, pch = 21) + 
  geom_node_text(aes(label = name), repel = T) + 
  labs(title = paste("Soil Rules for SSNC class", Daniels.class), caption = paste("SARM soil rules")) + 
  #scale_fill_viridis_d(guide = F) + 
  #scale_edge_alpha_identity(guide= F) + 
  scale_edge_colour_gradient2(limits=c(0,2.03), high = "red", mid =  "White", midpoint = 1 , low = "Black")+
  theme_graph() +
  theme(legend.position = "bottom")



################################################################################################ exact same for daniels rules 


oneset <- as(pub_rules_list[[Daniels.class]], "data.frame")
oneset_df <- cbind(str_extract_all(oneset[,1], "[a-zA-Z.]+", simplify=TRUE), oneset[,-1])
colnames(oneset_df)[c(1,2)] <- c("source", "destination") 
g <- oneset_df[!oneset_df$source==oneset_df$destination,]
#################################################
soil.names.in.nodes <- unique(str_extract(paste(c(g$source,g$destination)),"[^\"{}]+"))
nodes.yeet <- rep("NULL", length(soil.names.in.nodes))
for(name in 1:length(soil.names.in.nodes)){
  tryCatch(nodes.yeet[name] <-fetchOSD(soil.names.in.nodes[name])$subgroup, error=function(e) e)
}

colorcount <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))

pie.colors <- colorcount(max(as.numeric(factor(nodes.yeet))))
lookup <- data.frame(cbind(as.numeric(factor(nodes.yeet)),nodes.yeet,soil.names.in.nodes))
lookup[!duplicated(lookup[,1]),2]

lookup$colors <- sapply(lookup[,1],function(x) pie.colors[as.numeric(x)])
Subgroups <- nodes.yeet

scale.mid <-unboringed_oneset_df

################################################
g <- g %>% as_tbl_graph(edges=g[,1:3])


ggraph(g, layout = 'fr') + 
  #geom_node_voronoi(aes(fill = as.factor(community)), alpha = .4) + 
  geom_edge_link2(aes(color=lift),
                  #color = "#5851DB",
                  edge_width = .1,
                  arrow = arrow(angle = 15,length = unit(5, 'mm'), type="closed"),
                  start_cap = circle(3, 'mm'),
                  end_cap = circle(3, 'mm'))+
  geom_node_point(aes(fill = Subgroups), size = 5, pch = 21) + 
  geom_node_text(aes(label = name), repel = T) + 
  labs(title = paste("Soil Rules for class", Daniels.class), caption = paste("SSNC soil rules")) + 
  #scale_fill_viridis_d(guide = F) + 
  #scale_edge_alpha_identity(guide= F) + 
  scale_edge_colour_gradient2(limits=c(0,2.03), high = "red", mid =  "white", midpoint = 1 , low = "black")+
  theme_graph() +
  theme(legend.position = "bottom")






