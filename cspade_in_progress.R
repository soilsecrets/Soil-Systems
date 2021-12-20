# https://www.r-bloggers.com/2020/10/sequence-mining-my-browsing-history-with-arulessequences/


setwd("B:/modelNC")

#traverses <- read.csv("B:/Classified/df_catena_comp_percent_w_elevations.csv")

load("traverses_cluster_classes")
traverses2 <- traverses_cluster_classes[!is.na(traverses_cluster_classes$mlra_class),]



Daniels.class <- 16
sixteentraverses <- traverses2[traverses2$Class==Daniels.class,]

sixteentraverses <- sixteentraverses[order(sixteentraverses$Catena_ID, sixteentraverses$Component),]


oh_traverses <- sixteentraverses[,-c(1:6,ncol(sixteentraverses),ncol(sixteentraverses)-1)]
oh_traverses <- t(apply(oh_traverses, MARGIN = 1, FUN= function(x) x==max(x)))

elevation_oh_traverses <- cbind(sixteentraverses[,c(1:6)],oh_traverses)

sub_oh_traverses <- data.frame(oh_traverses)

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
transaction_traverses <-  as(sub_oh_traverses, "transactions")

#head(inspect(transaction_traverses))
transactionInfo(transaction_traverses)$sequenceID <- sixteentraverses$Catena_ID
transactionInfo(transaction_traverses)$eventID <- sixteentraverses$Component
transactionInfo(transaction_traverses)$Elevation <- sixteentraverses$Elevation
#head(inspect(transaction_traverses))


itemsets <- cspade(transaction_traverses, 
                   parameter = list(support = 0.01, maxgap=1), 
                   control = list(verbose = FALSE))







#inspect(itemsets)

#Convert Back to DS
itemsets_df <- as(itemsets, "data.frame") %>% as_tibble()
itemsets_df$sequence

intresting_items <- itemsets_df[!stringr::str_detect(itemsets_df$sequence,","),]

#Top 20 Frequent Item Sets
intresting_items %>%
  slice_max(support, n = 20) %>%
  ggplot(aes(x = fct_reorder(sequence, support),
             y = support,
             fill = sequence)) + 
  geom_col() + 
  geom_label(aes(label = support %>% scales::percent()), hjust = 0.5) + 
  labs(x = "Site", y = "Support", title = "Most commonly occuring Soils",
       caption = "**Support** is the percent of segments the contain the item set") + 
  scale_fill_discrete(guide = F) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, .1))) + 
  coord_flip() + 
  cowplot::theme_cowplot() + 
  theme(
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = 'plot',
    plot.title.position = 'plot'
  )


rules <- ruleInduction(itemsets, 
                       confidence = 0.01, 
                       control = list(verbose = FALSE))
rules_cleaned <- rules[!is.redundant(rules)]


rules_df <- as(rules_cleaned, "data.frame") %>% 
  as_tibble() %>% 
  separate(col = rule, into = c('lhs', 'rhs'), sep = " => ", remove = F)
# Removes self refering rules 
rules_df <- rules_df[!rules_df$lhs==rules_df$rhs,]



rules_df %>% 
  arrange(-confidence) %>% 
  select(lhs, rhs, support, confidence, lift) %>% 
  #head() %>% 
  knitr::kable()


rules_df %>% 
#  #Remove All Rules that Rock.outcrop
#  filter(!str_detect(rule, '\\{Rock.outcrop\\/\\}')) %>% 
  #Keep only Rule, Confidence, and Lift - 1
  transmute(rule, confidence, lift = lift) %>% 
  #Pivot Lift and confidence into a single column
  #pivot_longer(cols = c('confidence','lift'),
  pivot_longer(cols = c('confidence'),            
               names_to = "metric", 
               values_to = "value") %>% 
  group_by(metric) %>% 
  #Keep only the Top 10 Rules for Each Metric
  top_n(10, value) %>% 
  ungroup() %>% 
  # Reorder so that order is independent for each metrics
  ggplot(aes(x = tidytext::reorder_within(rule, value, metric),
             y = value,
             fill = rule)) + 
  geom_col() + 
  geom_label(aes(label = value %>% scales::percent()), 
             hjust = 0) +
  scale_fill_discrete(guide = F) + 
  tidytext::scale_x_reordered() + 
  scale_y_continuous(label = scales::percent, 
                     limits = c(0, 1),
                     expand = expansion(mult = c(0, .1))) + 
  labs(x = "Rule", 
       y = "", 
       title = "Top Rules by Confidence",
       caption = "**Confidence** is the probability RHS occurs 
       given LHS occurs <br>
       **Lift** is the increased liklihood of seeing LHS & RHS together vs. independent") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = 'plot',
    strip.text = element_textbox(
      size = 12,
      color = "white", fill = "#5D729D", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )


rules_df %>% 
  #  #Remove All Rules that Rock.outcrop
  #  filter(!str_detect(rule, '\\{Rock.outcrop\\/\\}')) %>% 
  #Keep only Rule, Confidence, and Lift - 1
  transmute(rule, confidence, lift = lift) %>% 
  #Pivot Lift and confidence into a single column
  #pivot_longer(cols = c('confidence','lift'),
  pivot_longer(cols = c('lift'),            
               names_to = "metric", 
               values_to = "value") %>% 
  group_by(metric) %>% 
  #Keep only the Top 10 Rules for Each Metric
  top_n(20, value) %>% 
  ungroup() %>% 
  # Reorder so that order is independent for each metrics
  ggplot(aes(x = tidytext::reorder_within(rule, value, metric),
             y = value,
             fill = rule)) + 
  geom_col() + 
  geom_label(aes(label = round(value,2)), 
             hjust = 0) +
  scale_fill_discrete(guide = F) + 
  tidytext::scale_x_reordered() + 
  scale_y_continuous(#label = scales::percent, 
                     #limits = c(0, 1),
                     expand = expansion(mult = c(0, .1))) + 
  labs(x = "Rule", 
       y = "", 
       title = "Top Rules by Lift",
       caption = "**Confidence** is the probability RHS occurs 
       given LHS occurs <br>
       **Lift** is the increased liklihood of seeing LHS & RHS together vs. independent") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = 'plot',
    strip.text = element_textbox(
      size = 12,
      color = "white", fill = "#5D729D", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )















#########################





df_oh_traverses <- as(transaction_traverses  , "data.frame")
transaction_traverses


collapsed_history_graph_dt2 <- df_oh_traverses %>% 
  group_by(eventID) %>% 
  transmute(sequenceID, source = items, Elevation=Elevation) %>% 
  mutate(destination = lead(source)) %>% 
  ungroup() %>%
  filter(!is.na(destination)) %>% 
  select(source, destination, eventID, Elevation) %>% 
  count(source, destination, wt=Elevation, name = 'instances')

collapsed_history_graph_dt <- df_oh_traverses %>% 
  group_by(eventID) %>% 
  transmute(sequenceID, source = items, Elevation=Elevation) %>% 
  mutate(destination = lead(source)) %>% 
  ungroup() %>%
  filter(!is.na(destination)) %>% 
  select(source, destination, eventID, Elevation) %>% 
  count(source, destination, name = 'instances')

collapsed_history_graph_dt[]


# %in% !collapsed_history_graph_dt$source

#length()


collapsed_history_graph_dt$Elevation <- collapsed_history_graph_dt2$instances/collapsed_history_graph_dt$instances

obs <- aggregate(instances~source, data=collapsed_history_graph_dt, sum)
collapsed_history_graph_dt$confidence <- 0
collapsed_history_graph_dt$support <- 0
for(row in 1:nrow(collapsed_history_graph_dt)){
  collapsed_history_graph_dt$confidence[row] <- collapsed_history_graph_dt$instances[row]/obs$instances[obs$source==collapsed_history_graph_dt$source[row]]
  collapsed_history_graph_dt$support[row] <- collapsed_history_graph_dt$instances[row]/nrow(df_oh_traverses)
  }




g <- collapsed_history_graph_dt %>% filter(instances > (min(tail(sort(instances), 31))))



# breaks via the natural breaks with the mean number of components +1 in a traverse (NC averages 3 comps)
# +1 due to if the traverse rounds to 1 then there is no relationship in a traverse without a +1.
unique_catena_ID <- unique(sixteentraverses$Catena_ID)
count.of.comps <- sapply(unique_catena_ID,function(x) sum(sixteentraverses$Catena_ID == x))

breaks <- getJenksBreaks(g$Elevation,round((mean(count.of.comps))+1,0))
breaks[1] <- breaks[1]-0.001
breaks[length(breaks)] <- breaks[length(breaks)]+0.001

group_tags <- cut(g$Elevation, 
                  breaks=breaks, 
                include.lowest=FALSE, 
                  right=FALSE,
                  labels = breaks[-1])



g$group <- as.numeric(group_tags) 
g[is.na(g$group),]



gl <- g %>%
  group_by(group, source) %>%
  summarise(a_sum=sum(instances))


gu <- g %>%
  group_by(group, destination) %>%
  summarise(a_sum=sum(instances))

gu$group <- gu$group +1
names(gu)[2] <- names(gl)[2]

gb <- rbind(gu,gl)

gbb <- gb %>%
  group_by(group, source) %>%
  summarise(a_sum=sum(a_sum)) 

gbbm <- gbb %>%
  group_by(group) %>%
  summarise(Max_comp=max(a_sum))

gbbmm <- gbb %>%
  group_by(group) %>%
  summarise(Max_comp=max(a_sum))



par(mfrow=c(1,length(unique(gbb$group))))
for(itter in 1:length(unique(gbb$group))){
pie(unlist(gbb[gbb$group==itter,3]), labels = paste(unlist(gbb[gbb$group==itter,2])))
}


max_comp_names <- NULL
for(i in unique(gbb$group)){
max_comp_names[i]<- gbb[gbb$group==i,2][gbb[gbb$group==i,3] == as.numeric(gbbm[gbbm$group==i,2])]
}

# prep for plotting traverse with piecharts 
soil.comp.names <-  str_extract(max_comp_names,"[^\"{}]+")
elevations <- breaks


####################
# Fetch soil profiles, this also requires internet access.
x2 <- soilDB::fetchOSD(soil.comp.names)

# Inverse the tops and bottoms of the soil profiles so its a depth map.
# Otherwise it plots upside down. 
x2$top<-(-(x2$top))

x2$bottom<-(-(x2$bottom))

# Fetching the soil profiles alphabetizes them. Fiugre out which order that is.
soil.order.plot <- rep(0,length(soil.comp.names))
for(name.of.comp in 1:length(soil.comp.names)){
  order <- str_locate(c(x2@site$id), toupper(soil.comp.names[name.of.comp]))[,1]
  try(soil.order.plot[name.of.comp]<- which(!is.na(order), arr.ind=TRUE))
  
  
}

# Create lines and points that connect the traverses on the plots. 
better.lines <- unlist(lapply(elevations,rep, times=2))
better.points <- seq(1.2,length(elevations)+.7,.5)

bottom.line <-NULL 
for(i in 1:length(elevations)){bottom.line[i] <-min(x2[soil.order.plot[i]]$bottom)}
bottom.lines <- better.lines+unlist(lapply(bottom.line,rep, times=2))
# If no soil. bedrock to the sky! 
for(y in 1:length(bottom.lines)){if(is.infinite(bottom.lines[y])) {
  bottom.lines[y]<- c(head(better.lines,1),
                      260+better.lines,
                      tail(250+better.lines,1),
                      max(elevations)+400,
                      max(elevations)+400)[y]} else Sys.sleep(0)}


for(plot.the.traverse in 1){
par(mfrow=c(1,1))
plot(c(1, length(soil.comp.names)+1), c(min(elevations)-600, max(elevations)+350),
     xaxs="i", xaxt = "n", yaxt = "n", xlab='Components/Elevation', ylab= "", 
     main= "Generalized Soil Traverse for Class 10", bg="black")+ 
  #axis(1, at=1:length(soil.comp.names), labels=soil.comp.names )+
  axis(1, at=1:length(soil.comp.names)+0.5, labels=paste0(round(elevations,0)," m"))+
  # Create a background that is soil colored. 
  rect(0,min(elevations)-600,length(elevations)+1,max(elevations)+400,col="sienna3") +
  
  # Plot the sky
  polygon(cbind(x=c(0,better.points,length(elevations)+1,length(elevations)+1,0),
                y=c(head(better.lines,1),260+better.lines,tail(250+better.lines,1),max(elevations)+500,max(elevations)+500)),border="brown", col= "sky blue") +
  # Plot the rock
  polygon(cbind(x=c(0,0.05+better.points,length(elevations)+1,length(elevations)+1),
                y=c(min(elevations)-750,-30+bottom.lines,tail(60+bottom.lines,1),min(elevations)-750)),border="brown", col = "grey") +
  
  # Plot each of the soil elevations by order and elevation. 
  for(i in 1:length(elevations)){
    if(soil.order.plot[i]==0) next else 
      plotSPC(x2[soil.order.plot[i]], width=.2, x.idx.offset = (.5+i)-1, add = TRUE, plot.depth.axis=FALSE,axis.line.offset = -38.5+((i-1)*7.75),
              y.offset = 250+ elevations[i], id.style = "side", abbr= TRUE, scaling.factor=2,max.depth=110)
    
    
    
    
  } 







#Plot a cloud, I like clouds. 
#for(cloud in 1){
#  grid::grid.circle(x=.09, y=.77, r=0.1)
#  grid::grid.circle(x=.1, y=.8, r=.1)
#  grid::grid.circle(x=.19, y=.8, r=.1)
#  grid::grid.circle(x=.147, y=.77, r=.1)
#  grid::grid.circle(x=.147, y=.825, r=.1)
#  grid::grid.circle(x=.140, y=.81, r=.1)
#  grid::grid.circle(x=.140, y=.79, r=.1)
#  grid::grid.circle(x=.16, y=.77, r=0.1)
#  grid::grid.circle(x=.163, y=.8, r=.1)
#  grid::grid.circle(x=.155, y=.8, r=.1)
#  grid::grid.circle(x=.157, y=.77, r=.1)
#  grid::grid.circle(x=.197, y=.825, r=.1)
#  grid::grid.circle(x=.19, y=.825, r=.1)
  
  
  
  
}

# The output of the above should be a plot of the soil profiles in a traverse. 

#par(mfrow=c(1,max(gbb$group)) ) # 1 row and 3 columns for plots
  
#Get colors for piechart 

soil.names.in.SS <- unique(str_extract(paste(unlist(gbb$source)),"[^\"{}]+"))
yeet <- rep("NULL", length(soil.names.in.SS))
for(name in 1:length(soil.names.in.SS)){
tryCatch(yeet[name] <-fetchOSD(soil.names.in.SS[name])$family, error=function(e) e)
}
colorcount <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
pie.colors <- colorcount(max(as.numeric(factor(yeet))))
lookup <- cbind(as.numeric(factor(yeet)),yeet,soil.names.in.SS)
lookup[!duplicated(lookup[,1]),2]

# plot piechart 
for(itter in 1:length(unique(gbb$group))){
  colors.to.use <-  pie.colors[as.numeric(lookup[match(str_extract(unlist(gbb[gbb$group==itter,2]),"[^\"{}]+") , lookup[,3]),1])]
    add.pie(unlist(gbb[gbb$group==itter,3]), labels = paste0(str_extract(unlist(gbb[gbb$group==itter,2]),"[^\"{}]+"), "=", round(unlist(gbb[gbb$group==itter,3])/sum(unlist(gbb[gbb$group==itter,3])),2)*100,"%"), x=(1*itter)+.5, y=elevations[itter]-500, col = colors.to.use, radius = 95)
}


# add a ledgend 
legend(x=length(soil.comp.names)-.705, y=min(elevations)-100, lookup[!duplicated(lookup[,1]),2], fill = pie.colors[as.numeric(lookup[!duplicated(lookup[,1]),1])], cex=.95, bty = "n", ncol=1)

#}


#################################
  











collapsed_history_graph_dt
unboringed_collapsed_history_graph_dt <- collapsed_history_graph_dt[!collapsed_history_graph_dt$source==collapsed_history_graph_dt$destination,]

filter_limit <- 21
scale.mid <- unboringed_collapsed_history_graph_dt %>% filter(instances > head(tail(sort(instances),filter_limit),1))

#####################

g <- unboringed_collapsed_history_graph_dt %>% 
  filter(instances > head(tail(sort(instances),filter_limit),1))

g$instances <- (1/g$instances)*10000 
#filter(confidence > 0.60) %>%

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

####################

  
g <- g %>% as_tbl_graph(edges=g[,1:3])


#clp <- igraph::cluster_optimal(g)
clp <- data.frame(membership=1)
g <- g %>% 
  activate("nodes") %>% 
  mutate(community = clp$membership)




set.seed(20201029)
ggraph(g, layout = 'fr') + 
  #geom_node_voronoi(aes(fill = as.factor(community)), alpha = .4) + 
  geom_edge_link2(aes(color=confidence),
                     #color = "#5851DB",
                     edge_width = .1,
                     arrow = arrow(angle = 15,length = unit(5, 'mm'), type="closed"),
                     start_cap = circle(3, 'mm'),
                     end_cap = circle(3, 'mm'))+
  geom_node_point(aes(fill = Subgroups), size = 5, pch = 21) + 
  geom_node_text(aes(label = name), repel = T) + 
  labs(title = paste("Soil Rules for class", Daniels.class), caption = paste("Top", filter_limit , "soil relationships")) + 
  #scale_fill_viridis_d(guide = F) + 
  #scale_edge_alpha_identity(guide= F) + 
  scale_edge_colour_gradient2(limits=c(min(scale.mid$confidence),max(scale.mid$confidence)+.03), high = "red", mid =  "grey", midpoint = mean(scale.mid$confidence), low = "black")+
  theme_graph() +
  theme(legend.position = "bottom")






########### (DELETE BELOW)
data.frame(g[1,])
plot.new()
op <- par(cex = 1.0)

c(, max(elevations)+350
  