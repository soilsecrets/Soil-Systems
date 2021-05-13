# https://www.r-bloggers.com/2020/10/sequence-mining-my-browsing-history-with-arulessequences/


setwd("B:/modelNC")

traverses <- read.csv("B:/Classified/df_catena_comp_percent_w_elevations.csv")
head(oh_traverses)

sixteentraverses <- traverses[traverses$Class==15,]

sixteentraverses <- sixteentraverses[order(sixteentraverses$Catena_ID, sixteentraverses$Component),]


oh_traverses <- sixteentraverses[,-c(1:6)]
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
                   parameter = list(support = 0.01, maxgap=50), 
                   control = list(verbose = FALSE))

#inspect(itemsets)

#Convert Back to DS
itemsets_df <- as(itemsets, "data.frame") %>% as_tibble()

#Top 10 Frequent Item Sets
itemsets_df %>%
  slice_max(support, n = 10) %>% 
  ggplot(aes(x = fct_reorder(sequence, support),
             y = support,
             fill = sequence)) + 
  geom_col() + 
  geom_label(aes(label = support %>% scales::percent()), hjust = 0.5) + 
  labs(x = "Site", y = "Support", title = "Most Frequently Visited Item Sets",
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
                       confidence = 0.1, 
                       control = list(verbose = FALSE))
inspect(head(rules, 20))                 

rules_cleaned <- rules[!is.redundant(rules)]


inspect(rules_cleaned)      

rules_df <- as(rules_cleaned, "data.frame") %>% 
  as_tibble() %>% 
  separate(col = rule, into = c('lhs', 'rhs'), sep = " => ", remove = F)

rules_df %>% 
  arrange(-confidence) %>% 
  select(lhs, rhs, support, confidence, lift) %>% 
  head() %>% 
  knitr::kable()


rules_df %>% 
#  #Remove All Rules that Rock.outcrop
#  filter(!str_detect(rule, '\\{Rock.outcrop\\/\\}')) %>% 
  #Keep only Rule, Confidence, and Lift - 1
  transmute(rule, confidence, lift = lift/max(lift)) %>% 
  #Pivot Lift and confidence into a single column
  pivot_longer(cols = c('confidence','lift'),
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
       title = "Top Rules by Confidence and Lift",
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


collapsed_history_graph_dt$Elevation <- collapsed_history_graph_dt2$instances/collapsed_history_graph_dt$instances

obs <- aggregate(instances~source, data=collapsed_history_graph_dt, sum)
collapsed_history_graph_dt$confidence <- 0
for(row in 1:nrow(collapsed_history_graph_dt)){
  collapsed_history_graph_dt$confidence[row] <- collapsed_history_graph_dt$instances[row]/obs$instances[obs$source==collapsed_history_graph_dt$source[row]]
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
     main= "Soil Traverse", bg="black")+ 
  #axis(1, at=1:length(soil.comp.names), labels=soil.comp.names )+
  axis(1, at=1:length(soil.comp.names)+0.5, labels=paste0(round(elevations,0),"M"))+
  # Create a background that is soil colored. 
  rect(0,min(elevations)-600,length(elevations)+1,max(elevations)+400,col="sienna3") +
  
  # Plot the sky
  polygon(cbind(x=c(0,better.points,length(elevations)+1,length(elevations)+1,0),
                y=c(head(better.lines,1),260+better.lines,tail(250+better.lines,1),max(elevations)+500,max(elevations)+500)),border="brown", col= "sky blue") +
  # Plot the rock
  polygon(cbind(x=c(0,0.05+better.points,length(elevations)+1,length(elevations)+1),
                y=c(min(elevations)-750,80+bottom.lines,tail(80+bottom.lines,1),min(elevations)-750)),border="brown", col = "grey") +
  
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
    add.pie(unlist(gbb[gbb$group==itter,3]), labels = paste0(str_extract(unlist(gbb[gbb$group==itter,2]),"[^\"{}]+"), "=", round(unlist(gbb[gbb$group==itter,3])/sum(unlist(gbb[gbb$group==itter,3])),2)*100,"%"), x=(1*itter)+.5, y=elevations[itter]-300, col = colors.to.use, radius = 150)
}


# add a ledgend 
legend(x=length(soil.comp.names)-.5, y=max(elevations)*.45, lookup[!duplicated(lookup[,1]),2], fill = pie.colors[as.numeric(lookup[!duplicated(lookup[,1]),1])], cex=1, bty = "n")

}





















#########################################
grid.echo()
b <- grid.grab()

pies <- recordPlot()

library(gridGraphics)

ggpubr::ggarrange(a, ggpubr::ggarrange(b), nrow = 2)
par(mfrow=c(2,1))
grid.draw(a)
grid.draw(b)


library(gridExtra)
grid.arrange(b, grid.arrange(a,ncol=1),nrow  = 2)



library(mapplots)


add.pie()


####################





# elevations next



gbbgu <- gbb$group %>% unique 









split_groups <- split(g,g$group)

sapply(split_groups, FUN=function(x) aggregate(x, by=list(x$source), FUN=sum))


#################################
  

collapsed_history_graph_dt
filter_limit <- 40
g <- collapsed_history_graph_dt %>% 
  filter(instances > head(tail(sort(instances),filter_limit),1)) %>% 
  as_tbl_graph()

#clp <- igraph::cluster_optimal(g)
clp <- data.frame(membership=1)
g <- g %>% 
  activate("nodes") %>% 
  mutate(community = clp$membership)



set.seed(20201029)
ggraph(g, layout = 'fr') + 
  #geom_node_voronoi(aes(fill = as.factor(community)), alpha = .4) + 
  geom_edge_link2(aes(edge_alpha=confidence*100, color=confidence),
                     #color = "#5851DB",
                     edge_width = .1,
                     arrow = arrow(angle = 15,length = unit(5, 'mm'), type="closed"),
                     start_cap = circle(3, 'mm'),
                     end_cap = circle(3, 'mm'))+
  geom_node_point(fill = 'orange', size = 5, pch = 21) + 
  geom_node_text(aes(label = name), repel = T) + 
  labs(title = "Soil Rules", caption = paste("Top", filter_limit , "soil relationships")) + 
  #scale_fill_viridis_d(guide = F) + 
  scale_edge_alpha_identity(guide= F) + 
  scale_edge_colour_gradient2(high = "brown")+
  theme_graph() +
  theme(legend.position = "bottom")




########### (DELETE BELOW)
data.frame(g[1,])
plot.new()
op <- par(cex = 1.0)

c(, max(elevations)+350