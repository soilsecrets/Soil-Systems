# this is the same as the other ruleset comparisons, but I just have to edit it to run off of fuzzy_traverses.csv

library(arulesSequences)
library(pheatmap)
library(RColorBrewer)

setwd("B:/modelNC")

traverses <- read.csv("fuzzy_traverses.csv")


rules.list <- list()
#sixteentraverses <- traverses[traverses$Class==Daniels.class,]

for(class in sort(unique(traverses$mlra_class))){

Daniels.class <- class

sixteentraverses <- traverses[order(traverses$Catena_ID, traverses$Component),]
sixteentraverses <- sixteentraverses[sixteentraverses$mlra_class==Daniels.class,]

oh_traverses <- sixteentraverses[,-c(1:6)]
oh_traverses <- t(apply(oh_traverses, MARGIN = 1, FUN= function(x) x==max(x)))

transaction_traverses <-  as(data.frame(oh_traverses), "transactions")


transactionInfo(transaction_traverses)$sequenceID <- sixteentraverses$Catena_ID
transactionInfo(transaction_traverses)$eventID <- sixteentraverses$Component
transactionInfo(transaction_traverses)$Elevation <- sixteentraverses$Elevation
transactionInfo(transaction_traverses)$Class <- sixteentraverses$mlra_class




itemsets <- cspade(transaction_traverses[transaction_traverses@itemsetInfo$Class==Daniels.class], 
                   parameter = list(support = if(class==10) 0.004 else (1/nrow(transaction_traverses[transaction_traverses@itemsetInfo$Class==Daniels.class])), maxlen=2), 
                   control = list(verbose = FALSE))

rules <- ruleInduction(itemsets, 
                       confidence = 0, 
                       control = list(verbose = FALSE))
rules_cleaned <- rules[!is.redundant(rules)]
rules.list[[class]] <- rules_cleaned
}

save(rules.list, file = "Fuzzy_class_rules_list_004support_01conf_length2")

rules.list.fuzzy <- rules.list

load("Daniels_class_rules_list_004support_01conf")

for(i in 1:16){
rules.list.fuzzy[[i]] <- rules.list[[i]]
}



rules.list.fuzzy[sapply(rules.list.fuzzy,is.null)] <- list(ruleInduction(itemsets[1], 
                                                                    confidence = 1, 
                                                                    control = list(verbose = FALSE)))

rules_df <- lapply(rules.list.fuzzy, function(x) as(x,"data.frame"))

#save(rules_df, file="daniels_and_cluster")


# number of rules for each class in order 
#unlist(lapply(rules.list, length))

conf.matrix <- data.frame(matrix(nrow=max(traverses$Class),ncol=max(traverses$Class)))
support.matrix <- data.frame(matrix(nrow=max(traverses$Class),ncol=max(traverses$Class)))
overlap.matrix <- data.frame(matrix(nrow=max(traverses$Class),ncol=max(traverses$Class)))
similarity.matrix <- data.frame(matrix(nrow=max(traverses$Class),ncol=max(traverses$Class)))

names(conf.matrix)  <- sort(unique(traverses$Class))
names(support.matrix)  <- sort(unique(traverses$Class))
names(overlap.matrix)  <- sort(unique(traverses$Class))
names(similarity.matrix)  <- sort(unique(traverses$Class))


row.names(conf.matrix)  <- sort(unique(traverses$Class))
row.names(support.matrix)  <- sort(unique(traverses$Class))
row.names(overlap.matrix)  <- sort(unique(traverses$Class))
row.names(similarity.matrix)  <- sort(unique(traverses$Class))




for(i in 1:max(traverses$Class)){
    for(j in 1:max(traverses$Class)){

  overlap.rules <- rules_df[[i]][rules_df[[i]][,1] %in% rules_df[[j]][,1],]       
  invert.overlap.rules <- rules_df[[j]][rules_df[[j]][,1] %in% rules_df[[i]][,1],]
  
  
  overlap.matrix[i,j] <- nrow(overlap.rules)
  similarity.matrix[i,j] <-   2*nrow(overlap.rules)/(length(rules_df[[i]][,1])+length(rules_df[[j]][,1]))       
  
  
  
  overlap.supports <-  1-(abs(overlap.rules$support-invert.overlap.rules$support)/pmax(overlap.rules$support,invert.overlap.rules$support))
  overlap.confidences <-  1-(abs(overlap.rules$confidence-invert.overlap.rules$confidence)/pmax(overlap.rules$confidence,invert.overlap.rules$confidence))    
  
  support.matrix[i,j] <- if(is.na(sum(overlap.supports)/nrow(overlap.rules))) 0 else sum(overlap.supports)/nrow(overlap.rules)
  conf.matrix[i,j] <- if(is.na(sum(overlap.confidences)/nrow(overlap.rules))) 0 else sum(overlap.confidences)/nrow(overlap.rules)
  
        
}}

# change this to any of the above matrix
cm <- similarity.matrix

heatmap <- function(data, rowN, colN, xTitle = "", yTitle = "", numColors=11){
  # transpose and rotate matrix clockswise 90 degrees 
  dataAdjusted <- t(apply(data,2,rev))
  
  image(1:ncol(data), 1:nrow(data), xlab = xTitle, ylab = yTitle, dataAdjusted, col = rev(brewer.pal(numColors,"RdYlBu")), axes = FALSE)
  axis(1, 1:ncol(data), colN)
  axis(2, nrow(data):1, rowN)
  
  for (x in 1:ncol(data))
    for (y in 1:nrow(data))
      # add text values into matrix based on transposed/rotated indices + round values to two digits
      text(x, y, round(dataAdjusted[x,y],2))
}


cm <- similarity.matrix
heatmap(cm,row.names(cm),names(cm), "heatmap for daniels classes")

#########################################################################################################################################




# load object traverses_cluster_classes
load("traverses_cluster_classes")
traverses2 <- traverses_cluster_classes[!is.na(traverses_cluster_classes$mlra_class),]


rules.list.clusters <- list()

for(class in sort(unique(traverses2$mlra_class))){
  
  Daniels.class <- class
  sixteentraverses <- traverses2[traverses2$mlra_class==Daniels.class,]
  sixteentraverses <- sixteentraverses[order(sixteentraverses$Catena_ID, sixteentraverses$Component),]
  
  oh_traverses <- sixteentraverses[,-c(1:6, ncol(sixteentraverses), ncol(sixteentraverses)-1)]
  oh_traverses <- t(apply(oh_traverses, MARGIN = 1, FUN= function(x) x==max(x)))
  
  sub_oh_traverses <- data.frame(oh_traverses)
  transaction_traverses <-  as(sub_oh_traverses, "transactions")
  transactionInfo(transaction_traverses)$sequenceID <- sixteentraverses$Catena_ID
  transactionInfo(transaction_traverses)$eventID <- sixteentraverses$Component
  transactionInfo(transaction_traverses)$Elevation <- sixteentraverses$Elevation
  
  itemsets <- cspade(transaction_traverses, 
                     parameter = list(support = 0.001, maxgap=1), 
                     control = list(verbose = FALSE))
  
  rules <- ruleInduction(itemsets, 
                         confidence = 0.01, 
                         control = list(verbose = FALSE))
  rules_cleaned <- rules[!is.redundant(rules)]
 
  rules.list.clusters[[class]] <- rules_cleaned
}

save(rules.list.clusters, file = "MLRA_Cluster_class_rules_list_001support_01conf")

# I accidently assigned the lists to the MLRA class numbers :(, well at least I can bind the lists easily.
safe <- rules.list.clusters
rules.list.clusters[1:length(rules.list)] <- rules.list

rules_df_clusters <- lapply(rules.list.clusters, function(x) try(as(x,"data.frame")))

conf.matrix.cluster <- data.frame(matrix(nrow=length(unique(traverses2$mlra_class)),ncol=length(unique(traverses2$mlra_class))))
support.matrix.cluster <- data.frame(matrix(nrow=length(unique(traverses2$mlra_class)),ncol=length(unique(traverses2$mlra_class))))
overlap.matrix.cluster <- data.frame(matrix(nrow=length(unique(traverses2$mlra_class)),ncol=length(unique(traverses2$mlra_class))))
similarity.matrix.cluster <- data.frame(matrix(nrow=length(unique(traverses2$mlra_class)),ncol=length(unique(traverses2$mlra_class))))

names(conf.matrix.cluster)  <- sort(unique(traverses2$mlra_class))
names(support.matrix.cluster)  <- sort(unique(traverses2$mlra_class))
names(overlap.matrix.cluster)  <- sort(unique(traverses2$mlra_class))
names(similarity.matrix.cluster)  <- sort(unique(traverses2$mlra_class))


row.names(conf.matrix.cluster)  <- sort(unique(traverses2$mlra_class))
row.names(support.matrix.cluster)  <- sort(unique(traverses2$mlra_class))
row.names(overlap.matrix.cluster)  <- sort(unique(traverses2$mlra_class))
row.names(similarity.matrix.cluster)  <- sort(unique(traverses2$mlra_class))

conf.matrix.cluster[,] <- 0 
support.matrix.cluster[,] <- 0
overlap.matrix.cluster[,] <- 0
similarity.matrix.cluster[,] <- 0

for(x in 1:length(sort(unique(traverses2$mlra_class)))){
  for(y in 1:length(sort(unique(traverses2$mlra_class)))){
    
    i <- sort(unique(traverses2$mlra_class))[x]
    j <- sort(unique(traverses2$mlra_class))[y]
    
    overlap.rules <- rules_df_clusters[[i]][rules_df_clusters[[i]][,1] %in% rules_df_clusters[[j]][,1],]       
    invert.overlap.rules <- rules_df_clusters[[j]][rules_df_clusters[[j]][,1] %in% rules_df_clusters[[i]][,1],]
    
    
    overlap.matrix.cluster[x,y] <- nrow(overlap.rules)
    similarity.matrix.cluster[x,y] <-   2*nrow(overlap.rules)/(length(rules_df_clusters[[i]][,1])+length(rules_df_clusters[[j]][,1]))       
    
    
    
    overlap.supports <-  1-(abs(overlap.rules$support-invert.overlap.rules$support)/pmax(overlap.rules$support,invert.overlap.rules$support))
    overlap.confidences <-  1-(abs(overlap.rules$confidence-invert.overlap.rules$confidence)/pmax(overlap.rules$confidence,invert.overlap.rules$confidence))    
    
    support.matrix.cluster[x,y] <- if(is.na(sum(overlap.supports)/nrow(overlap.rules))) 0 else sum(overlap.supports)/nrow(overlap.rules)
    conf.matrix.cluster[x,y] <- if(is.na(sum(overlap.confidences)/nrow(overlap.rules))) 0 else sum(overlap.confidences)/nrow(overlap.rules)
    
    
  }}


cm <- similarity.matrix.cluster[4:9,4:9]*overlap.matrix.cluster[4:9,4:9]
heatmap(cm,names(cm),names(cm), "heatmap for daniels classes")





(sum(similarity.matrix*conf.matrix)- sum(similarity.matrix==1))/nrow(similarity.matrix)
(sum(similarity.matrix.cluster*conf.matrix.cluster)- sum(similarity.matrix.cluster==1))/nrow(similarity.matrix.cluster)

(sum(similarity.matrix*support.matrix)- sum(similarity.matrix==1))/nrow(similarity.matrix)
(sum(similarity.matrix.cluster*support.matrix.cluster)- sum(similarity.matrix.cluster==1))/nrow(similarity.matrix.cluster)


(sum(conf.matrix)- sum(conf.matrix==1))/nrow(conf.matrix)
(sum(conf.matrix.cluster)- sum(conf.matrix.cluster==1))/nrow(conf.matrix.cluster)

(sum(support.matrix)- sum(support.matrix==1))/nrow(support.matrix)
(sum(support.matrix.cluster)- sum(support.matrix.cluster==1))/nrow(support.matrix.cluster)




###########################################################################
#lets compare both! 

conf.matrix.both <- data.frame(matrix(nrow=length(unique(traverses2$Class)),ncol=length(unique(traverses2$mlra_class))))
support.matrix.both <- data.frame(matrix(nrow=length(unique(traverses2$Class)),ncol=length(unique(traverses2$mlra_class))))
overlap.matrix.both <- data.frame(matrix(nrow=length(unique(traverses2$Class)),ncol=length(unique(traverses2$mlra_class))))
similarity.matrix.both <- data.frame(matrix(nrow=length(unique(traverses2$Class)),ncol=length(unique(traverses2$mlra_class))))

names(conf.matrix.both)  <- sort(unique(traverses2$mlra_class))
names(support.matrix.both)  <- sort(unique(traverses2$mlra_class))
names(overlap.matrix.both)  <- sort(unique(traverses2$mlra_class))
names(similarity.matrix.both)  <- sort(unique(traverses2$mlra_class))


row.names(conf.matrix.both)  <- sort(unique(traverses2$Class))
row.names(support.matrix.both)  <- sort(unique(traverses2$Class))
row.names(overlap.matrix.both)  <- sort(unique(traverses2$Class))
row.names(similarity.matrix.both)  <- sort(unique(traverses2$Class))

conf.matrix.both[,] <- 0 
support.matrix.both[,] <- 0
overlap.matrix.both[,] <- 0
similarity.matrix.both[,] <- 0


for(x in 1:length(sort(unique(traverses2$mlra_class)))){
  for(y in 1:length(sort(unique(traverses2$Class)))){
    
    i <- sort(unique(traverses2$mlra_class))[x]
    j <- sort(unique(traverses2$Class))[y]
    
    overlap.rules <- rules_df_clusters[[i]][rules_df_clusters[[i]][,1] %in% rules_df_clusters[[j]][,1],]       
    invert.overlap.rules <- rules_df_clusters[[j]][rules_df_clusters[[j]][,1] %in% rules_df_clusters[[i]][,1],]
    
    
    overlap.matrix.both[y,x] <- nrow(overlap.rules)
    similarity.matrix.both[y,x] <-   2*nrow(overlap.rules)/(length(rules_df_clusters[[i]][,1])+length(rules_df_clusters[[j]][,1]))       
    
    
    
    overlap.supports <-  1-(abs(overlap.rules$support-invert.overlap.rules$support)/pmax(overlap.rules$support,invert.overlap.rules$support))
    overlap.confidences <-  1-(abs(overlap.rules$confidence-invert.overlap.rules$confidence)/pmax(overlap.rules$confidence,invert.overlap.rules$confidence))    
    
    support.matrix.both[y,x] <- if(is.na(sum(overlap.supports)/nrow(overlap.rules))) 0 else sum(overlap.supports)/nrow(overlap.rules)
    conf.matrix.both[y,x] <- if(is.na(sum(overlap.confidences)/nrow(overlap.rules))) 0 else sum(overlap.confidences)/nrow(overlap.rules)
    
    
  }}


cm <- support.matrix.both
heatmap(cm,row.names(cm),names(cm), "heatmap for daniels classes")

daniels.df <- data.frame(rules_df_clusters[[1]])
daniels.df$class <- 1  

for(i in 2:16){
hold.df <- data.frame(rules_df_clusters[[i]])
hold.df$class <- i
daniels.df <- rbind(daniels.df,hold.df)

}


cluster.df <- data.frame(rules_df_clusters[[sort(unique(traverses2$mlra_class))[1]]])
cluster.df$class <- sort(unique(traverses2$mlra_class))[1]  

for(i in sort(unique(traverses2$mlra_class))[-1]){
  hold.df <- data.frame(rules_df_clusters[[i]])
  hold.df$class <- i
  cluster.df <- rbind(cluster.df,hold.df)
  
}

# looks like the cluster has more rules with the same support and confidence 
daniels_number_rules <- apply(overlap.matrix,1,function(x) max(x))
cluster_number_rules <- apply(overlap.matrix.cluster,1,function(x) max(x))
mean(daniels_number_rules)
mean(cluster_number_rules)

daniels.df$support %>% mean
daniels.df$confidence %>% mean
daniels.df$lift %>% mean

cluster.df$support %>% mean
cluster.df$confidence %>% mean
cluster.df$lift %>% mean

plot(aggregate(support~class, data=daniels.df, mean))
plot(aggregate(support~class, data=cluster.df, mean))
aggregate(confidence~class, data=daniels.df, mean) %>% plot
aggregate(confidence~class, data=cluster.df, mean) %>% plot

aggregate(lift~class, data=daniels.df, mean) %>% plot
aggregate(lift~class, data=cluster.df, mean) %>% plot



aggregate(support~class, data=daniels.df, mean)$support %>% sd
aggregate(support~class, data=cluster.df, mean)$support %>% sd

aggregate(confidence~class, data=daniels.df, mean)$confidence %>% sd
aggregate(confidence~class, data=cluster.df, mean)$confidence %>% sd

aggregate(lift~class, data=daniels.df, mean)$lift %>% sd
aggregate(lift~class, data=cluster.df, mean)$lift %>% sd




heatmap(overlap.matrix.both,row.names(conf.matrix.both),names(conf.matrix.both))





