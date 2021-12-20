#cluster class traverses 
library(dplyr)
setwd("B:/modelNC")

traverses <- read.csv("B:/Classified/df_catena_comp_percent_w_elevations.csv")

#load clusters 
cluster_classes228 <- read.csv("B:/modelNC/catena_fuzzyclass_228.csv")
cluster_classes234 <- read.csv("B:/modelNC/catena_fuzzyclass_234.csv")
cluster_classes240 <- read.csv("B:/modelNC/catena_fuzzyclass_240.csv")
cluster_classes241 <- read.csv("B:/modelNC/catena_fuzzyclass_241.csv")
cluster_classes261 <- read.csv("B:/modelNC/catena_fuzzyclass_261.csv")
cluster_classes262 <- read.csv("B:/modelNC/catena_fuzzyclass_262.csv")

#combine them and drop some rows
cluster_classes <- rbind(cluster_classes228[,c(4,3,2,ncol(cluster_classes228), ncol(cluster_classes228)-1)],cluster_classes234[,c(2,3,4,ncol(cluster_classes234), ncol(cluster_classes234)-1)], cluster_classes240[,c(2,3,4,ncol(cluster_classes240), ncol(cluster_classes240)-1)], cluster_classes241[,c(2,3,4,ncol(cluster_classes241), ncol(cluster_classes241)-1)], cluster_classes261[,c(2,3,4,ncol(cluster_classes261), ncol(cluster_classes261)-1)], cluster_classes262[,c(2,3,4,ncol(cluster_classes262), ncol(cluster_classes262)-1)])

head(cluster_classes)



cluster_classes$Traverse_ID  %>% unique %>% length 
names(cluster_classes)[1] <- "Catena_ID"

traverses$Catena_ID %>% unique %>% length 

traverses %>% head

fuzzy_traverses <- merge(cluster_classes, traverses)


write.csv(fuzzy_traverses, "fuzzy_traverses.csv")
