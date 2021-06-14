setwd("B:/modelNC")
library(dplyr)

traverses <- read.csv("B:/Classified/df_catena_comp_percent_w_elevations.csv")

fuzzy_class2 <- read.csv("catena_fuzzyclass_240.csv")

join_cluster_classes <- NULL
for( file in list.files(patter= "catena_fuzzyclass_")){
  fuzzy_class <- read.csv(file)  
  data_to_bind <- fuzzy_class[,c(4,ncol(fuzzy_class),3)]
  join_cluster_classes <- rbind(join_cluster_classes,data_to_bind)
  
}



names(join_cluster_classes)[1] <- "Catena_ID"

traverses_cluster_classes <- plyr::join(traverses,join_cluster_classes,"Catena_ID" )
save(traverses_cluster_classes, file="traverses_cluster_classes")

all.data.for.matrix
