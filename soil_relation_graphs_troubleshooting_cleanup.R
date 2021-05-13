# sharpshootR 

library(sharpshootR)
library(igraph)
library(RColorBrewer)l
library(scales)
library(splitstackshape)
library(dplyr)

setwd("B:/modelNC")
# Do not run the below section, load the file at the start of the next section
########################################################################################### SECTION 1
{
# read in all of the CSVs from the MLRA extraction. 
# we want to just append the "df_catena_comp_percent_w_elevations.csv"

comp.list <- read.csv("B:/Classified/df_catena_comp_percent_w_elevations.csv")
head(comp.list)


df.mlra <- data.frame(matrix(ncol=2))


for(watershed.number in 1:359){
  mlra <- read.csv(paste0("B:/mlracsv/mlra_classified_soilcatenas_",watershed.number,".csv"))
  mlra.traverseid <- data.frame(cbind(mlra$Classification,paste0("W",watershed.number,"T",mlra$traverse.groups)))
  mlra.traverseid <- mlra.traverseid[seq(1,nrow(mlra.traverseid),3),]
  df.mlra <- rbind(df.mlra, mlra.traverseid)
  
}

colnames(df.mlra) <- c("MLRA_ID", "Catena_ID")
# if a traverse is slightly outside of NC, it will get weird edge readings from the raster. assign those those to NA.
df.mlra$MLRA_ID[!as.numeric(df.mlra$MLRA_ID) %in% c(240,234,264,261,228,241,262)] <- NA

### now we want to tag the comp lists with MLRA id by looking up the traverse ID. 
library(dplyr)

comp.list.mlra <- join(comp.list,df.mlra, by="Catena_ID")

comp.list.mlra$MLRA_ID <- as.factor(comp.list.mlra$MLRA_ID)


save(comp.list.mlra, file= "comp_list_mlra")
}
########################################################################################### SECTION 2 
# Load the result of Section 1, found in "B:/Remap"  
# the result is a data matrix of 2700388 observations of soil map unit data with 628 varibles. 
# Most of the varibles are soil names with a precentage (0-100) of that named soil found in the map unit.  
# other data includes elevation, traverse ID, MLRA_ID, and Daniels Class. Elevation and MLRA_ID
# is dropped in the following section. 
load("B:/Remap/comp_list_mlra")

# load sample data set from Dylans sharpshootR package. 
data(amador)

# convert into adjacency matrix
m <- component.adj.matrix(amador)

# plot network diagram, with Amador soil highlighted
plotSoilRelationGraph(m, s='amador')
all.mlras.in.dataset <- as.numeric(comp.list.mlra$MLRA_ID %>% factor %>% levels)
# Split out one MLRA of transects and then run the fast greedy co-occurnce function.  
for(mlra in all.mlras.in.dataset){

#pull one MLRA
catena.data<- comp.list.mlra[comp.list.mlra$MLRA_ID==mlra,]
catena.data2<- catena.data[!is.na(catena.data$Catena_ID),-628]

# sample classes, 5000 was chosen from a skree plot for association rules. 
# Inbetween 1000-5000 transects were needed for accurately predicting 84% of soils Daniels Classes. 
test <- data.frame(stratified(catena.data2[-628], "Class", 5000))

# pull all components, Im affriad of changing this varible from "test" to something else... 
test <- catena.data2[catena.data2$Catena_ID %in% test$Catena_ID,]
# remove elevation, in hindsight, this was a bad idea. 
test <- test[,-2]

# prime matrix
data.for.matrix <- data.frame(matrix(nrow=sum(!test[,-c(1:5)]==0), ncol=3))
colnames(data.for.matrix) <- c("mukey", "compname", "comppct_r")

#data.for.matrix <- amador[1,]

# This loop takes Comp.list.MLRA (section 1 output) preps it to feed into the sharpshootr cooccurnce matrix. 
row <-1
for( i in 1:nrow(test)){
tryCatch({
test2  <- test[i,!test[i,]==0]
test3 <- test2[,-c(1:6)]  
names <- colnames(test2[,-c(1:5)])[-1]
#names <- append(names, test2$Class)
prs <- as.numeric(test3)
#prs <- append(prs, 20)
catprs <- cbind(test[i,]$Catena_ID, names, prs)  
#colnames(catprs) <- colnames(amador)

data.for.matrix[c(row:(row+nrow(catprs)-1)),] <- data.frame(catprs)
row <- row+nrow(catprs)

}, error = function(e) e
)

}


#remove that first one which is the amador soil. 
data.for.matrix<- data.frame(data.for.matrix[-1,])
data.for.matrix$comppct_r <- as.numeric(data.for.matrix$comppct_r)

# The catena ID is sometimes put in the comp names, it produces NAs and should be removed. 
data.for.matrix <- data.for.matrix[!is.na(data.for.matrix$comppct_r),]

#m <- component.adj.matrix(data.for.matrix)
m.o <- component.adj.matrix(data.for.matrix, method='occurrence', return.comm.matrix=TRUE, similarity=FALSE)

 graph <- plotSoilRelationGraph(m.o, plot.style="dendrogram")
graph$membership

save(graph, file=paste0("clustergraph_MLRA",mlra))

}

################################################# classifying watersheds 
setwd("B:/modelNC")
for(mlra in as.numeric(comp.list.mlra$MLRA_ID %>% factor %>% levels)){
  
  #pull one MLRA
  catena.data<- comp.list.mlra[comp.list.mlra$MLRA_ID==mlra,]
 # remove catena ID
  catena.data2<- catena.data[!is.na(catena.data$Catena_ID),-628]

rm(graph)  
load(file=paste0("B:/Remap/clustergraph_MLRA",mlra))


# make a matrix that holds all of the catena data in the same way the amador does.

catena.data2[is.na(catena.data2)] <- 0

# prime matrix
all.data.for.matrix <- matrix(nrow=sum(!catena.data2[,-c(1:6)]==0), ncol=3)
colnames(all.data.for.matrix) <- c("mukey", "compname", "comppct_r")

row <-1
for( i in 1:nrow(catena.data2)){
#  for( i in 1:100){
  tryCatch({
    test2  <- catena.data2[i,!catena.data2[i,]==0]
    test3 <- test2[,-c(1:6)]  
    names <- colnames(test2[,-c(1:5)])[-1]
    #names <- append(names, test2$Class)
    prs <- as.numeric(test3)
    #prs <- append(prs, 20)
    catprs <- cbind(catena.data2[i,]$Catena_ID, names, prs)  
    #colnames(catprs) <- colnames(amador)
    all.data.for.matrix[c(row:(row+nrow(catprs)-1)),] <- catprs
    row <- row+nrow(catprs)
    
  }, error = function(e) e
  )
  
}
 save(all.data.for.matrix, file = paste0("all_data_for_matrix",mlra))
}

# that takes 3 hours to run, just load it from remap 
load(file=paste0("B:/Remap/clustergraph_MLRA",mlra))
load(paste0("B:/modelNC/all_data_for_matrix",mlra))
 
all.data.for.matrix.narm <- data.frame(all.data.for.matrix[!is.na(all.data.for.matrix[,1]),])

all.data.for.matrix.narm$compname %>% unique %>% length

# sometimes one works, sometimes the other? 
comp.classes<- data.frame(graph$names,graph$membership)
#comp.classes<- data.frame(cbind(graph$names, as.numeric(rep(1,length(graph$membership[[1]])))))
#for(class in 2:length(graph)){
#  comp.classes<- rbind(comp.classes,data.frame(cbind(graph[[class]], as.numeric(rep(class,length(graph[[class]]))))))
#}

colnames(comp.classes) <- c("compname","clusterclass")

matrix.comp.class <- plyr::join(all.data.for.matrix.narm,comp.classes)
cat_id<- unique(matrix.comp.class$mukey)

#one.traverse <- matrix.comp.class[matrix.comp.class$mukey %in% itter,]
traverses.unclassed<- matrix.comp.class[!matrix.comp.class$compname %in% c(1:16),]
traverses.unclassed[is.na(traverses.unclassed$comppct_r)] <- 0
traverses.unclassed$comppct_r <-  as.numeric(traverses.unclassed$comppct_r)
 
ag <- aggregate(comppct_r~mukey+clusterclass, data=traverses.unclassed, FUN=sum)

comp.percent <- NULL
for(comp in 1:length(ag$comppct_r)){
comp.percent[comp] <- ag$comppct_r[comp]/sum(ag$comppct_r[ag$mukey==ag$mukey[comp]])
}

ag.class <- cbind(ag,comp.percent)

un.mu <- unique(ag.class$mukey)

trav.class <- data.frame(matrix(ncol=length(levels(factor(ag.class$clusterclass)))+1))
colnames(trav.class)<- c("Traverse_ID", levels(factor(ag.class$clusterclass)))
#trav.class

for(id in 1:length(un.mu)){
holding.vec <- un.mu[id]
for(class in 1:length(levels(factor(ag.class$clusterclass)))){
traverse <- ag.class[ag.class$mukey==un.mu[id],]
if(length(traverse[traverse$clusterclass==class,]$comp.percent)==0) holding.vec[class+1] <- 0 else
holding.vec[class+1] <- 0+as.numeric(traverse[traverse$clusterclass==class,]$comp.percent)
}

trav.class[id,] <- holding.vec
}

max.class <- NULL
max.precentage <- NULL
for(each in 1:length(trav.class$Traverse_ID)){
max.class[each]<- as.numeric(colnames(trav.class[each,-1])[trav.class[each,-1]==max(as.numeric(trav.class[each,-1]))][1])
max.precentage[each] <- max(as.numeric(trav.class[each,-1]))
}

# plot(max.precentage)
 sum(max.precentage>.4)


trav.fuzzyclass <- cbind(max.class,max.precentage,trav.class)
trav.fuzzyclass$mlra_id <- mlra
trav.fuzzyclass$mlra_class <- paste0(trav.fuzzyclass$mlra_id,trav.fuzzyclass$max.class)
write.csv(trav.fuzzyclass,file=paste0("catena_fuzzyclass_",mlra,".csv"))

}

}
