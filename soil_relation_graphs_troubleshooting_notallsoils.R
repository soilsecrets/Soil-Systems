# sharpshootR 

library(sharpshootR)
library(igraph)
library(RColorBrewer)l
library(scales)
library(splitstackshape)
library(dplyr)

setwd("B:\Remap")

###########################################################################################
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

##################################################################################################

load("comp_list_mlra")



# load sample data set
data(amador)

# convert into adjacency matrix
m <- component.adj.matrix(amador)

# plot network diagram, with Amador soil highlighted
plotSoilRelationGraph(m, s='amador')


for(mlra in as.numeric(comp.list.mlra$MLRA_ID %>% factor %>% levels)){
#pull one MLRA
catena.data<- comp.list.mlra[comp.list.mlra$MLRA_ID==mlra,]
catena.data2<- catena.data[!is.na(catena.data$Catena_ID),-628]

# sample classes
test <- data.frame(stratified(catena.data2[-628], "Class", 5000))
# pull all components
test <- catena.data2[catena.data2$Catena_ID %in% test$Catena_ID,]
# remove elevation
test <- test[,-2]

# prime matrix
data.for.matrix <- amador[1,]

for( i in 1:nrow(test)){
tryCatch({
test2  <- test[i,!test[i,]==0]
test3 <- test2[,-c(1:5)]  
names <- colnames(test2[,-c(1:4)])[-1]
names <- append(names, test2$Class)
prs <- as.numeric(test3)
prs <- append(prs, 20)
catprs <- cbind(test[i,]$Catena_ID, names, prs)  
colnames(catprs) <- colnames(amador)
data.for.matrix <- rbind(data.for.matrix, catprs)
}, error = function(e) e
)

  }
#remove that first one
data.for.matrix<- data.for.matrix[-1,]
data.for.matrix$comppct_r <- as.numeric(data.for.matrix$comppct_r)

#m <- component.adj.matrix(data.for.matrix)
m.o <- component.adj.matrix(data.for.matrix, method='occurrence', return.comm.matrix=TRUE, similarity=FALSE)

graph <- plotSoilRelationGraph(m.o, plot.style="dendrogram")
graph$membership


comp.classes<- data.frame(cbind(colnames(m.o),graph$membership))
colnames(comp.classes) <- c("compname","clusterclass")

matrix.comp.class <- plyr::join(data.for.matrix,comp.classes)
cat_id<- unique(matrix.comp.class$mukey)

#one.traverse <- matrix.comp.class[matrix.comp.class$mukey %in% itter,]
traverses.unclassed<- matrix.comp.class[!matrix.comp.class$compname %in% c(1:16),]

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
# sum(max.precentage>.4)


trav.fuzzyclass <- cbind(max.class,max.precentage,trav.class)
write.csv(trav.fuzzyclass,file=paste0("trav_fuzzyclass_",mlra,".csv"))
}
