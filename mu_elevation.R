# Combine elevations with presence data 


# load the environment environment_with_OH_data
library(mltools)
library(splitstackshape)
library(NbClust)
library(supcluster)
library(arules)
library(dplyr)
setwd("B:/Classified") 


# load entire named catena dataset. 
elevation <- read.csv("Classified_NC_Traverses_UniqueID_elevation.csv")
mukey <- read.csv("Classified_NC_Traverses_UniqueID_Mupolygonkeys.csv")


# take mu keys and get them ready for a matrix via list 



mukey.list <- list() 
for(i in 779087:nrow(mukey)){
  holding <- mukey[i,-1]
  holding <- holding[!is.na(holding)]
  holding <- holding[!holding==""]
  mukey.list[i] <- list(holding) 
  }

mukey.trans <- as(mukey.list, "transactions")

############ 

setwd("B:/Named")
load("named.catenas")

df.named.catenas

df.named.catenas.subsample <- data.frame(stratified(df.named.catenas, group = "Class", 1000))


named.list.sub <- list() 
for(i in 1:nrow(df.named.catenas.subsample)){
  holding <- df.named.catenas.subsample[i,-c(1,2)]
  holding <- holding[!is.na(holding)]
  holding <- holding[!holding==""]
  named.list.sub[i] <- list(holding) 
}




named.trans <- as(named.list.sub,"transactions")

named.matrix <- as(named.trans, "matrix")
oh.named <- one_hot(named.matrix)


# FOR LOOP HERE 
for(y in 1:nrow(oh.named)){
  
  # Pull out the a row of data
oh.holding <- oh.named[y,oh.named[y,]==TRUE]

# pull and clean row of names 
named.holding <- df.named.catenas.subsample[y,-c(1,2)]
named.holding <- named.holding[!is.na(named.holding)]
named.holding <- named.holding[!named.holding==""]

# pull and clean row of elevation 
# apparently some of the elevations are not there due to
# the size of the MU polygon or it being in the ocean. 
elevation.holding <- elevation[elevation$V1 == df.named.catenas.subsample[y,2],-c(1,2,3)]
elevation.holding <- elevation.holding[1:length(named.holding)]
# I'm picking assigning these elevations that are NA as 1 as an indicator of it
# existing rather than it being the correct elevation 
elevation.holding[is.na(elevation.holding)] <- 1

# bind together
holding.matrix <- cbind(named.holding,elevation.holding)


# loop to create a correctly ordered vector of elevations
elevation.ordered  <- NULL
for(i in 1:length(oh.named[y,oh.named[y,]==TRUE])){
  elevation.ordered[i] <- as.numeric(holding.matrix[,2][holding.matrix[,1] == names(oh.holding[i])])
}

# replace the TRUE values with the numeric values 
oh.named[y,oh.named[y,]==TRUE] <- elevation.ordered

}



max.elevations <- NULL
for(i in 1:nrow(oh.named)){
max.elevations[i]<- max(oh.named[i,],na.rm=TRUE)
}

plot(max.elevations)




################# Cluster time???? 

oh.named %>% head() 


# How do we cluster them? supervised clustering. k-means wont work 
nb <- NbClust(oh.named, distance = "maximum", min.nc = 10, max.nc = 20, method = "average", index ="gap")

unique(nb$Best.partition)
plot(nb$Best.partition)
sum(nb$Best.partition==1 )

scaled.oh.named <- oh.named/max(oh.named)
scaled.oh.named
dis.oh.named <- affinity(scaled.oh.named)

### hierarchical cluster attempt 

hclust


