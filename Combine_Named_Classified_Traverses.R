# This takes classified_soilcatenas_### and combines them into one CSV with W###T#### ids 
setwd("B:/Classified")

# create a matrix to hold all of the traverses, 22 col is based on the most MUs in a traverse. 
Matrix.of.holding <- matrix(nrow = 1, ncol = 22)

#

# This loop loads one of the 359 watersheds in NC for its classifed traverses, 
# adjust length as needed for number of files. Once loaded  This loop pulls out
# one data type and binds it to the above matrix. This loop also creates a unit
# W###T#### for each traverse. 

for(watershed in 1:359 ){
  
  # load a watershed of traverses 
    a.set.of.traverses <- read.csv(paste0("classified_soilcatenas_",watershed,".csv"))
  
  # Change from to 1,2,3 if you want mupolygonkey/elevation/natmapsym in that order
  the.type.of.data <- seq(from=1, to=length(a.set.of.traverses$traverse.groups), 3) 
  
  # pull out those rows from a.set.of.traverses
  a.set.of.traverses.subset <- a.set.of.traverses[the.type.of.data,-c(1,3)]
  
  # Make unit IDS
  Split.ID <- paste0("W",watershed,"T", a.set.of.traverses.subset$traverse.groups)
  
  # bind those together, drop the traverse groups.
  a.traverse.with.split.ids <- cbind(Split.ID, a.set.of.traverses.subset[,-2])
  
  # apparently have to fill this dataframe to 22 
  
   filled.traverse.with.split.ids <- matrix(nrow = nrow(a.traverse.with.split.ids), ncol = 22) 
    for(traverse in 1:nrow(a.traverse.with.split.ids)){  
      filled.traverse.with.split.ids[traverse,] <- c(paste(a.traverse.with.split.ids[traverse,]),
                                                    rep("",ncol(Matrix.of.holding)-length(a.traverse.with.split.ids[traverse,])))
    }
  
  # bind original matrix and a.traverse.with.split.ids
   Matrix.of.holding <- rbind(Matrix.of.holding,filled.traverse.with.split.ids)
  
} 

# remove that one dumb NA string at the top
Matrix.of.holding <- Matrix.of.holding[-1,]

write.csv(Matrix.of.holding, file="Classified_NC_Traverses_UniqueID_Mupolygonkey.csv")
