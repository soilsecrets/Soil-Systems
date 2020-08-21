load.packages <- pacman::p_load(soilDB ,aqp, foreach ,doParallel ,doSNOW)
load.packages

# This retreives the soil NatMapSymbolkey
# Create a matrix to hold the ID and the NatMapSymbolkey of each MU in the traverse. 
# ncol=19 will need to be changed if the max length of the ragged array is longer than 18 MUs. 
matrix.of.holding <- matrix(nrow=nrow(soils),ncol=19) 

# Populate the first column of the matrix with the IDs of each traverse. 
Soil.split.ID <- soils[,1]
soil.name.matrix <- cbind(Soil.split.ID,matrix.of.holding)


# Make a foreach cluster. Change detectCores() to detectCores()-1 
# if you want to play Kerbal Space Program while its running. 
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl,detectCores())

# This for each loopused soilDB to extract the names of the soil component that
# comprises the highest precentage of each MU in the traverses. 
# This takes roughly 90 seconds for 20 soil traverses. 
foreach(itter=1:nrow(soils), .packages="soilDB") %dopar% {
  
  # pull out one row of a soil system
  natmusys <- paste(soils[itter,4:22])[!paste(soils[itter,4:22]) ==""]
  
  # create the SQL string to query 
  a.string <- paste("nationalmusym=\'",natmusys,"\'",sep="")
  
  # Query and create a df for the soil system
  df.SDA.names <- sapply(a.string, function(x){get_component_from_SDA(WHERE = x)$compname})
  df.SDA.ammount <- sapply(a.string, function(x){get_component_from_SDA(WHERE = x)$comppct_r})
  
  # Find the max precentage of a soil comp
  df.SDA.ammount.max <- sapply(df.SDA.ammount,function(x){x==max(x)})
  
  # create a vector of names in order
  soil.vector.names <- paste(unlist(df.SDA.names)[unlist(df.SDA.ammount.max)])
  
  # create a NA vector as filler 
  soil.vector.filler <- rep(NA,ncol(soil.name.matrix)-1-length(unlist(df.SDA.names)[unlist(df.SDA.ammount.max)]))
  
  # combine the two lists and write to DF 
  soil.name.matrix[itter,2:20]<- c(soil.vector.names,soil.vector.filler)
}

# End the cluster
stopCluster(cl)
stopImplicitCluster()