# Precentages and lengths of soil components per transect weeeee

setwd("B:/Classified")

# libraries 
library(dplyr)
library(arules)
load.packages <- pacman::p_load(soilDB ,aqp, foreach ,doParallel ,doSNOW)

# load MUpolygon keys 
all.MUkeys <- read.csv("Classified_NC_Traverses_uniqueID.csv")
head(all.MUkeys)

# remove the classificationless soils 
all.MUkeys.na.rm <- all.MUkeys[!is.na(all.MUkeys$Classification),] 
all.MUkeys.na.rm <-all.MUkeys.na.rm[all.MUkeys.na.rm$Classification %in% c(1:16),-2]

# clean out NAs 
all.MUkeys.na.rm[is.na(all.MUkeys.na.rm)] <- "" 
soils <- all.MUkeys.na.rm


# set up a holding list for the various soil components. This list will be populated
# with itter of the for loop, traverse id, comp number of each polygon, and the component
# names of each polygon. finally the comp pecentages will be filled in to the comp names. 
# They don't all add up to 100% 

SDA.list <- list() 

# Make a foreach cluster. Change detectCores() to detectCores()-1 
# if you want to play Kerbal Space Program while its running. 
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl,detectCores())

# This for each loop used soilDB to extract the names of the soil component that
# This takes roughly 90 seconds for 20 soil traverses. 
# This doesn't take up too much room, only requires one package, and would prob 
# run really well on DISCOVERY HPC. 

SDA.list <-  foreach(itter=1:nrow(soils),.combine='c', .packages="soilDB") %dopar% {

  # test   
  #   for(itter in 222:230) {
  
  # pull out one row of a soil system
  natmusys <- paste(soils[itter,3:20])[!paste(soils[itter,3:20]) ==""]
  
  # create the SQL string to query 
  a.string <- paste("nationalmusym=\'",natmusys,"\'",sep="")
  
  # Query SDA for comp names and comp precentacges 
  df.SDA.names <- sapply(a.string, function(x){get_component_from_SDA(WHERE = x)$compname})
  df.SDA.ammount <- sapply(a.string, function(x){get_component_from_SDA(WHERE = x)$comppct_r})
  
  # create a col named data frame for each comp name. 
  for(comp in 1:length(a.string)){
    
  df.comp <- data.frame(matrix(ncol = length(df.SDA.names[[comp]])+3))
  colnames(df.comp)[1] <- "itter"
  colnames(df.comp)[2] <- "Traverse_ID"
  colnames(df.comp)[3] <- "Comp_Number"
  colnames(df.comp)[4:length(colnames(df.comp))] <- df.SDA.names[[comp]]
    
  df.comp$itter <- itter
  df.comp$Traverse_ID <- soils$Split.ID[itter]
  df.comp$Comp_Number <- comp
  df.comp[4:length(colnames(df.comp))] <- df.SDA.ammount[[comp]]
  
  
  
  }
  # create a list of the comp name data frame. This is then c() togethered via the foreach loop. 
  # and put into the SDA.list var in the order they are done processing. 
  list(df.comp) 
  
  }
  

head(SDA.list)

# End the cluster
stopCluster(cl)
stopImplicitCluster()


lapply(SDA.list, colnames) %>% unlist %>% unique 

