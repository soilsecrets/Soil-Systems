

# plot predicted with colors
setwd("B:/modelNC")
library(rgdal)
library(maptools)
library(plyr)

mlra_cluster_class  <- data.frame(read.csv("catena_fuzzyclass_228.csv"))
# Load in the predictions vs the actual with the
# pred.data <- trav.fuzzyclass
pred.data  <- mlra_cluster_class  

# Load in the traverses with MUpolygonkeys
mupolygon_lookup <- read.csv("B:/Remap/Classified_NC_Traverses_uniqueID_mupolykey.csv")
colnames(mupolygon_lookup)[2] <- "Traverse_ID"

mupolygon_lookup_fuzzyclass <- join(pred.data, mupolygon_lookup, "Traverse_ID")


# use regex to split the watershed IDS to load watersheds in a bit. 
watershed <- unlist(strsplit(mupolygon_lookup_fuzzyclass$Traverse_ID,"T\\d+"))
traverse <- unlist(sapply(mupolygon_lookup_fuzzyclass$Traverse_ID,function(x){strsplit(x,"W\\d+")}))
# I dont know why it returns a blank as well. and I don't think this will be needed anyway.
traverse <- traverse[!traverse==""]

# bind the split strings with the original data.
pred.data.watershed.traverse<- cbind(watershed, traverse, mupolygon_lookup_fuzzyclass) 
# They apparently came with row names, get rid of them. 
row.names(pred.data.watershed.traverse) <-NULL 
mupolygon_lookup_wpred_watershed <- pred.data.watershed.traverse 


{# This will take a long time, roughly half an hour, find matches for traverse IDS for the pred data. 
match.row <- sapply(pred.data.watershed.traverse$V1,function(x) (match(x,mupolygon_lookup$Split.ID)))

# Bind the together the pred.data with the match row data. 
match.pred.data <- cbind(match.row,pred.data.watershed.traverse)

# make sure it worked
tail(match.pred.data)

# Bind together a matrix that in the length of all of the traverses in NC and the traverse data.
# This will be filled with NAs, that we will fill with the pred class  at the appropriate rows. 
# This for loop will take half an hour or so, could be run in par with a for each loop. 
holding.pred <- matrix(nrow = nrow(mupolygon_lookup), ncol=1)
for(i in 1:nrow(match.pred.data)){
  holding.pred[match.pred.data[i,1]]  <- match.pred.data[i,3]
}

# Bind the output together, drop the row number column in the traverse data. 
mupolygon_lookup_wpred <- cbind(holding.pred,mupolygon_lookup[,-1])

# This has rows that were not classified. 
nrow(mupolygon_lookup_wpred)

# Remove the rows that do not 
na.rm.mupolygon_lookup_wpred <- mupolygon_lookup_wpred[!is.na(mupolygon_lookup_wpred$holding.pred),]



# split by watershed 
watershed.pred <- unlist(strsplit(mupolygon_lookup_wpred$Traverse_ID,"T\\d+"))
watershed.id <- unlist(strsplit(watershed.pred,"W"))
watershed.id <- watershed.id[!watershed.id==""]
mupolygon_lookup_wpred_watershed <- cbind(watershed.id,mupolygon_lookup_wpred)
}

# the below two loops take a watershed of soil polygons, extracts them by traverse, 
# classfies them based on pred and map class, and writes a shape file for each watershed (359 files). 

# still need proceced 
# failed on w283
cont <- as.numeric(c(288,289,29,309,312,313,319,325,326,329,333,344,348,352,356, 3, 43, 4,53, 54,55, 56,61, 65,68, 77,79, 7,81, 83,90, 91,93, 95, 9, 162,40, 41))


#for(watershed_number in as.numeric(sub(".","",mupolygon_lookup_wpred_watershed$watershed %>% unique))){
  for(watershed_number in cont){  
  
  # load watershed of traverses
  working.file.soils <-paste0("elevation_NCDEM30mCRS_mapunits_",watershed_number)
  load(paste0("B:/traverses/MUs/", working.file.soils))
  
  # I don't remeber what this does but I think its important.
  # ah it pulls out the traverses in watershed "watershed_number" comment made 8 months later.
  current_watershed_mukeys <- mupolygon_lookup_wpred_watershed[mupolygon_lookup_wpred_watershed$watershed==paste0("W",watershed_number),]
  
  # Extract polygons by each row of classified data and then assign them the pred and actual class values.
  # This chunk of code does that exact same thing as the loop but the rspbind won't let you just work with a null var.  
  watershed_polygons <- NULL
  watershed_traverses <- watershed_polygons <- elevation.tmapunits[elevation.tmapunits$mupolygonkey
                                                                   %in% current_watershed_mukeys[1,-c(1:12, length(current_watershed_mukeys),length(current_watershed_mukeys)-1)],]
  watershed_traverses$Class <- current_watershed_mukeys$Classification[1]
  #watershed_traverses$predClass <- current_watershed_mukeys$holding.pred[1]
  watershed_traverses$soilsysnumber <- current_watershed_mukeys$mlra_class[1]
  watershed_traverses$soilsysnumberprecent <- current_watershed_mukeys$max.precentage[1]
  
  
  
  # Do the same as the above for then entire length of the classfied dataframe. 
  for(row in 2:nrow(current_watershed_mukeys)){
    # Extract polygons by each row of classified data  
    watershed_polygons <- elevation.tmapunits[elevation.tmapunits$mupolygonkey %in% current_watershed_mukeys[row,-c(1:3)],]
    # create unique FIDS for the polygons incase they overlap. 
    number.of.polygons <- sum(!is.na(current_watershed_mukeys[row,-c(1:13, length(current_watershed_mukeys),length(current_watershed_mukeys)-1)])) 
    FIDS <- paste0(current_watershed_mukeys$Traverse_ID[row],"P",seq(1,number.of.polygons,1))
    
    # Assign them the predclass and class vars to color them later. 
    watershed_polygons$Class <- current_watershed_mukeys$Classification[row]
    #watershed_polygons$predClass <- current_watershed_mukeys$holding.pred[row]
    watershed_polygons$soilsysnumber <- current_watershed_mukeys$mlra_class[row]
    watershed_polygons$soilsysnumberprecent <- current_watershed_mukeys$max.precentage[row]
    watershed_polygons_FIDS<- spChFIDs(watershed_polygons,FIDS)
    
    
    watershed_traverses <- spRbind(watershed_traverses, watershed_polygons_FIDS)
    
  }
  
  #write shape file of the traverses
  try(writeOGR(watershed_traverses, dsn= "B:/modelNC", driver = "ESRI Shapefile", layer = paste0("pred",mlra,watershed_number)))
}

