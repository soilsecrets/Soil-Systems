#fastknn  

# Run KNN on each watershed 

#  Libraries 

library(rgdal)
library(raster)
library(plyr)
library(dplyr)
library(rgdal)
library(rgeos)
library(spatialEco)
# Settings 
# what MLRA do you want to process 
mlra <- 262
# set the WD
setwd(paste0("B:/modelNC/MLRA",mlra,"/flat"))

all.files <- list.files(pattern = ".shp")

################################################ Start for loop for each file in all.files
# remove already processed files if you have to restart the loop. 
all.files
f <- sub("flattened","",all.files)
processed <- sub("KNN","",list.files(paste0("B:/modelNC/MLRA",mlra,"/flat/KNN"), pattern = ".shp"))
all.files[!f %in% processed]

for(each.file in all.files[!f %in% processed]){
  file.working <- each.file
  ws <- sub(".shp","", sub(paste0("flattened",mlra),"",file.working ))
  #pull one watershed
  watershed <- shapefile(file.working)
  
  #find the unclassed soil polygons 
  
  watershed.unclassed <- watershed[watershed$sscl==0,]
  watershed.classed <- watershed[!watershed$sscl==0,]
  k <- min(c(10,length(watershed.classed)))
  ###############
  
  
  
  knn.poly <- data.frame(knn(watershed.unclassed,watershed.classed, k=k, indexes = TRUE))
  
  
  # FORLOOP HERE
  
  for(eachpolygon in 1:nrow(knn.poly)){
    one.poly <- watershed.unclassed[eachpolygon,]
    select.knn <- c(as.numeric(paste(knn.poly[eachpolygon,1:k])))
    distances <- mean(c(as.numeric(paste(knn.poly[eachpolygon,(k*2+1):(k*2+k)]))))
    
    
    
    knn.polygons <- watershed.classed[select.knn,]
    
    ag.soil <- aggregate(maxpct~sscl, data= knn.polygons, function(x) mean(x)*length(x))
    
    maxclass <- ag.soil[ag.soil$maxpct==max(ag.soil$maxpct),1]
    
    # I want to weight the distance from the polygon to the "precentage sure" in max class 
    # to do this I took the mean distence and inversly weight the sum of the 10 knn precentages.
    # if this is over... 100? it should be a value under 1, which is good! 
    # if its under 3 it'll be aprox the sum of the precentages, but with the mean of the means
    # being 7000, I think we will be fine. 
    maxpct <- ag.soil[ag.soil$maxpct==max(ag.soil$maxpct),2] * (1/log(distances))
    
    watershed$sscl[watershed$mplygnk %in% one.poly$mplygnk] <- maxclass
    watershed$maxpct[watershed$mplygnk %in% one.poly$mplygnk] <- maxpct
    
  }
  
  
  
  
  try(writeOGR(watershed, dsn= paste0("B:/modelNC/MLRA",mlra,"/flat/KNN"), driver = "ESRI Shapefile", layer = paste0("KNN",mlra,ws)))
  
}
