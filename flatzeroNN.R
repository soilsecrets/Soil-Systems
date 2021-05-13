# Run KNN on each watershed 

#  Libraries 

library(rgdal)
library(raster)
library(plyr)
library(dplyr)
library(rgdal)
library(rgeos)
# Settings 
# what MLRA do you want to process 
mlra <- 228
# set the WD
setwd(paste0("B:/modelNC/MLRA",mlra,"/flat"))

all.files <- list.files(pattern = ".shp")

################################################ Start for loop for each file in all.files
# remove already processed files if you have to restart the loop. 
all.files
f <- sub("flattened","",all.files)
processed <- sub("KNN","",list.files(paste0("B:/modelNC/MLRA",mlra,"/flat/KNN"), pattern = ".shp"))

for(each.file in all.files[!f %in% processed]){
  file.working <- each.file
  ws <- sub(".shp","", sub(paste0("flattened",mlra),"",file.working ))
  #pull one watershed
  watershed <- shapefile(file.working)
  
  #find the unclassed soil polygons 
  
  watershed.unclassed <- watershed[watershed$sscl==0,]
  trys <- 0
  
  while(length(watershed.unclassed)>=1){
    
    watershed.unclassed <- watershed[watershed$sscl==0,]
    trys <- trys+1 
    
    # LOOP HERE
    
    try(for(each.soil in 1:length(watershed.unclassed)){
      one.poly <- watershed.unclassed[each.soil,]
      
      # buffer by 10 units 
      buffered.polygon <- gBuffer(one.poly, width=10)
      
      over.poly <- watershed %over% buffered.polygon 
      over.poly[is.na(over.poly)] <-0 
      over.poly <- as.logical(over.poly)
      
      bufferedpolys <- watershed[over.poly,]
      
      ag.soil <- aggregate(maxpct~sscl, data= bufferedpolys, sum)
      
      maxclass <- ag.soil[ag.soil$maxpct==max(ag.soil$maxpct),1]
      maxpct <- ag.soil[ag.soil$maxpct==max(ag.soil$maxpct),2]/ (sum(bufferedpolys$sscl==maxclass)+1)
      
      watershed$sscl[watershed$mplygnk %in% one.poly$mplygnk] <- maxclass
      watershed$maxpct[watershed$mplygnk %in% one.poly$mplygnk] <- maxpct
    }
    )
    
    if(trys==100) break else print("another go around")
    
  }
  
  try(writeOGR(watershed, dsn= paste0("B:/modelNC/MLRA",mlra,"/flat/KNN"), driver = "ESRI Shapefile", layer = paste0("KNN",mlra,ws)))
}

