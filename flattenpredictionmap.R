#flattenpredictionmap
#  Libraries 

library(rgdal)
library(raster)
library(plyr)
library(dplyr)

# Settings 
# what MLRA do you want to process 
mlra <- 228
# set the WD
setwd(paste0("B:/modelNC/MLRA",mlra))

csv <- read.csv(file=paste0("B:/modelNC/catena_fuzzyclass_",mlra,".csv"))
all.files <- list.files(pattern = ".shp")

################################################ Start for loop for each file in all.files
# remove already processed files if you have to restart the loop. 
all.files
f <- sub("pred","",all.files)
processed <- sub("flattened","",list.files(paste0("B:/modelNC/MLRA",mlra,"/flat"), pattern = ".shp"))

for(each.file in all.files[!f %in% processed]){
file.working <- each.file
ws <- sub(".shp","", sub(paste0("pred",mlra),"",file.working ))


watershed <- shapefile(file.working)
watershed_number <- file.working %>% sub(pattern = ".shp",replacement = "") %>% sub(pattern = "pred...",replacement = "") %>% as.numeric

working.file.soils <-paste0("elevation_NCDEM30mCRS_mapunits_",watershed_number)
load(paste0("B:/traverses/MUs/", working.file.soils))

number.of.classes<- ncol(csv)-4

df.per <- data.frame(matrix(ncol=number.of.classes,nrow=length(elevation.tmapunits)))
colnames(df.per) <- c("sscl","maxpct",paste0(mlra,1:(ncol(csv)-6)))
df.per[is.na(df.per)] <-0

lws <- lapply(elevation.tmapunits$mupolygonkey, function(x) watershed$mplygnk %in% x)
lwsc <- lapply(lws,function(x) watershed[x,(ncol(watershed)-1):(ncol(watershed))])

blank.mu <- data.frame(matrix(ncol = 2))
colnames(blank.mu) <- c("solsysnmbr","slsysnmbrp")
blank.mu[1,] <- 0


lwsca <- lapply(lwsc,function(x) tryCatch(aggregate(slsysnmbrp~solsysnmbr,data=x, FUN=sum), error=function(ep){blank.mu}))

ldf <- lapply(lwsca, function(x) setNames(data.frame(rbind(data.frame((x))[,2])),data.frame((x))[,1]))

# Thank you User mrsdalloway https://stackoverflow.com/questions/34517711/replace-column-if-the-same-column-name-r
replace_imputed <- function(original, imputed){
  
  namestoChange <- names(unlist(imputed))
  
  for(i in 1:length(namestoChange)){
    original[namestoChange[i]] <- imputed[namestoChange[i]]
  }
  return(original)
  
}



for(i in 1:nrow(df.per)){
df.per[i,] <- replace_imputed(df.per[i,], unlist(ldf[i]))
}

for(i in 1:nrow(df.per)){
try(if(max(df.per[i,])==0) df.per[i,1 ] <-  0 else  df.per[i,1 ] <- as.numeric(colnames(df.per[i,])[df.per[i,]==max(df.per[i,])]))
  
}

for(i in 1:nrow(df.per)){
df.per[i,2] <- max(df.per[i,-c(1,2)])
}




flatened.soil.system.map <- cbind(elevation.tmapunits,df.per)

try(writeOGR(flatened.soil.system.map, dsn= paste0("B:/modelNC/MLRA",mlra,"/flat"), driver = "ESRI Shapefile", layer = paste0("flattened",mlra,ws)))
}
