# just disolve things based on watershed and build up... 

library(raster)
library(rgeos)
library(dplyr)


setwd("B:/modelNC/ncflat")
# load the agregated prediction of soil systems 
load("polykey_sscl_maxpct")
names(df.psm)[1] <- "mupolygonkey"

load.files <- list.files(path="B:/traverses/MUs", pattern= "elevation_NCDEM30mCRS")

for(file in 1:length(load.files)){
try(rm(m))
try(rm(m2))
try(rm(m3m))
try(rm(elevation.tmapunits))  

load(paste0("B:/traverses/MUs/",load.files[file])) 
m <- data.frame(as.numeric(elevation.tmapunits$mupolygonkey))
names(m) <- "mupolygonkey"


m2<- left_join(m,df.psm,by="mupolygonkey")
elevation.tmapunits$sccl <- m2$sccl
elevation.tmapunits$maxpct <- m2$maxpct

m3m <- raster::aggregate(elevation.tmapunits, "sccl")

shapefile(m3m, file=paste0("polysccl", file), overwrite=TRUE)

}

# rm(ls())


processed <- list.files(pattern= "polysccl\\d*.shp")

r <- shapefile(processed[1])

for(i in 2:length(processed)){
  try(rm(t))
  t <- shapefile(processed[i])
  r <- bind(r,t)
}

buff.r <- gBuffer(r, width=0.0001)
disolved.sccl <- raster::aggregate(buff.r, "sscl")
shapefile(disolved.sccl, file="Disolved_sccl_NC", overwrite=TRUE)

outline <- shapefile("B:/modelNC/North_Carolina_State_and_County_Boundary_Polygons.shp")


