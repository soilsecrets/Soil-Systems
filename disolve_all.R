library(raster)
library(rgeos)
library(dplyr)

setwd("B:/traverses/MUs")

load("all_elevation_tmapunits")
load("B:/modelNC/ncflat/polykey_sscl_maxpct")

mpoly <- data.frame(mupolygonkey= m$mupolygonkey)

names(df.psm)[1] <- "mupolygonkey"

mpred <- plyr::join(mpoly,df.psm, by="mupolygonkey")

# check to see if mpred is the same order. 
for(i in 1:nrow(mpoly)){
  if(mpoly[i,]==mpred[i,1]) 0+0 else print("oops")
  
}
# no "oops" so bind them back in 

m$sccl <- mpred$sccl
m$maxpct <- mpred$maxpct

setwd("B:/modelNC/ncflat")
save(m, file= "unflat_soil_polygons_with_sccl")

disolved_ssmap <- gUnaryUnion(m, id = m$sscl)

shapefile(disolved_ssmap,file="disolved_classed_ssmap_2")