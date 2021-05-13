setwd("B:/modelNC/ncflat")
processed <- list.files(pattern= "polysccl\\d*.shp")

library(raster)

library(rgeos)

processed <- list.files(pattern= "polysccl\\d*.shp")


r <- shapefile(processed[208])

for(i in 209:length(processed)){
  t <- shapefile(processed[i])
  #t <- gBuffer(t, width = 0)
  r <- bind(r,t)
  r <- raster::aggregate(r, "sccl")
}



# somethings wrong with 207?? 

r2 <- shapefile(processed[208])

for(i in 209:length(processed)){
t <- shapefile(processed[i])
#t <- gBuffer(t, width = 0)
r2 <- bind(r2,t)
r2 <- raster::aggregate(r2, "sccl")
}

# saving the files as halves let us see that WS 207 is ether covered via an overlap in other watersheds, or is infact in "r". 
shapefile(r, file="halfDisolved_sccl_NC", overwrite=TRUE)
shapefile(r2, file="halfDisolved_sccl_NC2", overwrite=TRUE)


r3 <- bind(r,r2)
r3 <- raster::aggregate(r3, "sccl")




shapefile(r, file="Disolved_sccl_NC2", overwrite=TRUE)