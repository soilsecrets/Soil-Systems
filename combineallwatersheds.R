setwd("B:/remapclusters")

library(rgdal)

hold <- shapefile("pred358.shp")
hold2 <- shapefile("pred1.shp")

neat <- bind(hold,hold2)

for(file in 23:357){
  try(rm(hold3))
  try(hold3 <- shapefile(paste0("pred",file,".shp")))
  try(neat <-   bind(neat,hold3))
  
}

shapefile(neat, filename="allwatersheds")



