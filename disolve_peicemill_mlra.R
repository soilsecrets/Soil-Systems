setwd("B:/modelNC/MLRA228/flat/KNN")


library(raster)
library(rgeos)
library(dplyr)

processed <- list.files(pattern= "KNN\\d*.shp")


allwatersheds <- shapefile("allwatersheds_flat")

s <- allwatersheds[!duplicated(allwatersheds$mplygnk),]
data.to.join <- data.frame(mplygnk=s$mplygnk, maxpct=s$maxpct,sscl=s$sscl, X2281=s$X2281, X2282=s$X2282, X2283=s$X2283)


r <- shapefile(processed[1])

e <- data.frame(mplygnk=r$mplygnk)

joined_flat <- right_join(data.to.join,e,"mplygnk")


r$sscl <- joined_flat$sscl
r$maxpct <- joined_flat$maxpct
r$X2281 <- joined_flat$X2281
r$X2283 <- joined_flat$X2281
r$X2282 <- joined_flat$X2281
r <- raster::aggregate(r, by="sscl")



# this forloop does the same as a the above but to each loaded file. 

for(i in 30:length(processed)){
  t <- shapefile(processed[i])
  b <- data.frame(mplygnk=as.character(t$mplygnk))
  
  joined_b <- right_join(data.to.join,b,"mplygnk")
  t$sscl <- joined_b$sscl
  t$maxpct <- joined_b$maxpct
  t$X2281 <- joined_b$X2281
  t$X2283 <- joined_b$X2281
  t$X2282 <- joined_b$X2281
  
  
  
  
  #t <- gBuffer(t, width = 0)
  r <- bind(r,t)
  r <- raster::aggregate(r, by="sscl")
}

