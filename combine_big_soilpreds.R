# load and flatten NC soil systems stacked predictions.


library(raster)
library(dplyr)


# load all map units for NC, varible named "m" 
load("B:/traverses/MUs/flat_elevation_tmapunits")




# Load all knn NCmlra SSytems stacked 
load("B:/modelNC/allMLRA/NCmlra_stacked")
m$sscl <- 0
m$maxpct <- 0


for(key in 1:length(m)){

mukey<- m$mupolygonkey[key]

g <- data.frame(cbind(NCmlra$sscl[NCmlra$mplygnk==mukey],NCmlra$maxpct[NCmlra$mplygnk==mukey]))
colnames(g) <- c("sscl","maxpct")
h <- aggregate(maxpct~sscl, data=g, FUN=sum)

try(m$sscl[key] <- h[h$maxpct==max(h$maxpct),1][1])
try(m$maxpct[key] <- h[h$maxpct==max(h$maxpct),2][1])

}

# after waiting many hours for this to finish, I realized that I could have extracted 
# the mupoloygon keys, sscl, and maxpct from m. this would allow me to drop all 25 gb of files as do a foreach, 
# then a simple join function. frankly just need an insert column 


mplygnk <- as.numeric(NCmlra$mplygnk)
sscl <- as.numeric(NCmlra$sscl)
maxpct <- as.numeric(NCmlra$maxpct)
mupolygonkey <- as.numeric(m$mupolygonkey)

df_ncmlra <- data.frame(cbind(mplygnk,sscl,maxpct))
df_m <- data.frame(mupolygonkey=mupolygonkey, sscl=rep(0,length(mupolygonkey)) , maxpct=rep(0,length(mupolygonkey)) )

rm(NCmlra)

setwd("B:/modelNC/ncflat")

save(df_ncmlra, file="df_ncmlra")
save(df_m, file="df_m")

# restart the session here 

load("df_m")
load("df_ncmlra")
library(raster)
library(dplyr)
library(doParallel)
library(doSNOW)

cl <- makePSOCKcluster(6)
registerDoParallel(cl, cores=6)

#polykey_sscl_maxpct <- foreach(key = 1:length(df_m$mupolygonkey), .combine = rbind) %dopar% ({
  polykey_sscl_maxpct <- foreach(key = 1:length(df_m$mupolygonkey), .combine = rbind, .maxcombine=100000, .multicombine = TRUE) %dopar% ({
    
  try(polykey <- df_m[key,1])
  try(g <- df_ncmlra[df_ncmlra$mplygnk==polykey,-1])
  try(h <- aggregate(maxpct~sscl, data=g, FUN=sum))
  if(nrow(g)==0) cbind(polykey,sccl=0,maxpct=0) else cbind(polykey, sccl=h[h$maxpct==max(h$maxpct),1][1], maxpct=h[h$maxpct==max(h$maxpct),2][1])
  
})
   
stopImplicitCluster()
stopCluster(cl)
  
  
df.psm <-  polykey_sscl_maxpct %>% data.frame
save(df.psm, file = "polykey_sscl_maxpct")
   
# classify final map    
load("B:/traverses/MUs/flat_elevation_tmapunits")
colnames(df.psm)[1] <- "mupolygonkey"
m$mupolygonkey <- as.numeric(m$mupolygonkey)
df.psm$mupolygonkey <- as.numeric(df.psm$mupolygonkey)


check <- NULL
for(i in 1:length(df.psm$mupolygonkey)){
check[i] <- m$mupolygonkey[i]==df.psm$mupolygonkey[i]
}

sum(check) #ok they are still the same just paste the values

m$sscl <- df.psm$sccl
m$maxpct <- df.psm$maxpct

save(m, file="undisolved_classed_ssmap")
shapefile(m,file="undisolved_classed_ssmap")

# disolve 

require("rgdal")
require("rgeos")

disolved_ssmap <- gUnaryUnion(m, id = m$sscl)

shapefile(disolved_ssmap,file="disolved_classed_ssmap")


