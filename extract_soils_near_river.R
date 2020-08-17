# This is a second attempt at making a stream adjacent soil MU extraction program. 
# You will need. Soil polygons, streams, watersheb boundry.  
setwd("D:/LRU/Adjacent_MUs_NC")
packages.to.use <- pacman::p_load(rbenchmark ,rgdal ,spdep ,ggplot2 ,leaflet ,sf ,RANN ,dplyr ,graphics ,ggmap
               ,raster ,soilDB ,aqp ,gdistance ,sp ,mapview ,elevatr ,geoknife ,foreach ,doParallel
               ,doSNOW ,rgeos, stringr, spdep)
packages.to.use


# Load WSB and streams RUN ONLY ONCE 
# for(once in 1:1){
  # Read in data of rivers and a watershed boundry.  
  # Import all rivers 
  NC.streams <- readOGR(dsn="D:/LRU/StreamsNC", layer="NCstreams")
  
  # Make same projection 
  tstreams <- spTransform(NC.streams, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  setwd("D:/LRU")
  save(tstreams, file = "NCtstreams_DEM30CRS")
  
  
  
 }

load("D:/LRU/NCtstreams_DEM30CRS")


# Select WSB and load soils of that area

# you have to partition this into chunks otherwise R will keep everything in memory.
# the below for loop ends the memory usage and starts everything back up.
# Change these to be your starts and ends
start <- seq(182,359,50)
end   <- seq(201,359,50)
end[4]<-359



for(t in 1:4){
cl <- makePSOCKcluster(2)
registerDoParallel(cl,2)
# not parrelel :(
foreach(water.shed = start[t]:end[t], .packages = c("rgeos", "raster")) %dopar% {

  # clear varibles 
  rm(list = setdiff(ls(), c("tstreams","water.shed")))  
  # current.water.shed <- str_remove(paste(water.shed),"elevation_DEM30mCRS_mapunits_")
  load(paste("elevation_NCDEM30mCRS_mapunits_",water.shed, sep=""))

  trivers <- spTransform(crop(tstreams,extent(elevation.tmapunits)), crs(proj4string(elevation.tmapunits)))

  # What MUS are over a river segment. Riversegment has to be over 0 units long. 
  MUS.over <- (elevation.tmapunits %over% trivers)$Shape_Leng
  MUS.over <- MUS.over>0
  MUS.over[is.na(MUS.over)] <- FALSE

  # Get list of MUS near rivers. 
  near.river.MUs <- elevation.tmapunits[MUS.over,]


  # SAVE THE NEW ELEVATION TAGGED FILE 
  save(near.river.MUs, file = paste("NCsoils_near_river_", water.shed, sep=""))

  # REMOVE OLD FILE
  file.remove(paste("elevation_NCDEM30mCRS_mapunits_",water.shed, sep=""))
}
stopCluster(cl)
stopImplicitCluster()
}
