# read in all map units for SW and save them 

setwd("D:/LRU/SavedMUs")
load.packages <- pacman::p_load(rbenchmark ,rgdal ,spdep ,ggplot2 ,leaflet ,sf ,RANN ,dplyr ,graphics ,ggmap
                                ,raster ,soilDB ,aqp ,gdistance ,sp ,mapview ,elevatr ,geoknife ,foreach ,doParallel ,doSNOW ,rgeos)
load.packages

# Read in WSB data layer 



All.wsbs <-readOGR(dsn="D:/LRU/WatershedboundriesSW", layer="SWWSB")
tAll.wsbs <- spTransform(All.wsbs, CRS(proj4string(DEM30m)))

cl <- makePSOCKcluster(8)
registerDoParallel(cl,8)
current_watershed <- NULL

for(current_watershed in 1:30){
  
  # Pick a watershed 
  twsb <- All.wsbs[current_watershed,]
  
  # Create a bounding box 
  bounding.box <- c(left = min(twsb@bbox[1,]), bottom = min(twsb@bbox[2,]),
                    right = max(twsb@bbox[1,]), top = max(twsb@bbox[2,]))
  
  # get map units
  mapunits <- NULL
  attempt2 <- 1
  while( is.null(mapunits) && attempt2 <= 100 ) {
    attempt2 <- attempt2 + 1
    Sys.sleep(3)
    try(mapunits <- mapunit_geom_by_ll_bbox(bounding.box))
  }
  
  proj4string(mapunits) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  tmapunits <- spTransform(crop(mapunits,extent(twsb)), CRS(proj4string(DEM30m)))
  
  # Save output locally
  save(tmapunits, file = paste('DEM30mCRS_mapunits_',current_watershed, sep = ""))
  
} 


ls

stopCluster(cl)

rm(mapunits)


plot(tAll.wsbs) 

add(tAll.wsbs[1,], col='BLUE')



load('DEM30mCRS_mapunits_30');plot(mapunits)

mapunits

