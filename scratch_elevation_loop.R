library(raster)

lat<- 32.29062
long <- -106.9191


test <- data.frame(long,lat)
bob <- SpatialPoints(test, proj4string=CRS("+proj=longlat +epsg=4326")) 
elee <- get_elev_point(bob)
what <- elee$elevation


bbox_start<- bbox(bob)
buff_start <- bbox_start-c(0.00001,0.00001,-0.00001,-0.00001)

current.polygon<- mapunit_geom_by_ll_bbox(buff_start)

proj4string(current.polygon)<- CRS("+proj=longlat +epsg=4326")

tcurrent.polygon<- spTransform(current.polygon, proj4string(DEM30m))



mean(mask(crop(DEM30m, tcurrent.polygon), tcurrent.polygon)@data@values, na.rm=TRUE)

<- buffer(current.polygon, width=1)

bbox_poly <- bbox(current.polygon)

buff_poly <- bbox_poly-c(0.00001,0.00001,-0.00001,-0.00001)
buffed.polygons <- mapunit_geom_by_ll_bbox(bbox_poly)

over.polygon <- buffed.polygons %over% buffer(current.polygon, width = 0.001)
over.polygon <- over.polygon>0
over.polygon[is.na(over.polygon)]<- FALSE
buffed.polygons <- buffed.polygons[over.polygon,]


length(buffed.polygons)


buffed.polygons$elevation <-0
for(polygon in 1:length(buffed.polygons)){buffed.polygons$elevation[polygon] <- get_elev_raster(
  locations = buffed.polygons[polygon,],
  prj = sp::proj4string(buffed.polygons),
  z=14,verbose = FALSE, clip = "locations")@data@values %>% 
  mean(na.rm=TRUE)}

buffed.polygons[buffed.polygons$elevation==max(buffed.polygons$elevation),]






is.na(x@data@values)

current.polygon@bbox
current.polygon

current.polygon$mupolygonkey


bbox.some.polygons<- bbox(current.polygon)+c(-1,1,-1,1) 
some.polygons <- mapunit_geom_by_ll_bbox(bbox=bbox.some.polygons)
proj4string(some.polygons)<-CRS("+proj=longlat +epsg=4326")



elevation.some.polygons <- get_elev_point(some.polygons)$elevation

some.polygons
mukeylist <- NULL
elevationlist <- NULL
for(polygon in 1:length(some.polygons)){
  mukeylist[polygon] <-some.polygons[polygon,]$mupolygonkey
  elevationlist[polygon] <- get_elev_point(some.polygons[polygon,])$elevation  
}

<- cbind(elevationlist,mukeylist,c(1:length(some.polygons)))





some.polygons[1,]
some.polygons

Winsor<- function(x){
  
  # Loop for each near polygon 
  # This gets adjacent all polygons, to do this we buffer the select polygon and
  # then intersect the soil map unitlayer buffered polygon. 
  current.polygon <- x
  
  # Loop for until polygonmukey = current mu key
  counter <- 1
  soil.system <- current.polygon@data$mupolygonkey
  
  system.time(
    while ( counter< 1000){
      counter <- counter+1
      
      # Buffer polygon
      buffered.polygons <- gBuffer(current.polygon, byid=TRUE, width=10)
      
      # Crop all map units to just include polygons in the buffer. This includes original polygon. 
      polygon.mapunits<- crop(elevation.tmapunits,buffered.polygons)
      
      # subset all mapunits to just include polygons bordering the original polygon
      polygon.fullmapunits <- elevation.tmapunits[elevation.tmapunits@data$mupolygonkey
                                                  %in% polygon.mapunits@data$mupolygonkey,]
      
      # Remove NAs
      polygon.fullmapunits@data$Mean_Elevation[is.na(polygon.fullmapunits@data$Mean_Elevation)]<-0
      
      # Get max elevation
      Max.Elevation <- max(unlist(polygon.fullmapunits@data$Mean_Elevation[!is.na(polygon.fullmapunits@data$Mean_Elevation)]))
      
      #Which polygon is the highest? 
      highestpolygon <- polygon.fullmapunits[polygon.fullmapunits@data$Mean_Elevation==Max.Elevation,][1,]
      
      # Is it the same polygon? 
      if(current.polygon@data$mupolygonkey==highestpolygon@data$mupolygonkey) break else  
        
        # if its not the same, make it the next polygon
        current.polygon <- elevation.tmapunits[elevation.tmapunits@data$mupolygonkey==highestpolygon@data$mupolygonkey,]
      # save to df 
      soil.system[counter]<-unlist(highestpolygon@data$mupolygonkey) 
    }
  )
  
  list(soil.system)
  
}






soil.mukeys <- Winsor(current.polygon)





We.already.ran.these <- SpatialPolygonsDataFrame(SpatialPolygons(list(),proj4string=proj4string(buffed.polygons)), data=data.frame())


test <- SDA_spatialQuery(buffer(current.polygon, width=0.001), what="geom")
proj4string(test)<- CRS("+proj=longlat +epsg=4326")
DEM30m <- raster::raster("B:/USA30mDEM/s_mosaic.tif")
test <- spTransform(test,proj4string(DEM30m))



cl <- makePSOCKcluster(8)
registerDoParallel(cl,8)

system.time(test$elevation <-unlist(foreach(one = 1:length(test), .packages = c("rgeos", "raster")) %dopar% {
  mean(mask(crop(DEM30m,test[one,]),test)@data@values,na.rm=TRUE)
}))


yeet <- crop(DEM30m,test)
yeet@data@values


test$mukey


holding$comppct_r





SDA_query("mukey=57006")

current.polygon<- fetchSDA_spatial(57006)[1,]

worthless <- SDA_spatialQuery(bob, "geom")
rbind(current.polygon,worthless)

