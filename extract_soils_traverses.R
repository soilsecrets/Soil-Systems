# This program takes a list of soils near rivers and walks them up a hill until it get to a local maximum. 
setwd("D:/LRU/traverse_starts_NC")
packages.to.use <- pacman::p_load(rbenchmark ,rgdal ,spdep ,ggplot2 ,leaflet ,sf ,RANN ,dplyr ,graphics ,ggmap
                                  ,raster ,soilDB ,aqp ,gdistance ,sp ,mapview ,elevatr ,geoknife ,foreach ,doParallel
                                  ,doSNOW ,rgeos, stringr, spdep)
packages.to.use

# Which file are you working on? increment this in a loop.
number.of.file <-1
# Make the files name
working.file <- paste0("NCsoils_near_river_",number.of.file)
working.file.soils <-paste0("elevation_NCDEM30mCRS_mapunits_",number.of.file)
# Load the file
load(paste0("D:/LRU/traverse_starts_NC/",working.file))
load(paste0("D:/LRU/MUs/",working.file.soils))

# INITILIZE THE BIG BOI FUNCTION 
big.boi<- function(x){
  
  # Loop for each near polygon 
  # get adjacent polygons, to do this we will buffer the select polygon and
  # then intersect the soil map unit layer
  # Buffer polygon, this can be looped eventually 
  current.polygon <- x
  
  # Loop for until polygonmukey = current mu key
  counter <- 1
  
  soil.system <- current.polygon@data$mupolygonkey
  
  system.time(
    while ( counter< 1000){
      counter <- counter+1
      
      # transform current polygon 
      # utmpolygons <- spTransform(current.polygon, CRS(proj4string(elevation.tmapunits)))
      
      # Buffer polygon
      buffered.polygons <- gBuffer(current.polygon, byid=TRUE, width=10)
      # transform the buffers to be used for cropping 
      # tbuffered.polygons <- spTransform(buffered.polygons, CRS(proj4string(elevation.tmapunits)))
      # Crop all map units to just include polygons in the buffer. This includes original polygon. 
      polygon.mapunits<- crop(elevation.tmapunits,buffered.polygons)
      # subset all mapunits to just include polygons bordering the original polygon
      polygon.fullmapunits <- elevation.tmapunits[elevation.tmapunits@data$mupolygonkey
                                        %in% polygon.mapunits@data$mupolygonkey,]
      
      # This pulls the elevation data from the buffered polygons from the elevation list.
      # the below loop used to check the online elevation database and took forever.
      # If you don't have a local DEM then uncomment the loop, but it'll take hours. 
      # elevations.buffered.polygons <- soil.elevation.list[soil.elevation.list[,3] %in% 
      #                                                     polygon.mapunits@data$mupolygonkey,]
      
      # get an average heigth of each polygon neighbor as well as the seed polygon using elevatr.
      # This somehow works using point data, and after messing around withit for a day I am
      # pretty sure its using the same projection systems now, however this could be incorrect 
      # and needs checked. 
      # This loop will attempt to get the elevation of each polygon 100 times before it errors out. 
      # heigths.of.neighbors <- NULL
      # i <- NULL
      # attempt <- 1
      # for(i in 1:length(polygon.fullmapunits)){
      #   
      #  elevations <- NULL
      #  attempt <- 1
      #  
      #  while( is.null(elevations) && attempt <= 1000 ) {
      #    attempt <- attempt + 1
      #    try(
      #      elevations <- get_elev_point(polygon.fullmapunits[i,],
      #                                   "+proj=longlat +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0")
      #    )
      #  }
      #  heigths.of.neighbors[i] <-elevations@data$elevation
      
      # }
      
      # Old script for above loop
      # highestpolygon <- max(polygon.fullmapunits@data$mupolygonkey[heigths.of.neighbors==max(heigths.of.neighbors)])
      
      # Which neighbor is the highest?
      # highestpolygon <- elevations.buffered.polygons[unlist(elevations.buffered.polygons[,1])==max(unlist(elevations.buffered.polygons[,1])),3]
      
      # Remove NAs
      polygon.fullmapunits@data$Mean_Elevation[is.na(polygon.fullmapunits@data$Mean_Elevation)]<-0
      
      # Get max elevation
      Max.Elevation <- max(polygon.fullmapunits@data$Mean_Elevation[!is.na(polygon.fullmapunits@data$Mean_Elevation)])
      
      #Which polygon is the highest? 
      highestpolygon <- polygon.fullmapunits[polygon.fullmapunits@data$Mean_Elevation==Max.Elevation,][1,]
      
      # is it the same polygon? 
      if(current.polygon@data$mupolygonkey==highestpolygon@data$mupolygonkey) break else  
        # if its not the same, make it the next polygon
        current.polygon <- elevation.tmapunits[elevation.tmapunits@data$mupolygonkey==highestpolygon@data$mupolygonkey,]
      # save to df 
      soil.system[counter]<-unlist(highestpolygon@data$mupolygonkey) 
    }
  )
  
  list(soil.system)
  
}



starting <-2 
ending <- 359
# loop
watershed <- NULL
system.time(for(watershed in start:endding){
  
  # An issue I was running into was not removing the varibles in the environment.
  # This should be run at the start of every loop to prevent issues.
  rm(list = setdiff(ls(), c("numCores", 'start', 'endding', "big.boi", 'watershed')))
  
  tryCatch({
    # Select a current polygon, if I use twsb, then I don't need to rewrite the code. 
    
    number.of.file <- watershed
    # Make the files name
    working.file <- paste0("NCsoils_near_river_",number.of.file)
    working.file.soils <-paste0("elevation_NCDEM30mCRS_mapunits_",number.of.file)
    # Load the file
    load(paste0("D:/LRU/traverse_starts_NC/",working.file))
    load(paste0("D:/LRU/MUs/",working.file.soils))
    
    
    
    # START OF PAR FUNCTION LOOP FUNCTION, MUST INCLUDE LIBRARY OF LOOP 
    # length(near.river.fullmapunits)
    cl <- makePSOCKcluster(8)
    registerDoParallel(cl,8)
    
    
    system.time(
    soilcatenas <-  foreach (i= 1:length(near.river.MUs)) %dopar% {
      library(rgdal)
      library(rgeos)
      library(raster)
      big.boi(near.river.MUs[i,])
    }
    )
    
    stopCluster(cl)
    
    
    # Save to different vector incase you rerun function or something.
    soilcatenas.saved <- soilcatenas
    
    # soil.elevation.list[soil.elevation.list[,3] %in% unlist(soilcatenas.saved[2]),]
    
    
    # ragged array from saved soil catenas rather than lists
    lengthsofsoils <- NULL
    for (i in 1:length(soilcatenas.saved)){
      lengthsofsoils[i]<- length(unlist(soilcatenas.saved[i]))
    }
    
    matrix.for.dataframe  <- matrix(0, ncol = max(lengthsofsoils))
    ragged.arrey <- data.frame(matrix.for.dataframe)
    
    for (i in 1:length(soilcatenas.saved)){
      
      # these are written very well to store the data, it creates a DF that saves MUkey, Elevation, Nat map sym.
      ragged.arrey[(i*3)-2,]<- c(unlist(soilcatenas.saved[i]),
                                 rep(NA,max(lengthsofsoils)-length(unlist(soilcatenas.saved[i]))))
      
      ragged.arrey[(i*3)-1,] <- c(sapply(unlist(soilcatenas.saved[i]), function(x){elevation.tmapunits@data$Mean_Elevation[elevation.tmapunits@data$mupolygonkey==x]}),
                                  rep(NA,max(lengthsofsoils)-length(unlist(soilcatenas.saved[i]))))
      
      ragged.arrey[(i*3)-0,] <- c(sapply(unlist(soilcatenas.saved[i]), function(x){elevation.tmapunits@data$nationalmusym[elevation.tmapunits@data$mupolygonkey==x]}),
                                  rep(NA,max(lengthsofsoils)-length(unlist(soilcatenas.saved[i]))))
      
      
    }
    
    
    
    
    # Remove NAs from the array   
    ragged.arrey[is.na(ragged.arrey)]<-""
    
    # create groups for each soil traverse. 
    traverse.groups  <- rep(1:length(soilcatenas.saved), each=3)
    
    traverse.data <- (cbind(traverse.groups,ragged.arrey))
    
    
    # Write output to CSV that is your assosiated with your current watershed. 
    write.csv(traverse.data,file = paste('soilcatenas_',watershed,".csv", sep=""), quote = FALSE)
    
    
    # PLOTING ALL CATENAS AS UNIQUE COLORS 
    
    
    # COLORS FOR THE PLOYGONS WEEEEEE
    # colors.for.plot <- rainbow(nrow(ragged.arrey))
    # Initilize a list to feed into a GG map. Lists of polygons to plot can just be fed into a ggmap.
    # we are initilizing the plot here with the WSB and trivers for context and to make the ploty plot line short. 
    # catena.plots <- list(geom_path(data = trivers, aes(x = long, y = lat, group = group), color = "blue"),
    #                     geom_path(data = twsb, aes(x = long, y = lat, group = group), color = "red", size = 1.5))
    #  catena.plots <- NULL
    # Another loop! Make a list of all of the soil canenas and then plot them by color.  
    # for(ragged.row in 1:nrow(ragged.arrey)){
      
    #  catena.plots <- c(catena.plots,
    #                    list(geom_polygon(data=elevation.tmapunits[elevation.tmapunits@data$mupolygonkey
    #                                                     %in% 
    #                                                       levels(factor(ragged.arrey[ragged.row,])),],
    #                                      aes(x = long, y = lat, group = group),
    #                                      color = colors.for.plot[ragged.row], size = .05)))
    #  
    # }
    
    
    
    
    # Plot all soil catenas and save to output. 
    
    # soilmap <- ggmap(get_map(location = extent(elevation.tmapunits), maptype = "topographic", source = "osm", color = "bw" )) + coord_cartesian() + catena.plots
    # save(soilmap, file = paste('soilmap_',current_watershed, sep = ""))
    # This saves the checkpoint to start if crashes in the middle :)   
    #save(watershed, file = "state.RData")
    
    
    # If error, skip and next watershed in loop
  }, error=function(e){cat("Aww heck. this ain't good, it looks like",conditionMessage(e), "\n")})
  
})













#############################

# buffer current polygon 
buffered.polygons <- gBuffer(near.river.MUs[current.polygon,], byid=TRUE, width=10)

# transform the buffers to be used for cropping 
tbuffered.polygons <- spTransform(buffered.polygons, CRS(proj4string(elevation.tmapunits)))

# Crop all map units to just include polygons in the buffer. This includes original polygon. 
polygon.mapunits<- crop(elevation.tmapunits,tbuffered.polygons)

# subset all mapunits to just include polygons bordering the original polygon
polygon.fullmapunits <- elevation.tmapunits[elevation.tmapunits@data$mupolygonkey
                                  %in% polygon.mapunits@data$mupolygonkey,]

# save to soil system
soil.system[counter]<-unlist(highestpolygon)





plot(buffered.polygons)


plot(x)
