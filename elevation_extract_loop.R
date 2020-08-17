# This one we want to calculate the zonal mean for elevation for each MU 

# Set WD for file that contains all tmapunit files that you want to append and remove with elevation. 
setwd("D:/LRU/MUs")

# List all the tmapunit file names.  
MU.files  <- list.files()
# CALCULATE THE MEAN ELEVATION FOR EACH MAP UNIT, APPEND OLD FILE, SAVE
# APPENDED FILE, AND REMOVE OLD FILE. 





for(file.name in MU.files){
rm(list = setdiff(ls(), c("numCores", 'start', 'endding', "DEM30m",
                          'tAll.wsbs', 'tstreams', "big.boi", 'watershed',
                          'MU.files', 'file.name')))
# LOAD MAPUNITS  
load(paste(file.name))

# CLEAR VARIBLES 
elevation.soils <- NULL
elevation.nationalmusym <- NULL
elevation.mukey <- NULL

# MAKE CLUSTER FOR FOREACH
cl <- makePSOCKcluster(8)
registerDoParallel(cl,8)

# FOREACH LOOP, EXTRACT MEAN ELEVATION OF EACH SOIL MAP UNIT IN THE WATERSHED. 
# 47 sec 
elevation.soils <- foreach(soilunit=1:length(tmapunits), .packages="raster", .combine='c') %dopar% {
  mean(mask(crop(DEM30m, tmapunits[soilunit,]), tmapunits[soilunit,])@data@values, na.rm=TRUE)}

# END THE CLUSTER (YOU ONLY NEED ONE, BUT WHY NOT BOTH)
stopCluster(cl)
stopImplicitCluster()

# NA HANDLE ELEVATION FOR SUB PIXEL POLYGONS 
elevation.soils.nafinder <- cbind(1:length(elevation.soils), elevation.soils)
resample.soil.na <- elevation.soils.nafinder[is.na(elevation.soils),1]
na.pixels <- sapply(resample.soil.na,function(x){mean(crop(DEM30m, tmapunits[x,])@data@values)})
elevation.soils[resample.soil.na] <- na.pixels


#




# this relys on that the dataframe is not sorted. Bad, but works. 
#Its just a calculated field. so its OK. 
# IT WORKS OK 
# edit polygon to have mean elevation 
elevation.tmapunits <- tmapunits
elevation.tmapunits$Mean_Elevation <- elevation.soils


# SAVE THE NEW ELEVATION TAGGED FILE 
save(elevation.tmapunits, file = paste("elevation_", file.name, sep=""))



# REMOVE OLD FILE
file.remove(paste(file.name))
}

