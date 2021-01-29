# This program extracts classification from MLRA map for each soil traverse in nc
# it is addapted from the NCSS class extraction and some names have not been changed. 
# the MLRA class used is the MLRA_ID from the NRCS shapefile found at:
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/home/?cid=nrcs142p2_053624

library(raster)
setwd("B:/MLRA")

# Apparently R doesn't have a statistical average mode function????
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Load Classified Map 
DEM30 <- raster::raster("B:/USA30mDEM/s_mosaic.tif")
NCSS <- raster::raster("B:/MLRA/mlras_in_nc_100mp.tif")
NCSS_proj <- projectExtent(NCSS,crs(proj4string(DEM30)))
NCSS_Pro <- projectRaster(NCSS,NCSS_proj)
save(NCSS_Pro,file="NCSS_Pro")
     NCSS_Pro@data@values[is.na(NCSS_Pro@data@values)]<-99
     max(NCSS_Pro@data@values)
     
     
     for(x in 1:359){
       # An issue I was running into was not removing the varibles in the environment.
       # This should be run at the start of every loop to prevent issues.
       rm(list = setdiff(ls(), c("getmode","NCSS_Pro","x")))  
       
       # Which watershed?
       watershed <- x
       
       # Load the map units to query from 
       load(paste0("E:/LRU/MUs/elevation_NCDEM30mCRS_mapunits_",x))
       
       # Read the generated soil catenas list 
       catenas <- read.csv(paste0("B:/Classified/classified_soilcatenas_",x,".csv"), stringsAsFactors=FALSE)
       
       # We just want to know what the MUkeys are in the traverses 
       catena.mupolygonkey <- catenas[seq(1,nrow(catenas),3),]
       
       traverse.classes <- NULL 
       for(traverse in 1:nrow(catena.mupolygonkey)){
         # Extract a traverse
         a.traverse <- elevation.tmapunits[as.numeric(elevation.tmapunits@data$mupolygonkey) %in% catena.mupolygonkey[traverse,],]  
         
         test <- NULL
         why.r <- "test"
         # extract a raster of the classes of the traverse. 
         tryCatch({assign(why.r,crop(NCSS_Pro,a.traverse))},error= function(x){assign(why.r,99)})
         
         
         
         # Whats the most common class (aka mode), 99 = void space or water so remove that. 
         if(is.null(test)) traverse.classes[traverse] <- 99 else traverse.classes[traverse] <- getmode(round(test@data@values[!round(test@data@values)%in% c(99,96,77,73,NA)]))
         
       }
       
       # This repetes the classification 3 times to fit in our original CSV data
       traverse.classes.merge <- NULL
       for(each in 1:length(traverse.classes)) {
         traverse.classes.merge <- append(traverse.classes.merge,rep(traverse.classes[each],3))}
       
       # Make a dataframe to give a name to the col
       df.traverse.classes.merge <- data.frame(traverse.classes.merge)
       colnames(df.traverse.classes.merge)<-"Classification"
       
       # Bind original CSV with new classifications 
       classified.soil.catenas <- cbind(df.traverse.classes.merge,catenas)
       
       write.csv(classified.soil.catenas,file = paste('mlra_classified_soilcatenas_',watershed,".csv", sep=""), quote = FALSE)
       
     }

     
     # read in all of the CSVs from the MLRA extraction. 
     # we want to just append the "df_catena_comp_percent_w_elevations.csv"
     
     comp.list <- read.csv("B:/Classified/df_catena_comp_percent_w_elevations.csv")
     head(comp.list)
     
     
     df.mlra <- data.frame(matrix(ncol=2))
     
     
     for(watershed.number in 1:359){
       mlra <- read.csv(paste0("B:/mlracsv/mlra_classified_soilcatenas_",watershed.number,".csv"))
       mlra.traverseid <- data.frame(cbind(mlra$Classification,paste0("W",watershed.number,"T",mlra$traverse.groups)))
       mlra.traverseid <- mlra.traverseid[seq(1,nrow(mlra.traverseid),3),]
       df.mlra <- rbind(df.mlra, mlra.traverseid)
       
     }
     
     colnames(df.mlra) <- c("MLRA_ID", "Catena_ID")
     # if a traverse is slightly outside of NC, it will get weird edge readings from the raster. assign those those to NA.
     df.mlra$MLRA_ID[!as.numeric(df.mlra$MLRA_ID) %in% c(240,234,264,261,228,241,262)] <- NA
     
     ### now we want to tag the comp lists with MLRA id by looking up the traverse ID. 
     library(dplyr)
     
     comp.list.mlra <- join(comp.list,df.mlra, by="Catena_ID")
     
     comp.list.mlra$MLRA_ID <- as.factor(comp.list.mlra$MLRA_ID)
     
     # save for later
     save(comp.list.mlra, file = "comp_list_mlra")     
     