library(aqp)
library(soilDB)
load.packages <- pacman::p_load(rbenchmark ,rgdal ,spdep ,ggplot2 ,leaflet ,sf ,RANN ,dplyr ,graphics ,ggmap
                                ,raster ,soilDB ,aqp ,gdistance ,sp ,mapview ,elevatr ,geoknife ,foreach ,doParallel ,doSNOW ,rgeos)
load.packages
setwd("B:/Classified")

holding <- read.csv("Classified_NC_Traverses.csv", stringsAsFactors = FALSE)

split.ID <- data.frame(paste0("W",holding$Watershed,"T",holding$TraverseGroup), stringsAsFactors = FALSE)
names(split.ID)<- "Split.ID"


tail(holding$Watershed)

# head(holding)
tail(holding)

holding <- cbind(split.ID,holding)

for(soil.class in c(14:16)){
  soils <- holding[seq(4,nrow(holding),3),]
  tail(soils)
  
  soils <- soils[soils$Classification==soil.class,]
  soils <- soils[!is.na(soils$Split.ID),]
  tail(soils)
  
  
  soils <- soils[,c(1,4,7:26)]
  
  soils[is.na(soils)]<-""
  
  
  # This retreives the nationalmusym
  matrix.of.holding <- matrix(nrow=nrow(soils),ncol=19)
  Soil.split.ID <- soils[,1]
  soil.name.matrix <- cbind(Soil.split.ID,matrix.of.holding)
  
  
  
  
  # Factor the nationalmusym
  unique.soilkeys <- levels(factor(unlist(soils[4:22])))
  unique.soilkeys <- levels(factor(unlist(soils[4:22])))
  
  
  
  # MAKE CLUSTER FOR FOREACH
  cl <- makePSOCKcluster(8)
  registerDoParallel(cl,8)
  
  soil.factor.names <- NULL
  soil.factor.names <- foreach(itter=1:length(unique.soilkeys), .packages="soilDB") %dopar% {
    
    # pull out one row of a soil system
    natmusys <- paste(unique.soilkeys[itter])
    # create the SQL string to query 
    a.string <- paste("nationalmusym=\'",natmusys,"\'",sep="")
    # Query and create a df for the soil system
    
    df.SDA.names <- NULL
    attempt1 <- 0
    
    while(is.null(df.SDA.names) && attempt1 <= 10){
      attempt1 <- attempt1 + 1
      Sys.sleep(3)
      tryCatch(df.SDA.names <- sapply(a.string, function(x){get_component_from_SDA(WHERE = x)$compname}),error=function(e){})
    }
    
    df.SDA.ammount <-NULL
    attempt2 <- 0
    while( is.null(df.SDA.ammount) && attempt2 <= 10 ) {
      attempt2 <- attempt2 + 1
      Sys.sleep(3)
      tryCatch(df.SDA.ammount<- sapply(a.string, function(x){get_component_from_SDA(WHERE = x)$comppct_r}), error=function(e){})
    }
    
    # Find the max precentage of a soil comp
    tryCatch(df.SDA.ammount.max <- df.SDA.ammount==max(df.SDA.ammount), error=function(e){})
    # create a vector of names in order
    tryCatch(soil.factor.names.finished <- paste(unlist(df.SDA.names)[unlist(df.SDA.ammount.max)]),error=function(e){})
    
    list(c(unique.soilkeys[itter],soil.factor.names.finished))
    
  }
  
  
  # END THE CLUSTER (YOU ONLY NEED ONE, BUT WHY NOT BOTH)
  stopCluster(cl)
  stopImplicitCluster()
  
  
  # THIS creates a dataframe to hold the unique keys and soil names
  
  df.soil.factor.names <- data.frame(matrix(ncol = 2,nrow = (length(soil.factor.names))))
  names(df.soil.factor.names) <- c("NatMUSysKey","Component_Name")
  
  for(i in 1:length(soil.factor.names)){
    df.soil.factor.names[i,] <- unlist(soil.factor.names[[i]])
  }
  
  
  
  # length(levels(factor(df.soil.factor.names$Component_Name)))
  
  for(i in 1:nrow(soils)){
    soil.name.matrix[i,2:20] <- sapply(soils[i,4:22],  function(x){df.soil.factor.names[df.soil.factor.names[,1] %in% x ,2]})
  }
  
  
  write.csv(soil.name.matrix, file = paste0("soil.name.matrix.", soil.class, ".csv"))
  
}

############################### wir
df.SDA.ammount==max(df.SDA.ammount)




for(itter in 621:634){
  natmusys <- paste(unique.soilkeys[itter])
  a.string <- paste("nationalmusym=\'",natmusys,"\'",sep="")
  tryCatch(get_component_from_SDA(WHERE = a.string), error=function(e){})
}






c(1,2,3,4)==max(c(1,2,3,4))
