# This file loads all of the rivers from nhdsets and merges them together. ArcGIS crashes when you try to do this. 

# Load the file names to work with. I use the .dbf becuase its unique rather than .shp which isn't sometimes (lock files) 
file.names <- list.files("D:/LRU/StreamsNC/streamlayers", pattern = ".dbf")

# remove the pattern.
file.names.cleaned <- stringr::str_remove(file.names, ".dbf")

# Read in all of the files, this will take a while  


sapply(file.names.cleaned,function(i){assign(paste(i), readOGR(dsn="D:/LRU/StreamsNC/streamlayers", layer=i))})


# Apparently you have to do this by hand becuase {} lives in its OWN UNIVERSE. 
x <- 0 
# RE RUN UNTIL DONE
x <- x+1
i<- file.names.cleaned[x]
assign(paste0("file",i), readOGR(dsn="D:/LRU/StreamsNC/streamlayers", layer=i))


NCStreams <- rbind(file204,file208,file301t,file302,file303,
      file304,file305,file306,file307,file313,
      file315,file505,file507,file510,file513,
      file601,file602)

# save in R file ASAP 
save(NCStreams, file = "NCStreams")

# Write as shape 
P <- shapefile(NCStreams, "NCstreams")



plot(NCStreams)

