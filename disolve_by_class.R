# disolve by class??

library(raster)
library(rgeos)
library(dplyr)
library(maptools)

setwd("B:/modelNC/MLRA234/flat/KNN")

# load in flattened MLRA watershed set
allwatersheds <- shapefile("allwatersheds_flat")

# bring in boundries and merge them together, sorry you have to rename them :(
nc.bound <- shapefile("B:/modelNC/NC_MLRAS")
nc.bound234 <- nc.bound[nc.bound$MLRA_ID==234,]
tnc234 <-spTransform(nc.bound234, crs(allwatersheds))

# you apparently have to "clean" the polygons as there are some self intersecting rings. I might just try a 0 width buffer,
# but that wasn't working either. This process took 15 hours for MLRA 228. 
cleaned_allwatersheds <- cleangeo::clgeo_Clean(allwatersheds)
save(cleaned_allwatersheds,file="cleaned_allwatersheds234")
cliped_allwatersheds <- raster::intersect(cleaned_allwatersheds,tnc234)
save(cliped_allwatersheds,file="cliped_allwatersheds")

# seperate by the classes, you again need to edit this by hand. 
class1 <- cliped_allwatersheds[cliped_allwatersheds$sscl==2341,]
class2 <- cliped_allwatersheds[cliped_allwatersheds$sscl==2342,]
class3 <- cliped_allwatersheds[cliped_allwatersheds$sscl==2343,]
class4 <- cliped_allwatersheds[cliped_allwatersheds$sscl==2344,]
class5 <- cliped_allwatersheds[cliped_allwatersheds$sscl==2345,]


# disolved, doesn't take too long
class1_disolved <- aggregate(class1, by="sscl")
class2_disolved <- aggregate(class2, by="sscl")
class3_disolved <- aggregate(class3, by="sscl")
class4_disolved <- aggregate(class4, by="sscl")
class5_disolved <- aggregate(class5, by="sscl")


# merge, works TOO quickly, like a second 
disolved_classes <- bind(class1_disolved,class2_disolved)
disolved_classes <- bind(class3_disolved, disolved_classes)
disolved_classes <- bind(class4_disolved, disolved_classes)
disolved_classes <- bind(class5_disolved, disolved_classes)

# output the files, does take a while to write a few GB of files. 
shapefile(disolved_classes, file="MLRA234_Disolved_SSmap_clipped_2")
shapefile(tnc234, "NC_MLRA234_Boundry")


############################## ALL SCRAPED BELOW 
#this takes 17 minutes or so, but is required. 
cleaned_disolved_classes <- cleangeo::clgeo_Clean(disolved_classes)



shapefile(cliped_disolved_classes, "MLRA228_Disolved_SSmap_clipped")




holes <- gDifference(tnc228,cliped_disolved_classes)
clean_holes <- cleangeo::clgeo_Clean(holes)
clean_holes


filed_holes <- gIntersects(allwatersheds,clean_holes)

