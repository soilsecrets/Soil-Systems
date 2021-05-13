# flatten combined watersheds into single MLRA SS map 

library(rgeos)
library(raster)


setwd("B:/modelNC/MLRA228/flat/KNN")

allwatersheds <- shapefile("allwatersheds228.shp")

mus <- unique(allwatersheds$mplygnk)

# set up a dataframe to join back to all watersheds
classes <- unique(allwatersheds$sscl)
df.mus <- data.frame(matrix(nrow = length(mus), ncol = length(classes)+3))
names(df.mus) <- c("mplygnk", "soilsys","prcnt" ,paste(classes))
df.mus$mplygnk <- mus 
df.mus[,-1] <- 0


# for loop will produce a large dataframe that contains mupolygonkeys, max soil system, sum observation percent,
# and the class sum observation percentages

for(amu in 1:length(mus)){
subset.ws <- allwatersheds[allwatersheds$mplygnk==mus[amu],]
subset.ag <- aggregate(maxpct~sscl, subset.ws,FUN=sum)


for(class in 1:length(classes)){
  df.mus[amu,3+class] <-  if(sum(subset.ag[,1]==classes[class])==0) as.numeric(0) else subset.ag[subset.ag[,1]==classes[class],2]
  }

# max and prc
df.mus[amu,2] <- as.numeric(names(df.mus)[-c(1:3)][df.mus[amu,-c(1:3)]==max(df.mus[amu,-c(1:3)])])[1]
df.mus[amu,3] <- max(df.mus[amu,-c(1:3)])  
}
 
# use the output to join to tabular data from allwatersheds
all.mupolykey <- data.frame(mplygnk=allwatersheds$mplygnk)
joined.all.mupolykey <- right_join(all.mupolykey,df.mus, "mplygnk")

# amend the allwatersheds file to contain the new joined data. To do this lets drop all of the old unflattened data and replace it. 

allwatersheds$sscl <- joined.all.mupolykey$soilsys
allwatersheds$maxpct <- joined.all.mupolykey$prcnt
allwatersheds$X2281 <- joined.all.mupolykey$`2281`
allwatersheds$X2283 <- joined.all.mupolykey$`2283`
allwatersheds$X2282 <- joined.all.mupolykey$`2282`


shapefile(allwatersheds, file="allwatersheds_flat")

allwatersheds <- shapefile("allwatersheds_flat")
t <- gBuffer(allwatersheds, width = 0)
r2 <- raster::aggregate(t, "sccl")
shapefile(allwatersheds, file="allwatersheds_disolved")
