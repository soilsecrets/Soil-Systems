# Precentages and lengths of soil components per transect
setwd("B:/Classified")

# libraries 
library(dplyr)
library(arules)
load.packages <- pacman::p_load(soilDB ,aqp, foreach ,doParallel ,doSNOW)

# load MUpolygon keys 
all.MUkeys <- read.csv("Classified_NC_Traverses_uniqueID.csv")
head(all.MUkeys)

# remove the classificationless soils 
all.MUkeys.na.rm <- all.MUkeys[!is.na(all.MUkeys$Classification),] 
all.MUkeys.na.rm <-all.MUkeys.na.rm[all.MUkeys.na.rm$Classification %in% c(1:16),-2]

# clean out NAs 
all.MUkeys.na.rm[is.na(all.MUkeys.na.rm)] <- "" 
soils <- all.MUkeys.na.rm

# create a vector of unique soil symbols
unique.syms <- soils[,3:20] %>% unlist() %>% unique
length(unique.syms)
 
# create a list to hold more lists.
SDA.list <- list() 
 
 # Make a foreach cluster. Change detectCores() to detectCores()-1 
 # if you want to play Kerbal Space Program while its running. 
 cl <- makePSOCKcluster(detectCores())
 registerDoParallel(cl,detectCores())

 SDA.list <-  foreach(itter=1:length(unique.syms),.combine='c', .packages="soilDB") %dopar% {
   # pull out one row of a soil system
   natmusys <- unique.syms[itter]
   
   # create the SQL string to query 
   a.string <- paste("nationalmusym=\'",natmusys,"\'",sep="")
   
   # Query SDA for comp names and comp precentacges 
   df.SDA.names <- tryCatch(sapply(a.string, function(x){get_component_from_SDA(WHERE = x)$compname}), error = function(e)"NULL")
   df.SDA.ammount <- tryCatch(sapply(a.string, function(x){get_component_from_SDA(WHERE = x)$comppct_r}), error = function(e)"0")
   
   # create a col named data frame for each comp name. 
   #for(comp in 1:length(df.SDA.names)){
     df.comp <- NULL 
     df.comp <- data.frame(matrix(ncol = length(df.SDA.names)+2))
     colnames(df.comp)[1] <- "itter"
     colnames(df.comp)[2] <- "natmusys"
     # colnames(df.comp)[3] <- "Comp_Number"
     colnames(df.comp)[3:length(colnames(df.comp))] <- df.SDA.names
     
     df.comp$itter <- itter 
     df.comp$natmusys <- natmusys
     #df.comp$Comp_Number <- comp
     df.comp[3:length(colnames(df.comp))] <- df.SDA.ammount
     
     
     
  # }
   
   # create a list of the comp name data frame. This is then c() togethered via the foreach loop. 
   # and put into the SDA.list var in the order they are done processing. 
   list(df.comp) 
   
 } 
 

# Once the above loop is finished, all the unique soil names can be extracted to be used as colomn names.
all.names.soils <- SDA.list %>% unlist %>% names %>% unique

# If the above loop gets an natmapsym that it can't find in the database, it'll name the soil "NULL" 
# "NULL" %in% all.names.soils
# sum(SDA.list %>% unlist %>% names == "NULL" )

df.catena.comp.pr <- data.frame(matrix(ncol = length(all.names.soils)+5))
colnames(df.catena.comp.pr)[1] <- "i"
colnames(df.catena.comp.pr)[2] <- "Catena_ID"
colnames(df.catena.comp.pr)[3] <- "Component"
colnames(df.catena.comp.pr)[5] <- "Natmapsys"
colnames(df.catena.comp.pr)[4] <- "Classification"
colnames(df.catena.comp.pr)[6:(length(all.names.soils)+5)] <- sort(all.names.soils)

head(SDA.list, 1)
# save(SDA.list, file="SDA_list")

load("SDA_list")

df.catena.comp.pr.join <- data.frame(matrix(ncol = length(all.names.soils)+1))
colnames(df.catena.comp.pr.join)[2:(length(all.names.soils)+1)] <- sort(all.names.soils)
colnames(df.catena.comp.pr.join)[1] <- "Natmapsys"


for(i in 1:length(SDA.list)){
  df.catena.comp.pr.join[i,] <- 0 
holding <- SDA.list[i]
soils.comps <- unlist(holding)[-c(1,2)]
df.catena.comp.pr.join$Natmapsys[i] <- unlist(holding)[2]
for(k in 1:length(soils.comps)){
  index.match <- match(names(soils.comps[k]),colnames(df.catena.comp.pr.join))
  df.catena.comp.pr.join[i,index.match] <- df.catena.comp.pr.join[i,index.match] +  as.numeric(soils.comps[k])

    }


  }



# look up each poloygons natmapsym from the object "soils" and place into a data frame
# Lets max  a big matrix to hold everything. We want a row for each compenent.
# so count each component.  
comp.count <- sum(!(soils[,-c(1,2)] %>% unlist ) == "")

# Make a really large dataframe with 2700390 rows. In hindsite, just needed the first 5 colomns.
# The Join_left function will take care of adding the additional colomns, and I end up dropping them 
# after this loop. 
df.catena.comp <- data.frame(matrix(nrow=comp.count, ncol = 5+ncol(df.catena.comp.pr.join)))
names(df.catena.comp) <- c("i", "Catena_ID", "Component", "Class", "NatMapsym", names(df.catena.comp.pr.join))


# I'm sure there is a faster, cleaner way to pull out rows and components of data. 
# but here is a solution that will run over night and get it done. A solid graveyard shift program.  
i<-0
for (soil in 1:nrow(soils)) {
  holding <- soils[soil,]
  holding <- holding[!holding==""]
  
  for(comp in 1:(length(holding)-2)){
   i <- 1+i 
   df.catena.comp$i[i] <- i
   df.catena.comp$Catena_ID[i] <- holding[1] 
   df.catena.comp$Component[i] <- comp
   df.catena.comp$Class[i] <- holding[2]
   df.catena.comp$NatMapsym[i] <- holding[(2+comp)]
    
  }
  
}

# save(df.catena.comp, file="df_catena_comp")

# Just do a left join, Not every problem needs to be solved with a for loop.
# Amazing what coding can be done while awake. 
d <- df.catena.comp[,1:5]
names(d)[5] <- "Natmapsys" 
df.catena.comp.matched <- left_join(d,df.catena.comp.pr.join)
# test to see if it messed up, there should be no NAs 
sum(is.na(df.catena.comp.matched$Norfolk))


# I ended up stopping the loop that pulls apart the individual comps twice. It took over 24 hours to run.
# While doing so, it looks like it overwrote two components. However, it doesn't look like "Class" 
# or the other identifiers were over writen. So ill just drop those 2 out of 2.7 million entries. 
# The came out as NAs at the very end of the dataframe. I'll drop them and then save the output. 

df.catena.comp.precent <- df.catena.comp.matched[-c((nrow(df.catena.comp.matched)-1):nrow(df.catena.comp.matched)),]
# write.csv(df.catena.comp.precent, file="df_catena_comp_precent.csv")

# ugh I guess we need to throw elevation into the mix. 
# this is an issue due to the previous mention of losing two rows of data. Otherwise, we could just pull each component
# data, turn it into a single 2.7million entry vector, and then slap that into the dataframe. 
# Instead we are going to have to create like a loop or something...


# Read in elevations 
all.Elevation <- read.csv("Classified_NC_Traverses_UniqueID_elevation.csv")
all.Elevation <- all.Elevation[!is.na(all.Elevation$V2),]
elev.match <- match(df.catena.comp.precent$Catena_ID, all.Elevation$V1)
elevation.col <- df.catena.comp.precent$Component+3

t <- head(elev.match)
t2 <- head(elevation.col)

head(all.Elevation)

# This runs fast enough, only a second or two. Does include NAs. 
# NAs end the traverse. 
for( i in 1:length(elev.match)){
elevations[i] <- all.Elevation[elev.match[i],elevation.col[i]]
}

# change the name so its correct, and also if this messes up we can start somewhere
df.catena.comp.percent <- df.catena.comp.precent
# replace the "index" number with the elevations
# I'm sure it will cause no problems :D 
df.catena.comp.percent[,1] <- elevations
names(df.catena.comp.percent)[1] <- "Elevation"

# write.csv(df.catena.comp.percent, file = "df_catena_comp_percent_w_elevations.csv")


