setwd("B:/Classified")

# https://www.youtube.com/watch?v=2otyDYe_V0o
library(arules)
library(arulesViz)
library(raster)
library(visNetwork)
library(igraph)
library(arulesCBA)

# set up a data frame for combining all of the traverses
holding <- data.frame(matrix(ncol = 25))
names(holding) <- c("Watershed","TransactionID","Classification","TransactionID2","TraverseGroup", paste("MU",1:20 ,sep=""))

# manipulate your ragged arrays into one big data frame
for(each in 1:359 ){
  
  soils2 <- read.csv(paste0("classified_soilcatenas_",each,".csv"),header = FALSE, skip = 1)
  
  watershed <- rep(each,nrow(soils2))
  soils3 <-cbind(watershed,soils2)
  filler <- matrix(ncol = 25-ncol(soils3),nrow=nrow(soils3))
  filled.soils <- cbind(soils3,filler)
  
  names(filled.soils)<-names(holding)
  
  holding <- rbind(holding,filled.soils)
}


holding


# write.csv(holding,file="Classified_NC_Traverses.csv")
holding <- read.csv("Classified_NC_Traverses.csv", stringsAsFactors = FALSE)

split.ID <- data.frame(paste0("W",holding$Watershed,"T",holding$TraverseGroup), stringsAsFactors = FALSE)
names(split.ID)<- "Split.ID"

# This will make sure that the split IDs are unique for the entire state. This should return 3 for the three rows of data. 
sum(split.ID=="W194T22")

tail(holding$Watershed)

# head(holding)
tail(holding)

holding <- cbind(split.ID,holding[,-1])

soils <- holding[seq(2,nrow(holding),3),]


write.csv(soils[,-c(2,4,5)],file="Classified_NC_Traverses_uniqueID_mupolykey.csv")

soils$Split.ID
unique(soils$Split.ID)



tail(soils)

soils <- soils[soils$Classification==12,]
soils <- soils[!is.na(soils$Split.ID),]
tail(soils)


soils <- soils[,c(1,4,7:26)]

soils[is.na(soils)]<-""



list.soils <- NULL
for(x in 1:nrow(soils)) {list.soils[x]<- list(soils[x,][!soils[x,]==""])} 


tsoils <- as(list.soils, "transactions")

save(tsoils, file="class_12_transactions")

str(soils)
# hist(holding[is.na(holding[,3]),1],nclass=359)





# Import data and make blanks NAs 
# soils <- read.csv("classified_soilcatenas_11.csv",header = FALSE, skip = 1)
soils[soils==""] <- NA

# Check structure to make sure they are factors 
# str(soils)
# soils$V1 <- as.factor(soils$V1)

# Rename for interprebility 
# names(soils) <- c("Watershed","TransactionID","Classification","TransactionID2","TraverseGroup", paste("MU",1:20 ,sep=""))

# Pull out the ID for use in the split function 
trans <- soils$Split.ID

# This makes a list and /could/ take a while.   
list.soils <- split(soils, trans)
tail(list.soils)

#Apparently NAs in the list makes the as() sad, remove them with lapply
list.soils.narm <- lapply(list.soils, function(x) x[!is.na(x)])

# Turn your list into a transaction dataset 
tsoils <- as(list.soils.narm, "transactions")

# look it worked 
inspect(tsoils)

# apply rule mining 
soils.rules <- apriori(tsoils, parameter = list(supp= .01, conf = 0.1, minlen=3))

inspect(soils.rules[1:12])

plot(soils.rules, method = "graph", engine='interactive')

plot(soils.rules, method="graph")



### Working on viz from https://bl.ocks.org/timelyportfolio/762aa11bb5def57dc27f

 ig <- plot(soils.rules,method="graph", control=list(type="items") )
ig_df <- get.data.frame( ig, what = "both" )

visNetwork(
  nodes = data.frame(
    id = ig_df$vertices$name
    ,value = ig_df$vertices$support # could change to lift or confidence
    ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label)
    ,ig_df$vertices
  )
  , edges = ig_df$edges
) %>%
  visOptions( highlightNearest = T )




###

arulesViz::inspectDT(soils.rules[1:100])

soils.rules@lhs@itemInfo




t
Grocer


###
data(Groceries)
str(Groceries)












# example 1: creating transactions form a list
a_list <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("c","e"),
  c("a","b","d","e")
)

## set transaction names
names(a_list) <- paste("Tr",c(1:5), sep = "")
a_list

## coerce into transactions
trans1 <- as(a_list, "transactions")

inspect(trans1)



yeet <- CBA(tsoils)
