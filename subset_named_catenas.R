# subset named catenas 
library(dplyr)
library(arules)
# library(arulesSequences)
library(arulesCBA)
library(arulesViz)
library(visNetwork)
library(igraph)
# load all soil catenas 

setwd("B:/Named")

# load one catena to initilize a dataframe
# df.named.catenas <- read.csv("soil_name_matrix_1.csv")
# The first row is a count which is not needed, Classification Qis needed however
# df.named.catenas[,1]<-1
#names(df.named.catenas)[1] <- "Class"

# load and bind and classify them. 
#for(i in 2:16){
#  df.named.catenas.to.bind <- read.csv(paste0("soil_name_matrix_",i,".csv"))
#  df.named.catenas.to.bind[,1]<-i
#  names(df.named.catenas.to.bind)[1] <- "Class"
#  df.named.catenas <- rbind(df.named.catenas,df.named.catenas.to.bind)
# }

# save(df.named.catenas,file="named.catenas")
# load the above made file
load(file="named.catenas")
# df.named.catenas <- data.frame(lapply(df.named.catenas, as.character), stringsAsFactors=FALSE)
df.named.catenas <- data.frame(lapply(df.named.catenas, as.factor))

# set seed
set.seed(495) 
# initilize dataframe with random sample for class 1
df.named.catenas.subset <- sample_n(df.named.catenas[df.named.catenas[,1]==1,-2],min(c(nrow(df.named.catenas[df.named.catenas[,1]==1,-2]),5000)))
# Bind the rest of the classes with 500 samples
for(i in 2:16){
  df.named.catenas.subset <- rbind(df.named.catenas.subset,sample_n(df.named.catenas[df.named.catenas[,1]==i,-2],min(c(nrow(df.named.catenas[df.named.catenas[,1]==i,-2]),5000))))
}

df.named.catenas.subset[1] <- as.factor(df.named.catenas.subset[1])

# save(df.named.catenas.subset,file = "named.catenas.subset")
# load(file = "named.catenas.subset")

# A RULES MINING 

#convert into trasactions, catenas must not be in FACTOR, otherwise itll take a long long time to run.  
catena.split <- split(df.named.catenas.subset,df.named.catenas.subset$V1) 
catena.split.NA <- lapply(catena.split, function(x) x[!is.na(x)])
catena.split.NA.RM <- lapply(catena.split.NA, function(x) x[!x==""])
catena.transactions <- as(df.named.catenas.subset[,-2],"transactions")

# save(catena.transactions,file="catena.transactions")
# load("catena.transactions")
inspect(catena.transactions)

catena.rules <- apriori(catena.transactions,
                        parameter = list(supp = 0.001, conf = 0.6, target = "rules", minlen=3),
                        appearance = list(rhs = c(1:16)))
inspect(catena.rules)



redundant.rules <- is.redundant(catena.rules)
relevent_rules <- catena.rules[redundant.rules == FALSE]


#arulesviz 
# IVIz

ig <- plot(catena.rules,method="graph", control=list(type="items") )
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







# what about classifications?

######################################################################
# I might just split the code here so lets start this as a preamble 



library(dplyr)
library(arules)
library(arulesSequences)
library(arulesCBA)
library(arulesViz)
library(visNetwork)
library(igraph)
library(splitstackshape)

setwd("B:/Named") 

# load entire named catena dataset. 
load(file="named.catenas")
# Subset it into training data based on Daniels Class 
df.named.catenas.subset <- stratified(df.named.catenas, group="Class", size=1000)



# This was done before factorization of df.named.catenas.subset. otherwise run below
# load(file="named.catenas")
# Learn classifier from transactions
# prepare DF by removing split ID
df.subset <- df.named.catenas[,-2]
# make classes a factor becuase they are the one var that isn't... 
df.subset$Class <- as.factor(df.subset$Class)
# Prepare transactions with this
subset_trans <- prepareTransactions(Class~.,data=df.subset)

# inspect(subset_trans) 
#RCAR MIGHT WORK BETTER WITH A ELESTICNET/CV BUILT IN 
classifier <- CBA(Class ~ ., data = df.named.catenas.subset, supp = 0.001,
                  conf = 0.6, verbose = TRUE, balanceSupport = TRUE, maxlen=19, minlen=2)
classifier

no.traverseid <- df.named.catenas.subset[,-2]



# set seed
set.seed(117) 
# initilize dataframe with random sample for class 1
df.named.catenas.subset2 <- sample_n(df.named.catenas[df.named.catenas[,1]==1,],1000)
# Bind the rest of the classes with 500 samples
for(i in 2:16){
  df.named.catenas.subset2 <- rbind(df.named.catenas.subset2,sample_n(df.named.catenas[df.named.catenas[,1]==i,],1000))
}


levels(factor(df.named.catenas$Class))
sum(is.na(df.named.catenas$Class))

df.subset2 <- df.named.catenas.subset2[,-2]
# make classes a factor becuase they are the one var that isn't... 
df.subset2$Class <- as.factor(df.subset2$Class)
# Prepare transactions with this
subset_trans2 <- prepareTransactions(Class~.,data=df.subset2)


all.catenas <- df.named.catenas
all.catenas$Class <- as.factor(all.catenas$Class)
subset_trans3 <- prepareTransactions(Class~.,data=all.catenas)

# predict(classifier,subset_trans2)
test <-table(pred = predict(classifier, all.catenas[,-2]), true = all.catenas[,1])



pred.test <- predict(classifier, subset_trans2, default=99)


inspect(subset_trans2)

truth.sums <- function(x){
  rowcol <- 1:16
  truth <- NULL
  for(i in 1:16){
    truth[i] <- x[rowcol[i],rowcol[i]]
  }
  n <- 751519
  untruth <- (n)-sum(truth)
  sum(truth)/n
}

truth.sums(test)

holding.truths <- matrix(nrow=9,ncol=9)
confsi<- c(.8, .7,.6,.5,.4,.2,.1, .95, .9)
sups <- c(.01, .001, .002, 0.003, .0015)
# looks like .5 conf and .001sup are the best for classification




# for(x in 1:9){
# for(y in 5){
classifier <- CBA(Class ~ ., data = df.named.catenas.subset, supp = sups[y],
                  conf = confsi[x], verbose = TRUE, balanceSupport = TRUE, maxlen=19, minlen=2)

test <-table(pred = predict(classifier, all.catenas[,-2], default=99), true = all.catenas[,1])
holding.truths[y,x] <- truth.sums(test)


# }}


t <- truth.sums(test)
t

id.predclass <- cbind(predict(classifier, all.catenas[,-2]), all.catenas[1:2])
write.csv(id.predclass, "id.predclass.csv")




df.named.catenas.subset[1] <- as.factor(df.named.catenas.subset[1])


# This big dumb function creates a sequence of random exp numbers that vary by 20% to the max length of 
# the max amount of soils catenas in any one class. This is to help find the best sample size. 

sample.size <- c(round(sapply(c(sapply(c(2^c(3:17), 155404),function(x)(rep(x,20)))),function(x)(rnorm(1,x,x*0.2)))),155404)


best.samples <- NULL
for(y in sample.size){
  # whats the best number of training samples?
  df.named.catenas.subset <- sample_n(df.named.catenas[df.named.catenas[,1]==1,-2],min(c(nrow(df.named.catenas[df.named.catenas[,1]==1,-2]),155404)))
  # Bind the rest of the classes with 500 samples
  
  for(i in 2:16){
    df.named.catenas.subset <- rbind(df.named.catenas.subset,sample_n(df.named.catenas[df.named.catenas[,1]==i,-2],min(c(nrow(df.named.catenas[df.named.catenas[,1]==i,-2]),155404))))
  }
  
  # Should have done this before this loop... 
  df.named.catenas.subset$Class <- as.factor(df.named.catenas.subset$Class)
  
  classifier <- CBA(Class ~ ., data = df.named.catenas.subset, supp = 0.001,
                    conf = 0.9, verbose = FALSE, balanceSupport = TRUE, maxlen=19, minlen=2)
  classifier$default<-99
  
  test <-table(pred = predict(classifier, df.named.catenas.subset[,-2]), true = all.catenas[,1])
  
  best.samples <- rbind(cbind(truth.sums(test),y), best.samples)
  
  
}

plot(best.samples)

classifier$default<-99
levels(df.named.catenas.subset$Class)<-c(levels(df.named.catenas.subset$Class),99)
levels(all.catenas$Class)<-c(levels(all.catenas$Class),99)

all.catenas

# sequences 
library(arulesSequences)


 