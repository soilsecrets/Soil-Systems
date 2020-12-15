# This script is to make CBA work on trained classes and then
# un supervised train it off of similar hyper parameters. 

# load the environment environment_with_OH_data
library(mltools)
library(splitstackshape)

# classed.catenas
# transform classes.catenas into a matrix
m.catena.subsample <- as.matrix(classed.catenas[,-2])

# create a list of these classed catenas to turn them into transaction data. 
catena.list.subsample <- list()
for(i in 1:nrow(m.catena.subsample)){
  holding <- m.catena.subsample[i,!m.catena.subsample[i,] == c("")]
  holding <- holding[!is.na(holding)]
  catena.list.subsample[i] <-  list(holding) 
}

# transform list into transactions. 
trans.classed.catenas <- as(catena.list.subsample, "transactions")
trans.classed.catenas %>% head() %>% inspect()


# Do the same as the above, but with a sub sample. 

sub.hold <- stratified(classed.catenas <-, group = "Class", size = 1000)
m.sub.hold <- as.matrix(sub.hold[,-2])



catena.list.subsample <- list()
for(i in 1:nrow(m.sub.hold)){
  holding <- m.sub.hold[i,!m.sub.hold[i,] == c("")]
  holding <- holding[!is.na(holding)]
  catena.list.subsample[i] <-  list(holding) 
}


sub.trans.classed.catenas <- as(catena.list.subsample, "transactions")
sub.trans.classed.catenas %>% head() %>% inspect()

# Now we have the two important objects trans.classed.catenas and sub.trans.classed.catenas
# sub is our training data and trans.classed.catenas is our test data. 


###########################################################################

