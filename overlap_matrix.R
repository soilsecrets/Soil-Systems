setwd("B:/modelNC")
load("ag_rules_list")
load("pub_rules_list")
heatmap <- function(data, rowN, colN, xTitle = "", yTitle = "", numColors=11){
  # transpose and rotate matrix clockswise 90 degrees 
  dataAdjusted <- t(apply(data,2,rev))
  
  image(1:ncol(data), 1:nrow(data), xlab = xTitle, ylab = yTitle, dataAdjusted, col = rev(brewer.pal(numColors,"RdYlBu")), axes = FALSE)
  axis(1, 1:ncol(data), colN)
  axis(2, nrow(data):1, rowN)
  
  for (x in 1:ncol(data))
    for (y in 1:nrow(data))
      # add text values into matrix based on transposed/rotated indices + round values to two digits
      text(x, y, round(dataAdjusted[x,y],2))
}



overlap.matrix.ag <- matrix(ncol=16, nrow=16)

for(x in c(1:16)){
  
for(y in c(1:16)){
    
    i <- ag_rules_list[[x]]
    j <- ag_rules_list[[y]]
    
    overlap.rules <- i %in% j     
    overlap.matrix.ag[x,y] <-sum(!is.na(overlap.rules))
 
    
}
#overlap.matrix.ag[x,x]<-0
}

sum(sapply(ag_rules_list, length))

sum(overlap.matrix.ag)/sum(sapply(ag_rules_list, length))
sum(overlap.matrix.ssnc)/sum(sapply(pub_rules_list, length))

overlap.matrix.ssnc <- matrix(ncol=16, nrow=16)

for(x in c(1:16)){
  
  for(y in c(1:16)){
    
    i <- pub_rules_list[[x]]
    j <- pub_rules_list[[y]]
    
    overlap.rules <- i %in% j     
    overlap.matrix.ssnc[y,x] <- sum(!is.na(overlap.rules))
    
    
  }
#overlap.matrix.ssnc[x,x] <- 0 
}


overlap.matrix.both <- matrix(ncol=16, nrow=16)

for(x in c(1:16)){
  
  for(y in c(1:16)){
    
    i <- pub_rules_list[[x]]
    j <- ag_rules_list[[y]]
    
    overlap.rules <- i %in% j     
    overlap.matrix.both[x,y] <- sum(!is.na(overlap.rules))
    
    
  }
}


overlap.matrix.both2 <- matrix(ncol=16, nrow=16)

for(x in c(1:16)){
  
  for(y in c(1:16)){
    
    i <- ag_rules_list[[x]]
    j <- pub_rules_list[[y]]
    
    overlap.rules <- i %in% j     
    overlap.matrix.both2[x,y] <- sum(!is.na(overlap.rules))
    
    
  }
  
}










cm <- overlap.matrix.ag
heatmap(cm,1:16,1:16, "heatmap of overlapping rules between classes for SSNC classes - HMSARM")

cm <- overlap.matrix.ssnc
heatmap(cm,1:16,1:16, "heatmap of overlapping rules between classes for SSNC classes - SSNC")

cm <- overlap.matrix.both
heatmap(cm,1:16,1:16, "heatmap of overlapping rules between SSNC and HMSARM")

cm <- overlap.matrix.both2
heatmap(cm,1:16,1:16, "heatmap of overlapping rules between HMSARM and SSNC")




agm <- NULL
ssncm <- NULL
bothm <- NULL 
both2m <- NULL
agp <- NULL
ssncp <- NULL
bothp <- NULL
both2p <- NULL

for(i in 1:16){
  
  
  agm <- overlap.matrix.ag[,i]
  ssncm <- overlap.matrix.ssnc[,i]
  bothm <- overlap.matrix.both[,i]
  both2m <- overlap.matrix.both2[,i]
  agp <- overlap.matrix.ag[i,]
  ssncp <- overlap.matrix.ssnc[i,]
  bothp <- overlap.matrix.both[i,]
  both2p <- overlap.matrix.both2[i,]
  
  
  
  
}

df.mp <- data.frame(cbind(agm,
                 ssncm,
                 bothm,
                 both2m,
                 agp,
                 ssncp,
                 bothp,
                 both2p ))


t.test(both2m,ssncm)


