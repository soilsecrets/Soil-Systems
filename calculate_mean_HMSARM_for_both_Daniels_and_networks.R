# get means for cluster classes 
library(dplyr)
library(fBasics)


setwd("B:/modelNC")

load(file="daniels_and_cluster_mlra")
rules_df <- rules_df_2

all.medians <- data.frame(matrix(ncol = 6))
names(all.medians) <- c("class","support","confidence","lift","SARMRules","HMSARMRules")



classes <- c(1,    2,    3,    4,    5,    6,    7,    8,    9,   10,   11,   12 ,  13 ,  14 ,  15  , 16, 228, 234, 240, 241, 261, 262, 2281, 2282, 2283, 2342, 2343, 2344, 2345, 2346, 2401, 2402, 2403, 2404, 2405,
             2412, 2413, 2414, 2415, 2611, 2613, 2614, 2615, 2621, 2622, 2623, 2624, 2625, 2626)

for(i in classes){
rule.df.single <- rules_df[[i]]

HMSARM <- rule.df.single[rule.df.single$lift>1.25  & rule.df.single$support>.0149,]
try(
  single.medians <- data.frame(class=i, support=HMSARM$support %>% median,
                               confidence= HMSARM$confidence %>% median,
                               lift=HMSARM$lift %>% median,
                               SARMRules=rule.df.single %>% nrow,
                               HMSARMRules=HMSARM %>% nrow)
)                               
              if(!nrow(HMSARM)==0) try(all.medians[i,] <- single.medians) else next

}

medians <- all.medians[!all.medians$SARMRules %>% is.na,]

sum(medians$HMSARMRules[-c(1:22)])/sum(medians$HMSARMRules[c(1:16)])


cluster.classes <- medians[c(1:16),]


stats.cluster <- data.frame(rbind(
cbind(
mean(cluster.classes$support),
mean(cluster.classes$confidence),
mean(cluster.classes$lift),
mean(cluster.classes$SARMRules),
mean(cluster.classes$HMSARMRules)
)
,

cbind(
stdev(cluster.classes$support),
stdev(cluster.classes$confidence),
stdev(cluster.classes$lift),
stdev(cluster.classes$SARMRules),
stdev(cluster.classes$HMSARMRules)
)
)
)


names(stats.cluster) <- c("support", "confidence","lift","SARMRules","HMSARMRules")
stats.cluster
