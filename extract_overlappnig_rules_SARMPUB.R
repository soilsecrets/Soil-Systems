library(stringr)



soilsystem <- 7


min_metrics <- matrix(ncol=4, nrow=16)
colnames(min_metrics) <-  c("ss", "sup", "con", "lift")


max_metrics <- matrix(ncol=4, nrow=16)
colnames(max_metrics) <-  c("ss", "sup", "con", "lift")

mean_metrics <- matrix(ncol=4, nrow=16)
colnames(mean_metrics) <-  c("ss", "sup", "con", "lift")

median_metrics <- matrix(ncol=4, nrow=16)
colnames(mean_metrics) <-  c("ss", "sup", "con", "lift")

sd_metrics <- matrix(ncol=4, nrow=16)
colnames(sd_metrics) <-  c("ss", "sup", "con", "lift")


count_metrics <- matrix(ncol=4, nrow=16)
colnames(count_metrics) <-  c("ss", "sup", "con", "lift")

sum_metrics <- matrix(ncol=4, nrow=16)
colnames(sum_metrics) <-  c("ss", "sup", "con", "lift")


# as good as tables 
ag_min_metrics <- matrix(ncol=4, nrow=16)
colnames(ag_min_metrics) <-  c("ss", "sup", "con", "lift")


ag_max_metrics <- matrix(ncol=4, nrow=16)
colnames(ag_max_metrics) <-  c("ss", "sup", "con", "lift")

ag_mean_metrics <- matrix(ncol=4, nrow=16)
colnames(ag_mean_metrics) <-  c("ss", "sup", "con", "lift")

ag_median_metrics <- matrix(ncol=4, nrow=16)
colnames(ag_mean_metrics) <-  c("ss", "sup", "con", "lift")

ag_sd_metrics <- matrix(ncol=4, nrow=16)
colnames(ag_sd_metrics) <-  c("ss", "sup", "con", "lift")

ag_count_metrics <- matrix(ncol=4, nrow=16)
colnames(ag_count_metrics) <-  c("ss", "sup", "con", "lift")

ag_sum_metrics <- matrix(ncol=4, nrow=16)
colnames(ag_sum_metrics) <-  c("ss", "sup", "con", "lift")







for(soilsystem in 1:16){

pub_rules <- read.csv("Pub_diagram_rules.csv")
pub_rules <- pub_rules[pub_rules[,3] %in% c(1,2),c(1,2,4)]
pub_rules <- pub_rules[pub_rules[,3]==soilsystem,]


rules11<- rules.list[[soilsystem]]
rules11 <- labels(rules11)
string_rules <- str_extract_all(rules11, "[a-zA-Z.]+", simplify=TRUE)
monostring <- paste0(string_rules[,1],string_rules[,2])
pub_monostring <- paste0(pub_rules[,1],pub_rules[,2])


sum(monostring %in% pub_monostring)
sum(monostring %in% pub_monostring)/length(pub_monostring)

length(pub_monostring)/length(monostring)
#inspect(sort(rules.list[[soilsystem]][monostring %in% pub_monostring], by="lift"))

table_overlap <- as(rules.list[[soilsystem]][monostring %in% pub_monostring], "data.frame")

min_metrics[soilsystem,] <- c(soilsystem, min(table_overlap$support), min(table_overlap$confidence), min(table_overlap$lift) )
max_metrics[soilsystem,] <- c(soilsystem, max(table_overlap$support), max(table_overlap$confidence), max(table_overlap$lift) )
mean_metrics[soilsystem,] <- c(soilsystem, mean(table_overlap$support), mean(table_overlap$confidence), mean(table_overlap$lift))
median_metrics[soilsystem,] <- c(soilsystem, median(table_overlap$support), median(table_overlap$confidence), median(table_overlap$lift))
sd_metrics[soilsystem,] <- c(soilsystem, sd(table_overlap$support), sd(table_overlap$confidence), sd(table_overlap$lift))
count_metrics[soilsystem,] <- c(soilsystem, rep(length(table_overlap$support),3))
sum_metrics[soilsystem,] <- c(soilsystem, sum(table_overlap$support), sum(table_overlap$confidence), sum(table_overlap$lift))

as_good_as_pub <- as(subset(rules.list[[soilsystem]], confidence > median(table_overlap$confidence) & support > 0.015 & lift >median(table_overlap$lift)), "data.frame")

ag_min_metrics[soilsystem,] <- c(soilsystem, min(as_good_as_pub$support), min(as_good_as_pub$confidence), min(as_good_as_pub$lift) )
ag_max_metrics[soilsystem,] <- c(soilsystem, max(as_good_as_pub$support), max(as_good_as_pub$confidence), max(as_good_as_pub$lift) )
ag_mean_metrics[soilsystem,] <- c(soilsystem, mean(as_good_as_pub$support), mean(as_good_as_pub$confidence), mean(as_good_as_pub$lift))
ag_median_metrics[soilsystem,] <- c(soilsystem, median(as_good_as_pub$support), median(as_good_as_pub$confidence), median(as_good_as_pub$lift))
ag_sd_metrics[soilsystem,] <- c(soilsystem, sd(as_good_as_pub$support), sd(as_good_as_pub$confidence), sd(as_good_as_pub$lift))
ag_count_metrics[soilsystem,] <- c(soilsystem, rep(length(as_good_as_pub$support),3))
ag_sum_metrics[soilsystem,] <- c(soilsystem, sum(as_good_as_pub$support), sum(as_good_as_pub$confidence), sum(as_good_as_pub$lift))


}


t.test(sd_metrics[,4],ag_sd_metrics[,4])




t.test(sd_metrics[,2],ag_sd_metrics[,2])




inspect(sort(as_good_as_pub, by="lift"))

as(sort(as_good_as_pub, by="lift"),"data.frame")$rule %in% as(sort(rules.list[[soilsystem]][monostring %in% pub_monostring], by="lift"), "data.frame")$rule











pub_rules[pub_monostring %in% monostring ,]


pub_rules2 <- read.csv("Pub_diagram_rules.csv")
pub_rules2 <- pub_rules2[pub_rules2[,3] %in% c(0,1),c(1,2,4)]
pub_rules2

comp.matrix <- matrix(ncol=16, nrow=16)
for(ssx in 1:16){
  for(ssy in 1:16){
    pub_rulesx <- pub_rules2[pub_rules2$SS==ssx,]
    pub_rulesy <- pub_rules2[pub_rules2$SS==ssy,]
    
    pub_monostringx <- paste0(pub_rulesx[,1],pub_rulesx[,2])
    pub_monostringy <- paste0(pub_rulesy[,1],pub_rulesy[,2])
    comp.matrix[ssx,ssy]    <- sum(pub_monostringx %in% pub_monostringy)
    
  }}




