library(stringr)

setwd("B:/modelNC")
load("Daniels_class_rules_list_004support_01conf_length2")

ag_rules_list <- list()

for(soilsystem in 1:16){
  
  ag_rules_list[[soilsystem]] <- subset(rules.list[[soilsystem]], confidence > .186 & support > 0.0149 & lift > 1.25)

}


ag_rules_list

pub_rules_list <- list()
for(soilsystem in 1:16){
  
  pub_rules <- read.csv("Pub_diagram_rules.csv")
  pub_rules <- pub_rules[pub_rules[,3] %in% c(0,1),c(1,2,4)]
  pub_rules <- pub_rules[pub_rules[,3]==soilsystem,]
  
  
  rules11<- rules.list[[soilsystem]]
  rules11 <- labels(rules11)
  string_rules <- str_extract_all(rules11, "[a-zA-Z.]+", simplify=TRUE)
  monostring <- paste0(string_rules[,1],string_rules[,2])
  pub_monostring <- paste0(pub_rules[,1],pub_rules[,2])
  
  
  #inspect(sort(rules.list[[soilsystem]][monostring %in% pub_monostring], by="lift"))
  
  pub_rules_list[[soilsystem]] <- rules.list[[soilsystem]][monostring %in% pub_monostring]
}




save(ag_rules_list, file="ag_rules_list")
save(pub_rules_list, file="pub_rules_list")




