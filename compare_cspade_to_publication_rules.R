#compare cspade to publication rules 
library(stringr)

setwd("B:/modelNC")
load("Daniels_class_rules_list_004support_01conf_length2")

soil_rules_list <- list()
for(soilsystem in 1:16){

  soil_rules <- data.frame(matrix(ncol=2))
  colnames(soil_rules) <- c("start", "end")

for(rule in 1:length(labels(rules.list[[soilsystem]]))){  
  cleaned_rules_list <- str_replace_all(labels(rules.list[[soilsystem]]), "\\.","") 
soil_rules[rule,] <- str_extract_all(cleaned_rules_list[rule],"\\w+")[[1]]

}
  soil_rules_list[[soilsystem]] <- soil_rules
  
}



soil_rules_list[[soilsystem]]
labels(rules.list[[1]])=="."



