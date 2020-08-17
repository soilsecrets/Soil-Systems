# This code preforms apriori pattern mining on named soil traverses. 

# Read in a CSV of named, classed soil traverses.  
soil.name.matrix <- read.csv("soil.name.matrix.12.csv")

# Blanks as NAs 
soil.name.matrix[soil.name.matrix==""] <- NA

# Split as named lists
list.soils <- split(soil.name.matrix[,2:20], soil.name.matrix[,1])

# Remove NAs 
list.soils.narm <- lapply(list.soils, function(x) x[!is.na(x)])

# Turn list into transaction set, takes a while and sometimes crashes...
tsoils <- as(list.soils.narm, "transactions")

# Mine some patterns 
soils.rules <- apriori(tsoils, parameter = list(supp= .001, conf = 0.20, minlen=3))


inspect(soils.rules)

plot(soils.rules, method="graph")

redundant.rules <- is.redundant(soils.rules)
relevent_rules <- soils.rules[redundant.rules == FALSE]



# IVIz
IVIZ <- function(x){
ig <- plot(x,method="graph", control=list(type="items") )
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
}

IVIZ(relevent_rules)
