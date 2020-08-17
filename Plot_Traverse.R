library(soilDB)
library(stringr)
library(aqp)
# Load soil comp National MU System names, you could do this by loading in an excel file as well. 
natmusyskeys <- c("2rzmv",  "1tw7", "1try", "1tt5", "2tdt9",    "1ttp")

# Load Elevations 
elevations <- round(c(571.9813302,  574.8354123,    589.7961442,    630.0997302,    670.3012818,    759.1583134),1)

# Create function to plot Soil Profiles in a catena
plot.soil.traverse <- function(natmusyskeys,elevations){
  
  # Get Soil Component names, this is done with internet access. An error here is that it pulls the first one listed rather than the most frequen one.
  # This pulls out the most abundant component in the polygon. 
  soil.comp.names <- NULL
  for(itter in 1:length(natmusyskeys)){
    
    # pull out one key to query
    natmusys <- paste(natmusyskeys[itter])
    
    # create the SQL string to query 
    a.string <- paste("nationalmusym=\'",natmusys,"\'",sep="")
    # Query and create a df for the soil system
    
    # This while loop querys the SDA for the names of the soil components in the selected polygon. 
    df.SDA.names <- NULL
    attempt1 <- 0
    while(is.null(df.SDA.names) && attempt1 <= 10){
      attempt1 <- attempt1 + 1
      Sys.sleep(3)
      tryCatch(df.SDA.names <- get_component_from_SDA(WHERE = a.string)$compname,error=function(e){})
    }
    
    # This while loop querys the SDA for the abundance of the soil components in the selected polygon. 
    df.SDA.ammount <-NULL
    attempt2 <- 0
    while( is.null(df.SDA.ammount) && attempt2 <= 10 ) {
      attempt2 <- attempt2 + 1
      Sys.sleep(3)
      tryCatch(df.SDA.ammount<- get_component_from_SDA(WHERE = a.string)$comppct_r, error=function(e){})
    }
    
    # Find the max precentage of a soil comp
    tryCatch(df.SDA.ammount.max <- df.SDA.ammount==max(df.SDA.ammount), error=function(e){})
    # create a vector of names in order
    tryCatch(soil.comp.names[itter] <- paste(unlist(df.SDA.names)[unlist(df.SDA.ammount.max)]),error=function(e){})
  }
  
  # Fetch soil profiles, this also requires internet access.
  x2 <- fetchOSD(soil.comp.names)
  
  # Inverse the tops and bottoms of the soil profiles so its a depth map.
  # Otherwise it plots upside down. 
  x2$top<-(-(x2$top))
  x2$bottom<-(-(x2$bottom))
  
  # Fetching the soil profiles alphabetizes them. Fiugre out which order that is.
  soil.order.plot <- NULL
  for(name.of.comp in 1:length(soil.comp.names)){
    order <- str_locate(c(x2@site$id), toupper(soil.comp.names[name.of.comp]))[,1]
    soil.order.plot[name.of.comp]<- which(!is.na(order), arr.ind=TRUE)
  }
  
  # Create lines and points that connect the traverses on the plots. 
  better.lines <- unlist(lapply(elevations,rep, times=2))
  better.points <- seq(1.2,length(elevations)+.7,.5)
  
  bottom.line <-NULL 
  for(i in 1:length(elevations)){bottom.line[i] <-min(x2[soil.order.plot[i]]$bottom)}
  bottom.lines <- better.lines+unlist(lapply(bottom.line,rep, times=2))
  
  
  # Plot the traverse with this long function! 
  # Format the plot. 
  plot(c(1, length(soil.comp.names)+1), c(min(elevations)-100, max(elevations)+300),
       xaxs="i", xaxt = "n", yaxt = "n", xlab='Components', ylab= "Elevation", 
       main= "Soil Traverse", bg="black")+ 
    axis(1, at=1:length(soil.comp.names), labels=soil.comp.names )+
    # Create a background that is soil colored. 
    rect(0,min(elevations)-200,length(elevations)+1,max(elevations)+400,col="sienna3") +
    
    # Plot the sky
    polygon(cbind(x=c(0,better.points,length(elevations)+1,length(elevations)+1,0),
                  y=c(head(better.lines,1),260+better.lines,tail(250+better.lines,1),max(elevations)+400,max(elevations)+400)),border="brown", col= "sky blue") +
    # Plot the rock
    polygon(cbind(x=c(0,0.05+better.points,length(elevations)+1,length(elevations)+1),
                  y=c(min(elevations)-150,80+bottom.lines,tail(80+bottom.lines,1),min(elevations)-150)),border="brown", col = "grey") +
    
    # Plot each of the soil elevations by order and elevation. 
    for(i in 1:length(elevations)){
      plotSPC(x2[soil.order.plot[i]], width=.2, x.idx.offset = (.5+i)-1, add = TRUE, plot.depth.axis=FALSE,axis.line.offset = -38.5+((i-1)*7.75),
              y.offset = 250+ elevations[i], id.style = "side", abbr= TRUE, scaling.factor=2,max.depth=120)
    }
  
  # Plot a cloud, I like clouds. 
  for(cloud in 1){
    grid::grid.circle(x=.15, y=.75, r=0.10)
    grid::grid.circle(x=.18, y=.8, r=0.10)
    grid::grid.circle(x=.15, y=.8, r=0.10)
    grid::grid.circle(x=.1, y=.75, r=0.10)
    grid::grid.circle(x=.1, y=.85, r=0.10)
  }
  # The output of the above should be a plot of the soil profiles in a traverse. 
}

# Plot example soils. 
plot.soil.traverse(natmusyskeys,elevations)