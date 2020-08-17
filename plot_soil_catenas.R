# Plot soil catenas 


library(aqp)
library(soilDB)

soilDB::

SoilProfileCollection()

soilDB::
aqp::plot()

unique.soilkeys<- c("lwss",	"lwsw",	"lwsv",	"lwk6",	"ktw7")


natmusys <- paste(unique.soilkeys[itter])
a.string <- paste("nationalmusym=\'",natmusys,"\'",sep="")
spc.1 <- tryCatch(get_component_from_SDA(WHERE = a.string), error=function(e){})

SDA_query(a.string)

SPC.1 <- fetchSDA(WHERE = a.string)
plot(SPC.1)


spc.2 <- fetchSDA(a.string)



aqp::plot(spc.2)

spc.2



### dylans code 

if(requireNamespace("curl") &
   curl::has_internet() &
   require(aqp) &
   require("ggplot2") 
   require("gridExtra") 
   require("viridis")) 
  
  {
  # query soil components by areasymbol and musym
  test = fetchSDA(WHERE = "areasymbol ='IN005'AND musym ='MnpB2'")
  # profile plot 
  plot(test)
  # convert the data for depthplot
  clay_slice = horizons(slice(test, 0:200 ~ claytotal_l + claytotal_r + claytotal_h))
  names(clay_slice) <- gsub("claytotal_", "", names(clay_slice))
  om_slice = horizons(slice(test, 0:200 ~ om_l + om_r + om_h))
  names(om_slice) = gsub("om_", "", names(om_slice))
  
  test2 = rbind(data.frame(clay_slice, var = "clay"),data.frame(om_slice, var = "om")
  )             
  
  h = merge(test2, site(test)[c("nationalmusym", "cokey", "compname", "comppct_r")],by = "cokey",all.x = TRUE)
  
  # depth plot of clay content by soil component
  
  gg_comp <- function(x) {ggplot(x) +geom_line(aes(y = r, x = hzdept_r)) +
      geom_line(aes(y = r, x = hzdept_r)) +
      geom_ribbon(aes(ymin = l, ymax = h, x = hzdept_r), alpha = 0.2) +
      xlim(200, 0) +xlab("depth (cm)") +facet_grid(var ~ nationalmusym +
      paste(compname, comppct_r)) +coord_flip()
    }
  
  g1 <- gg_comp(subset(h, var == "clay"))
  g2 <- gg_comp(subset(h, var == "om"))
  grid.arrange(g1, g2)
  
  
  
  
  

    ############
  
  
spc.3  <- fetchOSD("Burton")
plot(spc.3)  
  
  
  data(sp1)
  # promote sp1 data to SoilProfileCollection
  depths(sp1) <- id ~ (-1*top + bottom)
  # move site data
  site(sp1) <- ~ group
  # compute indices
  # merged into`sp1`with implicit left-join on idname(sp1)
  horizons(sp1) <- horizonColorIndices(sp1, hue="hue", value="value", chroma="chroma")
  # visualize
  par(mar=c(0, 1, 3, 1))
  plot(sp1, color='hurst_redness')
  plot(sp1, color='barron_torrent_redness')
  plot(sp1, color='buntley_westin')
  
  
  