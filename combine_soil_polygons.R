# combine all soil data 

library(raster)

setwd("B:/traverses/MUs")
NC.files <- list.files(, pattern = "elevation_NCDEM30mCRS_mapunits")

system.time(
for(file in 1:359){
load(NC.files[file])
assign(paste0("nc",file),elevation.tmapunits)

}

)


rm(elevation.tmapunits)
rm(NC.files)
rm(file)
rm(test)
rm(k)
rm(m)
rm(dfs)


dfs <- lapply(.GlobalEnv, is.projected) 

system.time(
m <- do.call(bind, list(mget(names(dfs))))
)

rm(list=ls(pattern = "nc"))
rm(dfs)

save(m, file = "all_elevation_tmapunits")
load("all_elevation_tmapunits")

u <- union(m)
save(u, file = "flat_elevation_tmapunits")
 
m <- m[duplicated(m$mupolygonkey),]
save(m, file = "flat_elevation_tmapunits")

msmall <- m[,-c(1,2,3,6,7)]

rm(msmall)





