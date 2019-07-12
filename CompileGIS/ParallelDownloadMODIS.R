# Gates Dupont

library(doParallel)
library(dplyr)
library(foreach)
library(MODIS)
library(raster)
library(sp)
library(SpaDES)
library(sf)

select = dplyr::select

#----Extent----
df = expand.grid(data.frame(lat = c(-60, 40), long = c(-125,-30)))

# sp extent object
spdf = df
coordinates(spdf) = ~long+lat

# sf extent object
ext = st_as_sf(df, coords = c("long", "lat")) %>%
  st_set_crs(4326)
ext_grid = st_make_grid(ext, cellsize = 4.5, what="centers")


#----Split up----

# define block to break the raster into
r = raster(spdf, res=0.5) %>%
  splitRaster(nx = 2, ny=3)


#----Plotting this division----

plot(spdf)
maps::map(add=T)
p = rasterToPolygons(raster(spdf, res=0.5))
plot(p, border='gray', add=T)

for(i in 1:6){
  plot(rasterToPolygons(r[[i]]), border=rainbow(6)[i], add=T)
}


#----MODIS----

# making parallel cluster
cl = makeCluster(6)
registerDoParallel(cl)

# initializing the outputs
tifs_lc = vector("list", 6)
new_names = vector("list", 6)

# splitting necessary tiles into 6 parts
tiles.split = vector("list", 6)
for(i in 1:6){
  tiles.split[[i]] = getTile(r[[i]])
}

# parallelizing the MODIS data download
foreach(i = 1:6, .packages=c('MODIS', 'rgeos', 'purr', 'dplyr')) %dopar% {
  # download tiles and combine into a single raster for each year
  tifs_lc[[i]] <- runGdal(product = "MCD12Q1", collection = "006", SDSstring = "01", 
                     tileH = tiles.split[[i]]@tileH, tileV = tiles.split[[i]]@tileV,
                     begin = "2001.01.01", end = "2017.12.31", 
                     outDirPath = "data/GIS", job = "modis") %>% 
    pluck("MCD12Q1.006") %>% 
    unlist()
  
  # rename tifs to have more descriptive names
  new_names[[i]] <- format(as.Date(names(tifs_lc[[i]])), "%Y") %>% 
    sprintf("modis_mcd12q1_umd_%s.tif", .) %>% 
    file.path(dirname(tifs_lc[[i]]), .)
  file.rename(tifs_lc[[i]], new_names[[i]])
}
stopCluster(cl) # ending parallels
