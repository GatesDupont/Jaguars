library(dplyr)
library(rnaturalearth)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(RStoolbox)
select = dplyr::select

#----Making spatial extent----

# making a function for coordinates() w/in a pipe
coordinates_iP = function(spdf){
  coordinates(spdf) = ~long+lat
  return(spdf)
}

df = expand.grid(data.frame(lat = c(-60, 40), long = c(-125,-30)))
spdf = coordinates_iP(df)

#----Creating study raster----

# getting extent shapefile
names_iP = function(spolydf, newLayerName){
  names(spolydf) = newLayerName
  return(spolydf)
}

# rasterize a shapefile with a new resolution
rasterize2 = function(shapefile, resolution) {
  # Make empty raster
  r = raster(ncol=500, nrow=500)
  # set extent to shapefile
  extent(r) = extent(shapefile)
  # set desired resolution
  res(r) = resolution
  # assign random values to pixels
  r[] = runif(n = ncell(r), min=0, max=1)
  # rasterize the shapefile
  r_shp = rasterize(shapefile, r)
  return(r_shp)
}

# get world polys, crop, dissolve, rename, rasterize
master = ne_countries(type = 'countries', scale = 'small') %>%
  crop(spdf) %>%
  aggregate() %>%
  as('SpatialPolygonsDataFrame') %>%
  names_iP(.,'studyArea') %>%
  #writeOGR(dsn=stdyshp_dir, layer='studyArea', driver='ESRI Shapefile') %>%
  rasterize2(., 0.1)


#----Canopy Height----

library(R.utils)

# make directory
ch_dir = "~/Desktop/canopyHeight"
if (!dir.exists(ch_dir)) {
  dir.create(ch_dir)
}

# download file from online
download.file("https://landscape.jpl.nasa.gov/resources/Simard_Pinto_3DGlobalVeg_JGR.tif.gz",
              destfile = paste0(ch_dir, "/Simard_Pinto_3DGlobalVeg_JGR.tif.gz"))

# unzip the file
gunzip(paste0(ch_dir, "/Simard_Pinto_3DGlobalVeg_JGR.tif.gz"))

# read as raster and crop
canopy_height = "~/Desktop/canopyHeight/Simard_Pinto_3DGlobalVeg_JGR.tif" %>%
  raster()


#----Raster coregistering----
m.canopy_height = canopy_height %>%
  projectRaster(from = ., to = master, method="bilinear")
