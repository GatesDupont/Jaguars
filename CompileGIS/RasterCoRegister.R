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


#----Raster coregistering----

# canopy height
m.canopy_height = "data/GIS/canopyHeight/Simard_Pinto_3DGlobalVeg_JGR.tif" %>%
  raster() %>%
  projectRaster(from = ., to = master, method="bilinear")

# human density
m.human_density_15 = "data\\GIS\\humanDensity\\gpw-v4-population-density-rev11_2015_30_sec_tif\\gpw_v4_population_density_rev11_2015_30_sec.tif" %>%
  raster() %>%
  projectRaster(from = ., to = master, method="bilinear")

# human footprint
m.human_footprint = "data\\GIS\\humanFootprint\\wildareas-v3-2009-human-footprint-geotiff\\wildareas-v3-2009-human-footprint.tif" %>%
  raster() %>%
  projectRaster(from = ., to = master, method="bilinear")

# elevation
m.srtm = "data/GIS/srtm/srtm.tif" %>%
  raster() %>%
  projectRaster(from = ., to = master, method="ngb")
