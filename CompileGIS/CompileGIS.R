# Gates Dupont

"Compiling GIS data for Jaguar Project"

library(sf)
library(sp)
library(rnaturalearth)
library(dplyr)
library(raster)
library(rgdal)
library(MODIS)
library(velox)
library(viridis)
library(tidyverse)
library(elevatr)

# resolve namespace conflicts
select = dplyr::select
projection = raster::projection

# MANUAL DOWNLOADS

"Please follow the links to manually download the data laid out below.
 These cannot be downlaoded directly into R as doing so involves
 logging into the site."

#----Human Footprint----

# make directory
hf_dir = "data/GIS/humanFootprint"
if (!dir.exists(hf_dir)) {
  dir.create(hf_dir)
}

# Info:
# https://sedac.ciesin.columbia.edu/data/set/wildareas-v3-2009-human-footprint

# Download
# https://sedac.ciesin.columbia.edu/downloads/data/wildareas-v3/wildareas-v3-2009-human-footprint/wildareas-v3-2009-human-footprint-geotiff.zip


#----Human Density---- (2000, 05, 10, 15)

# make directory
hd_dir = "data/GIS/humanDensity"
if (!dir.exists(hd_dir)) {
  dir.create(hd_dir)
}

# Info:
# https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11

# Download:
# https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-density-rev11/gpw-v4-population-density-rev11_2000_30_sec_tif.zip
# https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-density-rev11/gpw-v4-population-density-rev11_2005_30_sec_tif.zip
# https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-density-rev11/gpw-v4-population-density-rev11_2010_30_sec_tif.zip
# https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-density-rev11/gpw-v4-population-density-rev11_2015_30_sec_tif.zip



# AUTOMATIC DOWNLOADS

" The rest of the data can be downloaded automatically using 
  the following code"


#----Full BBox----

# making a function for coordinates() w/in a pipe
coordinates_iP = function(spdf){
  coordinates(spdf) = ~long+lat
  return(spdf)
}

# data.frame extent
df = expand.grid(data.frame(lat = c(-60, 40), long = c(-125,-30)))

# sp extent
spdf = coordinates_iP(df)

# sf extent
ext = st_as_sf(df, coords = c("long", "lat")) %>%
  st_set_crs(4326)
ext_grid = st_make_grid(ext, cellsize = 4.5, what="centers")


#----File to save spatial data----

gpkg_dir <- "data/GIS"
if (!dir.exists(gpkg_dir)) {
  dir.create(gpkg_dir)
}
f_ne <- file.path(gpkg_dir, "gis-data.gpkg")


#----Study area shapefile----

# making file path for shapefile
mr_dir = "data/GIS/masterRaster"
if (!dir.exists(mr_dir)) {
  dir.create(mr_dir)
}

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


#----Political boundaries----

# land border with lakes removed
ne_land <- ne_download(scale = 50, category = "cultural",
                       type = "admin_0_countries_lakes",
                       returnclass = "sf") %>%
  filter(CONTINENT %in% c("North America", "South America")) %>%
  st_set_precision(1e6) %>%
  st_union()

# country lines
ne_country_lines <- ne_download(scale = 50, category = "cultural",
                                type = "admin_0_boundary_lines_land",
                                returnclass = "sf") %>% 
  st_geometry()
ne_country_lines <- st_intersects(ne_country_lines, ne_land, sparse = FALSE) %>%
  as.logical() %>%
  {ne_country_lines[.]}


#----Elevation----

# file path to save elevation data
srtm_dir <- "data/GIS/srtm"
if (!dir.exists(srtm_dir)) {
  dir.create(srtm_dir)
}

# setting up objects for loop
elev.points = st_coordinates(ext_grid)
elev.tiles = list()

# loop through coord grid to get tiles
for(i in 1:length(elev.points[,2])){
  tryCatch({
    elev.tiles[i] = getData("SRTM", lon=elev.points[i,1], lat=elev.points[i,2], path=srtm_dir)},
    error=function(e){})
}
elev.tiles = elev.tiles[-which(lapply(elev.tiles,is.null) == T)]


# create string of raster names
elev.tiles.names = "elev.tiles[[1]]"
for(i in 2:length(elev.tiles)){
  elev.tiles.names = paste0(elev.tiles.names, ", ", "elev.tiles[[", as.character(i), "]]")
}

# Mosaicking tiles together
call4mosaic = paste0("mosaic(", elev.tiles.names, 
                     ", fun=mean, filename='data/GIS/srtm/srtm.tif', overwrite=TRUE)")
r_srtm = eval(parse(text = call4mosaic))
srtm = crop(r_srtm, spdf)


#----MODIS Landcover----

# get list of tiles required to cover this bcr
tiles <- getTile(st_bbox(ext))
tiles@tile

# download tiles and combine into a single raster for each year
tifs_lc <- runGdal(product = "MCD12Q1", collection = "006", SDSstring = "01", 
                   tileH = tiles@tileH, tileV = tiles@tileV,
                   begin = "2001.01.01", end = "2017.12.31", 
                   outDirPath = "data/GIS", job = "modis") %>% 
  pluck("MCD12Q1.006") %>% 
  unlist()

# rename tifs to have more descriptive names
new_names <- format(as.Date(names(tifs_lc)), "%Y") %>% 
  sprintf("modis_mcd12q1_umd_%s.tif", .) %>% 
  file.path(dirname(tifs_lc), .)
file.rename(tifs_lc, new_names)


#----Canopy Cover----

# download tiles and combine into a single raster for each year
tifs_cc <- runGdal(product = "MOD44B", collection = "006", SDSstring = "01", 
                   tileH = tiles@tileH, tileV = tiles@tileV,
                   begin = "2000.04.01", end = "2017.12.31", 
                   outDirPath = "data/GIS", job = "modis") %>% 
  pluck("MOD44B.006") %>% 
  unlist()

# rename tifs to have more descriptive names
new_names <- format(as.Date(names(tifs_cc)), "%Y") %>% 
  sprintf("modis_mod44b_umd_%s.tif", .) %>% 
  file.path(dirname(tifs_cc), .)
file.rename(tifs_cc, new_names)


#----Canopy Height----

library(R.utils)

# make directory
ch_dir = "data/GIS/canopyHeight"
if (!dir.exists(ch_dir)) {
  dir.create(ch_dir)
}

# download file from online
download.file("https://landscape.jpl.nasa.gov/resources/Simard_Pinto_3DGlobalVeg_JGR.tif.gz",
              destfile = paste0(ch_dir, "/Simard_Pinto_3DGlobalVeg_JGR.tif.gz"))

# unzip the file
gunzip("data/GIS/canopyHeight/Simard_Pinto_3DGlobalVeg_JGR.tif.gz")

# read as raster and crop
canopy_height = "data/GIS/canopyHeight/Simard_Pinto_3DGlobalVeg_JGR.tif" %>%
  raster() %>%
  crop(., spdf)


#---Climate----

# Pulling all worldclim data
worldclim = getData("worldclim", var="bio", res = 2.5) # 2.5 minutes of a degree (about 4.5 km at eq)
tmin = getData("worldclim", var="tmin", res = 2.5)
tmax = getData("worldclim", var="tmax", res = 2.5)
prec = getData("worldclim", var="prec", res = 2.5)

# stacking and cropping worldclim
climate = stack(worldclim, tmin, tmax, prec) %>%
  crop(., spdf)


#----Roads----

# make directory
rd_dir = "data/GIS/roads"
if (!dir.exists(rd_dir)) {
  dir.create(rd_dir)
}

# download file from online for North America
download.file("http://geoservice.pbl.nl/download/opendata/GRIP4/GRIP4_Region1_vector_shp.zip",
              destfile = paste0(rd_dir, "/GRIP4_Region1_vector_shp.zip"))
unzip(paste0(rd_dir, "/GRIP4_Region1_vector_shp.zip"), exdir = paste0(rd_dir, "/GRIP4_Region1_vector_shp"))

# download file from online for Central & South America
download.file("http://geoservice.pbl.nl/download/opendata/GRIP4/GRIP4_Region2_vector_shp.zip",
              destfile = paste0(rd_dir, "/GRIP4_Region2_vector_shp.zip"))
unzip(paste0(rd_dir, "/GRIP4_Region2_vector_shp.zip"), exdir = paste0(rd_dir, "/GRIP4_Region2_vector_shp"))

# combine the shapefiles
rd_na = shapefile(paste0(rd_dir, "/GRIP4_Region1_vector_shp/GRIP4_region1.shp"))
rd_csa = shapefile(paste0(rd_dir, "/GRIP4_Region2_vector_shp/GRIP4_region2.shp"))

ext <- extent(p)
r <- raster(ext, res=50000)  
r <- rasterize(p, r, field=1)
plot(r)


#----Output----

unlink(f_ne)
write_sf(ne_land, f_ne, "ne_land")
write_sf(ne_country_lines, f_ne, "ne_country_lines")
