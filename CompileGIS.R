# Gates Dupont

"Compiling GIS data for Jaguar Project"

library(sf)
library(rnaturalearth)
library(dplyr)
library(raster)
library(MODIS)
library(velox)
library(viridis)
library(tidyverse)
library(elevatr)

# resolve namespace conflicts
select <- dplyr::select
projection <- raster::projection


#----Full BBox----

df = expand.grid(data.frame(lat = c(-60, 40), long = c(-125,-30)))

# sp extent
spdf = df
coordinates(spdf) = ~long+lat

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


#----Landcover----

# get list of tiles required to cover this bcr
tiles <- getTile(st_bbox(ext))
tiles@tile

# download tiles and combine into a single raster for each year
tifs <- runGdal(product = "MCD12Q1", collection = "006", SDSstring = "01", 
                tileH = tiles@tileH, tileV = tiles@tileV,
                begin = "2016.01.01", end = "2017.12.31", 
                outDirPath = "data/GIS", job = "modis") %>% 
  pluck("MCD12Q1.006") %>% 
  unlist()

# rename tifs to have more descriptive names
new_names <- format(as.Date(names(tifs)), "%Y") %>% 
  sprintf("modis_mcd12q1_umd_%s.tif", .) %>% 
  file.path(dirname(tifs), .)
file.rename(tifs, new_names)


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


#----Human density----

# make directory
hd_dir = "data/GIS/humanDensity"
if (!dir.exists(hd_dir)) {
  dir.create(hd_dir)
}

hd00 = "https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-density-rev11/gpw-v4-population-density-rev11_2000_30_sec_tif.zip"
hd05 = "https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-density-rev11/gpw-v4-population-density-rev11_2005_30_sec_tif.zip"
hd10 = "https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-density-rev11/gpw-v4-population-density-rev11_2010_30_sec_tif.zip"
hd15 = "https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-density-rev11/gpw-v4-population-density-rev11_2015_30_sec_tif.zip"


hd = vector("list", 4)
for(i in hd){
  download.file(i, destfile = "data/GIS/`human density`")
}


#----Human footprint----

# make directory
hf_dir = "data/GIS/humanFootprint"
if (!dir.exists(hf_dir)) {
  dir.create(hf_dir)
}

# download file directly from online
download.file(
  "https://sedac.ciesin.columbia.edu/downloads/data/wildareas-v3/wildareas-v3-2009-human-footprint/wildareas-v3-2009-human-footprint-geotiff.zip",
  destfile = paste0(hf_dir, "/wildareas-v3-2009-human-footprint-geotiff.zip"))

unzip(zipfile = paste0(hf_dir, "/wildareas-v3-2009-human-footprint-geotiff.zip"),
      exdir = "wildareas-v3-2009-human-footprint-geotiff")


#----output----
unlink(f_ne)
write_sf(ne_land, f_ne, "ne_land")
write_sf(ne_country_lines, f_ne, "ne_country_lines")
