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
df = expand.grid(data.frame(lat = c(-56.4, 37.9), long = c(-124.7,-32.9)))
ext = st_as_sf(df, coords = c("long", "lat")) %>%
  st_set_crs(4326)
ext_grid = st_make_grid(ext, cellsize = 4.5, what="centers")
plot(ext_grid, pch=20, col="red")
maps::map(add=TRUE)

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
call4mosaic = paste0("mosaic(", elev.tiles.names, ", fun=mean, filename='data/GIS/srtm/srtm.tif')")
r_srtm = eval(parse(text = call4mosaic))


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

#----output----
unlink(f_ne)
write_sf(ne_land, f_ne, "ne_land")
write_sf(ne_country_lines, f_ne, "ne_country_lines")
