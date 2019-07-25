library(sp)
library(sf)
library(dplyr)
library(rnaturalearth)
library(gfcanalysis)
library(rgdal)
library(raster)


# making a function for coordinates() w/in a pipe
coordinates_iP = function(spdf){
  coordinates(spdf) = ~long+lat
  return(spdf)
}

# creating extent
df = expand.grid(data.frame(lat = c(-60, 41), long = c(-125,-30))) # change from 41 to 41 to fix extent|tile overlap
spdf = coordinates_iP(df)
crs(spdf) = CRS("+init=epsg:4326")

sfext = st_as_sf(df, coords = c("long", "lat")) %>%
  st_set_crs(4326)

# setting a directory for download
data_folder = "Documents/Jaguar/data/GIS/gfc_test_spdfID"

# getting study area polygon
aoi = ne_countries(type = 'countries', scale = 'small', returnclass = "sp") %>%
  crop(spdf) %>%
  aggregate() %>%
  SpatialPolygonsDataFrame(., data.frame( ID=1:length(.)))

# Calculate the google server URLs for the tiles needed to cover the AOI
tiles = calc_gfc_tiles(aoi)

# Check to see if these tiles are already present locally, and download them if 
# they are not.
download_tiles(tiles, data_folder)

# Extract the GFC data for this AOI from the downloaded GFC tiles, mosaicing 
# multiple tiles as necessary (if needed to cover the AOI), and saving  the 
# output data to a GeoTIFF (can also save in ENVI format, Erdas format, etc.).
gfc_data = extract_gfc(aoi, data_folder, filename=file.path(data_folder, 'gfc_extract.tif'))
