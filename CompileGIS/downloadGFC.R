library(sp)
library(sf)
library(dplyr)
library(rnaturalearth)
library(gfcanalysis)
library(rgdal)


# making a function for coordinates() w/in a pipe
coordinates_iP = function(spdf){
  coordinates(spdf) = ~long+lat
  return(spdf)
}

# creating extent
df = expand.grid(data.frame(lat = c(3, 10), long = c(-125,-75)))
spdf = coordinates_iP(df)

# setting a directory for download
data_folder = "./Documents/Jaguar/data/GIS/gfc"

# getting study area polygon
aoi = ne_countries(type = 'countries', scale = 'small') %>%
  crop(spdf) %>%
  aggregate() %>%
  as_Spatial()

# Calculate the google server URLs for the tiles needed to cover the AOI
tiles = calc_gfc_tiles(aoi)

# Check to see if these tiles are already present locally, and download them if 
# they are not.
download_tiles(tiles, data_folder)

# Extract the GFC data for this AOI from the downloaded GFC tiles, mosaicing 
# multiple tiles as necessary (if needed to cover the AOI), and saving  the 
# output data to a GeoTIFF (can also save in ENVI format, Erdas format, etc.).
gfc_data = extract_gfc(aoi, data_folder, filename=file.path(data_folder, 'gfc_extract.tif'))
