library(sp)
library(sf)
library(dplyr)
library(rnaturalearth)
library(gfcanalysis)
library(rgdal)

data_folder <- "Documents/testR/gfctest2"

# Load a demo AOI from the P drive - notice that first we specify the folder 
# the shapefile is in, and then the name of the shapefile without the '.shp'
aoi <- readOGR(system.file('extdata', package='gfcanalysis'), 'ZOI_NAK_2012')


aoi = ne_states(iso_a2 = "US", returnclass = "sf") %>%
  filter(name == "Texas") %>%
  st_set_crs(4326) %>%
  as_Spatial()

# Calculate the google server URLs for the tiles needed to cover the AOI
tiles <- calc_gfc_tiles(aoi)

# Check to see if these tiles are already present locally, and download them if 
# they are not.
download_tiles(tiles, data_folder)

# Extract the GFC data for this AOI from the downloaded GFC tiles, mosaicing 
# multiple tiles as necessary (if needed to cover the AOI), and saving  the 
# output data to a GeoTIFF (can also save in ENVI format, Erdas format, etc.).
gfc_data <- extract_gfc(aoi, data_folder, filename='gfc_NAK_extract.tif')
