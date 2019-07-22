library(gfcanalysis)
library(dplyr)
library(rnaturalearth)
library(rgdal)

# making a function for coordinates() w/in a pipe
coordinates_iP = function(spdf){
  coordinates(spdf) = ~long+lat
  return(spdf)
}

# renaming shapefile layer in pipe
names_iP = function(spolydf, newLayerName){
  names(spolydf) = newLayerName
  return(spolydf)
}

# creating extent
df = expand.grid(data.frame(lat = c(-60, 40), long = c(-125,-30)))
spdf = coordinates_iP(df)

# getting study area polygon
master = ne_countries(type = 'countries', scale = 'small') %>%
  crop(spdf) %>%
  aggregate() %>%
  as('SpatialPolygonsDataFrame') %>%
  names_iP(.,'studyArea')


# Calculate the google server URLs for the tiles needed to cover the AOI
tiles = calc_gfc_tiles(master)

# download tiles
download_tiles(tiles, "~/Jaguar/data/GIS/gfc")

# mosaic the 4 bands together
ls = list.files("~/Jaguar/data/GIS/gfc")

treecover.files = ls[grep("treecover2000", ls)]


treecover = vector("list", 70)
for(i in 1:70){
  treecover[[i]] = raster(paste0("C:/Users/dupon/Documents/Jaguar/data/GIS/gfc/", treecover.files[i]))
}


# create string of raster names
treecover.names = "treecover[[1]]"
for(i in 2:length(treecover)){
  treecover.names = paste0(treecover.names, ", ", "treecover[[", as.character(i), "]]")
}

# Mosaicking tiles together
call4mosaic = paste0("mosaic(", treecover.names, 
                     ", fun=mean, filename='C:/Users/dupon/Documents/Jaguar/data/GIS/gfc/gfc_treecover2000.tif', overwrite=TRUE)")
r_treecover = eval(parse(text = call4mosaic))


# r = mosaic(treecover[[1]], treecover[[2]])

r_test = mosaic(treecover[[1]], treecover[[2]], fun=mean)

# moasic and save tiles
gfc_data <- extract_gfc(mcrop, "~/Jaguar/data/GIS/gfc", filename='gfc_extract.tif')

mcrop = crop(master, tiles)
mcrop@bbox <- as.matrix(0.2*extent(tiles))
