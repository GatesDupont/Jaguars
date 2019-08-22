# Gates Dupont

library(maps)
library(sp)
library(sf)

set_coords = function(df, crs_in){
  coordinates(df) = ~x+y
  sp::proj4string(df) = crs_in
  return(df)
}

#----Getting Sumatra polygon----
sumatra = map_data("world") %>%
  filter(region == "Indonesia") %>%
  filter(subregion == "Sumatra") %>%
  select(x = long, y = lat) %>%
  set_coords(st_crs(4326)$proj4string) %>%
  spPolygons()

plot(sumatra)
extent(sumatra)

#----Getting climate data----
worldclim = getData("worldclim", var = "bio", res = 2.5, path="data/GIS/worldclim") %>%
  crop(sumatra)

#----Getting elevation data----

# setting up objects for loop
elev.points = sumatra %>%
  st_make_grid(cellsize = 4.5, what="centers") %>%
  st_coordinates()
elev.tiles = list()

# loop through coord grid to get tiles
for(i in 1:length(elev.points[,2])){
  tryCatch({
    elev.tiles[[i]] = getData("SRTM", lon=elev.points[i,1], lat=elev.points[i,2])},
    error=function(e){})
}

# check if they're all there
if(all(unlist(lapply(elev.tiles,is.null))) == T){
  elev.tiles = elev.tiles[-which(lapply(elev.tiles,is.null) == T)]
}

# create string of raster names
elev.tiles.names = "elev.tiles[[1]]"
for(i in 2:length(elev.tiles)){
  elev.tiles.names = paste0(elev.tiles.names, ", ", "elev.tiles[[", as.character(i), "]]")
}

# Mosaicking tiles together
call4mosaic = paste0("mosaic(", elev.tiles.names, ", fun=mean)")
r_srtm = eval(parse(text = call4mosaic))
Sys.time()
srtm = crop(sumatra, spdf)
Sys.time()
