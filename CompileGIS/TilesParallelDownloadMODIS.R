# Gates Dupont                #
# Jaguar Global Meta-analysis #
# July 17, 2019               #
# # # # # # # # # # # # # # # #

library(MODIS)
library(dplyr)
library(purrr)
library(doParallel)
library(foreach)


#----Create? and set dir----
Jaguar_dir <- "~/Jaguar"
if (!dir.exists(Jaguar_dir)) {
  dir.create(Jaguar_dir)
}
setwd(Jaguar_dir)

#----Read in manually-selected tiles----
# Could use getTiles() but I hard-coded out of annoyance! :)
tiles = read.csv("data/GIS/modis/MODIS_tiles.csv")
tilesH = tiles$H
tilesV = tiles$V

#----Loop through tiles and download----

# making parallel cluster
cl = makeCluster(20)
registerDoParallel(cl)

foreach(i = 1:42, .packages=c('MODIS', 'rgeos', 'purrr', 'dplyr')) %dopar% {
  # download tiles and combine into a single raster for each year
  tifs = runGdal(product = "MCD12Q1", collection = "006", SDSstring = "01",
                 tileH = tilesH[i], tileV = tilesV[i],
                 begin = "2001.01.01", end = "2017.12.31",
                 outDirPath = "data/GIS/modis/tiles", job = paste0("modis_",tilesH[i],"_",tilesV[i])) %>%
    pluck("MCD12Q1.006") %>%
    unlist()
  
  # rename tifs to have more descriptive names
  new_names = format(as.Date(names(tifs)), "%Y") %>%
    sprintf(paste0("modis_mcd12q1_h", tilesH[i], "_v", tilesV[i], "_umd_%s.tif"), .) %>%
    file.path(dirname(tifs), .)
  file.rename(tifs, new_names)
}
stopCluster(cl)
