library(raster)
library(ggplot2)
library(rasterVis)
library(viridis)

r = raster(file.path(getwd(), "Desktop/modis_mcd12q1_jaguar_2001.tif"))

image(
  r,
  col = c("#00000000", (viridis(16))),
  xaxs = "i",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE,
  asp = 1,
  axes = FALSE,
)
