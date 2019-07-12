library(wdpar)
library(dplyr)
library(rgdal)
library(raster)
library(cdlTools)


# download all protexted areas
# (excluding areas represented as point localities)
wdpa_data = wdpa_fetch("global")
wdpa_data = download.file("https://protectedplanet.net/downloads/WDPA_Jul2019?type=shapefile", destfile = "~/Desktop/wdpa.shp")

#----*State Contour----
states.full = c("Massachusetts")
us = getData('GADM', country = 'US', level = 1)
st.contour = us[us$NAME_1 %in% states.full,]
st.contour = spTransform(st.contour, CRS("+init=epsg:4326"))
st.contour = aggregate(st.contour) # remove boundaries between states

#----*Extent points---
ur = c(bbox(st.contour)[2,2], bbox(st.contour)[1,2])
ll = c(bbox(st.contour)[2,1], bbox(st.contour)[1,1])
ul = c(ur[1],ll[2])
lr = c(ll[1],ur[2])

extent.long = c(ur[2],ll[2],ul[2],lr[2])
extent.lat = c(ur[1],ll[1],ul[1],lr[1])
extent.df = data.frame(cbind(extent.long, extent.lat))

#----*Spatial points object with embedded extent----
coordinates(extent.df) = ~extent.long+extent.lat
study.extent.corners = SpatialPoints(extent.df, CRS("+init=epsg:4326"))

#---Cropscape----
CropScape = getCDL("MA", 2017)

#----Rivers----
na = shapefile("/Users/gatesdupont/Downloads/na_riv_15s/na_riv_15s.shp") # could use readOGR
ca = shapefile("/Users/gatesdupont/Downloads/ca_riv_15s/ca_riv_15s.shp")
sa = shapefile("/Users/gatesdupont/Downloads/sa_riv_15s/sa_riv_15s.shp")

na_ma = crop(na, study.extent.corners)
na_ma = spTransform(na_ma, crs(CropScape$MA2017))

r = raster(na_ma, res=30)

library(rgeos)
dd = gDistance(na_ma, as(r,"SpatialPoints"), byid=TRUE)
r[] = apply(dd,1,min)
plot(r)

# Combining shapefiles?
"UNION"
date()
rivs_u = na %>%
  raster::union(., ca) %>%
  raster::union(., sa)
date()

"BIND"
date(); rivs_b = bind(na, ca, sa); date()
