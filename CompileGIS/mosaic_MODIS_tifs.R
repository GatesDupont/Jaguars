library(raster)

wd = setwd("C:\\Users\\MTobler\\Documents\\Jaguar\\data\\GIS\\modis\\tiles_complete")

ls = list.files()

years = 2001:2016
r_modis = vector("list", 16)
for(j in 1:length(years)){
  rl = vector("list", 43)
  for(i in 1:43){
    p2r = file.path(wd, ls[i])
    
    rl[[i]] = p2r %>%
      list.files() %>%
      pluck(grep(as.character(years[j]), .)) %>%
      file.path(p2r, .) %>%
      raster(.)
  }
  
  
  # create string of raster names
  rl.names = "rl[[1]]"
  for(i in 2:43){
    rl.names = paste0(rl.names, ", ", "rl[[", as.character(i), "]]")
  }
  
  # Mosaicking tiles together
  call4mosaic = paste0("mosaic(", rl.names, 
                       ", fun=mean)")
  r_modis[[j]] = eval(parse(text = call4mosaic))
  writeRaster(x = r_modis[[j]], filename = file.path(wd, paste0("modis_mcd12q1_jaguar_", years[j], ".tif")))
}








rf = vector("list", 16)
for(i in 1:length(years)){
  rf[[i]] = raster(file.path(wd, paste0("modis_mcd12q1_jaguar_", years[i], ".tif")))
}


plot(rf[[1]])
plot(rf[[2]])
plot(rf[[3]])


test = overlay(rf[[1]], rf[[16]], fun=function(r1,r2){return(r2-r1)})
