library(raster)

wd = setwd("C:\\Users\\MTobler\\Documents\\Jaguar\\data\\GIS\\modis\\tiles_complete")

ls = list.files()

r_modis = vector("list", 16)
for(j in 2001:2016){
  rl = vector("list", 42)
  for(i in 1:42){
    p2r = file.path(wd, ls[[i]])
    
    rl[[i]] = p2r %>%
      list.files() %>%
      pluck(grep(as.character(j), .)) %>%
      file.path(p2r, .) %>%
      raster(.)
  }
  
  
  # create string of raster names
  rl.names = "rl[[1]]"
  for(i in 2:42){
    rl.names = paste0(rl.names, ", ", "rl[[", as.character(i), "]]")
  }
  
  # Mosaicking tiles together
  call4mosaic = paste0("mosaic(", rl.names, 
                       ", fun=mean, filename='C:/Users/MTobler/Documents/Jaguar/data/GIS/modis/tiles_complete/modis_mcd12q1_", j, ".tif ', overwrite=TRUE)")
  r_modis[[j]] = eval(parse(text = call4mosaic))
}

# missed V12 H11
