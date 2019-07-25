library(raster)
library(virtualspecies)

worldclim = getData("worldclim", var = "bio", res = 10)

# Generate response functions
my.parameters = formatFunctions(bio1 = c(fun = 'dnorm', mean = 250, sd = 50),
                                 bio12 = c(fun = 'dnorm', mean = 4000, sd = 2000))

# Generate virtual species
my.first.species = generateSpFromFun(raster.stack = worldclim[[c("bio1", "bio12")]],
                                      parameters = my.parameters,
                                      plot = TRUE)

pa = convertToPA(my.first.species, species.prevalence = 0.05, plot = TRUE)

# Sampling of distribution
pa.points = sampleOccurrences(
  my.first.species$suitab.raster,
  n = 10,
  type = "presence-absence"
)
