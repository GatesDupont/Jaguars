library(raster)
library(virtualspecies)
library(xgboost)
library(dismo)
library(randomForest)
library(dplyr)
library(purrr)
predict = raster::predict

#----Environmental covariate data----
worldclim = getData("worldclim", var = "bio", res = 10)

wc = worldclim[[c("bio1", "bio12")]]


#----Response functions----
params = formatFunctions(
  bio1 = c(fun = 'dnorm', mean = 250, sd = 50),
  bio12 = c(fun = 'dnorm', mean = 4000, sd = 2000)
)


#----Virtual species----
vsp = generateSpFromFun(
  raster.stack = wc,
  parameters = params,
  plot = TRUE
)


#----Presence-absence distribution----
pa = convertToPA(vsp, species.prevalence = 0.05, plot = TRUE)

#----Presence-absence sampling----
pa.pts = sampleOccurrences(
  pa,
  n = 250,
  sample.prevalence = 0.5,
  type = "presence-absence"
)


#----Extract enviornmental covariates----
lc = extract(wc, pa.pts$sample.points[,c(1,2)], df=T)


#----Assosciate covariates----
df = cbind(Observed = pa.pts$sample.points[,c(4)], lc[,c(2,3)])


#----Split train/test----
# Random 60/40 split
samp = sample(nrow(df), round(0.6 * nrow(df)))
train = (df[samp,])
test = (df[-samp,])


#----Running the model----
sdm = randomForest(Observed~bio1+bio12, train)
evaluate(test[test$Observed==1,], test[test$Observed==0,], sdm)


#----Predicting from raster stack----
sdm_pred = predict(wc, sdm)


#----Plotting true distribution and modeled----
sdm.plot = function(raster, title){
  image(raster, main = title,
        col = rev(terrain.colors(100)),
        xaxs = "i", xaxt = 'n', yaxt = 'n', ylab = "", xlab = "",
        ann = T, asp = 1, axes = FALSE)
}


par(mfrow=c(1,2))
sdm.plot(vsp$suitab.raster, title = "True")
sdm.plot(sdm_pred, title = "Modeled")
par(mfrow=c(1,1))
