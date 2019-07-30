library(raster)
library(virtualspecies)
library(xgboost)
library(dismo)
library(randomForest)
library(dplyr)
library(tidyr)
library(purrr)
library(mlr)
library(sf)
predict = stats::predict
extract = raster::extract
select = dplyr::select

# making a function for coordinates() w/in a pipe
coordinates_iP = function(df, crs_in){
  coordinates(df) = ~x+y
  sp::proj4string(df) = crs_in
  return(df)
}

#----Environmental covariate data----
worldclim = getData("worldclim", var = "bio", res = 10) %>%
  .[[c("bio1", "bio12")]]

wc = cbind(coordinates(worldclim), values(worldclim)) %>%
  as.data.frame() %>%
  coordinates_iP(df = ., crs_in = CRS(st_crs(4326)$proj4string))


#----Response functions----
params = formatFunctions(
  bio1 = c(fun = 'dnorm', mean = 250, sd = 50),
  bio12 = c(fun = 'dnorm', mean = 4000, sd = 2000)
)


#----Virtual species----
vsp = generateSpFromFun(
  raster.stack = worldclim,
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

pa.pts.sp = pa.pts %>%
  .["sample.points"] %>%
  as.data.frame() %>%
  .[,c(1,2)] %>%
  select(x = 1, y=2) %>%
  SpatialPoints(., proj4string = crs(wc))

#----Extract enviornmental covariates----
lc = raster::extract(worldclim, pa.pts.sp@coords, sp = T) %>%
  as.data.frame() %>%
  mutate(x = pa.pts.sp@coords[,1]) %>%
  mutate(y = pa.pts.sp@coords[,2] ) %>% 
  select(3, 4, 1, 2)


#----Assosciate covariates----
df = cbind(lc[,c(1,2)], occupied = as.factor(pa.pts$sample.points[,c(4)]), lc[,c(3,4)])


#----Split train/test----
# Random 60/40 split
samp = sample(nrow(df), round(0.6 * nrow(df)))
df$data = "train"
df[-samp,"data"] = "test"

train = df[df$data == "train", names(df) != "data"]
test = df[df$data == "test", names(df) != "data"]


#----XGBoost in MLR----
trainTask = makeClassifTask(data = train, target = "occupied", positive = 1)
testTask = makeClassifTask(data = test, target = "occupied")

set.seed(1)
# Create an xgboost learner that is classification-based and outputs labels (as opposed to probabilities)
xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "prob",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 200
  )
)

# Create a model
xgb_model = train(xgb_learner, task = trainTask)

library(mgcv)
xgb_model = gam(occupied ~ s(x,y) + s(bio1) + s(bio12), data = train, family = binomial(link = "logit"))

# Global prediction
newdata.xgb = as.data.frame(wc)
result = stats::predict(xgb_model, newdata = newdata.xgb)

# convert prediction to raster
xgb.pred.raster = worldclim$bio1
values(xgb.pred.raster) = as.numeric(result)
values(xgb.pred.raster)[is.na(values(worldclim$bio1))] = NA


#-----Plotting the model----
plot(xgb.pred.raster)


#----Compare similarlities----
v_true = na.omit(as.numeric(values(pa$pa.raster)))
v_mod = na.omit(as.numeric(values(xgb.pred.raster)))

vtab = table(v_true, v_mod)
(vgood = (vtab[1,1] + vtab[2,2])/sum(vtab))


#----AUC score----
pred1 = predict(xgb_model, testTask)
perfMes = generateThreshVsPerfData(pred1, measures = list(fpr, tpr, mmce))
plotROCCurves(perfMes)

mlr::performance(pred1, mlr::auc)


#----Predicting conditional response curves----
wcb.range = function(worldclim_raster_layer){
  result = worldclim_raster_layer %>%
    values() %>%
    na.omit() %>%
    range()
  return(result)
}

wcb1.range = wcb.range(worldclim$bio1)
wcb12.range = wcb.range(worldclim$bio12)

newdata.wcb1 = data.frame(
  x = mean(train$x),
  y = mean(train$y),
  bio1 = seq(wcb1.range[1], wcb1.range[2], by=1),
  bio12 = vsp$details$parameters$bio12$max
)

newdata.wcb12 = data.frame(
  x = mean(train$x),
  y = mean(train$y),
  bio1 = vsp$details$parameters$bio1$max,
  bio12 = seq(wcb12.range[1], wcb12.range[2], by=1)
)

wcb1.rc = stats::predict(xgb_model, newdata = newdata.wcb1)
wcb12.rc = stats::predict(xgb_model, newdata = newdata.wcb12)

par(mfrow=c(2,2))
plot(dnorm(x=c(-269:314), mean = 250, sd = 50), type="l", ylab = "Probability of Presence", xlab = "Bio1")
plot(wcb1.rc$data$prob.1~c(-269:314), type="l", ylab = "Probability of Presence", xlab = "Bio1")

plot(dnorm(x=c(0:9916), mean = 4000, sd = 2000), type="l", ylab = "Probability of Presence", xlab = "Bio12")
plot(wcb12.rc$data$prob.1~c(0:9916), type="l", ylab = "Probability of Presence", xlab = "Bio12")

