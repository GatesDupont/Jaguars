library(raster)
library(virtualspecies)
library(xgboost)
library(dismo)
library(randomForest)
library(dplyr)
library(purrr)
library(mlr)
predict = raster::predict

#----Environmental covariate data----
wc = getData("worldclim", var = "bio", res = 10) %>%
  .[[c("bio1", "bio12")]]

wc = cbind(coordinates(wc), values(wc)) %>%
  SpatialPointsDataFrame(.[,c("x","y")], proj4string = crs(wc))


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

pa.pts.sp = pa.pts %>%
  .["sample.points"] %>%
  as.data.frame() %>%
  .[,c(1,2)] %>%
  select(x = 1, y=2) %>%
  SpatialPoints(., proj4string = crs(wc))

#----Extract enviornmental covariates----
lc = raster::extract(wc, pa.pts.sp, sp = T) %>%
  as.data.frame()


#----Assosciate covariates----
df = cbind(lc[,3:4], occupied = as.factor(pa.pts$sample.points[,c(4)]), lc[,1:2])


#----Split train/test----
# Random 60/40 split
samp = sample(nrow(df), round(0.6 * nrow(df)))
df$data = "train"
df[-samp,"data"] = "test"

train = df[df$data == "train", names(df) != "data"]
test = df[df$data == "test", names(df) != "data"]


#----Running the model----
sdm = randomForest(as.numeric(occupied)~bio1+bio12+x+y, train)
evaluate(test[test$occupied==1,], test[test$occupied==0,], sdm)


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


#----XGBoost in MLR----
trainTask <- makeClassifTask(data = train, target = "occupied", positive = 1)
testTask <- makeClassifTask(data = test, target = "occupied")

wcTask = makeClassifTask(data = as.data.frame(values(wc)), target="occupied")


set.seed(1)
# Create an xgboost learner that is classification-based and outputs labels (as opposed to probabilities)
xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "response",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 200
  )
)

binom_learner  = makeLearner(
  cl = "classif.binomial",
  link = "logit",
  predict.type = "prob",
  fix.factors.prediction = TRUE
)

# Create a model
xgb_model <- train(xgb_learner, task = trainTask)
binom_model = train(binom_learner, task = trainTask)


result <- predict(binom_model, testTask)



