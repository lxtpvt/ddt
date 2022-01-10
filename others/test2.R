library(datasets)
library(randomForest)
library(rpart.plot)
library(foreach)
library(doParallel)
#=========================================================================================================
# run simulation
#=========================================================================================================
# The dataset
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
  carb <- factor(carb)
})
# covariates data matrix
X=mtcars2[,-1]
# Data range
sR = setSamplingRegion(X_range = dataRange(X))
covNames = sR$data_range$names

#===========================================
# number of simulations
nSim=3

#===========================================
# set rpart parameters for fitting stump
ctl <- rpart.control(xval=10, minbucket=5, minsplit=10,  cp=0, maxdepth = 1)
rpartParas = setRpartPara(method = "anova", control=ctl, predict_type="vector")

#===========================================
# Black-box model, random forest
mtcars_rf <- randomForest(mpg ~ ., data = mtcars2, importance = TRUE)

#===========================================
# set sampling parameters

# root
pcaPara_root = setSamplingParameters(null = F, X=X, percentageVariance = 0.7)
# other nodes
pcaPara = setSamplingParameters(null = F, X=NULL, percentageVariance = 0.7, nRandomSampling = 50)

#===========================================
# run for first split

# set root sampling strategies

stg = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                             samplingRegion=sR, samplingParameters=pcaPara)
n_X = dim(X)[1]
samplingStrategies = list()
sampleSize = list()
n = 1
for (i in 1:n) {
  samplingStrategies = append(samplingStrategies, list(stg))
  sampleSize = append(sampleSize, list(i*5*n_X))
}

#===============================================================================

a=inductionByLevel(level=3, data=X, samplingMethod=marginalRandomSampling, samplingParameters=pcaPara_root,
                   stumpFun=stumps, nSim, fitedModel=mtcars_rf, sampleSize, rpartParas)














