library(randomForest)
library(rpart.plot)
setwd("/home/lxt/GitHub/ddt/others")
library(datasets)
head(mtcars)
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
  carb <- factor(carb)
})
summary(mtcars2)
str(mtcars2)
X=mtcars2[,-1]
n_X = dim(X)[1]
sampleSize = list()
n = 5
for (i in 1:5) {
  #samplingStrategies = append(samplingStrategies, list(stgPca))
  sampleSize = append(sampleSize, list(i*100*n_X))
}
nSim=100
simSize = rep(nSim,n)
sR = setSamplingRegion(X_range = dataRange(X))
load(file = "stumpsRes.RData")

simResMatStump = stumpsToMat(stumpsRes)
pmf_mat = firstClassStabilityStump(sR$data_range$names, simResMatStump)
par(mfrow = c(3,2))
plotFCstability(pmf_mat, 1)
plotSCstability("disp", T, simResMatStump, simSize, sampleSize)

#=====================
secondClassStabilityStump = secondClassStabilityStump(nameCov="disp", isNumeric=T, simResMatStump)
info_1 = createNodeInfo(nid=1, parentNodeInfo=NULL, secondClassStabilityStump, dataRange=sR)
samplingRegion(2,dataRange(X),info_1$pseudoTreeInfo)->sR_2
pcaPara = setSamplingParameters(null = F, X=NULL, percentageVariance = 0.7, nRandomSampling = 5000)
stgPca = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                             samplingRegion=sR_2,samplingParameters=pcaPara)
#=====================
n_X = dim(X)[1]
samplingStrategies = list()
sampleSize = list()

for (i in c(1,3,5)) {
  #samplingStrategies = append(samplingStrategies, list(stgPca))
  sampleSize = append(sampleSize, list(i*100*n_X))
}
nSim=100
simSize = rep(nSim,3)

#=====================
mtcars_rf <- randomForest(mpg ~ ., data = mtcars2, importance = TRUE)
ctl <- rpart.control(xval=10, minbucket=5, minsplit=10,  cp=0, maxdepth = 1)
rpartParas = setRpartPara(method = "anova", control=ctl, predict_type="vector")
stumpsRes2 = stumps(nSim, fitedModel=mtcars_rf, samplingStrategies, sampleSize, rpartParas)
save(stumpsRes2, file = "stumpsRes2.RData")
#=====================
#=====================
load(file = "stumpsRes2.RData")
simResMatStump = stumpsToMat(stumpsRes2)
pmf_mat = firstClassStabilityStump(sR$data_range$names, simResMatStump)
par(mfrow = c(2,2))
plotFCstability(pmf_mat, 2)
plotSCstability("wt", T, simResMatStump, simSize, sampleSize)
#===========================================================================================

samplingRegion(3,dataRange(X),info_1$pseudoTreeInfo)->sR_3

pcaPara = setSamplingParameters(null = F, X=NULL, percentageVariance = 0.7, nRandomSampling = 5000)
stgPca = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                             samplingRegion=sR_3,samplingParameters=pcaPara)

n_X = dim(X)[1]
samplingStrategies = list()
sampleSize = list()

for (i in c(1)) {
  samplingStrategies = append(samplingStrategies, list(stgPca))
  sampleSize = append(sampleSize, list(i*100*n_X))
}
nSim=100

mtcars_rf <- randomForest(mpg ~ ., data = mtcars2, importance = TRUE)
ctl <- rpart.control(xval=10, minbucket=5, minsplit=10,  cp=0, maxdepth = 1)
rpartParas = setRpartPara(method = "anova", control=ctl, predict_type="vector")

stumpsRes3 = stumps(nSim, fitedModel=mtcars_rf, samplingStrategies, sampleSize, rpartParas)

save(stumpsRes3, file = "stumpsRes3.RData")

#=====================
n_X = dim(X)[1]
samplingStrategies = list()
sampleSize = list()

for (i in c(1)) {
  samplingStrategies = append(samplingStrategies, list(stgPca))
  sampleSize = append(sampleSize, list(i*100*n_X))
}
nSim=100
simSize=nSim

load(file = "stumpsRes3.RData")
simResMatStump = stumpsToMat(stumpsRes3)
pmf_mat = firstClassStabilityStump(sR$data_range$names, simResMatStump)
par(mfrow = c(2,2))
plotFCstability(pmf_mat, 3)
plotSCstability("wt", T, simResMatStump, simSize, sampleSize)




