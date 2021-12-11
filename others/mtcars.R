library(ddt)
library(rpart.plot)
library(randomForest)
library(datasets)

setwd("/home/lxt/GitHub/ddt/others")
# the mtcars data set
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
  carb <- factor(carb)
})
summary(mtcars2)
str(mtcars2)


# The black-box model - random forest
mtcars_rf <- randomForest(mpg ~ ., data = mtcars2, importance = TRUE)

# Covariate data matrix
X = mtcars2[,-1]

# Set distillation decision tree (rpart) parameters
ctl <- rpart.control(xval=10, minbucket=5, minsplit=10,  cp=0, maxdepth = 20)
rparas = setRpartPara(method = "anova", control=ctl, predict_type="vector")

#===============================================================================
# 1. Stable sample size
#===============================================================================
# Set sampling strategy for initial sampling
#==========================================
# (1) The initial sampling region is defined by the mtcars data set
sR = setSamplingRegion(X_range = dataRange(X))
#==========================================
# (2.1) Use uniform random sampling strategy
para = setSamplingParameters(null=T)
stg = setSamplingStrategy(samplingMethod="randomSampling",
                             samplingRegion=sR,samplingParameters=para)
# (2.2) Use marginal random sampling (PCA) strategy, X is the data set for PCA,
# 0.7 is proportion of variation interpreted by first n transformed covariates (Z = XW).
pcaPara = setSamplingParameters(null = F, X=X, percentageVariance = 0.7)
stgPca = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                          samplingRegion=sR,samplingParameters=pcaPara)
#==========================================
# (3) Stable sample size
# saving time, skip this
sampleSize_v=seq(0, 500, by = 10)[-1]*dim(X)[1]

simSize = stableSampleSize(fitedModel = mtcars_rf, samplingStrategy=stg, rpartParas=rparas,
                              sampleSize_v=sampleSize_v, criterion="criterionMse")

simSizePca = stableSampleSize(fitedModel = mtcars_rf, samplingStrategy=stgPca,
                              rpartParas=rparas,
                              sampleSize_v=sampleSize_v, criterion="criterionMse")

save(simSize,file = "simSize.RData")
save(simSizePca,file = "simSizePca.RData")

# saving time, run this
load("simSize.RData")
load("simSizePca.RData")

plotStableSize(list(simSizePca,simSize),c("Marginal (PCA) sampling","Random sampling"),
               "MSE","Data: mtcars, Black-box model: Random Forest")

abline(h=0.2, col = "blue")
abline(h=0.3, col = "green")

# fit the tree with real data
temp <- rpart.control(xval=10, minbucket=2, minsplit=4,  cp=0, maxdepth = 10)
tree_car <- rpart(mpg ~ ., data = mtcars2, method = "anova", control = temp)
# testing data
X_te = do.call(stgPca$samplingMethod,
               list(stgPca$samplingRegion,
                    stgPca$samplingParameters, 5000))
y_te = do.call("predict",list(mtcars_rf,X_te))
# predit
pred = predict(object = tree_car, newdata = X_te, type = rparas$predict_type)
# calculate distance
distance_tree_Forest = criterionMse(y_te, pred, rparas$predict_type)

#===============================================================================
# 2. Tree stability initial study
#===============================================================================
# With a distance criterion, for example MSE = 0.2, select a initial stable sample size 8000.
# Set the number of simulations
# saving time, skip this
nSim = 100
simRes <- simStability(nSim = nSim, fitedModel = mtcars_rf,
                      samplingStrategies = list(stgPca), sampleSize = list(8000),
                      rpartParas = rparas)
save(simRes,file = "simRes.RData")

# saving time, run this
load("simRes.RData")

#==========================================
# (1) Analyze the simulation results
stb_list <- stabilityAnalyze(simRes)
save(stb_list,file = "stb_list.RData")


load("stb_list.RData")

#==========================================
# (2) Select the first-class stability
par(mfrow = c(1,1))
first_class_criterion = 0.8
stableSplits(stb_list$node_list,first_class_criterion)->ss
unlist(ss$stableNodes)

stableTrees(ss$stableTrees)
minStableTrees(ss,simRes)->min_stbts
min_stbts$minStableNodes

snipNodes(min_stbts$minStableNodes)->snip_nodes_ids
snip_nodes_ids

minStableTree <- snip.rpart(min_stbts$minStableTrees$trees[[1]], toss = snip_nodes_ids)
rpart.plot(minStableTree,roundint=FALSE)

plotSplitStability(stb_list$node_list,nid = 3, nTop = 2, colnames(dataRange(X)$numeric))
plotSplitStability(stb_list$node_list,nid = 2, nTop = 2, colnames(dataRange(X)$numeric))

nTopUnstableSplits(stb_list$node_list,first_class_criterion,1)

#==========================================
# (3) Add more sampling data in sampling region 2
region1 = setSamplingRegion(X_range = dataRange(X))
nextNid = nTopUnstableSplits(stb_list$node_list,first_class_criterion,1)
region2 = nextSamplingRegion(nextNid, dataRange(X), stb_list, simRes, first_class_criterion)

# set sampling strategies in the two regions
pcaPara1 = setSamplingParameters(null = F, X=X, percentageVariance = 0.7)
pcaPara2 = setSamplingParameters(null = F, X=NULL, percentageVariance = 0.7, nRandomSampling=5000)

stgPca1 = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                             samplingRegion=region1,samplingParameters=pcaPara1)
stgPca2 = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                              samplingRegion=region2,samplingParameters=pcaPara2)
# set sample size in the two regions
sampleSize1 = 8000
sampleSize2 = 2000

nSim = 100
simNextRes <- simStability(nSim = nSim, fitedModel = mtcars_rf,
                       samplingStrategies = list(stgPca1,stgPca2),
                       sampleSize = list(sampleSize1,sampleSize2),
                       rpartParas = rparas)
save(simNextRes,file = "simNextRes.RData")
stb_next_list <- stabilityAnalyze(simNextRes)
save(stb_next_list,file = "stb_next_list.RData")

load(file = "simNextRes.RData")
load(file = "stb_next_list.RData")
par(mfrow = c(1,1))
first_class_criterion = 0.8
stableSplits(stb_next_list$node_list,first_class_criterion)->ss
unlist(ss$stableNodes)

stableTrees(ss$stableTrees)
minStableTrees(ss,simNextRes)->min_stbts
min_stbts$minStableNodes

snipNodes(min_stbts$minStableNodes)->snip_nodes_ids
snip_nodes_ids

minStableTree <- snip.rpart(min_stbts$minStableTrees$trees[[1]], toss = snip_nodes_ids)
rpart.plot(minStableTree,roundint=FALSE)

plotSplitStability(stb_next_list$node_list,nid = 4, nTop = 2, colnames(dataRange(X)$numeric))
dataRange(X)

nTopUnstableSplits(stb_next_list$node_list,first_class_criterion,1)

#==========================================
# (4) Add more sampling data in sampling region 16
region1 = setSamplingRegion(X_range = dataRange(X))
region2 = nextSamplingRegion(2, dataRange(X), stb_next_list, simNextRes, first_class_criterion)
region4 = nextSamplingRegion(4, dataRange(X), stb_next_list, simNextRes, first_class_criterion)

# set sampling strategies in the two regions
pcaPara1 = setSamplingParameters(null = F, X=X, percentageVariance = 0.7)
pcaPara2 = setSamplingParameters(null = F, X=NULL, percentageVariance = 0.7, nRandomSampling=5000)
pcaPara4 = setSamplingParameters(null = F, X=NULL, percentageVariance = 0.7, nRandomSampling=5000)

stgPca1 = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                              samplingRegion=region1,samplingParameters=pcaPara1)
stgPca2 = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                              samplingRegion=region2,samplingParameters=pcaPara2)
stgPca4 = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                              samplingRegion=region4,samplingParameters=pcaPara4)
# set sample size in the two regions
sampleSize1 = 8000
sampleSize2 = 2000
sampleSize4 = 2000

nSim = 100
simNext2Res <- simStability(nSim = nSim, fitedModel = mtcars_rf,
                           samplingStrategies = list(stgPca1,stgPca2,stgPca4),
                           sampleSize = list(sampleSize1,sampleSize2,sampleSize4),
                           rpartParas = rparas)
save(simNext2Res,file = "simNext2Res.RData")



stb_next2_list <- stabilityAnalyze(simNext2Res)
save(stb_next2_list,file = "stb_next2_list.RData")


load(file = "simNext2Res.RData")
load(file = "stb_next2_list.RData")
par(mfrow = c(1,1))
first_class_criterion = 0.8
stableSplits(stb_next2_list$node_list,first_class_criterion)->ss
unlist(ss$stableNodes)

stableTrees(ss$stableTrees)
minStableTrees(ss,simNext2Res)->min_stbts
min_stbts$minStableNodes

snipNodes(min_stbts$minStableNodes)->snip_nodes_ids
snip_nodes_ids

minStableTree <- snip.rpart(min_stbts$minStableTrees$trees[[1]], toss = snip_nodes_ids)
rpart.plot(minStableTree,roundint=FALSE)

plotSplitStability(stb_next2_list$node_list,nid = 1, nTop = 2, colnames(dataRange(X)$numeric))
dataRange(X)

nTopUnstableSplits(stb_next2_list$node_list,first_class_criterion,1)







