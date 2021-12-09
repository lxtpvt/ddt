#library(rpart)
library(rpart.plot)
library(randomForest)
library(pdp)           # for partial dependence plots, includes dataset boston

#===============================================================================
# dataset -- BostonHousing
library(mlbench)
data(BostonHousing)

head(BostonHousing)
str(BostonHousing)

#X = BostonHousing[,4]
X = BostonHousing[,-c(14)]

X.nrows = dim(X)[1]
X.ncols = dim(X)[2]

dataRange(as.data.frame(X))->dr

# f(X) is random forest
boston_rf <- randomForest(medv ~ ., data = BostonHousing, importance = TRUE)
pd <- partial(boston_rf, pred.var = c("crim", "indus"))
rwb <- colorRampPalette(c("blue", "white", "red"))
pdp.colored <- plotPartial(pd, contour = TRUE, col.regions = rwb)
pdp.colored


# tree fitted with the real data
temp <- rpart.control(xval=10, minbucket=2, minsplit=4,  cp=0, maxdepth = 10)
boston_tree <- rpart(medv ~ ., data = BostonHousing, method = "anova", control = temp)
rpart.plot(boston_tree)

boston_rf <- randomForest(medv ~ ., data = BostonHousing, importance = TRUE)
#treeInfo(boston_tree)->info
getRids(96)
treeInfo(boston_tree)
samplingRegion(96,dataRange(X),treeInfo(boston_tree))



nSim = 100

stg = setSamplingStrategy(method="randomSampling",samplingRegion=dataRange(X),para=NULL)


ctl <- rpart.control(xval=10, minbucket=2, minsplit=4,  cp=0, maxdepth = 10)
rparas = setRpartPara(method = "anova", control=ctl, predict_type="vector")


simRes <- simStability(nSim = nSim, fitedModel = boston_rf,
                       samplingStrategy = stg, sampleSize = 1000, rpartParas = rparas)


node_list <- stabilityAnalyze(simRes)


#===============================================================================
# dataset -- iris
library(datasets)
head(iris)
X=iris[,-4]
dataRange(X)
temp <- rpart.control(xval=10, minbucket=2, minsplit=4,  cp=0, maxdepth = 10)
tree_iris <- rpart(Petal.Width ~ Species+Petal.Length, data = iris,
                   method = "anova", control = temp)
rpart.plot(tree_iris)
treeInfo(tree_iris)
samplingRegion(100,dataRange(X),treeInfo(tree_iris))


#===============================================================================
# dataset -- mtcars
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

temp <- rpart.control(xval=10, minbucket=2, minsplit=4,  cp=0, maxdepth = 10)
tree_car <- rpart(mpg ~ ., data = mtcars2, method = "anova", control = temp)
rpart.plot(tree_car)

#=====================
# test tree functions
splitTable(treeInfo(tree_car))

X=mtcars2[,-1]
id = 77
digits=5
minlength=0L

getRids(id)
dataRange(X)
treeInfo(tree_car)
rangeConditions(id,treeInfo(tree_car))
samplingRegion(id,dataRange(X),treeInfo(tree_car))

#=====================
X=mtcars2[,-1]
sR = setSamplingRegion(dataRange(X))
# try random sampling
randomSampling(sR, n=100)
# try pca sampling
pcaSamplingSizeOne(X, sR, percentageVariance=0.7)
para = setSamplingParameters(F,X,0.7)
marginalRandomSampling(samplingRegion = sR, samplingParameters = para, n = 100)

#==================================
# try tree stability -- mtcars
# Random forest

mtcars_rf <- randomForest(mpg ~ ., data = mtcars2, importance = TRUE)
X = mtcars2[,-1]
sR = setSamplingRegion(dataRange(X))
para = setSamplingParameters(F,X,0.7)

stg = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                          samplingRegion=sR,samplingParameters=para)

ctl <- rpart.control(xval=20, minbucket=5, minsplit=10,  cp=0, maxdepth = 10)
rparas = setRpartPara(method = "anova", control=ctl, predict_type="vector")

# stable sample size
sampleSize_v=seq(0, 300, by = 10)[-1]*dim(X)[1]

a = stableSampleSize(fitedModel = mtcars_rf, samplingStrategy=stg, rpartParas=rparas,
                     sampleSize_v=sampleSize_v, criterion="criterionMse")
plotStableSize(a)
abline(h=0.3)

# structure stability, 3000 looks ok
nSim = 100
simRes <- simStability(nSim = nSim, fitedModel = mtcars_rf,
                       samplingStrategy = stg, sampleSize = 3000, rpartParas = rparas)

node_list <- stabilityAnalyze(simRes)

plotSplitStability(node_list,nid = 5, nTop = 3,c("disp","hp","drat","wt","qsec"))


#=====================
# rpart optimization

splitTable(treeInfo(tree_car))

printcp(tree_car)

plotcp(tree_car)

# find best cp






