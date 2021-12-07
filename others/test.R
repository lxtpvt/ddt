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
#treeInfo(boston_tree)->info
getRids(96)
treeInfo(boston_tree)
samplingRegion(96,dataRange(X),treeInfo(boston_tree))


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
# try pcaSampling
X.continuous = mtcars2[,3:7]
(dataRange(X.continuous))$numeric -> spR

pcaSampling(X.continuous,2,10000,spR)->pcaS

car_rf <- randomForest(mpg ~ ., data = mtcars2, importance = TRUE)
pd <- partial(car_rf, pred.var = c("wt", "cyl"))
rwb <- colorRampPalette(c("blue", "white", "red"))
pdp.colored <- plotPartial(pd, contour = TRUE, col.regions = rwb)
pdp.colored

#=====================
# rpart study

splitTable(treeInfo(tree_car))

printcp(tree_car)

plotcp(tree_car)


f <- function(x) print(x^2)
A <- c(2,3,4)
ff = "pcaSampling"
do.call(ff, list(X.continuous,2,10000,spR))

#==================================
# try tree stability -- mtcars
library(datasets)
head(mtcars)
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
  carb <- factor(carb)
})
# Random forest
mtcars_rf <- randomForest(mpg ~ ., data = mtcars2, importance = TRUE)
# pd <- partial(mtcars_rf, pred.var = c("cyl", "hp"))
# rwb <- colorRampPalette(c("blue", "white", "red"))
# pdp.colored <- plotPartial(pd, contour = TRUE, col.regions = rwb)
# pdp.colored


nSim = 100
X = mtcars2[,-1]

stg = setSamplingStrategy(method="randomSampling",samplingRegion=dataRange(X),para=NULL)


ctl <- rpart.control(xval=10, minbucket=2, minsplit=4,  cp=0, maxdepth = 10)
rparas = setRpartPara(method = "anova", control=ctl, predict_type="vector")


simRes <- simStability(nSim = nSim, fitedModel = mtcars_rf,
             samplingStrategy = stg, sampleSize = 1000, rpartParas = rparas)




















