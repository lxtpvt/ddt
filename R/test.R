library(rpart)
library(rpart.plot)
library(randomForest)
library(pdp)           # for partial dependence plots, includes dataset boston


library(mlbench)
data(BostonHousing)

head(BostonHousing)
str(BostonHousing)

X = BostonHousing[,-14]

X.nrows = dim(X)[1]
X.ncols = dim(X)[2]

rangeContinuous(X)

# f(X) is random forest
boston_rf <- randomForest(medv ~ ., data = BostonHousing, importance = TRUE)
pd <- partial(boston_rf, pred.var = c("crim", "indus"))
rwb <- colorRampPalette(c("blue", "white", "red"))
pdp.colored <- plotPartial(pd, contour = TRUE, col.regions = rwb)
pdp.colored


# tree fitted with the real data
temp <- rpart.control(xval=10, minbucket=2, minsplit=4,  cp=0, maxdepth = 4)
boston_tree <- rpart(medv ~ ., data = BostonHousing, method = "anova", control = temp)
rpart.plot(boston_tree)
