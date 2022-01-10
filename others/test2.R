library(datasets)
library(randomForest)
library(rpart.plot)
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
nSim=1

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
stgPca_root = setSamplingStrategy(samplingMethod="marginalRandomSampling",
                             samplingRegion=sR,samplingParameters=pcaPara_root)
n_X = dim(X)[1]
samplingStrategies = list()
sampleSize = list()
n = 1
for (i in 1:n) {
  samplingStrategies = append(samplingStrategies, list(stgPca_root))
  sampleSize = append(sampleSize, list(i*100*n_X))
}

# run simulation
stumpsRes_root=stumps(nSim, fitedModel=mtcars_rf, samplingStrategies, sampleSize, rpartParas)
# analysis simulation results and store it.
stumps_mat_root = stumpsToMat(stumpsRes_root$stump_list)
secStb = secondClassStabilityStump(nameCov="disp",
                                   isNumeric=T, stumps_mat_root)
info_root = createNodeInfo(nid=1, parentNodeInfo=NULL, secStb, sR$data_range)

samplingRegion(2*1,sR$data_range,info_root$pseudoTreeInfo)->sR_l
samplingRegion((2*1+1), sR$data_range,info_root$pseudoTreeInfo)->sR_r

# test random sampling

randomSampling(sR_l,n=50000)->rs_l
randomSampling(sR_r,n=50000)->rs_r

y_l = predict(mtcars_rf,rs_l)
y_r = predict(mtcars_rf,rs_r)

df_l = data.frame(rs_l,y_l)
df_r = data.frame(rs_r,y_r)

stump_l = rpart(y_l~., data = df_l, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_l)

stump_r = rpart(y_r~., data = df_r, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_r)

#============================================
marginalRandomSampling(sR_l, pcaPara, 5000) -> mrs_l
marginalRandomSampling(sR_r, pcaPara, 5000) -> mrs_r

y_l = predict(mtcars_rf,mrs_l)
y_r = predict(mtcars_rf,mrs_r)

df_l = data.frame(mrs_l,y_l)
df_r = data.frame(mrs_r,y_r)

stump_l = rpart(y_l~., data = df_l, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_l)

stump_r = rpart(y_r~., data = df_r, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_r)

#============================================

sR_l_l = sR_l
sR_l_l$id_range$numeric[,"wt"][1]=2.310
sR_l_r = sR_l
sR_l_r$id_range$numeric[,"wt"][2]=2.310

# test random sampling

randomSampling(sR_l_l,n=100000)->rs_l
randomSampling(sR_l_r,n=100000)->rs_r

y_l = predict(mtcars_rf,rs_l)
y_r = predict(mtcars_rf,rs_r)

df_l = data.frame(rs_l,y_l)
df_r = data.frame(rs_r,y_r)

stump_l = rpart(y_l~., data = df_l, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_l)

stump_r = rpart(y_r~., data = df_r, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_r)

#============================================
marginalRandomSampling(sR_l_l, pcaPara, 50000) -> mrs_l
marginalRandomSampling(sR_l_r, pcaPara, 50000) -> mrs_r

y_l = predict(mtcars_rf,mrs_l)
y_r = predict(mtcars_rf,mrs_r)

df_l = data.frame(mrs_l,y_l)
df_r = data.frame(mrs_r,y_r)

stump_l = rpart(y_l~., data = df_l, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_l)

stump_r = rpart(y_r~., data = df_r, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_r)


# 2.304

sR_r_l = sR_r
sR_r_l$id_range$numeric[,"wt"][1]=2.304
sR_r_r = sR_r
sR_r_r$id_range$numeric[,"wt"][2]=2.304


randomSampling(sR_r_l,n=100000)->rs_l
randomSampling(sR_r_r,n=100000)->rs_r

y_l = predict(mtcars_rf,rs_l)
y_r = predict(mtcars_rf,rs_r)

df_l = data.frame(rs_l,y_l)
df_r = data.frame(rs_r,y_r)

stump_l = rpart(y_l~., data = df_l, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_l)

stump_r = rpart(y_r~., data = df_r, method = rpartParas$method, control = rpartParas$control)
rpart.plot(stump_r)
















