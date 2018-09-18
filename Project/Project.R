data=read.csv("/Users/cyunguo/Desktop/Video_Games_Sales_as_at_22_Dec_2016.csv")
data=na.omit(data)
#summary(data$Global_Sales)
data$User_Score <- as.numeric(data$User_Score)
data$Critic_Score <- as.numeric(data$Critic_Score)

mod1 <- lm(Global_Sales~Critic_Score+User_Score, data = data)
summary(mod1)
mod2 <- lm(Global_Sales~Critic_Score+Critic_Count+User_Score+User_Count, data = data)
summary(mod2)
mod3 <- lm(Global_Sales~Critic_Score+Critic_Count+User_Score+User_Count+Critic_Score:Critic_Count+User_Score:User_Count, data = data)
summary(mod3)

set.seed(12345)
trainid = sample(1:nrow(data), size = trunc(nrow(data) * 0.8))
trainData = data[trainid,]
testData = data[-trainid,]
trainx = model.matrix(Global_Sales ~ . - NA_Sales - EU_Sales - JP_Sales - Other_Sales - Global_Sales - Name-Publisher-Year_of_Release - Developer - Rating, data = trainData)[, -1]
trainy = trainData$Global_Sales
testx = model.matrix(Global_Sales ~ . - NA_Sales - EU_Sales - JP_Sales - Other_Sales - Global_Sales - Name-Publisher-Year_of_Release - Developer - Rating, data = testData)[, -1]
testy = testData$Global_Sales

mse <- function(model, y, data) {
  yhat <- predict(model, data)
  mean((y - yhat)^2) 
}
train_mse = mse(mod2, trainData$Global_Sales, trainData) 
test_mse = mse(mod2, testData$Global_Sales, testData)
train_mse
test_mse

library("FNN")
k_range = c(95,96,97,98,99,100,101,102,103,104) 
trainMSE = c()
for(i in 1:length(k_range)){
  knnTrain <- knn.reg(train = scale(trainData[ ,c(11, 12, 13, 14)]), test = scale(trainData[ ,c(11, 12, 13, 14)]),
                      y = trainData$Global_Sales, k = k_range[i])
  trainMSE[i] <- mean((trainData$Global_Sales - knnTrain$pred)^2)
}
testMSE = c()
for(i in 1:length(k_range)){
  knnTest <- knn.reg(train = scale(trainData[ ,c(11, 12, 13, 14)]), test = scale(testData[ ,c(11, 12, 13, 14)]),
                     y = trainData$Global_Sales, k = k_range[i])
  testMSE[i] <- mean((testData$Global_Sales - knnTest$pred)^2)
}
trainMSE
testMSE

library(glmnet)
cv.out = cv.glmnet(trainx, trainy, alpha = 1)
bestlambda = cv.out$lambda.min
model.lasso = glmnet(trainx, trainy, lambda = bestlambda,alpha = 1)
lasso_train_pred = predict(model.lasso, s = bestlambda, newx = trainx)
lasso_train_mse = mean((lasso_train_pred - trainy) ^ 2)
lasso_test_pred = predict(model.lasso, s = bestlambda, newx = testx)
lasso_test_mse = mean((lasso_test_pred - testy) ^ 2)
bestlambda
lasso_train_mse
lasso_test_mse
lasso_coef = predict(model.lasso, s = bestlambda, type = "coefficient")[1:200,]

grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(trainx,trainy,alpha=0,lambda=grid)
set.seed(1) 
cv.out_ridge=cv.glmnet(trainx,trainy,alpha=0)
bestlam=cv.out_ridge$lambda.min
bestlam
ridge.pred_train=predict(ridge.mod,s=bestlam,newx=trainx)
ridgeTrainMSE=mean((ridge.pred_train-trainy)^2)
ridge.pred_test=predict(ridge.mod,s=bestlam,newx=testx)
ridgeTestMSE=mean((ridge.pred_test-testy)^2)
ridgeTrainMSE
ridgeTestMSE

library(ISLR)
hitPCA <- prcomp(x=trainx, center = T, scale = F)
plot(hitPCA)
#trainx.centered <- apply(trainx, 2, function(x) x - mean(x))
#hitEigen <- eigen(var(trainx.centered))
#cbind("eigen" = hitEigen$vectors[, 1], "prcomp" = hitPCA$rotation[, 1])
#summary(hitPCA)$x[,1]

library(pls)
set.seed(1)
hitPCR <- pcr(Global_Sales ~ Critic_Score+Critic_Count+User_Score+User_Count, data = trainData, subset = trainx, scale = TRUE, validation = "CV")
summary(hitPCR)
validationplot(hitPCR, val.type = "MSEP", legendpos = "topright")
train_x = model.matrix(Global_Sales ~Critic_Score+Critic_Count+User_Score+User_Count , data = trainData)[, -1]
train_y = trainData$Global_Sales
test_x = model.matrix(Global_Sales ~Critic_Score+Critic_Count+User_Score+User_Count, data = testData)[, -1]
test_y = testData$Global_Sales
hitPCR.pred <- predict(hitPCR, test_x, ncomp = 4) 
PCRTestMSE <- mean((hitPCR.pred -test_y)^2)
PCRTestMSE

d <- data.frame("TestMSE" = c(PCRTestMSE, PLSTestMSE, ridgeTestMSE, lasso_test_mse,test_mse, testMSE[6])) 
rownames(d) <- c("PCR", "PLS", "Ridge", "Lasso","Linear Regression","KNN")
knitr::kable(d)

anova(mod3,mod4)

mod9 <- lm(trainData$Global_Sales~hitPCA$x[,1])
