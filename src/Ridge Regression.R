clean_data <- read.csv("C:/Users/DELL/Downloads/clean_data.csv")
install.packages("MASS")
library(MASS)
fit.ridge<-lm.ridge(X~. , data = clean_data, lambda=seq(0,100,10))
plot(fit.ridge)
install.packages("glmnet")
install.packages("caret")
install.packages("debugme")
library(debugme)
library(caret)
library(glmnet)
colnames(clean_data)
clean_data <- na.omit(clean_data)
set.seed(100)
ind = sample(1,nrow(clean_data),0.8*nrow(clean_data))
train = clean_data[ind,]
test = clean_data[-ind,]
dim(train)
dim(test)
cols = c("X","Id","MSSubClass",   "MSZoning"  ,    "LotFrontage"  , "LotArea"  ,     "Street"    ,   
         "Alley"     ,  "LotShape"  ,   "LandContour" , "Utilities"  , "LotConfig" ,  "LandSlope" ,   "Neighborhood" ,
          "Condition1"  ,  "Condition2"  ,  "BldgType"   ,   "HouseStyle"  ,  "OverallQual" ,  "OverallCond" ,  "YearBuilt" ,   
          "YearRemodAdd" , "RoofStyle"  ,   "RoofMatl"  ,    "Exterior1st",   "Exterior2nd" ,  "MasVnrType" ,   "MasVnrArea" ,  
          "ExterQual"   ,  "ExterCond"  ,   "Foundation",    "BsmtQual"  ,    "BsmtCond"  ,    "BsmtExposure",  "BsmtFinType1", 
          "BsmtFinSF1"  ,  "BsmtFinType2",  "BsmtFinSF2" ,   "BsmtUnfSF" ,    "TotalBsmtSF" ,  "Heating" ,      "HeatingQC" ,   
          "CentralAir"  ,  "Electrical"  ,  "X1stFlrSF"  ,   "X2ndFlrSF" ,    "LowQualFinSF",  "GrLivArea" ,    "BsmtFullBath", 
          "BsmtHalfBath" , "FullBath"   ,   "HalfBath"   ,   "BedroomAbvGr" , "KitchenAbvGr",  "KitchenQual" ,  "TotRmsAbvGrd", 
          "Functional"  ,  "Fireplaces" ,   "FireplaceQu" ,  "GarageType" ,   "GarageYrBlt",   "GarageFinish",  "GarageCars",   
          "GarageArea"  ,  "GarageQual" ,   "GarageCond" ,   "PavedDrive" ,   "WoodDeckSF" ,   "OpenPorchSF" ,  "EnclosedPorch",
          "X3SsnPorch"  ,  "ScreenPorch" ,  "PoolArea"  ,    "PoolQC"  ,      "Fence"   ,      "MiscFeature" ,  "MiscVal" ,     
          "MoSold"      ,  "YrSold" ,     "SaleType"   ,   "SaleCondition", "SalePrice")
dummies <- dummyVars(X ~ ., data = clean_data[,cols])
train_dummies = predict(dummies, newdata = train[,cols])
debug_contr_error(clean_data)
test_dummies = predict(dummies, newdata = test[,cols])
print(dim(train_dummies))

custom<- trainControl(method = "repeatedcv",number = 10,repeats = 5,verboseIter = T)
set.seed(1234)
ridge <- train(X~., train, method = 'glmnet' , tuneGrid = expand.grid(alpha=0 , lambda =seq(0.01, 1, length=5))
               ,trControl =custom)
plot(ridge)




x = clean_data[,!names(clean_data) %in% "SalePrice"]
x
y = clean_data$X
grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)
summary(ridge_mod)
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda =grid )
best_lambda <- ridge_cv$lambda.min
best_fit <- ridge_cv$glmnet.fit
best_ridge <- glmnet(x, y, alpha = 0, lambda = 0.01)
coef(best_ridge)
index <- createDataPartition(clean_data$X, p = .80, list = FALSE)
train <- clean_data[index, ]
test <- clean_data[-index, ]
pred <- predict(best_ridge, s = best_lambda, newx = test)