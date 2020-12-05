train_dummy <- read.table("../data/processed/train_data_with_dummy.csv", sep = ",", header = T)
head(train_dummy)
test_dummy <- read.table("../data/processed/test_data_with_dummy.csv", sep = ",", header = T)
head(test_dummy)

custom<- trainControl(method = "repeatedcv",number = 10,repeats = 5,verboseIter = T)
set.seed(1234)
ridge <- train(y=train_dummy$SalePrice, x=train_dummy[,!names(train_dummy) %in% "SalePrice"], method = 'glmnet' , tuneGrid = expand.grid(alpha=0 , lambda =seq(0.001, 0.1, by=0.005))
               ,trControl =custom)
ridge$bestTune
min(ridge$results$RMSE)
a <- predict(ridge,test_dummy[,!names(test_dummy) %in% "SalePrice"])
exp(a)
rmse_rf <- sqrt(mean((exp(test_dummy$SalePrice) -exp(a))^2))
rmse_rf

rss <- sum((exp(a) - exp(test_dummy$SalePrice)) ^ 2)  ## residual sum of squares
tss <- sum((exp(test_dummy$SalePrice) - mean(exp(test_dummy$SalePrice))) ^ 2)  ## total sum of squares
rsq_rf <- 1 - rss/tss
rsq_rf
ridgevar <- varImp(ridge)
plot(ridge)
