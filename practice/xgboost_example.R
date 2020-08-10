library(xgboost)
# load data
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
# fit model
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nround = 2,
               nthread = 2, objective = "binary:logistic")
# predict
pred <- predict(bst, test$data)
pred


heart = read.table("http://www-bcf.usc.edu/~gareth/ISL/Heart.csv", header = T, sep = ",")[,-1]
heart = na.omit(heart) 
numOfRows = nrow(heart)
set.seed(1)
train_idx = sample(1:numOfRows,size = numOfRows * 0.7) 
test_idx = setdiff(1:numOfRows, train_idx) 
traindata = heart[train_idx,]
testdata = heart[test_idx,]

x = data.matrix(traindata[,-14])
y = as.integer(traindata[,14])-1
test = data.matrix(testdata[,-14])

bst_heart <- xgboost(data = x, label = y, max.depth = 2, eta = 0.8, nround = 8,
                     nthread = 10, objective = "binary:logistic")

bstcv_heart <- xgb.cv(data = x, label = y, max.depth = 2, eta = 0.8, nround = 8,
                      nthread = 10, objective = "binary:logistic",nfold = 10)

pred <- predict(bst_heart, test)
predfactor = ifelse(pred>0.5,"Yes","No")
t = table(testdata[,14],predfactor)
sum(diag(t))/sum(t)