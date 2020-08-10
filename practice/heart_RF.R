library(randomForest)
heart = read.table("http://www-bcf.usc.edu/~gareth/ISL/Heart.csv", header = T, sep = ",")[,-1]
heart = na.omit(heart) 
numOfRows = nrow(heart)
set.seed(1)
train_idx = sample(1:numOfRows,size = numOfRows * 0.7) 
test_idx = setdiff(1:numOfRows, train_idx) 
traindata = heart[train_idx,]
testdata = heart[test_idx,]
heart_RF = randomForest(AHD ~ ., traindata)

heart_predict = predict(heart_RF, newdata = testdata)
cm = table(testdata$AHD,heart_predict)
auc = sum(diag(cm))/nrow(testdata)
auc

##
data("iris")
iris = data.frame(iris)
numOfRows = nrow(iris)
set.seed(1)
train_idx = sample(1:numOfRows,size = numOfRows * 0.7) 
test_idx = setdiff(1:numOfRows, train_idx) 
traindata = iris[train_idx,]
testdata = iris[test_idx,]
iris_RF = randomForest(Species ~ ., traindata)


library(ggplot2)
data("diamonds")
View(diamonds)
