library(rpart)
library(C50)
library(ranger)
library(e1071)
library(ggplot2)

pokertrain <- data.table::fread('pokertrain.txt')
pokertest <- data.table::fread('pokertest.txt')

View(pokertrain)

pokertrain[] <- lapply(pokertrain, as.factor)
pokertest[] <- lapply(pokertest, as.factor)

# plot
ggplot(pokertrain, aes(V11)) + geom_bar()

# rpart
pokerrp <- rpart(V11 ~ . ,pokertrain)
rppred <- predict(pokerrp, pokertest, type = "class")
rpconfusion = table(rppred, pokertest$V11)
print(1 - sum(diag(rpconfusion)) / nrow(pokertest))

# C50
pokerc50 <- C5.0(V11 ~ . ,pokertrain)
c50pred <- predict(pokerc50, pokertest, type = "class")
c50confusion = table(c50pred, pokertest$V11)
print(1 - sum(diag(c50confusion)) / nrow(pokertest))

# randomforest
pokerrf <- ranger(V11 ~ . ,pokertrain)
rfpred <- predict(pokerrf, pokertest)
rfconfusion = table(rfpred$predictions, pokertest$V11)
print(1 - sum(diag(rfconfusion)) / nrow(pokertest))

# svm
pokersvm <- svm(V11 ~ . ,pokertrain)
svmpred <- predict(pokersvm, pokertest)
svmconfusion = table(svmpred, pokertest$V11)
print(1 - sum(diag(svmconfusion)) / nrow(pokertest))