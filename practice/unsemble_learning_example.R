# dataset : Adult 
adult = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                   sep=",",header=F,col.names=c("age", "type_employer", "fnlwgt", "education", 
                                                "education_num","marital", "occupation", "relationship", "race","sex",
                                                "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                   fill=FALSE,strip.white=T)
View(adult)

# Split "adult" dataset into training and testing data
set.seed(1); adult.numOfRows = nrow(adult)
adult.train_idx = sample(1:adult.numOfRows, adult.numOfRows * .9)
adult.test_idx = setdiff(1:adult.numOfRows, adult.train_idx)

# tree controling
library(rpart)
adult.tree_controling = rpart.control(maxdepth = 2, minsplit = 15)

# strong learner
adult.strong_learner = rpart(income ~ ., data = adult[adult.train_idx,],
                             control = rpart.control(minsplit = 10, cp = 0.005))

# bagging
library(adabag)
adult.bagging = bagging(income ~ ., data = adult[adult.train_idx,], 
                        mfinal = 10, control = adult.tree_controling)

# boosting (adaboost)
adult.bootsing = boosting(income ~., data = adult[adult.train_idx,], 
                          mfinal = 10, control = adult.tree_controling, 
                          coeflearn = "Freund")

# compare bagging and random forest
library(ranger)
adult.rf = ranger(income ~., data = adult[adult.train_idx,], num.trees = 10)



# ROC
library(pROC)

adult_measures = Map(function(m) {
  #set.seed(1); fit = f(income ~ . , adult[adult.train_idx, ]);
  pred_prob = predict(m, newdata = adult[adult.test_idx, ], type = "prob")[, "<=50K"]
  return(roc(adult[adult.test_idx, "income"], pred_prob))
}, list("strong learner" = adult.strong_learner, "bagging" = adult.bagging,
        "boosting" = adult.bootsing, "random forest" = adult.rf))

# prediction
pred_prob_strong = predict(adult.strong_learner, newdata = adult[adult.test_idx, ], type = "prob")[, "<=50K"]
pred_prob_bagging = predict(adult.bagging, newdata = adult[adult.test_idx, ])$prob[, 2]
pred_prob_boosting = predict(adult.bootsing, newdata = adult[adult.test_idx, ], type = "prob")$prob[, 2]
pred_prob_rf = predict(adult.rf, data = adult[adult.test_idx, ], predict.all = T, type ="response")$predictions
pred_prob_strong_a = ifelse(pred_prob_strong > 0.5 , 1, 0)
pred_prob_bagging_a = ifelse(pred_prob_bagging > 0.5 , 1, 0)
pred_prob_boosting_a = ifelse(pred_prob_boosting > 0.5 , 1, 0)
pred_prob_rf_a = ifelse(pred_prob_rf > 1.5 , 1, 0)

# Plot ROCs
plot.roc(adult_measures[[1]], print.thres = "best", 
         print.thres.best.method = "youden", col = "blue")
plot.roc(adult_measures[[2]], print.thres = "best", 
         print.thres.best.method = "youden", col = "red", add = T)

