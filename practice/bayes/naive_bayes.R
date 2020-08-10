library(naivebayes)
ind_iris <- sample(1:nrow(iris), size = round(0.3 * nrow(iris)))
iris_train <- iris[-ind_iris, ]
iris_test <- iris[ind_iris, ]

nb_iris <- naive_bayes(Species ~ ., iris_train)
pre_ir = predict(nb_iris, iris_test)
head(predict(nb_iris, iris_test, type = "class"))
table(pre_ir,iris_test[,5])