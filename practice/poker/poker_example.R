source("poker_feature_engineering.R")
pokerTrain = data.table::fread("pokertrain.txt", data.table = F)
pokerTest = data.table::fread("pokertest.txt", data.table = F)

colnames(pokerTrain) = c("S1", "C1", "S2", "C2", "S3", "C3", "S4", "C4", "S5", "C5",
                         "pokerHand")

colnames(pokerTest) = c("S1", "C1", "S2", "C2", "S3", "C3", "S4", "C4", "S5", "C5",
                        "pokerHand")

pokerTrain = poker_feature_engineering(formula = pokerHand ~ ., data = pokerTrain,
                                       parallel_cores = 10)

colnames(pokerTrain)[2:11] = c("S1", "C1", "S2", "C2", "S3", "C3", "S4", "C4", "S5", "C5")

pokerTest = poker_feature_engineering(formula = pokerHand ~ ., data = pokerTest,
                                      parallel_cores = 10)

colnames(pokerTest)[2:11] = c("S1", "C1", "S2", "C2", "S3", "C3", "S4", "C4", "S5", "C5")

for(i in 1:11){
  pokerTrain[,i] = as.factor(pokerTrain[,i])
  pokerTest[,i] = as.factor(pokerTest[,i])
}

# Formula
formula <- as.formula(data_label ~ .)

parameter_list = list()

parameter1 = matrix(c(2:11, rep(50, 10)), ncol = 2)

parameter1 = apply(parameter1, 1, function(x){
  return(list("maxnode" = x[1], "ntree" = x[2]))
})

for(i in c(1)) {
  parameter_list[[i]] = parameter1
}

source("train_DRF.R")
set.seed(1)
DRF_m <- train_DRF(formula = formula, train_d = pokerTrain, parallel_cores = 10,
                   cascade_structure = FALSE, ordered_factor = FALSE,
                   parameter_list = parameter_list )

source("mapTestingDataset.R")
train_d_s = mapTestingDataset(DRF_model = DRF_m, test_data = pokerTrain, parallel_cores = 10,
                              cascade_structure = FALSE, ordered_factor = FALSE)

load("pokerTest.RData")



test_d_s = mapTestingDataset(DRF_model = DRF_m, test_data = pokerTest, parallel_cores = 10,
                             cascade_structure = FALSE, ordered_factor = FALSE)

acc = table(test_d_s[[1]]$data_label, pokerTest$data_label)

acc = sum(diag(acc)) / nrow(pokerTest)