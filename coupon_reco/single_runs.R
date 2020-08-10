library(data.table)
library(dplyr)
library(caret)
library(nmfgpu4R)
library(Matrix)
library(lsa)
library(parallel)
library(tsensembler)

# 設定路徑
Sys.setenv(CUDA_PATH = "/usr/local/cuda")
Sys.setenv(CUDA_ROOT = "/usr/local/cuda")
Sys.setenv(CUDA_HOME = "/usr/local/cuda")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))
Sys.setenv(NMFGPU_ROOT = "/usr/local/nmfgpu")
Sys.setenv(LD_LIBRARY_PATH = paste(Sys.getenv("LD_LIBRARY_PATH"), "/usr/local/cuda/lib64", sep = ":"))
# Initialize nmfgpu4R
nmfgpu4R.init()
chooseDevice(0)

######
# Read data
# 
file_name <- list.files("~/coupon_reco/data/", pattern = "*.csv")
file_name <- paste0("~/coupon_reco/data/", file_name)
dat_list <- Map(file = file_name, f = fread, data.table = F, nThread = 1)
# Get coupon data (train)
coupon_list_train_df <- dat_list$`~/coupon_reco/data/coupon_list_train.csv`
training_coupon_id <- coupon_list_train_df$COUPON_ID_hash

# New training set
coupon_list_train_df <- dat_list$`~/coupon_reco/data/coupon_list_train.csv`
# New validation set
coupon_list_test_df <- dat_list$`~/coupon_reco/data/coupon_list_test.csv`
# Get user data (all)
user_list_df <- dat_list$`~/coupon_reco/data/user_list.csv`
# Get unique coupon ID
coupon_train_id <- coupon_list_train_df$COUPON_ID_hash
coupon_test_id <- coupon_list_test_df$COUPON_ID_hash
# Get unique user ID
user_train_id <- user_list_df$USER_ID_hash
# Get train coupon's area data
area_train = dat_list$`~/coupon_reco/data/coupon_area_train.csv`
# Get test coupon's area data
area_test = dat_list$`~/coupon_reco/data/coupon_area_test.csv`
# Get train purchase log 
purchase_log_train_df <- dat_list$`~/coupon_reco/data/coupon_detail_train.csv`
# Get test purchase log 
purchase_log_test_df <- dat_list$`~/coupon_reco/data/coupon_detail_test.csv`

######
# Data preprocessing
# 
### Translate Japanese to English
CAPSULE_TEXT_translate <- list("宅配" = "Delivery service",
                               "グルメ" = "Food",
                               "ホテル" = "Hotel",
                               "ヘアサロン" = "Hair salon",
                               "旅館" = "Japanese hotel",
                               "リラクゼーション" = "Relaxation",
                               "その他" = "Other",
                               "エステ" = "Spa",
                               "レジャー" = "Leisure",
                               "レッスン" = "Lesson",
                               "ネイル・アイ" = "Nail and eye salon",
                               "ギフトカード" = "Gift card",
                               "ペンション" = "Resort inn",
                               "民宿" = "Japanse guest house",
                               "健康・医療" = "Health and medical",
                               "WEBサービス" = "Web service",
                               "ビューティー" = "Beauty",
                               "貸別荘" = "Vacation rental",
                               "ロッジ" = "Lodge",
                               "通学レッスン" = "Class",
                               "通信講座" = "Correspondence course",
                               "ゲストハウス" = "Guest house",
                               "公共の宿" = "Public hotel",
                               "イベント" = "Event",
                               "ビューティ" = "Beauty")
for(capsule in names(CAPSULE_TEXT_translate)) {
  print(capsule)
  coupon_list_train_df[coupon_list_train_df$CAPSULE_TEXT == capsule, "CAPSULE_TEXT"] <- CAPSULE_TEXT_translate[capsule]
  coupon_list_test_df[coupon_list_test_df$CAPSULE_TEXT == capsule, "CAPSULE_TEXT"] <- CAPSULE_TEXT_translate[capsule]
}
GENRE_NAME_translate <- list("宅配" = "Delivery service",
                             "グルメ" = "Food",
                             "ホテル・旅館" = "Hotel and Japanese hotel",
                             "ヘアサロン" = "Hair salon",
                             "リラクゼーション" = "Relaxation",
                             "その他のクーポン" = "Other coupon",
                             "エステ" = "Spa",
                             "レッスン" = "Lesson",
                             "レジャー" = "Leisure",
                             "ネイル・アイ" = "Nail and eye salon",
                             "ギフトカード" = "Gift card",
                             "健康・医療" = "Health and medical",
                             "ビューティー" = "Beauty")
for(genre in names(GENRE_NAME_translate)) {
  print(genre)
  coupon_list_train_df[coupon_list_train_df$GENRE_NAME == genre, "GENRE_NAME"] <- GENRE_NAME_translate[genre]
  coupon_list_test_df[coupon_list_test_df$GENRE_NAME == genre, "GENRE_NAME"] <- GENRE_NAME_translate[genre]
}
### Create one hot in train area 
# create one hot dataframe with each ID, ID will replease.
area_onehot <- dummies::dummy.data.frame(area_train, names=c("SMALL_AREA_NAME"), sep="_")[,-56]
# Get ID information and bind each ID with same column.
splitted_area_onehot <- split(area_onehot, area_onehot$COUPON_ID_hash)
each_id_area <- Map(splitted_area_onehot, 
                    f = function(X) { sapply(X = X[, -56], FUN = any) })
encoded_area_train <- do.call(rbind, each_id_area)
encoded_area_train <- as.data.frame(encoded_area_train)
# Add ID column
encoded_area_train$COUPON_ID_hash <- rownames(encoded_area_train)
# Change logic format to integer
encoded_area_train[,c(1:55)] <- mutate_all(encoded_area_train[,c(1:55)], funs(as.numeric))

### Create one hot in test area 
area_onehot <- dummies::dummy.data.frame(area_test, names=c("SMALL_AREA_NAME"), sep="_")[,-56]
# Get ID information and bind each ID with same column.
splitted_area_onehot <- split(area_onehot, area_onehot$COUPON_ID_hash)
each_id_area <- Map(splitted_area_onehot, 
                    f = function(X) { sapply(X = X[, -56], FUN = any) })
encoded_area_test <- do.call(rbind, each_id_area)
encoded_area_test <- as.data.frame(encoded_area_test)
# Add ID column
encoded_area_test$COUPON_ID_hash <- rownames(encoded_area_test)
# Change logic format to integer
encoded_area_test[,c(1:55)] <- mutate_all(encoded_area_test[,c(1:55)], funs(as.numeric))

###########
#  TRAIN  #
###########
### 整理關於時間的資料
# 將使用期限作為 duration,並補上 duration 的 NA 值
coupon_list_train_df[, c("DISPFROM", "DISPEND")] <- NULL
coupon_list_train_df[, c("VALIDFROM", "VALIDEND")] <- NULL
# 有些資料使用期限為 NA，故將空直補為0並轉換成新欄位沒有限制
normParam <- preProcess(coupon_list_train_df[, c("DISPPERIOD", "VALIDPERIOD")], 
                        method = c("range"), rangeBounds = c(0, 1))
coupon_list_train_df[, c("DISPPERIOD", "VALIDPERIOD")] <- predict(normParam, coupon_list_train_df[, c("DISPPERIOD", "VALIDPERIOD")])
coupon_list_train_df$DISPPERIOD[is.na(coupon_list_train_df$DISPPERIOD)] <- 0
coupon_list_train_df$VALIDPERIOD[is.na(coupon_list_train_df$VALIDPERIOD)] <- 0
# 新增 unlimited duration
coupon_list_train_df$UNLIMITED_DISPPERIOD <- ifelse(coupon_list_train_df$DISPPERIOD==0, 1, 0)
coupon_list_train_df$UNLIMITED_VAILDPERIOD <- ifelse(coupon_list_train_df$VALIDPERIOD==0, 1, 0)
# 將區域資料移除
coupon_list_train_df[, c("small_area_name", "ken_name", "large_area_name")] <- NULL

### 整理關於價錢的資料
# 對價錢做前處理
coupon_list_train_df$DISCOUNT_PRICE <- 1 / log10(coupon_list_train_df$DISCOUNT_PRICE)
coupon_list_train_df$CATALOG_PRICE <- 1 / log10(coupon_list_train_df$CATALOG_PRICE)
coupon_list_train_df$CATALOG_PRICE[which(is.infinite(coupon_list_train_df$CATALOG_PRICE))] <- 1
coupon_list_train_df$PRICE_RATE <- (coupon_list_train_df$PRICE_RATE*coupon_list_train_df$PRICE_RATE) / (100*100)

### 把"星期幾可使用的狀態"做onehot
# 將 NA 補上1
coupon_list_train_df[is.na(coupon_list_train_df)] <- 1
coupon_list_train_df <- dummies::dummy.data.frame(coupon_list_train_df, 
                                                  names=c("USABLE_DATE_MON", "USABLE_DATE_TUE",
                                                          "USABLE_DATE_WED", "USABLE_DATE_THU",
                                                          "USABLE_DATE_FRI", "USABLE_DATE_SAT",
                                                          "USABLE_DATE_SUN", "USABLE_DATE_HOLIDAY",
                                                          "USABLE_DATE_BEFORE_HOLIDAY"), sep="_")

### 把 coupon_list_train_df$CAPSULE_TEXT, GENRE_NAME 做 onehot
coupon_list_train_df <- dummies::dummy.data.frame(coupon_list_train_df,
                                                  names = c("CAPSULE_TEXT", "GENRE_NAME"),sep="_")

### 把coupon_detail 和 coupon_area 合併
coupon_list_train_df = merge(coupon_list_train_df, encoded_area_train, all = T)
# 有些票卷並沒有限制購買區域，故全區皆可購買
coupon_list_train_df[is.na(coupon_list_train_df)] <- 1

###########
#  TEST   #
###########
### 整理關於時間的資料
# 將使用期限作為 duration,並補上 duration 的 NA 值
coupon_list_test_df[, c("DISPFROM", "DISPEND")] <- NULL
coupon_list_test_df[, c("VALIDFROM", "VALIDEND")] <- NULL
# 有些資料使用期限為 NA，故將空直補為0並轉換成新欄位沒有限制
coupon_list_test_df[, c("DISPPERIOD", "VALIDPERIOD")] <- predict(normParam, coupon_list_test_df[, c("DISPPERIOD", "VALIDPERIOD")])
coupon_list_test_df$DISPPERIOD[is.na(coupon_list_test_df$DISPPERIOD)] <- 0
coupon_list_test_df$VALIDPERIOD[is.na(coupon_list_test_df$VALIDPERIOD)] <- 0
# 新增 unlimited duration
coupon_list_test_df$UNLIMITED_DISPPERIOD <- ifelse(coupon_list_test_df$DISPPERIOD==0, 1, 0)
coupon_list_test_df$UNLIMITED_VAILDPERIOD <- ifelse(coupon_list_test_df$VALIDPERIOD==0, 1, 0)
# 將區域資料移除
coupon_list_test_df[, c("small_area_name", "ken_name", "large_area_name")] <- NULL

### 整理關於價錢的資料
# 對價錢做前處理
coupon_list_test_df$DISCOUNT_PRICE <- 1 / log10(coupon_list_test_df$DISCOUNT_PRICE)
coupon_list_test_df$CATALOG_PRICE <- 1 / log10(coupon_list_test_df$CATALOG_PRICE)
coupon_list_test_df$CATALOG_PRICE[which(is.infinite(coupon_list_test_df$CATALOG_PRICE))] <- 1
coupon_list_test_df$PRICE_RATE <- (coupon_list_test_df$PRICE_RATE*coupon_list_test_df$PRICE_RATE) / (100*100)

### 把"星期幾可使用的狀態"做onehot
# 將 NA 補上1
coupon_list_test_df[is.na(coupon_list_test_df)] <- 1
coupon_list_test_df <- dummies::dummy.data.frame(coupon_list_test_df, 
                                                 names=c("USABLE_DATE_MON", "USABLE_DATE_TUE",
                                                         "USABLE_DATE_WED", "USABLE_DATE_THU",
                                                         "USABLE_DATE_FRI", "USABLE_DATE_SAT",
                                                         "USABLE_DATE_SUN", "USABLE_DATE_HOLIDAY",
                                                         "USABLE_DATE_BEFORE_HOLIDAY"), sep="_")

### 把 coupon_list_test_df$CAPSULE_TEXT, GENRE_NAME 做 onehot
coupon_list_test_df <- dummies::dummy.data.frame(coupon_list_test_df,
                                                 names = c("CAPSULE_TEXT", "GENRE_NAME"),sep="_")

### 把coupon_detail 和 coupon_area 合併
coupon_list_test_df = merge(coupon_list_test_df, encoded_area_test, all = T)
# 有些票卷並沒有限制購買區域，故全區皆可購買
coupon_list_test_df[is.na(coupon_list_test_df)] <- 1

### Train 和 Test 的 column 欄位不同
diff_colname <- setdiff(colnames(coupon_list_train_df), colnames(coupon_list_test_df))
# 將缺少值補上
for (i in 1:length(diff_colname)) {
  coupon_list_test_df[, diff_colname[i]] <- 0
}

purchase_sparse_train_mtx <- sparseMatrix(i = sapply(purchase_log_train_df$USER_ID_hash, function(X) which(X == user_train_id)),
                                          j = sapply(purchase_log_train_df$COUPON_ID_hash, function(X) which(X == coupon_train_id)),
                                          x = purchase_log_train_df$ITEM_COUNT,
                                          dims = c(length(user_train_id), length(coupon_train_id)),
                                          dimnames = list(user_train_id, coupon_train_id))
purchase_sparse_train_mtx <- as.matrix(purchase_sparse_train_mtx)

####
# Purchase NMF
r <- 50 
purchase_nmf <- nmf(data = purchase_sparse_train_mtx, r, algorithm="mu", initMethod="EIn-NMF", maxiter = 500, runs = 10, verbose = T)

####
# Coupon NMF
rownames(coupon_list_train_df) = coupon_list_train_df$COUPON_ID_hash
rownames(coupon_list_test_df) = coupon_list_test_df$COUPON_ID_hash
coupon_list_train_df$COUPON_ID_hash <- NULL
coupon_list_test_df$COUPON_ID_hash <- NULL
coupon_list_train_mtx <- as.matrix(coupon_list_train_df)
coupon_list_test_mtx <- as.matrix(coupon_list_test_df)

### train nmf
r <- 50
coupon_train_nmf <- nmf(data = t(coupon_list_train_mtx), r, algorithm="mu", initMethod="EIn-NMF", maxiter = 500, runs = 5, verbose = T)

#NMF encoding
encoding.train <- t(predict(coupon_train_nmf))
encoding.test <- t(predict(coupon_train_nmf, t(coupon_list_test_mtx)))

#### cosine simility
# Multi-cores method
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("encoding.train", "encoding.test", "cosine"))
sim_list <- parLapply(cl, seq(nrow(encoding.train)),
                      function(i) {
                        apply(encoding.test, MARGIN = 1, function(Y) {
                          cosine(encoding.train[i, ], Y)
                        })
                      })
stopCluster(cl)
sim_mtx <- do.call(rbind, sim_list)
rownames(sim_mtx) <- rownames(encoding.train)
sim_mtx <- t(sim_mtx)

#### softmax ####
sim_mtx <- apply(sim_mtx, MARGIN = 2, softmax)
sim_mtx <- t(sim_mtx)

# Extract fatorized matrix
H_train_mtx <- purchase_nmf$H
W_train_mtx <- purchase_nmf$W

# Align
H_train_mtx <- H_train_mtx[, order(colnames(H_train_mtx))]
sim_mtx <- sim_mtx[order(rownames(sim_mtx)), ]

all(rownames(sim_mtx) == colnames(H_train_mtx))

# Reconstruct H test
H_test_mtx <- H_train_mtx %*% sim_mtx
purchase_test_mtx <- W_train_mtx %*% H_test_mtx

top10_coupons <- apply(purchase_test_mtx, 1, FUN = function(X) {
  names(X)[order(X)][1:10]
})

answer <- apply(top10_coupons, 2, paste, collapse = " ")

library(data.table)
submit_example <- fread("~/coupon_reco/data/sample_submission.csv", data.table=F)
submit_example$PURCHASED_COUPONS <- answer[submit_example$USER_ID_hash]
fwrite(submit_example, "~/submit.csv")
