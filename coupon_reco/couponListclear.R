library(dplyr)

###############################
###  coupon area to onehot  ###
###############################

area_train = dat_list$`~/coupon_reco/data/coupon_area_train.csv`
area_test = dat_list$`~/coupon_reco/data/coupon_area_test.csv`
# check how many arae in column one
# test = unique(area_train$SMALL_AREA_NAME)

### Train area create one hot
# create one hot dataframe with each ID, ID will replease.
area_onehot <- dummies::dummy.data.frame(area_train, names=c("SMALL_AREA_NAME"), sep="_")[,-56]
# Get ID information and bind each ID with same column.
coupon_id <- split(area_onehot, area_onehot$COUPON_ID_hash)
each_id_area <- Map(coupon_id , f = function(X) {
  sapply(X = X[, -56], FUN = any)
})
area_train_encoder <- do.call(rbind, each_id_area); rm(each_id_area,coupon_id,area_onehot );
area_train_encoder <- as.data.frame(area_train_encoder)
# 新增ＩＤ欄位
area_train_encoder$COUPON_ID_hash <- rownames(area_train_encoder)
#將欄位由logi更改為integer
area_train_encoder[,c(1:55)]<-mutate_all(area_train_encoder[,c(1:55)],funs(as.numeric))


### Test area create one hot
area_onehot <- dummies::dummy.data.frame(area_test, names=c("SMALL_AREA_NAME"), sep="_")[,-56]
# Get ID information and bind each ID with same column.
coupon_id <- split(area_onehot, area_onehot$COUPON_ID_hash)
each_id_area <- Map(coupon_id , f = function(X) {
  sapply(X = X[, -56], FUN = any)
})
area_test_encoder <- do.call(rbind, each_id_area); rm(each_id_area,coupon_id,area_onehot);
area_test_encoder <- as.data.frame(area_test_encoder)
# 新增ＩＤ欄位
area_test_encoder$COUPON_ID_hash <- rownames(area_test_encoder)
#將欄位由logi更改為integer
area_test_encoder[,c(1:55)]<-mutate_all(area_test_encoder[,c(1:55)],funs(as.numeric))

rm(area_train,area_test,area,capsule,files,genre)

#####################################
###  coupon train data prepocess  ###
#####################################

### 整理關於時間的資料

# 將使用期限作為duration,並補上duration的ＮＡ值
coupon_list_train_df$DISPFROM <- NULL
coupon_list_train_df$DISPEND <- NULL
coupon_list_train_df$VALIDFROM <- NULL
coupon_list_train_df$VALIDEND <- NULL
# 有些資料使用期限為NA，故將空直補為0並轉換成新欄位沒有限制
coupon_list_train_df$VALIDPERIOD[is.na(coupon_list_train_df$VALIDPERIOD)] <- 0
#新增unlimited duration
coupon_list_train_df$Unlimited_duration <- ifelse(coupon_list_train_df$VALIDPERIOD==0,1,0)

### 將區域資料移除
# coupon_list_train_df$small_area_name <- NULL
# coupon_list_train_df$ken_name <- NULL
# coupon_list_train_df$large_area_name <- NULL

coupon_train <- coupon_list_train_df
### 整理關於價錢的資料

# 對價錢做前處理
coupon_train$DISCOUNT_PRICE <- 1/log10(coupon_train$DISCOUNT_PRICE)
coupon_train$PRICE_RATE <- (coupon_train$PRICE_RATE*coupon_train$PRICE_RATE)/(100*100)
# 將不介於0~1之間的資料轉換為0~1之間，有CATALOG_PRICE,DISPPERIOD,VAILDPERIOD.
coupon_train$CATALOG_PRICE <- coupon_train$CATALOG_PRICE / ifelse(max(coupon_train$CATALOG_PRICE) == 0, 1, max(coupon_train$CATALOG_PRICE))
coupon_train$DISPPERIOD <- coupon_train$DISPPERIOD / ifelse(max(coupon_train$DISPPERIOD) == 0, 1, max(coupon_train$DISPPERIOD))
coupon_train$VALIDPERIOD <- coupon_train$VALIDPERIOD / ifelse(max(coupon_train$VALIDPERIOD) == 0, 1, max(coupon_train$VALIDPERIOD))

### 把"星期幾可使用的狀態"做onehot
# 將ＮＡ補上1
coupon_train[is.na(coupon_train)]<- 1
coupon_train <- dummies::dummy.data.frame(coupon_train, names=c("USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_MON",           
                                                                        "USABLE_DATE_TUE","USABLE_DATE_WED",           
                                                                        "USABLE_DATE_THU","USABLE_DATE_FRI",           
                                                                        "USABLE_DATE_SAT","USABLE_DATE_SUN",           
                                                                        "USABLE_DATE_HOLIDAY","USABLE_DATE_BEFORE_HOLIDAY"), sep="_")


### 把 coupon_train$CAPSULE_TEXT 做 onehot
coupon_train <- dummies::dummy.data.frame(coupon_train,names = c("CAPSULE_TEXT","GENRE_NAME","large_area_name","ken_name","small_area_name"),sep="_")





### 把coupon_detail 和 coupon_area 合併
coupon_train = merge(coupon_train,area_train_encoder,all = T)
#有些票卷並沒有限制購買區域，故全區皆可購買
coupon_train[is.na(coupon_train)]<- 0

#sum(is.na(coupon_train))

#coupon_train2 <- apply(X = coupon_train[,-1],MARGIN = 2, FUN = function(X)  X / ifelse(max(X) == 0, 1, max(X)))
#coupon_train2 <- as.data.frame(coupon_train2)
#coupon_train2$COUPON_ID_hash <- coupon_train$COUPON_ID_hash


#####################################
###  coupon test data prepocess  ###
#####################################

####  Test data
# 將使用期限作為duration,並補上duration的ＮＡ值
coupon_list_test_df$DISPFROM <- NULL
coupon_list_test_df$DISPEND <- NULL
coupon_list_test_df$VALIDFROM <- NULL
coupon_list_test_df$VALIDEND <- NULL
# 有些資料使用期限為NA，故將空直補為0並轉換成新欄位沒有限制
coupon_list_test_df$VALIDPERIOD[is.na(coupon_list_test_df$VALIDPERIOD)] <- 0
#新增unlimited duration
coupon_list_test_df$Unlimited_duration <- ifelse(coupon_list_test_df$VALIDPERIOD==0,1,0)

coupon_test <- coupon_list_test_df

# 對價錢做前處理
coupon_test$DISCOUNT_PRICE <- 1/log10(coupon_test$DISCOUNT_PRICE)
coupon_test$PRICE_RATE <- (coupon_test$PRICE_RATE*coupon_test$PRICE_RATE)/(100*100)
# 將不介於0~1之間的資料轉換為0~1之間，有CATALOG_PRICE,DISPPERIOD,VAILDPERIOD.
coupon_test$CATALOG_PRICE <- coupon_test$CATALOG_PRICE / ifelse(max(coupon_test$CATALOG_PRICE) == 0, 1, max(coupon_test$CATALOG_PRICE))
coupon_test$DISPPERIOD <- coupon_test$DISPPERIOD / ifelse(max(coupon_test$DISPPERIOD) == 0, 1, max(coupon_test$DISPPERIOD))
coupon_test$VALIDPERIOD <- coupon_test$VALIDPERIOD / ifelse(max(coupon_test$VALIDPERIOD) == 0, 1, max(coupon_test$VALIDPERIOD))


### 把"星期幾可使用的狀態"做onehot
# 將ＮＡ補上1
coupon_test[is.na(coupon_test)]<- 1
coupon_test <- dummies::dummy.data.frame(coupon_test, names=c("USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_MON",           
                                                              "USABLE_DATE_TUE","USABLE_DATE_WED",           
                                                              "USABLE_DATE_THU","USABLE_DATE_FRI",           
                                                              "USABLE_DATE_SAT","USABLE_DATE_SUN",           
                                                              "USABLE_DATE_HOLIDAY","USABLE_DATE_BEFORE_HOLIDAY"), sep="_")
### 把 coupon_train$CAPSULE_TEXT 做 onehot
coupon_test <- dummies::dummy.data.frame(coupon_test,names = c("CAPSULE_TEXT","GENRE_NAME","large_area_name","ken_name","small_area_name"),sep="_")


### 把coupon_detail 和 coupon_area 合併
coupon_test = merge(coupon_test,area_test_encoder,by = "COUPON_ID_hash",all = T)
coupon_test[is.na(coupon_test)]<- 0

# train 和 test 的column欄位不同
train_name <- colnames(coupon_train)
test_name <- colnames(coupon_test)
diff_colname <- setdiff(train_name,test_name)
#將缺少值補上
for (i in 1:length(diff_colname)) {
  coupon_test[,diff_colname[i]] <- 0
}

#coupon_test2 <- apply(X = coupon_test[,-1],MARGIN = 2, FUN = function(X)  X / ifelse(max(X) == 0, 1, max(X)))
#coupon_test2 <- as.data.frame(coupon_test2)
#新增unlimited duration
#coupon_test2$Unlimited_duration <- ifelse(coupon_test2$VALIDPERIOD==0,0,1)
#coupon_test2$COUPON_ID_hash <- coupon_test$COUPON_ID_hash
save(coupon_test,file ="~/coupon_reco/clear_data/coupon_test.RData")
save(coupon_train,file ="~/coupon_reco/clear_data/coupon_train.RData")


