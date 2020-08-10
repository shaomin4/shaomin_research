####
# NMF
library(nmfgpu4R)
# 設定路徑
Sys.setenv(CUDA_PATH = "/usr/local/cuda")
Sys.setenv(CUDA_ROOT = "/usr/local/cuda")
Sys.setenv(CUDA_HOME = "/usr/local/cuda")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))
Sys.setenv(NMFGPU_ROOT = "/usr/local/nmfgpu")
Sys.setenv(LD_LIBRARY_PATH = paste(Sys.getenv("LD_LIBRARY_PATH"), "/usr/local/cuda/lib", sep = ":"))
# Initialize nmfgpu4R
nmfgpu4R.init()
chooseDevice(0)

rownames(coupon_train) = coupon_train$COUPON_ID_hash
rownames(coupon_test) = coupon_test$COUPON_ID_hash
coupon_train$COUPON_ID_hash <- NULL
coupon_test$COUPON_ID_hash <- NULL



### train nmf
r <- seq(10, 100, 5)
r <- c(seq(1:9),r)
for(i in seq(r)) {
  k <- r[[i]]
  print(k)
  coupon_train_nmf <- nmf(data = t(coupon_train), k, algorithm="mu", initMethod="EIn-NMF", maxiter = 50000, runs = 5, verbose = T)
  save(coupon_train_nmf, file = paste0("~/coupon_reco/coupon_train/coupon_train_nmf_", k, ".RData"))
}


# Read NMF RData
trainNMF_list <-list()
for(i in seq(r)){
  trainNMF_list[[i]] <- get(load(paste0("~/coupon_reco/coupon_train/coupon_train_nmf_", r[i], ".RData")))
}


#NMF encoding
idx <- 14
encoding.train <-  t(predict(trainNMF_list[[idx]]))
encoding.test <- t(predict(trainNMF_list[[idx]],t(coupon_test)))


### Compute similarity between train coupons and test coupons
library(StatMatch)
distances <- gower.dist(data.x = encoding.train, data.y = encoding.test)
colnames(distances) <- rownames(encoding.test)
rownames(distances) <- rownames(encoding.train)
sim_mtx <- distances

# cosine simility
library(lsa)
sim_mtx<-apply(encoding.train, MARGIN = 1, function(X) {
  apply(encoding.test, MARGIN = 1, function(Y) {
    cosine(X, Y)
  })
})


#### 切比雪夫距离 ####
# 國際象棋玩過麼？國王走一步能夠移動到相鄰的8個方格中的任意一個。那麼國王從格子(x1,y1)走到格
# 子(x2,y2)最少需要多少步？自己走走試試。你會發現最少步數總是max( | x2-x1 | , | y2-y1 | ) 步 
# 有一種類似的一種距離度量方法叫切比雪夫距離。
#sim_mtx <- dist(encoding.train,encoding.test,method = "maximum")
#class(sim_mtx) <- NULL
#sim_mtx <- as.matrix(sim_mtx)
#dim(sim_mtx)

#### 绝对值距离 or 曼哈頓距離(Manhattan Distance) ####
# 從名字就可以猜出這種距離的計算方法了。想像你在曼哈頓要從一個十字路口開車到另外一個十字路口，
# 駕駛距離是兩點間的直線距離嗎？顯然不是，除非你能穿越大樓。實際駕駛距離就是這個「曼哈頓距離」
# 而這也是曼哈頓距離名稱的來源， 曼哈頓距離也稱為城市街區距離(City Block distance)。
#sim_mtx <- dist(encoding.train,encoding.test,method = "manhattan")
#class(sim_mtx) <- NULL

#### 閔可夫斯基距離(Minkowski Distance) ####
# 閔氏距離不是一種距離，而是一組距離的定義。
# (1) 閔氏距離的定義 -> 兩個n維變量a(x11,x12,…,x1n)與 b(x21,x22,…,x2n)間的閔可夫斯基距離定義為：
# 其中p是一個變參數。當p=1時，就是曼哈頓距離,當p=2時，就是歐氏距離,當p→∞時，就是切比雪夫距離
# 根據變參數的不同，閔氏距離可以表示一類的距離。
# (2)閔氏距離的缺點
# 閔氏距離，包括曼哈頓距離、歐氏距離和切比雪夫距離都存在明顯的缺點。
# 舉個例子：二維樣本(身高,體重)，其中身高範圍是150~190，體重範圍是50~60，有三個樣本：a(180,50)，b(190,50)，c(180,60)。那麼a與b之間的閔氏距離（無論是曼哈頓距離、歐氏距離或切比雪夫距離）等於a與c之間的閔氏距離，但是身高的10cm真的等價於體重的10kg麼？因此用閔氏距離來衡量這些樣本間的相似度很有問題。
# 簡單說來，閔氏距離的缺點主要有兩個：(1)將各個份量的量綱(scale)，也就是「單位」當作相同的看待了。
#                                     (2)沒有考慮各個份量的分佈（期望，方差等)可能是不同的。
# cor_minkowski <- dist(encoding.train,encoding.test,method = "minkowski", p=1)
# class(cor_minkowski) <- NULL

#### 欧几里德距离  ####
#cor_euclidean <- dist(encoding.train,encoding.test,method = "euclidean")
#class(cor_euclidean) <- NULL

#### softmax ####
library(tsensembler)
sim_mtx <- apply(sim_mtx, MARGIN = 1, softmax)
sim_mtx <- t(sim_mtx)


### read nmf RData
purchaseNMF_list <-list()
r <- seq(1, 30)
for(i in seq(r)) {
  purchaseNMF_list[[i]] <- get(load(paste0("~/coupon_reco/purchase_nmf/purchase_nmf_", r[i], ".RData")))
}

idx_j <- 30
purchase_nmf <- purchaseNMF_list[[idx_j]]

H_mtx <- purchase_nmf$H
W_mtx <- purchase_nmf$W

H_mtx <- H_mtx[, order(colnames(H_mtx))]

ggg <- H_mtx %*% sim_mtx
gg3 <- W_mtx %*% ggg

gg4 <- apply(gg3, 1, function(X) {
  GG <- sort(X, decreasing = T)[1:10]
  GG <- GG[GG>0]
  return(GG)
})

answer <- sapply(gg4, function(X) paste(names(X), collapse = " "))
# output <- sapply(names(answer), function(X) paste(X, answer[[X]], sep = ","))
# names(output) <- NULL
library(data.table)
submit_example <- fread("~/coupon_reco/data/sample_submission.csv", data.table=F)
submit_example$PURCHASED_COUPONS <- answer[submit_example$USER_ID_hash]
fwrite(submit_example, "~/submit.csv")

FuAPk <- function (uNumToExamineK, vbYreal, vdYhat){
  # The real Y values is sorted by predicted Y values in decending order(decreasing=TRUE) 
  vbYreal_sort_d <- vbYreal[order(vdYhat, decreasing=TRUE)]
  vbYreal_sort_d <- vbYreal_sort_d[1:uNumToExamineK]
  uAveragePrecision <- sum(cumsum(vbYreal_sort_d) * vbYreal_sort_d / seq_along(vbYreal_sort_d)) /
    sum(vbYreal_sort_d)
  uAveragePrecision
}

sapply(names(gg4), function(X) {
  print(sapply(names(gg4[[X]]), function(Y) {
    purchase_sparse_mtx1_valid[X, Y] > 0
  }))
})
vbYreal <- c(1, 0, 1, 0,0,1,0,0,1,1,0,0,0,0,0)
vdYhat <- c(10.91, .89, .88, .85, .71, .70, .6, .53, .5, .4, .3, .3, .3, .3, 10.1)

FuAPk(15, vbYreal, vdYhat)
