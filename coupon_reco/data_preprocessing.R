library(data.table)

files <- list.files("~/coupon_reco/data/")
files <- paste0("~/coupon_reco/data/", files)
dat_list <- Map(X = files, function(X) fread(file = X, data.table = F, nThread = 10))
View(head(dat_list$`~/coupon_reco/data/coupon_area_train.csv`))
View(dat_list$`~/coupon_reco/data/coupon_list_train.csv`)
View(head(dat_list$`~/coupon_reco/data/coupon_list_train.csv`))
View(head(dat_list$`~/coupon_reco/data/coupon_visit_train.csv`))

coupon_list_train_df <- dat_list$`~/coupon_reco/data/coupon_list_train.csv`
coupon_list_test_df <- dat_list$`~/coupon_reco/data/coupon_list_test.csv`

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
}

View(table(coupon_list_test_df$CAPSULE_TEXT, coupon_list_train_df$GENRE_NAME))
View(dat_list$`~/coupon_reco/data/coupon_area_train.csv`)
View(dat_list$`~/coupon_reco/data/coupon_detail_train.csv`)
View(dat_list$`~/coupon_reco/data/prefecture_locations.csv`)
View(dat_list$`~/coupon_reco/data/sample_submission.csv`)
View(dat_list$`~/coupon_reco/data/user_list.csv`)

fivenum(table(dat_list$`~/coupon_reco/data/coupon_detail_train.csv`$COUPON_ID_hash))
