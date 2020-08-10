## Get document year one hot matrix
load("/data_lake/arXiv_ML/ML_meta_2019.RData")

all_df <- all_df[grep(pattern = "(astro|cond-|gr|hep|math-ph|nlin|nucl|physics|quant|q-bio)",
                      x = all_df$categories,
                      invert = T),]

# Cut text data into slices as per year.
paperIDYear <- all_df[, 1:2]
paperIDYear$submitYear <- as.integer(substr(paperIDYear$submitted, 1, 4))
paperIDYear <- paperIDYear[-which(paperIDYear$id=="1804.01653v2"),]
paperIDYear <- paperIDYear[-which(paperIDYear$id=="1206.3247v1"),]
paperIDYear$submitted <- NULL

#1804.01653v2
#1206.3247v1

dym <- paperIDYear

rownames(dym) <- dym$id
dym$id <- NULL
dym <- as.matrix(dym)

tfym <- cbind(dym,dtm)
tfym <- as.matrix(tfym)

library(dplyr)
#test <- as.data.frame(tfym)

tfym <- test %>% 
  group_by(submitYear) %>% 
  summarise_all(sum) 

tfym$submitYear = NULL
tfym <- as.matrix(tfym)

# save(tfym,file="~/research/ML_version/data/tfym_2019.RData")







