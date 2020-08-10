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


for(unique_value in unique(paperIDYear$submitYear)){
  
  paperIDYear[paste(unique_value, sep = "")] <- ifelse(paperIDYear$submitYear == unique_value, 1, 0)
  
}

#1804.01653v2
#1206.3247v1

dym <- paperIDYear

rownames(dym) <- dym$id
dym$id <- NULL
dym <- as.matrix(dym)

# save(dym,file="~/research/ML_version/data/dym_2019.RData")
