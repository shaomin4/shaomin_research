### The following is a simple example which seeks to explain the outcome of a model 
### predicting cancer based on biopsy results:

library(MASS)
library(lime)
data(biopsy)

# First we'll clean up the data a bit
biopsy$ID <- NULL
biopsy <- na.omit(biopsy) # 699 -> 683
names(biopsy) <- c('clump thickness', 'uniformity of cell size', 
                   'uniformity of cell shape', 'marginal adhesion',
                   'single epithelial cell size', 'bare nuclei', 
                   'bland chromatin', 'normal nucleoli', 'mitoses',
                   'class')
# 腫瘤良性、惡性預測
#（“團塊厚度”，“像元大小的均勻性”，“細胞形狀的均勻性”，“邊緣粘附”，
#“單個上皮細胞大小”，“裸核”，“平淡染色質”，“正常核仁”，“有絲分裂”，'類'）

# Now we'll fit a linear discriminant model on all but 4 cases
set.seed(4)
test_set <- sample(seq_len(nrow(biopsy)), 4)
prediction <- biopsy$class
biopsy$class <- NULL
model <- lda(biopsy[-test_set, ], prediction[-test_set])
predict(model, biopsy[test_set, ])

# If we use the model to predict the 4 remaining cases we get some 
# pretty solid predictions:

# But lets see how these predictions came to be, using lime.
explainer <- lime(biopsy[-test_set,], model, bin_continuous = TRUE, quantile_bins = FALSE)
explanation <- explain(biopsy[test_set, ], explainer, n_labels = 1, n_features = 4)
# Only showing part of output for better printing
explanation[, 2:9]

# 
plot_features(explanation, ncol = 1)


