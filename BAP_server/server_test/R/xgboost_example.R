# more information : https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html

library('xgboost')

# Simulate N x p random matrix with some binomial response dependent on pp columns
set.seed(111)
N <- 1000000
p <- 50
pp <- 25
X <- matrix(runif(N * p), ncol = p)
betas <- 2 * runif(pp) - 1
sel <- sort(sample(p, pp))
m <- X[, sel] %*% betas - 1 + rnorm(N)
y <- rbinom(N, 1, plogis(m))

tr <- sample.int(N, N * 0.75)
dtrain <- xgb.DMatrix(X[tr,], label = y[tr])
dtest <- xgb.DMatrix(X[-tr,], label = y[-tr])
wl <- list(train = dtrain, test = dtest)

# Compare to the 'hist' algorithm:
param <- list(objective = 'reg:logistic', eval_metric = 'auc', subsample = 0.5,max_bin = 64, tree_method = 'hist')

pt <- proc.time()
bst_hist <- xgb.train(param, dtrain, watchlist = wl, nrounds = 500)
proc.time() - pt

# An example of running 'gpu_hist' algorithm
# which is
# - similar to the 'hist'
# - the fastest option for moderately large datasets
# - current limitations: max_depth < 16, does not implement guided loss
# You can use tree_method = 'gpu_hist' for another GPU accelerated algorithm,
# which is slower, more memory-hungry, but does not use binning.
param <- list(objective = 'reg:logistic', eval_metric = 'auc', subsample = 0.5, nthread = 4,
              max_bin = 64, tree_method = 'gpu_hist')
pt <- proc.time()
bst_gpu <- xgb.train(param, dtrain, watchlist = wl, nrounds = 50)
proc.time() - pt

