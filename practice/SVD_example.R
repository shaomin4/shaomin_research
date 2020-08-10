dtm = data.frame("programming"=c(2,0,3,0,3), "database"=c(3,3,4,0,0),
                 "SQL" = c(1,2,5,0,0), "finance" = c(0,2,0,5,0),
                 "rate" = c(0,2,0,2,0),
                 "market" = c(0,1,0,3,0), "data" = c(1,2,0,2,2))
rownames(dtm) = paste("doc", 1:5, sep = "_")
dtm
dtm_svd = svd(dtm)
dtm_svd
dtm_svd$u %*% diag(c(dtm_svd$d[1:4],0) ) %*% t(dtm_svd$v)
dtm_svd = svd(dtm, nu=min(5,2) ,nv=min(2,7)) 

dtm_loc = data.frame("doc_term" = c(rownames(dtm),colnames(dtm)),
                     "dt_type" = c(rep("document",5), rep("term",7)),
                     "x1" = c(dtm_svd$u[,1], dtm_svd$v[,1]),
                     "x2" = c(dtm_svd$u[,2], dtm_svd$v[,2]) ) 

ggplot(dtm_loc, aes(x = x1, y = x2, color=factor(dt_type))) +
  geom_point(size = 3) + geom_text(aes(label=doc_term),hjust=0,
                                   vjust=0) + xlim(-0.75,0) + xlab("Topic 1") + ylab("Topic 2")




# PCA using SVD
data(USArrests); X = scale(as.matrix(USArrests)) # Center the data
svd_X = svd(X)
# We can recover the original scaled matrix
X_prime = svd_X$u %*% diag(c(svd_X$d[1:4]) ) %*% t(svd_X$v)
# Low-rank approximation (say rank = 2) or "noise reduction"
# of the original matrix
X_prime = svd_X$u %*% diag(c(svd_X$d[1:2], 0, 0) ) %*% t(svd_X$v)
# Each columns in V contains loadings of each PC
svd_X$v
# Each column in U contains information about the observations
# Let's plot them onto a 2-D "concept map".
# We may evaluate the similarity of states in terms of PC1 and PC2
U = data.frame(svd_X$u)
rownames(U) = rownames(USArrests)
colnames(U) = paste("PC_", 1:4, sep = "")
ggplot(U, aes(x = PC_1, y = PC_2, )) + geom_point(size = 3)+
  xlim(-0.3, 0.4) + geom_text(aes(label=rownames(U)),
                              hjust=0, vjust=0, size = 4) + xlab("PC 1") + ylab("PC 2")
