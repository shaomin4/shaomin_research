# Least square estimation example

data(mtcars);

# beta_hat = inverse(X’X) - X’Y

Y = as.matrix(mtcars$mpg,ncol=1);
# Intercept vector has to be 1
X = cbind(matrix(rep(1,32),ncol=1), mtcars[, c("wt")]);


b = solve(t(X) %*% X ) %*% (t(X) %*% Y);

mpg_wt = lm(mpg ~ wt, data=mtcars);

# projection of the y-vector on the column space of X
X = as.matrix( mtcars[, c("wt")], ncol=1);
proj_X = X %*% solve(t(X) %*% X ) %*% t(X) ;


solve(t(X) %*% X ) %*% t(X) %*% Y

#############################

lm(mpg ~ wt, data= mtcars)

y = mtcars$mpg
x = mtcars$wt
x = cbind(1,x)
theta = solve(t(x) %*% x ) %*% (t(x) %*% y)
theta


i = diag(2)
i[1,1]=0

