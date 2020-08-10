x <- matrix(c(5, 1, 4, 3, 5, 2, 2, 4, 4), nrow = 3)
clue::solve_LSAP(x)
clue::solve_LSAP(x, maximum = TRUE)
## To get the optimal value (for now):
y <- clue::solve_LSAP(x)
sum(x[cbind(seq_along(y), y)])
