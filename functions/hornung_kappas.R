# Kappa functions from Hornung, 2020 
## https://link.springer.com/article/10.1007/s00357-018-9302-x
## See the supplementary materials

unweightedkappa <- function(ytrue, yhat) {
  
  require("psych")
  
  x <- data.frame(ytrue=ytrue, yhat=yhat)
  
  cohen.kappa(x)$kappa
  
}

linearkappa <- function(ytrue, yhat) {
  
  require("psych")
  
  x <- data.frame(ytrue=ytrue, yhat=yhat)
  
  J <- length(unique(c(x$ytrue, x$yhat)))
  
  myw <- matrix(0, ncol = J, nrow = J)
  myw[] <- abs((col(myw) - row(myw)))
  myw <- 1 - myw/(J - 1)
  
  cohen.kappa(x, w=myw)$weighted.kappa
  
}

quadratickappa <- function(ytrue, yhat) {
  
  require("psych")
  
  x <- data.frame(ytrue=ytrue, yhat=yhat)
  
  J <- length(unique(c(x$ytrue, x$yhat)))
  
  myw <- matrix(0, ncol = J, nrow = J)
  myw[] <- abs((col(myw) - row(myw)))^2
  myw <- 1 - myw/(J - 1)^2
  
  cohen.kappa(x, w=myw)$weighted.kappa
  
}