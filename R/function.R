#'GLH
#'
#'General Linear Hypothesis
#'
#'@param x a list of columns of covariates
#'
#'@param y continuous outcome
#'
#'@param intercept whether to include intercept in the model, the default is True
#'
#'@param c contrast matrix,
#'
#'@param alpha significance level, the default value is 0.05
#'
#'@param rhs constant matrix, the default value  is a scalar zero.
#'
#'@return F statistic, P value
#'
#'@examples
#'square(3)
#'
#'@export
#'
#'


library(Matrix)
GLH <- function(x,y,intr = TRUE, c,rhs = 0 ,alpha = 0.05){
  n = length(y) ### the number of observations
  if (intr == F & length(x)==1) {  #convert a vector into a matrix
    x= as.matrix(x)
  }
  if (length(x)>1) { #unlist the x, convert it into matrix
    x= matrix(unlist(x), nrow = n, ncol=length(x))
  }
  if (intr == TRUE) { # add intercept column into matrix
    x = cbind(rep(1,n),x)
  }
  rank = rankMatrix(c)[[1]]# calculate the rank of contrast matrix
  p = dim(x)[2] # the p-1 covariates
  if (length(rhs) == 1){
    rhs = as.matrix(rep(rhs,nrow(c)))
  } else {
    rhs = as.matrix(rhs)
  }
  XTX_inv = solve(t(x)%*%x)
  beta_hat = XTX_inv %*% t(x) %*% y
  res = x%*%beta_hat-y
  sigma_hat = (t(res) %*%res)/(n-p)
  numarator = t(c%*%beta_hat - rhs) %*% solve((c%*%XTX_inv%*%t(c))) %*%(c%*%beta_hat - rhs)
  F_value = as.vector(numarator/rank/sigma_hat)

  p_statistic = pf(F_value, df1=rank,df2=(n-p),lower.tail = F)

  return(list(F_statistic = F_value, p_value = p_statistic))
}


