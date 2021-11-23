#'GLH
#'
#'General Linear Hypothesis
#'
#'@name GLH
#'
#'@param x a list of columns of covariates
#'
#'@param y continuous outcome
#'
#'@param intercept whether to include intercept in the model, the default is True
#'
#'@param predict predict matrix used to estimate the outcomes, the columns numbers should be equal to number of covariates
#'
#'@param contrast contrast matrix,
#'
#'@param alpha significance level, the default value is 0.05
#'
#'@param rhs constant matrix, the default value  is a scalar zero.
#'
#'@return The coefficients of covariates, and some basic statistics. The estimated predicted outcome based on given data.
#' F statistics and p valuse for the general linear hypothesis
#'
#'@examples
#'data(mtcars)
#'ma = GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg,intr = T)
#'mb = GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = T,contrast = matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs = 0.8)
#'@export


if (!require("Matrix",character.only = T)) {
  install.packages("Matrix")
}
library(Matrix) ##used to calculate the rank of matrix

GLH <- function(x,y,intr = TRUE, predict = NULL,contrast = NULL,rhs = 0 ,alpha = 0.05){
  y = as.matrix(y)
  row_name = names(x)
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
  p = dim(x)[2] # the p-1 covariates

  ### coeffecient part ###

  XTX_inv = solve(t(x)%*%x)
  beta_hat = XTX_inv %*% t(x) %*% y
  res = x%*%beta_hat-y
  sigma_hat = as.vector((t(res) %*%res)/(n-p))
  var_beta_hat = sigma_hat * XTX_inv
  beta_var = diag(var_beta_hat)
  beta_error = sqrt(beta_var)
  t_statistic = beta_hat/beta_error
  pt_statistic = 2*(1-pt(abs(t_statistic),df = (n-p)))
  conint_low = beta_hat + qt(alpha/2,(n-p))*beta_error
  conint_high = beta_hat - qt(alpha/2,(n-p))*beta_error
  coeff = cbind(beta_hat,beta_error,t_statistic,pt_statistic,conint_low,conint_high)
  colnames(coeff) = c("Estimate","Std. Error","t value","Pr(>|t|)","Confident interval:Low",
                      "Confident interval:High" )
  if (intr == T) {
    rownames(coeff) = c("Intercept",paste0("beta",1:(p-1)))
  } else {
    rownames(coeff) = c(paste0("beta",1:p))
  }

  ###R squared and adjusted R squared ###

  y_var = var(y)[1,1]
  R_squared = 1-sigma_hat*(n-p)/(n-1)/y_var
  R_squared_adjusted = 1-sigma_hat/y_var
  R_list= cbind("Multiple R-squared"=R_squared,"Adjusted R-squared"=R_squared_adjusted)

  ###prediction function ####

  if (!is.null(predict)) {
    if (!is.matrix(predict)){
      warning("Automatically covert the predict into a matrix,please check whether the result is expected")
      predict = matrix(predict,byrow = T,nrow = 1)
    }
    if (ncol(predict) == (p-1)){
      warning("Automatically append 1s into the prediction matrix, please check whether the result is expected")
      predict=cbind(1,predict)
    } else if (ncol(predict) != p & ncol(predict) != p-1) {
      stop("The predict matrix does not match the correct dimmensions")
    }
    predicted_value = predict %*% beta_hat
    var_predicted_value = sigma_hat %*% predict %*% XTX_inv %*% t(predict)
    predicted_value_error = sqrt(diag(var_predicted_value))
    predicted_value_confint_low = predicted_value +  qt(alpha/2,(n-p))*predicted_value_error
    predicted_value_confint_high = predicted_value -  qt(alpha/2,(n-p))*predicted_value_error
    predicted_result = cbind(predicted_value,predicted_value_error,
                             predicted_value_confint_low,predicted_value_confint_high)
    colnames(predicted_result) = c("Estimated value","Std Error","Confint low","Confint high")
  } else {
    message("Didn't use prediction function")
    predicted_result=NULL
  }

  ### General linear hypothesis###

  if (!is.null(contrast)){
    if (!is.matrix(contrast)) {
      warning("Automatically covert the predict into a 1 row contrast matrix, please check whether the result is expected")
      contrast = matrix(contrast,byrow = T,nrow = 1)
    }
    if (ncol(contrast) == p-1) {
      warning("Automatically append 0s into the contrast matrix, please check whether the result is expected")
      contrast = cbind(0,contrast)
    } else if (ncol(contrast) != p & ncol(contrast) != p-1) {
      stop("The contrast matrix does not match the correct dimmensions")
    }
    rank = rankMatrix(contrast)[[1]]# calculate the rank of contrast matrix
    if (length(rhs) == 1){
      rhs = as.matrix(rep(rhs,nrow(contrast)))
    } else {
      rhs = as.matrix(rhs)
    }
    numerator = t(contrast%*%beta_hat - rhs) %*% solve((contrast%*%XTX_inv%*%t(contrast))) %*%(contrast%*%beta_hat - rhs)
    F_value = as.vector(numerator/rank/sigma_hat)
    pF_statistic = pf(F_value, df1=rank,df2=(n-p),lower.tail = F)
    Hypo_list=cbind("F statistic" = F_value, "DF1" = rank,"DF2"=n-p,"p_value" = pF_statistic)
  } else {
    message("Didn't use the general linear hypothesis function")
    Hypo_list=NULL
  }
  return(list(coefficient = coeff,R_square = R_list,
              Prediction = predicted_result,Hypothesis = Hypo_list))
}


