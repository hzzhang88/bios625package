# bios625package
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/hzzhang88/bios625package/workflows/R-CMD-check/badge.svg)](https://github.com/hzzhang88/bios625package/actions)
  
  [![codecov](https://codecov.io/gh/hzzhang88/bios625package/branch/main/graph/badge.svg?token=8TMLI4I85X)](https://codecov.io/gh/hzzhang88/bios625package)
  <!-- badges: end -->

The usage of GLH package is to allow users to use simple and multiple linear regression, predict the value and conduct the general linear hypothesis simultaneously. In this package, we export the output, including the estimated coefficients, confident intervals, p values, R square values, the predicted values with errors, F statistics and its p-value, etc. Compared to the base lm function and the well-established "car" package, our package is more efficient and memory-saving when running the same tasks. Since some arguments should be manually prepared before being passed into functions, some basic statistical knowledge is needed if the users want to fully use this package. But we provide the tutorial and description to help users better access and understand the functionalities of these methods.

Installation
--------------------

You can install the released version of GLH from GitHub. First, you can run the below code in your console to remove other package with overlapped name to avoid any possible bugs.
```{r}
.rs.restartR()
remove.packages("bios625package")
```

Then you can run
```{r}
install.packages('devtools')
devtools::install_github('hzzhang88/bios625package', build_vignettes = T)
library("bios625package")
```
In order to run the packages successfully, this will automatically install some necessary packages so that users can access the vignettes!

Which type of data I should use?
----------------------------------------
The linear regression module is the core of this function. If you misspecify the parameters, the results may be wrong and even exports errors. The *y* should be continuous outcome, and be passed into function as a list or vector. The *x* is a list of predictors which can be a list or vector. The *intr* is a boolean parameter which determines whether the model should include the intercept or not, and the default value is _TURE_. The *predict* can be a vector or a matrix, depending on how many values you want to predict. The function will automatically check the type and dimension of this parameter, when it conducts some transformations, it will output some warning messages to remind users in case the results may be unexpected. The *contrast* can be a vector or a matrix, depending on how many conditions you want to test. The function will automatically check the type and dimension of this parameter, when it conducts some transformations, it will output some warning messages to remind users in case the results may be unexpected. The *alpha* is the significance level whose default level is _0.05_, which is able be changed into any value between 0~1. The *rhs* is the right-hand-side vector for hypothesis with as many entries as rows in the hypothesis matrix; can be omitted, in which case it defaults to a vector of zeroes. 

Examples
-----------------------------------------
These are basic example which shows you how to solve a common problem and illustrate the usage of this function in the package:

```{r example}
data("mtcars")
library(bios625package)
m0 = GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg,intr = T)

m2 = GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, predict = c(6,100,3.75,2.90),intr = T)['Prediction']

m3 = GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, contrast = matrix(c(0,0,1,0,0,0,0,0,1,0),byrow = T,nrow =2),intr = T)["Hypothesis"]

m4 = GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = T,contrast = matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs = 0.8)["Hypothesis"]
```


For more detailed examples or for more information, please use
```{r}
browseVignettes(package = 'bios625package')
```
and click HTML to see more complex examples and how to use these functions in a more complete way. 
