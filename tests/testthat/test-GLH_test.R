test_that("multiplication works", {
  x1 = sample(20:80,10000, replace = T)
  x2 = sample(20:80,10000, replace = T)
  x3 = sample(20:80,10000, replace = T)
  y = rnorm(10000,mean = 0,sd=sqrt(10))
  data("mtcars")
  library(car)
  # check if the coefficient are the same
  expect_equal(as.vector(lm(y~x1+x2+x3)$coefficients['(Intercept)']),
               as.vector(GLH(list(x1,x2,x3),y,intr = T)$coefficient['Intercept','Estimate']))
  expect_equal(as.vector(lm(y~-1+x1+x2+x3)$coefficients['x1']),
               as.vector(GLH(list(x1,x2,x3),y,intr = FALSE)$coefficient['beta1','Estimate']))
  # expect_equal(as.vector(lm(y~-1+x1)$coefficients['x1']),
  #              as.vector(GLH(x1,y,intr = FALSE)$coefficient['beta1','Estimate']))
  # check the warning message of not using prediction and general linear hypothesis
  expect_message(GLH(list(x1,x2,x3),y,intr = T)$coefficient['Intercept','Estimate'],"Didn't use prediction function")
  expect_message(GLH(list(x1,x2,x3),y,intr = T)$coefficient['Intercept','Estimate'],"Didn't use the general linear hypothesis function")
  #check if the p values are the same
  expect_equal(as.vector(car::linearHypothesis(model =lm(mtcars$mpg~mtcars$cyl+mtcars$hp+mtcars$drat+mtcars$wt),hypothesis.matrix=matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs=0.8)[2,"Pr(>F)"]),
               as.vector(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = TRUE,contrast = matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs = 0.8)$Hypothesis[1,"p_value"]))

  expect_equal(as.vector(car::linearHypothesis(model =lm(mtcars$mpg~mtcars$cyl+mtcars$hp+mtcars$drat+mtcars$wt),hypothesis.matrix=matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs=0.8)[2,"Pr(>F)"]),
               as.vector(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = TRUE,contrast = matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs =matrix(c(0.8),1,1))$Hypothesis[1,"p_value"]))

  expect_equal(as.vector(car::linearHypothesis(model =lm(mtcars$mpg~mtcars$cyl+mtcars$hp+mtcars$drat+mtcars$wt),hypothesis.matrix=matrix(c(0,0,1,-1,0,0,0,0,1,-1),byrow = T,nrow =2),rhs=c(0.8,0.8))[2,"Pr(>F)"]),
               as.vector(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = TRUE,contrast = matrix(c(0,0,1,-1,0,0,0,0,1,-1),byrow = T,nrow =2),rhs =c(0.8,0.8))$Hypothesis[1,"p_value"]))

  # expect_equal(as.vector(round(0.2493,4)),round(as.vector(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = TRUE,
  #                                                             contrast = matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs = matrix(c(0.8),1,1))$Hypothesis[1,"p_value"]),4))
  #
  #
  # expect_equal(as.vector(round(0.2493,4)),round(as.vector(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = TRUE,
  #                                   contrast = matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs = 0.8)$Hypothesis[1,"p_value"]),4))
  # expect_equal(as.vector(round(0.2493,4)),round(as.vector(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = TRUE,
  #                                                             contrast = matrix(c(0,0,1,-1,0),byrow = T,nrow =1),rhs = matrix(c(0.8),1,1))$Hypothesis[1,"p_value"]),4))
  #
  # expect_equal(as.vector(round(0.04809,4)),round(as.vector(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = TRUE,
  #                                                             contrast = matrix(c(0,0,1,-1,0,0,0,0,1,-1),byrow = T,nrow =2),rhs = c(0.8,0.8))$Hypothesis[1,"p_value"]),4))
  # # check the warning messages
  expect_warning(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, predict = c(6,100,3.75,2.90),intr = T),
               "Automatically covert the predict into a matrix,please check whether the result is expected")
  expect_warning(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, predict = c(6,100,3.75,2.90),intr = T),
              "Automatically append 1s into the prediction matrix, please check whether the result is expected")

  expect_warning(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = T,contrast = c(0,0,1,-1,0),rhs = 0.8),
                 "Automatically covert the predict into a 1 row contrast matrix, please check whether the result is expected")
  expect_warning(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = T,contrast = matrix(c(0,1,-1,0),byrow = T,nrow =1),rhs = 0.8),
                  "Automatically append 0s into the contrast matrix, please check whether the result is expected")

  #check the errors
  expect_error(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, predict = c(6,100),intr = T),
                "The predict matrix does not match the correct dimmensions")
  #
  expect_error(GLH(x= list(mtcars$cyl,mtcars$hp,mtcars$drat,mtcars$wt), y = mtcars$mpg, intr = T,contrast = matrix(c(1,-1,0),byrow = T,nrow =1),rhs = 0.8),
               "The contrast matrix does not match the correct dimmensions")

})
