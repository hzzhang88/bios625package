test_that("multiplication works", {
  x1 = sample(20:80,10000, replace = T)
  x2 = sample(20:80,10000, replace = T)
  x3 = sample(20:80,10000, replace = T)
  y = rnorm(10000,mean = 0,sd=sqrt(10))

  # check if the coefficient are the same
  expect_equal(as.vector(lm(y~x1+x2+x3)$coefficients['(Intercept)']),
               as.vector(GLH(list(x1,x2,x3),y,intr = T)$coefficient['Intercept','Estimate']))

  # check the warning message of not using prediction and general linear hypothesis
  expect_message(GLH(list(x1,x2,x3),y,intr = T)$coefficient['Intercept','Estimate'],"Didn't use prediction function")

  #check if the p values are the same
  expect_equal(as.vector(car::linearHypothesis(model =lm(y~x1+x2+x3),
              hypothesis.matrix=matrix(c(0,1,-1,0,0,0,1,-1),byrow=T,nrow = 2),rhs=c(0,0))[2,"Pr(>F)"]),
               as.vector(GLH(list(x1,x2,x3),y,
              contrast =matrix(c(0,1,-1,0,0,0,1,-1),byrow=T,nrow = 2),rhs=0 ,intr = T)$Hypothesis[1,'p_value']))

})
