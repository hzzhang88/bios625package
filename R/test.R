library("sas7bdat")
library("tidyverse")
library("car")
completedata <- read.sas7bdat("F:/bios650/hw2/completedata.sas7bdat")
# creating categorical variable for age
completedata = mutate(completedata,Age_quartiles = cut(completedata$Age,c(45.00,56.00,64.00,76.00,97.00),include.lowest =TRUE,labels = c(0:3)))

iqr_range <- IQR(completedata$Fatalism)
mean_value <- mean(completedata$Fatalism)
completedata <- completedata %>% mutate(Fatalism_centered = (Fatalism -mean_value)/iqr_range )
m0 = lm(Depression ~  Fatalism_centered + Sex + R_E, data= completedata )
summary(m0)
contrast_matrix = matrix(c(0,1,-1,0,0,0,1,-1),byrow = T, nrow = 2 )
car::linearHypothesis(model = m0, hypothesis.matrix=contrast_matrix, rhs= c(0,0))
