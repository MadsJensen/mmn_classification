setwd("~/Projects/Bochra_klassifikation/Data/Within")


set.seed(42)
library(glmnet)
library(ggplot2)

Coma1 <- read.csv("coma1.csv")
control1 <- read.csv("control1.csv")


foo <- subset(control1, Conditions != "standard")

X <- as.matrix(foo[,2:3])
y <- droplevels(foo[,1])
y <- as.matrix(y)

X <- as.matrix(control1[,2:3])
y <- as.matrix(control1[,1])


cv.model.glmnet  <- cv.glmnet(X, y,
                              family = "multinomial",
                              alpha = 1,
                              nfolds = 10,
                              standardize = TRUE)

(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "response"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)


ggplot(control1, aes(Conditions, Peak)) + 
  geom_boxplot() 

ggplot(control1, aes(Conditions, time2peak)) + 
  geom_boxplot() 
  
