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
y <- as.matri((x(control1[,1])


cv.model.glmnet  <- cv.glmnet(X, y,
                              family = "multinomial",
                              alpha = .5,
                              nfolds = 10,
                              standardize = F)
(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "response"))


(mean(pred.glmnet == y))
table(pred.glmnet, y)


ggplot(control1, aes(Conditions, Peak)) + 
  geom_boxplot() 

ggplot(control1, aes(Conditions, time2peak)) + 
  geom_boxplot() 
  
n=500;p=30
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta3=matrix(rnorm(30),10,3)
beta3=rbind(beta3,matrix(0,p-10,3))
f3=x%*% beta3
p3=exp(f3)
p3=p3/apply(p3,1,sum)
g3=rmult(p3)
set.seed(10101)
cvfit=cv.glmnet(x,g3,family="multinomial")
plot(cvfit)
title("Multinomial Family",line=2.5)