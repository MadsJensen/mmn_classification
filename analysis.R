

set.seed(42)

source("/home/mje/Projects/mmn_classification/make_feature_dataframe.R")

library(MASS)
library(glmnet)
library(nnet)

model.meanData <- polr(group~value, Hess=TRUE, data=meanData)
pred.meanData <- predict(model.meanData, meanData, type="p")

model.mean <- polr(group ~ value, Hess=TRUE, data = data)
model.mean2 <-update(model.mean, .~. + condition)

model.max <- polr(group ~ value, Hess=TRUE, data=maxDataMean)
summary(model.max)

pred.max <- predict(model.max, maxDataMean, type="p")
print(pred.max)

model.std <- polr(group ~ value, Hess=TRUE, data=stdDataMean)
summary(model.std)
pred.std <- predict(model.std, stdDataMean, type="p")
print(pred.std)

model.time <- polr(group ~ value, Hess=TRUE, data=timeDataMean)
summary(model.time)
pred.time <- predict(model.time, timeDataMean, type="p")
print(pred.time)


dataCombine <- cbind(maxDataMean, stdDataMean$value, timeDataMean$value)
names(dataCombine) <- c("subId", "max", "group", "std", "time")

model.comb <- polr(group ~ max + std + time, Hess = TRUE, data = dataCombine)
summary(model.comb)
pred.comb <- predict(model.comb, dataCombine, type="probs")
print(pred.comb)

dataStandard <- dataCombine
dataStandard$max <- scale(dataStandard$max)
dataStandard$std <- scale(dataStandard$std)
dataStandard$time <- scale(dataStandard$time)

model.stand <- polr(group ~ max + std + time, Hess = TRUE, data = dataStandard)
summary(model.stand)
pred.stand <- predict(model.stand, dataStandard, type="probs")
print(pred.stand)






rank_order <- function(predictions) {
    names_frame <- matrix(nrow=dim(predictions)[1],
                          ncol = dim(predictions)[2])
    names_frame <- as.data.frame(names_frame)

    for (i  in seq(predictions[,1])) {
        foo <- sort(-predictions[i,])
        names_frame[i,] <- names(foo)
    }
    
    return(names_frame)
    }


    
names_frame$correct <- group
names(names_frame) <- c("1", "2", "3", "4", "correct")


#### multi reg 

dataCombine$group <- relevel(dataCombine$group, ref="coma")
group <- dataCombine$group



test_mean_null <- multinom(group ~ 1 , data = dataCombine)
test_mean_max <- update(test_mean_null, .~. + max)
test_mean_std <- update(test_mean_null, .~. + std)
test_mean_time <- update(test_mean_null, .~. + time)
test_mean_max_std <- update(test_mean_max, .~. + std)
test_mean_max_time <- update(test_mean_max, .~. + time)
test_mean_std_time <- update(test_mean_std, .~. + time)
test_mean_max_std_time <- update(test_mean_max_std, .~. + time)
test_all <- multinom(group ~ max*std*time, data = dataCombine)

anova(test_mean_null, test_mean_max, test_mean_std, test_mean_time, test_mean_max_std,
      test_mean_max_time, test_mean_std_time, test_mean_max_std_time, test_all)

pred.mean_max_std <- predict(test_mean_max_std, dataCombine, Type="class", se=TRUE)
print(pred.mean_max_std)

table(pred.mean_max_std, group)
mean(pred.mean_max_std == group)     

pred.mean_max_std_time <- predict(test_mean_max_std_time, dataCombine, type="class")
print(pred.mean_max_std_time)

table(pred.mean_max_std_time, group)
mean(pred.mean_max_std_time == group)     




z <- summary(test_mean_max_std)$coefficients/summary(test_mean_max_std)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

head(pp <- fitted(test_mean_max_std))

lpp <- melt(pp.write, id.vars = c("max", "std"), value.name = "probability")
head(lpp)  # view first few rows




test_max<- multinom(group ~ Drt_max + Frq_max + Gap_max + Int_max + Spc_max
                 , data = dataWide)
pred.max <- predict(test_max, dataWide, "probs")


test_std <- multinom(group ~ Drt_std + Frq_std + Gap_std + Int_std + Spc_std
                 , data = dataWide)
pred.std <- predict(test_std, dataWide, "probs")

test_time <- multinom(group ~ Drt_time + Frq_time + Gap_time + Int_time + Spc_time
                 , data = dataWide)
pred.time <- predict(test_time, dataWide, "probs")
 

test_max_std <- multinom(group ~ Drt_max + Frq_max + Gap_max + Int_max + Spc_max +
                 Drt_std + Frq_std + Gap_std + Int_std + Spc_std
                 , data = dataWide)
class.pred.max_std <- predict(test_max_std, dataWide, "class")


test_all <- multinom(group ~ Drt_max + Frq_max + Gap_max + Int_max + Spc_max +
                 Drt_std + Frq_std + Gap_std + Int_std + Spc_std +
                 Drt_time + Frq_time + Gap_time + Int_time + Spc_time
                 , data = dataWide)
pred.all <- predict(test_all, dataWide, "class")


table(class.pred.max_std,  group)

result <- matrix(nrow = 28, ncol = 1)
result <- as.data.frame(result)

for (i in seq(dataWide[,1])) {
    test_data <- dataCombine[i,]
    train_data <- dataCombine[-i,]
    train_model <- multinom(group ~ max + std, data = train_data)
    result[i,] <- predict(train_model, test_data, type = "class")
}


for (i in seq(dataWide[,1])) {
    test_data <- dataWide[i,]
    train_data <- dataWide[-i,]
    train_model <- multinom(group ~
                            Drt_max + Frq_max + Gap_max + Int_max + Spc_max +
                            Drt_std + Frq_std + Gap_std + Int_std + Spc_std +
                            Drt_time + Frq_time + Gap_time + Int_time + Spc_time,
                            data  = train_data)
    result[i,] <- predict(train_model, test_data, type = "response")
}


#### glmnet ####
X <- dataWide[,1:18]
X <- as.matrix(X)
y <-  dataWide$group


model.glmnet <- glmnet(X, y,
                       family = "multinomial",
                       alpha = 1,
                       standardize = FALSE,
                       )
(pred.glmnet <- predict(model.glmnet, newx = X, type = "class"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)

cv.model.glmnet  <- cv.glmnet(X, y,
    family = "multinomial",
    alpha = 0.4,
#   nfolds = 29,
    type.measure="mse",
    standardize = FALSE)
(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)


#### DRT ####

X <- dataWide[,1:3]
X <- as.matrix(X)
cv.model.glmnet  <- cv.glmnet(X, y,
                              family = "multinomial",
                              alpha = 0,
                              nfolds = 29,
                              standardize = TRUE)
(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)


X <- dataWide[,4:6]
X <- as.matrix(X)
cv.model.glmnet  <- cv.glmnet(X, y,
                              family = "multinomial",
                              alpha = 0,
                              nfolds = 29,
                              standardize = TRUE)
(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)

X <- dataWide[,7:9]
X <- as.matrix(X)
cv.model.glmnet  <- cv.glmnet(X, y,
                              family = "multinomial",
                              alpha = 0,
                              nfolds = 29,
                              standardize = TRUE)
(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)

X <- dataWide[,10:12]
X <- as.matrix(X)
cv.model.glmnet  <- cv.glmnet(X, y,
                              family = "multinomial",
                              alpha = 0,
                              nfolds = 29,
                              standardize = TRUE)
(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)

X <- dataWide[,13:15]
X <- as.matrix(X)
cv.model.glmnet  <- cv.glmnet(X, y,
                              family = "multinomial",
                              alpha = 0,
                              nfolds = 29,
                              standardize = TRUE)
(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)


X <- dataWide[,16:18]
X <- as.matrix(X)
cv.model.glmnet  <- cv.glmnet(X, y,
                              family = "multinomial",
                              alpha = 0,
                              nfolds = 29,
                              standardize = TRUE)
(pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class"))
(mean(pred.glmnet == y))
table(pred.glmnet, y)


#### Permutation test ####
    X <- dataWide[,1:18]
    X <- as.matrix(X)
    y <-  dataWide$group
    
    cv.model.glmnet  <- cv.glmnet(X, y,
                                  family = "multinomial",
                                  alpha = 0.5,
                                  #   nfolds = 29,
                                  type.measure="mse",
                                  standardize = FALSE)
    pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class")
    pred_score = mean(pred.glmnet == y)
    
    number_of_permutations = 1000
    
    perm_score = NULL
    for (i in 1 : number_of_permutations) {
        
        # Sample from the combined dataset
        y.random = sample (y, replace = FALSE)
        
        cv.model.glmnet  <- cv.glmnet(X, y,
                                      family = "multinomial",
                                      alpha = 0.5,
                                      #   nfolds = 29,
                                      type.measure="mse",
                                      standardize = FALSE)
        pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class")
        perm_score[i]  = mean(pred.glmnet == y.random)
        
    }
    
    # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
    
    pvalue = sum(abs(perm_score) >= abs(pred_score)) / length(perm_score)
    print (pvalue)



j = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 0.9, 1)
pred_score = NULL
for (i in 1 : length(j)){
    cv.model.glmnet  <- cv.glmnet(X, y,
                                  family = "multinomial",
                                  alpha = j[i],
                                  type.measure="mse",
#                                   nfolds = 29,
                                  standardize = FALSE)
    pred.glmnet <- predict(cv.model.glmnet, newx = X, s = "lambda.min", type = "class")
    pred_score[i] = mean(pred.glmnet == y)
}

