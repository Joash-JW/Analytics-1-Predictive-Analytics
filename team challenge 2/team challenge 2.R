#set working directory here
library(caret)
library(data.table)
library(rpart)
library(rpart.plot)

# set categorical variables
health <- fread("health_ins_cust.csv")
health$sex <- factor(health$sex)
health$marital.stat <- factor(health$marital.stat)
health$housing.type <- factor(health$housing.type)
health$health.ins <- factor(health$health.ins)
health$recent.move <- factor(health$recent.move)
health$is.employed <- factor(health$is.employed)
health$state.of.res <- factor(health$state.of.res)

# data cleaning - negative income & age <18, age >99
health[income == -8700, income:= 8700] 
health[nchar(age) == 1 | nchar(age)>2, age := NA]

# data cleaning - predict NA values
library(missForest)
health.clean <- missForest(health)
health.clean.dt <- data.table(health.clean$ximp)

# get rid of customer id column
health.clean.dt[,custid := NULL]
health.clean.dt[, income.new := income/15000]

# to balance data
customsmote <- list(name = "Custom Smote",
                    func = function (x, y) {
                      library(DMwR)
                      dat <- if (is.data.frame(x)) x else as.data.frame(x)
                      dat$.y <- y
                      dat <- SMOTE(.y ~ ., data = dat, perc.over = 100, 
                                   perc.under = 500, k = 16)
                      list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                           y = dat$.y)
                    },
                    first = TRUE)

#control model training parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        sampling = customsmote, seeds = 0:30)


#logistic regression with 10 fold cross-validation
logm2 <- train(health.ins ~.-income, data = health.clean.dt, 
               method = "glm", metric = "Accuracy", family = binomial, 
               trControl = control)
summary(logm2$finalModel)
# best variables sex, is.employed, marital.status, age, income.new
logm2 <- train(health.ins ~ sex + is.employed + income.new + marital.stat
               + age, data = health.clean.dt, method = "glm", metric = "Accuracy",
               family = binomial, trControl = control)
OR <- exp(coef(logm2$finalModel))
OR
OR.CI <- exp(confint(logm2$finalModel))
OR.CI
predict.logm2 <- predict(logm2, newdata = health.clean.dt)
threshold <- sum(health.clean.dt$health.ins == T)/length(health.clean.dt$health.ins)
confusionMatrix(data=health.clean.dt$health.ins, 
                reference = predict.logm2 , prevalence = threshold)

# CART model with 10 fold cross-validation
tree <- train(health.ins ~. - income, data = health.clean.dt, method = "rpart", 
              metric = "Accuracy", trControl = control)
predict.tree <- predict(tree, newdata = health.clean.dt)
confusionMatrix(data = health.clean.dt$health.ins, predict.tree, 
                prevalence = threshold)
prp(tree$finalModel, type=2, extra=104, nn=T, nn.box.col = 'light blue')
tree$finalModel$cptable
## note everytime clear object from workspace, smote gives different data, hence different results
