# setwd here
library(data.table)

#Import data file
data <- fread("IBM HR Data - Cleaned.csv")

data$Attrition.cat <- factor(data$Attrition.cat)
data$BusinessTravel <- factor(data$BusinessTravel)
data$Department <- factor(data$Department)
data$Education <- factor(data$Education, ordered = T)
data$EducationField <- factor(data$EducationField)
data$EnvironmentSatisfaction <- factor(data$EnvironmentSatisfaction, ordered = T)
data$Gender <- factor(data$Gender)
data$JobInvolvement <- factor(data$JobInvolvement, ordered = T)
data$JobLevel <- factor(data$JobLevel, ordered = T)
data$JobRole <- factor(data$JobRole)
data$JobSatisfaction <- factor(data$JobSatisfaction, ordered = T)
data$MaritalStatus<- factor(data$MaritalStatus)
data$OverTime <- factor(data$OverTime)
data$PerformanceRating <- factor(data$PerformanceRating, ordered = T)
data$RelationshipSatisfaction <- factor(data$RelationshipSatisfaction, ordered = T)
data$StockOptionLevel <- factor(data$StockOptionLevel, ordered = T)
data$WorkLifeBalance <- factor(data$WorkLifeBalance, ordered = T)
data$`EmployeeSource` <- factor(data$`EmployeeSource`)


# relevel marital status
data$MaritalStatus <- relevel(data$MaritalStatus, ref = "Single")

# splitting train test set
library(caTools)
set.seed(2004)
train <- sample.split(Y = data$Attrition.cat, SplitRatio = 0.7)
trainset <- subset(data, train == T)
testset <- subset(data, train == F)

# variable selection
m1 <- glm(Attrition.cat ~., data=trainset, family = binomial)
summary(m1)
#using p-val: age,BusinessTravel,dept,distfromhome,education, environsatis
#gender,jobinv,joblevel,jobrole,jobsatis,marital,nocompaniesworked,overtime,r/ssatis,
#stockoption,totalworkingyears, traingtimeslastyr, worklifebalance,
#yearsincurrentrole,yearssincelastpromo,yearswithcurrmanager

formula.fit <- formula(Attrition.cat ~ Age+BusinessTravel+ Department + 
                         DistanceFromHome+Education+EnvironmentSatisfaction + 
                         Gender+JobInvolvement + JobLevel + JobSatisfaction + JobRole+
                         MaritalStatus+NumCompaniesWorked + OverTime + 
                         RelationshipSatisfaction +StockOptionLevel + TotalWorkingYears+
                         TrainingTimesLastYear + WorkLifeBalance+YearsInCurrentRole+
                         YearsSinceLastPromotion+YearsWithCurrManager)

# to balance data
customsmote <- list(name = "Custom Smote",
                    func = function (x, y) {
                      library(DMwR)
                      dat <- if (is.data.frame(x)) x else as.data.frame(x)
                      dat$.y <- y
                      dat <- SMOTE(.y ~ ., data = dat, perc.over = 100, 
                                   perc.under = 470, k = 100)
                      list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                           y = dat$.y)
                    },
                    first = TRUE)

# set controls: 10-fold with 3 repetition and smote
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=1, 
                        sampling = customsmote, seeds = 0:10)

#set threshold value
threshold <- sum(data$Attrition.cat == "Left")/length(data$Attrition.cat)

# Logistic Regression
m1 <- train(formula.fit, data=trainset, method="glm", family=binomial, 
            trControl = control, metric = "Accuracy")

car::vif(m1$finalModel)
summary(m1$finalModel)

exp(coef(m1$finalModel))
exp(confint(m1$finalModel)) 
predict.m1 <- predict(m1, newdata=testset)
confusionMatrix(data=testset$Attrition.cat, reference=predict.m1, prevalence=threshold)
#0.8453


formula.fit2 <- formula(Attrition.cat ~ Age+BusinessTravel+
                         DistanceFromHome+Education+EnvironmentSatisfaction + 
                         Gender+JobInvolvement + JobLevel + JobSatisfaction + JobRole+
                         MaritalStatus+NumCompaniesWorked + OverTime + 
                         RelationshipSatisfaction +StockOptionLevel + TotalWorkingYears+
                         TrainingTimesLastYear + WorkLifeBalance+YearsInCurrentRole+
                         YearsSinceLastPromotion+YearsWithCurrManager)


m1vif <- train(formula.fit2, data=trainset, method="glm", family=binomial, 
               trControl = control, metric = "Accuracy")

summary(m1vif$finalModel)
car::vif(m1vif$finalModel)
exp(coef(m1vif$finalModel))
exp(confint(m1vif$finalModel)) 
predict.m1vif <- predict(m1vif, newdata=testset)
confusionMatrix(data=testset$Attrition.cat, reference=predict.m1vif, prevalence=threshold)
#0.8439




# CART
m2 <- train(Attrition.cat ~. -`Application ID`, data=trainset, method = "rpart",
            trControl = control)
cp.opt <- m2$bestTune$cp
m2$finalModel$variable.importance
rpart.plot::prp(m2$finalModel, type=2, extra=104, nn=T, nn.box.col = 'light blue')
predict.m2 <- predict(m2, newdata=testset)
confusionMatrix(data=testset$Attrition.cat, reference=predict.m2, prevalence=threshold)
#0.8447

# randomForest
# to get estimate of mtry value, tuning parameter for randomforest
# mtry is normally about square root of number of predictors (30-2(attrition-applicationid))
randomForest::tuneRF(trainset, trainset$Attrition.cat, ntreeTry = 500, 
                     mtryStart = floor(sqrt(28)))
# mtry = 3
control <- trainControl(method="repeatedcv", number=10, repeats=1, seeds = 0:10)
m3 <- train(Attrition.cat ~. -`Application ID`, data=trainset, method="rf", metric="Accuracy",
            trControl = control, tuneGrid = data.frame(mtry = 3))
plot(m3$finalModel)
randomForest::varImpPlot(m3$finalModel, main="Variable Importance (randomForest model)")
predict.m3 <- predict(m3, newdata=testset)
confusionMatrix(data=testset$Attrition.cat, reference=predict.m3, prevalence=threshold)
# 0.9946
