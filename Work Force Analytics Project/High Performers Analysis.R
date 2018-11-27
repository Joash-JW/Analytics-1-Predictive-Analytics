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

summary(data)


# subset by performance rating
data.hp <- data[PerformanceRating==4]
data.hp$PerformanceRating <- NULL
summary(data.hp)

# ================= Analysis for high performers ========================
# variable selection
m0 <- glm(Attrition.cat ~.-`Application ID`, data=data.hp, family = binomial)
summary(m0)
#using p-val: Age +BusinessTravel+DistanceFromHome+education+EnvironmentSatis+JobInvolvement+
#JobLevel+JobRole+JobSatisfaction+NumCompaniesWorked+overtime+RelationshipSatisfaction+
#stockoptionlevel+TrainingTimesLastYear+WorkLifeBalance+YearsInCurrentRole+EmployeeSource

formula.fit <- formula(Attrition.cat ~ Age +BusinessTravel+ DistanceFromHome +Education+
                         EnvironmentSatisfaction+JobInvolvement+JobLevel+JobRole+
                         JobSatisfaction+NumCompaniesWorked+OverTime+RelationshipSatisfaction+
                         StockOptionLevel+TrainingTimesLastYear+WorkLifeBalance+
                         YearsInCurrentRole+EmployeeSource)

# to balance data
customsmote <- list(name = "Custom Smote",
                    func = function (x, y) {
                      library(DMwR)
                      dat <- if (is.data.frame(x)) x else as.data.frame(x)
                      dat$.y <- y
                      dat <- SMOTE(.y ~ ., data = dat, perc.over = 100, 
                                   perc.under = 500, k = 46)
                      list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                           y = dat$.y)
                    },
                    first = TRUE)

# set controls 10-fold
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        sampling = customsmote, seeds = 0:30)

#set threshold value
threshold <- sum(data$Attrition.cat == "Left")/length(data$Attrition.cat)

# Logistic Regression
m1 <- train(formula.fit, data=data.hp, method="glm", family=binomial, 
            trControl = control, metric = "Accuracy")
summary(m1$finalModel)
exp(coef(m1$finalModel))
exp(confint(m1$finalModel))
car::vif(m1$finalModel)
predict.m1 <- predict(m1, newdata=data.hp)
confusionMatrix(data=data.hp$Attrition.cat, reference=predict.m1, prevalence=threshold)

#CART
m2 <- train(Attrition.cat ~. -`Application ID`, data=data.hp, method="rpart", 
            trControl = control, metric = "Accuracy")
m2$finalModel$variable.importance
predict.m2 <- predict(m2, newdata=data.hp)
confusionMatrix(data=data.hp$Attrition.cat, reference=predict.m2, prevalence=threshold)

write.csv(exp(coef(m1$finalModel)), "highperformloghp.csv")
write.csv(m2$finalModel$variable.importance, "highperformCARThp.csv")





