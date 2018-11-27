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
highperformer <- data[PerformanceRating == 4]

#cluster age 
agecluster <- kmeans(highperformer$Age, centers = 2)
clusteredbyage <- highperformer[,cluster:=agecluster$cluster]
agecluster1 <- clusteredbyage[cluster==1]
agecluster2 <- clusteredbyage[cluster==2]

summary(agecluster1$Age)
summary(agecluster2$Age)

# subset by age group
millennials.hp <- highperformer[Age<=35]
nonmillennials.hp <- highperformer[Age>35]
millennials.hp$Age <- NULL
nonmillennials.hp$Age <- NULL
millennials.hp$PerformanceRating <- NULL
nonmillennials.hp$PerformanceRating <- NULL

# ================= Analysis for millennials who are high performers ========================
# variable selection
m0 <- glm(Attrition.cat ~.-`Application ID`, data=millennials.hp, family = binomial)
summary(m0)
#using p-val: biztrav, education, jobinv, jobrole, jobsatis, marital, monthlyincome
#nocompaniesworked,overtime,r/ssatis,stockoption, traingtimeslastyr
#yearsatcompany,yearsincurrentrole,empsource

formula.fit <- formula(Attrition.cat ~ BusinessTravel+Education+
                         JobInvolvement+JobRole+JobSatisfaction + 
                         MaritalStatus+MonthlyIncome+OverTime+RelationshipSatisfaction+
                         NumCompaniesWorked+StockOptionLevel+TrainingTimesLastYear+
                         YearsInCurrentRole +YearsAtCompany+EmployeeSource)

# to balance data
customsmote <- list(name = "Custom Smote",
                    func = function (x, y) {
                      library(DMwR)
                      dat <- if (is.data.frame(x)) x else as.data.frame(x)
                      dat$.y <- y
                      dat <- SMOTE(.y ~ ., data = dat, perc.over = 100, 
                                   perc.under = 500, k = 30)
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
m1 <- train(formula.fit, data=millennials.hp, method="glm", family=binomial, 
            trControl = control, metric = "Accuracy")
summary(m1$finalModel)
exp(coef(m1$finalModel))
exp(confint(m1$finalModel))
car::vif(m1$finalModel)
predict.m1 <- predict(m1, newdata=millennials.hp)
confusionMatrix(data=millennials.hp$Attrition.cat, reference=predict.m1, prevalence=threshold)

# CART
m2 <- train(Attrition.cat ~. -`Application ID`, data=millennials.hp, method="rpart", 
            trControl = control, metric = "Accuracy")
m2$finalModel$variable.importance
predict.m2 <- predict(m2, newdata=millennials.hp)
confusionMatrix(data=millennials.hp$Attrition.cat, reference=predict.m2, prevalence=threshold)

# export model for millennials to csv
write.csv(exp(coef(m1$finalModel)), "millennialsloghp.csv")
write.csv(m2$finalModel$variable.importance, "millennialsCARThp.csv")

# ==================== Analysis for non-millennials ==========================
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

# variable selection
m3 <- glm(Attrition.cat ~. -`Application ID`, data=nonmillennials.hp, family = binomial)
summary(m3)
#using p-val: biztrav,distfromhome,education, jobinv, joblevel
#jobrole,jobsatis,marital, overtime,r/ssatis,stockoption,empsource

formula.fit1 <- formula(Attrition.cat ~ BusinessTravel+DistanceFromHome+
                          Education+JobInvolvement+JobLevel+JobRole+
                          JobSatisfaction+MaritalStatus+OverTime+
                          RelationshipSatisfaction+StockOptionLevel+EmployeeSource)

# Logistic Regression
m3 <- train(formula.fit1, data=nonmillennials.hp, method="glm", family=binomial, 
            trControl = control, metric = "Accuracy")
summary(m3$finalModel)
exp(confint(m3$finalModel))
exp(coef(m3$finalModel))
car::vif(m3$finalModel)
predict.m3 <- predict(m3, newdata=nonmillennials.hp)
confusionMatrix(data=nonmillennials.hp$Attrition.cat, reference=predict.m3, prevalence=threshold)

# CART
m4 <- train(Attrition.cat ~. -`Application ID`, data=nonmillennials.hp, method="rpart", 
            trControl = control, metric = "Accuracy")
m4$finalModel$variable.importance
predict.m4 <- predict(m4, newdata=nonmillennials.hp)
confusionMatrix(data=nonmillennials.hp$Attrition.cat, reference=predict.m4, prevalence=threshold)

#export better model for non-millennials to csv
write.csv(exp(coef(m3$finalModel)), "nonmillennialsloghp.csv")
write.csv(m4$finalModel$variable.importance, "nonmillennialsCARThp.csv")
