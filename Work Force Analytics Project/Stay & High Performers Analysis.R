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


# ============= High performers whom stays ========================
#only interested in employees who stay
data.stay <- data[Attrition.cat=="Stay"]
data.stay$Attrition.cat <- NULL

# finding profiles ------------------------------------------------
#only interested in profiles
data.stay1 <- data.stay
data.stay1$Department <- NULL
data.stay1$EnvironmentSatisfaction <- NULL
data.stay1$JobInvolvement <- NULL
data.stay1$BusinessTravel<- NULL
data.stay1$JobLevel<- NULL
data.stay1$JobRole<- NULL
data.stay1$JobSatisfaction<- NULL
data.stay1$OverTime<- NULL
data.stay1$PercentSalaryHike<- NULL
data.stay1$RelationshipSatisfaction<- NULL
data.stay1$StockOptionLevel<- NULL
data.stay1$TrainingTimesLastYear<- NULL
data.stay1$YearsAtCompany<- NULL
data.stay1$YearsInCurrentRole<- NULL
data.stay1$YearsWithCurrManager <- NULL
data.stay1$EmployeeSource<- NULL
data.stay1$WorkLifeBalance<- NULL

# variable selection
m6 <- glm(PerformanceRating ~.-`Application ID`, data=data.stay1, family = binomial)
summary(m6)

formula.fit1 <- formula(PerformanceRating ~Education + Gender + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears
                        + YearsSinceLastPromotion)

# to balance data
customsmote <- list(name = "Custom Smote",
                    func = function (x, y) {
                      library(DMwR)
                      dat <- if (is.data.frame(x)) x else as.data.frame(x)
                      dat$.y <- y
                      dat <- SMOTE(.y ~ ., data = dat, perc.over = 100, 
                                   perc.under = 450, k = 100)
                      list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                           y = dat$.y)
                    },
                    first = TRUE)

# set controls: 10-fold with 3 repetition and smote
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=2, 
                        sampling = customsmote, seeds = 0:20)

#set threshold value
threshold <- sum(data.stay1$PerformanceRating == 4)/length(data.stay1$PerformanceRating)

# Logistic Regression
m7 <- train(formula.fit1, data=data.stay1, method="glm", family=binomial, 
            trControl = control, metric = "Accuracy")
summary(m7$finalModel)
exp(coef(m7$finalModel))
exp(confint(m7$finalModel))
car::vif(m7$finalModel)

# CART
m8 <- train(PerformanceRating ~. -`Application ID`, data=data.stay1, method = "rpart",
            trControl = control)
cp.opt <- m8$bestTune$cp
m8$finalModel$variable.importance
rpart.plot::prp(m8$finalModel, type=2, extra=104, nn=T, nn.box.col = 'light blue')

levels(data$Attrition.cat)
levels(data.stay1$PerformanceRating)
