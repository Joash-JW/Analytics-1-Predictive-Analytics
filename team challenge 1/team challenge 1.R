#set working directory here
library(data.table)
library(car)
lawsuit.dt <- fread("Lawsuit.csv")

#data cleaning and factorising
lawsuit.dt[, "sal_inc" := Sal95 - Sal94]
lawsuit.dt$Rank <- factor(lawsuit.dt$Rank, levels = c("3","2", "1"),ordered = T)
lawsuit.dt$Dept <- factor(lawsuit.dt$Dept, ordered = T)
lawsuit.dt$Clin <- factor(lawsuit.dt$Clin, ordered = T)
lawsuit.dt$Cert <- factor(lawsuit.dt$Cert, ordered = T)
lawsuit.dt$Gender <-factor(lawsuit.dt$Gender, ordered = T)

#building linear regression model
lawsuit.full <- lm(sal_inc ~ Gender + Dept + Clin + Cert + Prate + Exper + Rank, data = lawsuit.dt)
lawsuit.null <- lm(sal_inc ~ 1, data = lawsuit.dt)
lawsuit.temp <- lm(sal_inc ~ Gender + Exper + Dept, data = lawsuit.dt)
lawsuit1 <- step(lawsuit.full, formula(lawsuit.null), direction = "backward")
lawsuit2 <- step(lawsuit.null, formula(lawsuit.full), direction = "forward")
lawsuit3 <- step(lawsuit.temp, formula(lawsuit.full), direction = "both")
summary(lawsuit1)
summary(lawsuit2)
summary(lawsuit3)

#checking for multicollinearity
vif(lawsuit1)
vif(lawsuit2)
vif(lawsuit3)

#comparing linear regression models
lawsuit.temp1 <- lm(sal_inc ~ Gender, data = lawsuit.dt)
lawsuit.test <- step(lawsuit.temp1, formula(lawsuit.full), direction = "forward")
results <- c(summary(lawsuit1), summary(lawsuit.test))
which.max(results$adj.r.squared)
results <- c(AIC(lawsuit1, lawsuit.test, k=2))
which.min(results$AIC)
results <- c(BIC(lawsuit1, lawsuit.test))
which.min(results$BIC)

# Calculate Relative Importance for Each Predictor
install.packages("relaimpo")
library(relaimpo)
calc.relimp(law7,type=c("first"),
            rela=TRUE)

anova(law2, law7)

anova(law7, law2)
                                    