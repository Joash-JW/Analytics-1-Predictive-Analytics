# **Analytics 1: Predictive Analytics - Predictive Analytics using R programming**

### Consist of codes on Data Cleaning (e.g to balance imbalance data when we are interested in rare cases) and model building such as Linear Regression, Logistic Regression and Classification and Regression Tree.

#### Team Challenge 1
#### Team Challenge 2
#### Work Force Analytics Project

##### Team Challenge 1:
  - United States District Court of Houston had a case that arises under Title VII of the Civil Rights Act of 1964, 42 U.S.C. 200e et seq.  The plaintiffs in this case were all female doctors at Houston College of Medicine who claimed that the College has engaged in a pattern and practice of discrimination against women in giving promotions and setting salaries. [Click here for more details of the case](https://www.kaggle.com/hjmjerry/gender-discrimination/home)
  -This project aims to determine whether there is gender discrimination or not through Data Analytics. Through analysing the factors in the data, a Linear Regression model is then developed (gender variable dropped) by extracting the important factors that affect the salary increment. The model is then validated by checking for multicollinearity. Another Linear Regression model which included gender is developed and we compared the 2 models using Adjusted-R^2, AIC, BIC and ANOVA.
  - Packages used:
    - data.table
    - car
    - relaimpo
  - Data: Lawsuit.csv

##### Team Challenge 2: 
  - A project to determine factors that affect whether a person has health insurance or not. In this project, Logistic Regression and Classification and Regression Tree is used to predict a binary outcome (have health insurance or not). The two models are then compared and their respective predictive accuracy are displayed through a confusion matrix. In the data, majority have health insurance and SMOTE (Synthetic Minority Over-sampling Technique) was used with cross-validation to balance the data.
  - Packages used:
    - caret
    - data.table
    - rpart
    - rpart.plot
    - missForest
    - DMwR
  - Data: health_ins_cust.csv
