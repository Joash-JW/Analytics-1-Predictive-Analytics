# **Analytics 1: Predictive Analytics - Predictive Analytics using R programming**

### Consist of codes on Data Cleaning (e.g to balance imbalance data when we are interested in rare cases) and model building such as Linear Regression, Logistic Regression and Classification and Regression Tree.

#### List of Projects:
 - #### [Team Challenge 1](https://github.com/Joash-JW/Analytics-1-Predictive-Analytics/blob/master/README.md#team-challenge-1-1)
 - #### [Team Challenge 2](https://github.com/Joash-JW/Analytics-1-Predictive-Analytics/blob/master/README.md#team-challenge-2-1)
 - #### [Work Force Planning Analytics Project](https://github.com/Joash-JW/Analytics-1-Predictive-Analytics/blob/master/README.md#work-force-planning-analytics-project-1)

##### Team Challenge 1:
  - United States District Court of Houston had a case that arises under Title VII of the Civil Rights Act of 1964, 42 U.S.C. 200e et seq.  The plaintiffs in this case were all female doctors at Houston College of Medicine who claimed that the College has engaged in a pattern and practice of discrimination against women in giving promotions and setting salaries. [Click here for more details of the case](https://www.kaggle.com/hjmjerry/gender-discrimination/home)
  -This project aims to determine whether there is gender discrimination or not through Data Analytics. Through analysing the factors in the data, a Linear Regression model is then developed (gender variable dropped) by extracting the important factors that affect the salary increment. The model is then validated by checking for multicollinearity. Another Linear Regression model which included gender is developed and we compared the 2 models using Adjusted-R^2, AIC, BIC and ANOVA.
  - Data: Lawsuit.csv
  - Packages used:
    - data.table
    - car
    - relaimpo

##### Team Challenge 2: 
  - A project to determine factors that affect whether a person has health insurance or not. In this project, Logistic Regression and Classification and Regression Tree is used to predict a binary outcome (have health insurance or not). The two models are then compared and their respective predictive accuracy are displayed through a confusion matrix. In the data, majority have health insurance and SMOTE (Synthetic Minority Over-sampling Technique) was used with cross-validation to balance the data.
  - Data: health_ins_cust.csv
  - Packages used:
    - caret
    - data.table
    - rpart
    - rpart.plot
    - missForest
    - DMwR
    
##### Work Force Planning Analytics Project:
 - This project aims to determine factors that affect Attrition in companies and whether a certain employee will leave a company given its data. We first performed Data Cleaning and Exploration on our dataset. We then built a model based on the data to determine whether a person will leave the company or not. This is done through the use of Logistic Regression Model, Classification and Regression Tree Model and RandomForest Model. 
 - After obtaining our model for employees in general, we then performed K-means clustering on our dataset based on Age to group them into 2 groups of employees. The 2 groups are roughly the Age group of Millennials and Non-Millennials. We then analyzed factors which affects Attrition based on that Age group. This is done through comparing Logistic Regression and Classification and Regression Tree to obtain significant factors (factors with High Odds Ratio and Variable Importance).
 - Other than analyzing Age, we analyzed High Performing Employees as well, since most companies are interested in Attrition of High Performers. The same was done to obtain significant variables that affect Attrition for High Performers.
 - Data: [IBM HR Data.csv](https://www.kaggle.com/dgokeeffe/ibm-hr-wmore-rows) (Dataset after Data Cleaning: IBM HR Data - Cleaned.csv)
 - Note that the data obtained is fake.
 - Packages used:
   - caret
   - data.table
   - DMwR
   - caTools
   - purrr
   - corrplot
   - tidyr
   - ggplot2
   - ggcorrplot
   - ggalt
   - dplyr
