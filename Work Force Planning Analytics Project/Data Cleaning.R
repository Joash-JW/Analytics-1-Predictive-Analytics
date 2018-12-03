library(data.table)
#set working directory here

#Import data file
data <- fread("IBM HR Data.csv")

#summary of raw data
summary(data)

#DATA CLEANING----------------------------------------------------------------------------------------------------
data1 <- data

#EmployeeNumber, Application ID: to integer

warning(as.integer(data1$EmployeeNumber))
warning(as.integer(data1$`Application ID`))
#Warning message:NAs introduced by coercion 
#to see why (using original data):
vec_NA<-data1[(is.na(as.integer(EmployeeNumber))==TRUE | is.na(as.integer(`Application ID`))==TRUE),]
#to find row numbers
which(is.na(as.integer(data1$EmployeeNumber))==TRUE | is.na(as.integer(data1$`Application ID`))==TRUE)
#deleting rows with values similar to 'test'
data1 <- data1[c(-8475,-22920,-23167,-23239,-23467,-23532)]
#app_id = ????? :-23239
#converting the columns to integer type 
#now the coerced NAs are the ones with blank values 
data1$EmployeeNumber <- as.integer(data1$EmployeeNumber)  
data1$`Application ID` <- as.integer(data1$`Application ID`)  

#DistanceFromHome: to integer

warning(as.integer(data1$DistanceFromHome))
#Warning message:NAs introduced by coercion 
#to see why:
vec_NA<-data1[(is.na(as.integer(DistanceFromHome))==TRUE ),]
#to find row numbers
which(is.na(as.integer(data1$DistanceFromHome))==TRUE )
#row 3808 misalligned
data1[3808, 4:ncol(data1)]<-data1[3808, 5:ncol(data1)]
data1[3808,  `Employee Source`:= NA]
#converting the column to integer type 
#now the coerced NAs are the ones with blank values 
data1$DistanceFromHome <- as.integer(data1$DistanceFromHome)  

#HourlyRate: to integer
warning(as.integer(data1$HourlyRate))
#Warning message:NAs introduced by coercion 
#to see why:
vec_NA<-data1[(is.na(as.integer(HourlyRate))==TRUE ),]
#to find row numbers
which(is.na(as.integer(data1$HourlyRate))==TRUE )
#row 6147 misalligned
data1[6147, 10:ncol(data1)]<-data1[6147, 11:ncol(data1)]
data1[6147,  `Employee Source`:= NA]
#now the coerced NAs are the ones with blank values 
data1$HourlyRate <- as.integer(data1$HourlyRate)

#MonthlyIncome : to integer
data1$MonthlyIncome <- as.integer(data1$MonthlyIncome)
#PercentSalaryHike : to double
data1$PercentSalaryHike <- as.double(data1$PercentSalaryHike)
#EmployeeCount : to integer
data1$EmployeeCount <-as.integer(data1$EmployeeCount) 

#Convert to categorical
data1$Attrition <- factor(data1$Attrition)
summary(data1$Attrition)
#Blank Values Convert to NA
data1[Attrition=="", 'Attrition'] <- NA
data1$Attrition <- factor(data1$Attrition)
summary(data1$Attrition)

data1$Education <- factor(data1$Education, ordered = T)
summary(data1$Education)

data1$BusinessTravel <- factor(data1$BusinessTravel)
summary(data1$BusinessTravel)
#Blank Values Convert to NA
data1[BusinessTravel=="", 'BusinessTravel'] <- NA
data1$BusinessTravel <- factor(data1$BusinessTravel)
summary(data1$BusinessTravel)

data1$Department <- factor(data1$Department)
summary(data1$Department)
#Blank Values Convert to NA
data1[Department=="", 'Department'] <- NA
data1$Department <- factor(data1$Department)
summary(data1$Department)

data1$EducationField <- factor(data1$EducationField)
summary(data1$EducationField)
#Delete row with test as value 
data1<-data1[EducationField!='Test']
#Blank Values Convert to NA
data1[EducationField=="", 'EducationField'] <- NA
data1$EducationField <- factor(data1$EducationField)
summary(data1$EducationField)

data1$EnvironmentSatisfaction <- factor(data1$EnvironmentSatisfaction, ordered = T)
summary(data1$EnvironmentSatisfaction)

data1$Gender <- factor(data1$Gender)
summary(data1$Gender)
#Blank Values Convert to NA
data1[Gender=="", 'Gender'] <- NA
data1$Gender <- factor(data1$Gender)
summary(data1$Gender)

data1$JobInvolvement <- factor(data1$JobInvolvement, ordered = T)
summary(data1$JobInvolvement)

data1$JobLevel <- factor(data1$JobLevel, ordered = T)
summary(data1$JobLevel)

data1$JobRole <- factor(data1$JobRole)
summary(data1$JobRole)
#Blank Values Convert to NA
data1[JobRole=="", 'JobRole'] <- NA
data1$JobRole <- factor(data1$JobRole)
summary(data1$JobRole)

data1$JobSatisfaction <- factor(data1$JobSatisfaction, ordered = T)
summary(data1$JobSatisfaction)
#Blank Values Convert to NA
data1[JobSatisfaction=="", 'JobSatisfaction'] <- NA
data1$JobSatisfaction <- factor(data1$JobSatisfaction)
summary(data1$JobSatisfaction)

data1$MaritalStatus<- factor(data1$MaritalStatus)
summary(data1$MaritalStatus)
#Blank Values Convert to NA
data1[MaritalStatus=="", 'MaritalStatus'] <- NA
data1$MaritalStatus <- factor(data1$MaritalStatus)
summary(data1$MaritalStatus)

data1$OverTime <- factor(data1$OverTime)
summary(data1$OverTime)
#Blank Values Convert to NA
data1[OverTime=="", 'OverTime'] <- NA
data1$OverTime <- factor(data1$OverTime)
summary(data1$OverTime)

data1$PerformanceRating <- factor(data1$PerformanceRating)
summary(data1$PerformanceRating)

data1$RelationshipSatisfaction <- factor(data1$RelationshipSatisfaction)
summary(data1$RelationshipSatisfaction)

data1$WorkLifeBalance <- factor(data1$WorkLifeBalance, ordered = T)
summary(data1$WorkLifeBalance)

data1$`Employee Source` <- factor(data1$`Employee Source`)
summary(data1$`Employee Source`)
#Delete row with test as value 
data1<-data1[`Employee Source`!='Test']
#Blank Values Convert to NA
data1[`Employee Source`=="", 'Employee Source'] <- NA
data1$`Employee Source` <- factor(data1$`Employee Source`)
summary(data1$`Employee Source`)


data1$StockOptionLevel <- factor(data1$StockOptionLevel, ordered = T)
summary(data1$StockOptionLevel)

data1$Over18 <- factor(data1$Over18, ordered = T)
summary(data1$Over18)
#Blank Values Convert to NA
data1[Over18=="", 'Over18'] <- NA
data1$Over18 <- factor(data1$Over18)
summary(data1$Over18)



#--------------------------------------------removing duplicated data: 

sum(duplicated(data1))
#14 duplicated rows

library("dplyr")
data1 <- distinct(data1)
#COMMENT: can use unique in the same package



#---------------------------------------------remove irrelevant columns and rows:

#all values are just 1
data1$EmployeeCount<- NULL

#has repeated values, not unique identifier, Application Id is unique identifier
data1$EmployeeNumber<- NULL

#age column already exists, so over18 redundant
data1$Over18 <- NULL

#all values are just 80
data1$StandardHours <- NULL


#COMMENT corr graph and matrix right ? ( cmatrix warning : the standard deviation is zero ), only correl to explain ?

#seeing if hourly, monnthly and daily rates are correlated with any other columns:
library(purrr)
cdt<- data1 %>% keep(is.numeric)   
cmatrix <- cor(cdt,use = "complete.obs")
round(cmatrix, 2)
library(corrplot)
corrplot(cmatrix, method="circle",type = "lower")
#hourly, monnthly and daily rates are not correlated to anything and make no sense
#Remove these 3 columns 
data1$DailyRate <- NULL
data1$MonthlyRate <- NULL
data1$HourlyRate <- NULL


sum(is.na(data1))
#247 rows with NA values
#since probably not accurate and since 247 is very less compared to the total number of rows
#remove rows with NA
data1 <- na.omit(data1)

#since terminated employees are irrelevant to analyzing attrition 
#remove rows that Attrition = Termination 
data1 <- data1[Attrition != "Termination"]
data1$Attrition <- factor(data1$Attrition)




#-----------------------------------------finding outliers in continuous variables:

boxplot(data1$Age) 
boxplot(data1$DistanceFromHome) 
boxplot(data1$PercentSalaryHike)
#no outliers

boxplot(data1$MonthlyIncome) 
#Some outliers at the top but it is acceptable since not many employees have high salaries.

boxplot(data1$NumCompaniesWorked) 
#One outlier with  the companies worked as 9, but this is acceptable since it may be possible. 

boxplot(data1$TotalWorkingYears) 
# Outliers at the top but it is acceptable as total working years can vary. 


boxplot(data1$TrainingTimesLastYear) 
# Some outliers but it is of a reasonable range of 0-6 training times.


boxplot(data1$YearsAtCompany)
boxplot(data1$YearsInCurrentRole)
boxplot(data1$YearsSinceLastPromotion)
boxplot(data1$YearsWithCurrManager)
# Outliers at the top but it is acceptable since there may be employees who may have  stayed very long with companies

#COMMENT : boxplot against cat variables like T and F each have boxplot




#-----------------------------------------Checking if relationships b/w column are valid:

#Age- TotalWorkingYears < 18
data1 <-data1[(Age- TotalWorkingYears < 18)==F]


#TotalWorkingYears < YearsAtCompany, error, remove 
data1[TotalWorkingYears < YearsAtCompany]
data1<- data1[(TotalWorkingYears < YearsAtCompany) == F]

#COMMENT : more relationships checking ?
#num of year at comp = 0 and hike >0




#DATA VISUALIZATION----------------------------------------------------------------------------------------------------

par(mar=c(1,1,1,1))
library(tidyr)
library(ggplot2)

#COMMENT: correlation how all the years are correlated prob cause...
#correlogram using ggplot 
library(ggcorrplot)
ggcorrplot(cmatrix, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram", 
           ggtheme=theme_bw)

#COMMENT: can try with diff variables, or if over crowded can split with x axis or y axis 

#4D bubble plot - education,education field, num of years at company, Monthly income ( per dept)
theme_set(theme_bw())  # pre-set the bw theme.

#Human Resources
g <- ggplot(data1[Department=="Human Resources"], aes(NumCompaniesWorked, MonthlyIncome)) + 
  labs(subtitle="Comp Worked vs Monthly Income",
       title="Bubble chart")

g + geom_jitter(aes(col=EducationField, size=Education)) + 
  geom_smooth(aes(col=EducationField), method="lm", se=F)

#R & D
g <- ggplot(data1[Department=="Research & Development"], aes(NumCompaniesWorked, MonthlyIncome)) + 
  labs(subtitle="Comp Worked vs Monthly Income",
       title="Bubble chart")

g + geom_jitter(aes(col=EducationField, size=Education)) + 
  geom_smooth(aes(col=EducationField), method="lm", se=F)

# R and D, Companies Worked < 2
g <- ggplot(data1[Department=="Research & Development" & NumCompaniesWorked<2], aes(NumCompaniesWorked, MonthlyIncome)) + 
  labs(subtitle="Comp Worked vs Monthly Income",
       title="Bubble chart")

g + geom_jitter(aes(col=EducationField, size=Education)) + 
  geom_smooth(aes(col=EducationField), method="lm", se=F)

#COMMENT : divergence from mode of monthly income ?
#diverging plot - normalized monthly income according to job role 
theme_set(theme_bw())  

data2<-data1
# compute normalized monthly income
data2$inc_z <- round((data2$MonthlyIncome - mean(data2$MonthlyIncome))/sd(data2$MonthlyIncome), 2)  
data2$MonthlyIncome_type <- ifelse(data2$inc_z < 0, "below", "above")  # above / below avg flag
data2 <- data2[order(data2$inc_z), ]  # sort
#data2$JobRole <- factor(data2$JobRole, levels = data2$JobRole)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(data2, aes(x=JobRole, y=inc_z, label=inc_z)) + 
  geom_bar(stat='identity', aes(fill=MonthlyIncome_type), width=.5)  +
  scale_fill_manual(name="Income", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised income from 'data2'", 
       title= "Diverging Bars") + 
  coord_flip()

#COMMENT: area graph percent sal hike vs Monthly income, use mode as baseline  

# dumbell years since promotion against job level

#install.packages("tidyselect")
library(ggalt)
theme_set(theme_classic())

#calculate  avg monthly income and avg % hike for each job level
data3 <- data2 %>%
  select(JobLevel, MonthlyIncome,PercentSalaryHike) %>%
  group_by(JobLevel) %>%
  summarise(MonthlyIncome = mean(MonthlyIncome), PercentSalaryHike = mean(PercentSalaryHike))
#finding out last year Monthly income using the obtained avg parameters for each job level
data3$IncomeLastYear <- data3$MonthlyIncome/(1+data3$PercentSalaryHike/100)

gg <- ggplot(data3, aes(x=IncomeLastYear, xend=MonthlyIncome, y=JobLevel, group=JobLevel)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                point.colour.l="#0e668b") + 
  scale_x_continuous(label=waiver()) + 
  labs(x=NULL, 
       y=NULL, 
       title="Dumbbell Chart"
       #, 
       #subtitle="Pct Change: 2013 vs 2014", 
       #caption="Source: https://github.com/hrbrmstr/ggalt"
       ) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)



#histograms - all the numeric columns: 
dev.off()
data1 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
#COMMENT: using `bins = 30`warning 


#categorywise barchart - distribution of overtime over department

# From on a categorical column variable
g <- ggplot(data1, aes(Department))
g + geom_bar(aes(fill=OverTime), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart"
       #, 
       #subtitle="Manufacturer of vehicles", 
       #caption="Source: Manufacturers from 'mpg' dataset"
       )

#COMMENT: Proportional Histogram - total working years over Job Role 

#Density Plot - Training time Last year for each Job Role 
g <- ggplot(data1, aes(TrainingTimesLastYear))
g + geom_density(aes(fill=factor(JobRole)), alpha=0.8) + 
  labs(title="Density plot"
       #, 
       #subtitle="City Mileage Grouped by Number of cylinders",
       #caption="Source: mpg",
       #x="City Mileage",
       #fill="# Cylinders"
       )

#Box Plot - Year at company for each Dept 
g <- ggplot(data1, aes(Department, YearsAtCompany))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot"
       #, 
       #subtitle="City Mileage grouped by Class of vehicle",
       #caption="Source: mpg",
       #x="Class of Vehicle",
       #y="City Mileage"
       )

#LOllipop Graph - overall satisfactiom per Job Role 

#COMMENT : all ranking graph can order from biggest to smallest
#COMMENT: why so many Lollipop

data2$TotalSatis<-as.integer(data2$EnvironmentSatisfaction)+as.integer(data2$JobSatisfaction)+as.integer(data2$RelationshipSatisfaction)+as.integer(data2$JobInvolvement)+as.integer(data2$WorkLifeBalance)

ggplot(data2, aes(x=JobRole, y=TotalSatis)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=JobRole, 
                   xend=JobRole, 
                   y=0, 
                   yend=TotalSatis)) + 
  labs(title="Lollipop Chart", 
       subtitle="Job Role Vs Total Satisfaction", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#COMMENT: pyramid - how many people are current employee ( retained ) at each satis level / each year in company / cat variable 

#Pyramid - 


#COMMENT: order pyramid

ggplot(data2, aes(x = JobRole , y =TotalSatis , fill = Attrition)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = waiver(),   # Breaks
                     labels = waiver()) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Pyramid") +
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette


