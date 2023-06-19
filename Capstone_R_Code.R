
# Capstone R Code

# load libraries
library(dplyr)
library(anytime)
library(readr)
library(data.table)
library(ISLR)
library(MASS)
library(car)
library(pROC)
library(gbm)
library(GGally)

# Load 2 datasets

# Voter File
vf_data<-fread("C:/Users/peter/Downloads/DS 785/ncvoter_Statewide/ncvoter_Statewide.txt", select = c("ncid","status_cd","voter_status_desc","voter_status_reason_desc", "state_cd","registr_dt","race_code","ethnic_code","party_cd","gender_code","birth_year","age_at_year_end","drivers_lic"))

# Vote History
vh_data<-fread("C:/Users/peter/Downloads/DS 785/ncvhis_Statewide/ncvhis_Statewide.txt", select = c("ncid","election_lbl","voting_method","voted_county_id"))
# 33,831,528

# Keep only 2020 voters
vh_data<-vh_data[vh_data$election_lbl=="11/03/2020"]
# 5,542,959





### Data Cleaning ###

# Combine datasets into one
raw_data <- merge(vf_data,vh_data, by='ncid', all.x=TRUE)
raw_data <- unique(raw_data)

# Convert registr_dt to Date
raw_data$registr_dt <- as.Date(raw_data$registr_dt, "%m/%d/%Y")

# Only keep new voters
raw_data<- raw_data %>%
  filter(registr_dt >= anydate("01/01/2020") & registr_dt <= anydate("11/1/2020"))
# 890,757

# Remove inactive/dead voters
raw_data<-raw_data[raw_data$voter_status_reason_desc!=c("DENIED","REMOVED","DECEASED")]
# 1,335,081


# Take a random sample of 50k
set.seed(123, sample.kind = "Rounding")
subset_data<- raw_data[sample(nrow(raw_data),50000),]

table(subset_data$Voted2020)

# Create new df using only columns needed
subset_data<-subset_data[,c("ncid","status_cd","registr_dt","race_code","ethnic_code","party_cd","gender_code","age_at_year_end","drivers_lic","election_lbl")]

# Create dependent variable, identifying whether or not someone voted
subset_data <- subset_data %>%
  mutate(Voted2020 = case_when(
    endsWith(election_lbl,"0") ~ 1,
    is.na(election_lbl) ~ 0
  ))

# Data types of each variable
sapply(raw_data,class)

# Create new variable extracting only the month from registration date
subset_data$registration_month<- strftime(subset_data$registr_dt,"%m")

# Create new df of cleaned columns
subset_data2<-subset_data[,c("ncid","Voted2020","age_at_year_end")]

# Make dummy variables for all categorical predictors
subset_data2$Male <- ifelse(subset_data$gender_code == 'M', 1, 0) 
subset_data2$Female <- ifelse(subset_data$gender_code == 'F', 1, 0)

subset_data2$Race_White <- ifelse(subset_data$race_code == 'W', 1, 0)
subset_data2$Race_Black <- ifelse(subset_data$race_code == 'B', 1, 0)
subset_data2$Race_Asian <- ifelse(subset_data$race_code == 'A', 1, 0)
subset_data2$Race_AmericanIndian <- ifelse(subset_data$race_code == 'I', 1, 0)
subset_data2$Race_PacificIslander <- ifelse(subset_data$race_code == 'P', 1, 0)
subset_data2$Race_Mixed <- ifelse(subset_data$race_code == 'M', 1, 0)
subset_data2$Race_Other <- ifelse(subset_data$race_code == 'O', 1, 0)

subset_data2$Ethnicity_Hispanic <- ifelse(subset_data$ethnic_code == 'HL', 1, 0)
subset_data2$Ethnicity_NotHispanic <- ifelse(subset_data$ethnic_code == 'NL', 1, 0)

subset_data2$Party_DEM <- ifelse(subset_data$party_cd == 'DEM', 1, 0)
subset_data2$Party_REP <- ifelse(subset_data$party_cd == 'REP', 1, 0)
subset_data2$Party_LIB <- ifelse(subset_data$party_cd == 'LIB', 1, 0)

subset_data2$RegMonth_JAN <- ifelse(subset_data$registration_month == '01', 1, 0)
subset_data2$RegMonth_FEB <- ifelse(subset_data$registration_month == '02', 1, 0)
subset_data2$RegMonth_MAR <- ifelse(subset_data$registration_month == '03', 1, 0)
subset_data2$RegMonth_APR <- ifelse(subset_data$registration_month == '04', 1, 0)
subset_data2$RegMonth_MAY <- ifelse(subset_data$registration_month == '05', 1, 0)
subset_data2$RegMonth_JUN <- ifelse(subset_data$registration_month == '06', 1, 0)
subset_data2$RegMonth_JUL <- ifelse(subset_data$registration_month == '07', 1, 0)
subset_data2$RegMonth_AUG <- ifelse(subset_data$registration_month == '08', 1, 0)
subset_data2$RegMonth_SEP <- ifelse(subset_data$registration_month == '09', 1, 0)
subset_data2$RegMonth_OCT <- ifelse(subset_data$registration_month == '10', 1, 0)
subset_data2$RegMonth_NOV <- ifelse(subset_data$registration_month == '11', 1, 0)

# Create final cleaned dataset to be used
data <- subset_data2[,-c("ncid")]

# Check for normality in quantitative variable
hist(data$age_at_year_end, main="Age Distribution in Sample",col="light blue",xlab="Age",ylab="Count")
# Left skewed

# Log transform to create a normal distribution
data$age_at_year_end<-log(data$age_at_year_end+1)

# Should be more normally distributed now
hist(data$age_at_year_end, main="Age Distribution in Sample",col="light blue",xlab="Age",ylab="Count")
# Left skewed

table(subset_data2$Voted2020)


###############################################################################

# Machine Learning Method 1: Logistic Regression

# Base model
glm.model = glm(Voted2020 ~., data=data,family = "binomial")
summary(glm.model)


# Find optimum predictors
library(MASS)
step.model <- glm.model %>% stepAIC(trace = FALSE)
coef(step.model)


# New base model with only optimum predictors
glm.model <- glm(Voted2020 ~. -RegMonth_NOV-Party_LIB, data=data, family = "binomial")
summary(glm.model)

# Check for VIF
library(car)
vif(glm.model)


# Run new model with predictors less than a VIF of 10
glm.model <- glm(Voted2020 ~ age_at_year_end + Female + Race_White + Race_Black  + Race_AmericanIndian + Race_Mixed + Race_Other + Ethnicity_Hispanic + Ethnicity_NotHispanic + Party_DEM + Party_REP, data=data, family = "binomial")
summary(glm.model)


# Cross Validation for Logistic Regression
n=dim(data)[1]
k=5
groups=c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))

set.seed(123, sample.kind = "Rounding")
cvgroups=sample(groups,n)

predictvals=rep(-1,n)

for(i in 1:k){
  groupi=(cvgroups==i)
  fit=glm(Voted2020 ~ age_at_year_end + Female + Race_White + Race_Black  + Race_AmericanIndian + Race_Mixed + Race_Other + Ethnicity_Hispanic + Ethnicity_NotHispanic + Party_DEM + Party_REP, data=data[!groupi,],family="binomial")
  predictvals[groupi]=predict(fit,data[groupi,],type="response")
}



# ROC Curve
myroc=roc(response=data$Voted2020,predictor=predictvals)
plot.roc(myroc)

auc(myroc)


# Table
table(predictvals>0.6,data$Voted2020)
predicted.classes <- ifelse(predictvals > .6, "pos", "neg")
# Correctly predicted: 40,678 81.36%
# Misclassification Rate: 9,322



###############################################################################

# Machine Learning Method 2: Linear Discriminant Analysis

# Base LDA Model
n = dim(data)[1]

model.lda = lda(Voted2020 ~., data=data)

par(mfrow=c(2,2))
plot(model.lda)

plot(model.lda, data$Voted2020)

fittedclass2 = predict(model.lda,data=data)$class
table(data$Voted2020,fittedclass2)
Error = sum(data$Voted2020 != fittedclass2)/n; Error


# Check class distributions for both outcomes
plot(model.lda, type="both",main="Discriminant Function Values")


# Cross Validation
n=dim(data)[1]
k=5
groups=c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))

set.seed(123, sample.kind = "Rounding")
cvgroups=sample(groups,n)

predictvals2=rep(-1,n)

for (i in 1:k)  {
  groupi=(cvgroups==i)
  fit2=lda(Voted2020 ~., data=data[!groupi,])
  newdata = data.frame(data[cvgroups==i,])
  predictvals2[cvgroups==i] = as.character(predict(fit2,newdata)$class)
}


# Model Summary
model.lda


# Table
CVmodel = sum(predictvals2!=data$Voted2020)/n; CVmodel
table(data$Voted2020,predictvals2)
# Correctly predicted: 40,777 81.5%
# Misclassifcation: 4,649 18.6%




###############################################################################

# Machine Learning Method 3: Quadratic Discriminant Analysis


# QDA Base model
qda.model = qda(Voted2020 ~ .-RegMonth_NOV, data=data)

# Check class distributions for both outcomes
plot(qda.model, type="both")


# Cross Validation
library(MASS)
n=dim(data)[1]
k=5
groups=c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))

set.seed(123, sample.kind = "Rounding")
cvgroups=sample(groups,n)

predictvals3=rep(-1,n)

for (i in 1:k)  {
  fit3=qda(Voted2020 ~ .-RegMonth_NOV, data=data[!groupi,])
  newdata = data.frame(data[cvgroups==i,])
  predictvals3[cvgroups==i] = as.character(predict(fit3,newdata)$class)
}

# Table
Error3 = sum(subset_data$Voted2020 != fittedclass3)/n; Error3
table(data$Voted2020,predictvals3)
# Correctly predicted: 37,925 75.85%
# Incorreclty: 12,075: 24.15


# Model Summary
qda.model





###############################################################################

# Method 4: Boosting

library(gbm)
library(caret) 			# for general data preparation and model fitting
library(rpart.plot)
library(tidyverse)

data$age_at_year_end<-subset_data2$age_at_year_end

# Base model
set.seed(123,sample.kind = "Rounding")
boost = gbm(Voted2020 ~ .,data=data,distribution="bernoulli",n.trees=1000,
            shrinkage=0.01,interaction.depth=5)
summary(boost)


boost_pred=predict(boost,newdata=data,n.trees=1000,type="response")
# Table
table(boost_pre>.5,data$Voted2020)
a = table(boost_pred>.5,data$Voted2020)
(a[1,2] + a[2,1])/n


# Important predictors
p1<-plot(boost,i="age_at_year_end")
p2<-plot(boost,i="RegMonth_JAN")
p3<-plot(boost,i="Race_White")
p4<-plot(boost,i="RegMonth_OCT")
p1 
p2
p3
p4


print(model_result)

# Best model ntrees(200), max_depth(3), Accuracy (81.5)




##############
# Grid search
##############

## WARNING: This runs for almost an hour :(


# Finding optimum number of trees
data_train=data[1:40000,]
data_test <- data[40001:50000,]

optimal_trees <- list()
library(caret)

trainControl <- trainControl(method = "cv",
                             number = 5,
                             returnResamp="all", ### use "all" to return all cross-validated metrics
                             search = "grid")

tuneGrid <- expand.grid(
  n.trees = c(100, 1000),
  interaction.depth = c( 1,3,5),
  shrinkage = c(0.1,0.01, 0.001),
  n.minobsinnode=c(5,10)
)

gbm_op <- train(Voted2020 ~.,
                data = data_train,
                method = "gbm",
                tuneGrid = tuneGrid,
                trControl = trainControl,
                verbose=FALSE)


plot(gbm_op)


trellis.par.set(caretTheme())
plot(gbm_op, metric = "Kappa")

# Model from Grid Search
set.seed(123,sample.kind = "Rounding")
boost = gbm(Voted2020 ~ .,data=data,distribution="bernoulli",n.trees=1000,
            shrinkage=0.01,interaction.depth=5)
summary(boost)




# Cross Validation for boosting

n=dim(data)[1]
k=5

groups=c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))

set.seed(123, sample.kind = "Rounding")
cvgroups=sample(groups,n)

boost.pred=rep(-1,n)

for(i in 1:k){
  groupi=(cvgroups==i)
  fit4 = gbm(Voted2020 ~ .-RegMonth_NOV, data=data[!groupi,], distribution="bernoulli", n.trees=200, shrinkage=0.3, interaction.depth=3)
  boost.pred[groupi]=predict(fit4,newdata=data[groupi],n.trees=200,type="response")
  
}


# Table
table(boost_pre>.5,data$Voted2020)
a = table(boost.pred>.5,data$Voted2020)
(a[1,2] + a[2,1])/n




##############################################################################

# Analysis

# Important predictors
p1<-plot(boost,i="age_at_year_end")
p2<-plot(boost,i="RegMonth_JAN")
p3<-plot(boost,i="Race_White")
p4<-plot(boost,i="RegMonth_OCT")




# Age

p1<-plot(boost,i="age_at_year_end")
p1

# Create a new df grouping age into categories
age_df <-
subset_data %>%
  mutate(age_range = case_when(age_at_year_end >= 18 &  age_at_year_end <= 34 ~ '18-34',
                               age_at_year_end >= 35 &  age_at_year_end <= 49 ~ '35-49',
                               age_at_year_end >= 50 &  age_at_year_end <= 64 ~ '50-64',
                               age_at_year_end >= 65 ~ '65+'))

# Look at tunrout by age group
table(age_df$Voted2020, age_df$age_range)






# Race White

p2<-plot(boost,i="Race_White")
p2
table(subset_data2$Voted2020, subset_data2$Race_White)
table(subset_data2$Race_White)
# Voted on left, white on top

# Grouped Bar Plot
counts <- table(subset_data2$Voted2020, subset_data2$Race_White)
age_plot<-barplot(counts, main="Voter Turnout by Race: White",
                  xlab="Race White", col=c("seashell2","pale green"),
                  legend = rownames(counts), beside=TRUE)
text(age_plot, font=2, col=2:8)




# Registration Month

p3<-plot(boost,i="RegMonth_JAN")
p4<-plot(boost,i="RegMonth_OCT")
p3;p4

table(subset_data2$Voted2020, subset_data2$RegMonth_JAN) #5,741 72%

table(subset_data2$Voted2020, subset_data2$RegMonth_OCT) #14,311 84.6%














