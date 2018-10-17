
library(shiny)
library(shinythemes)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(dplyr)
library(shiny)
#churn <- read.csv("D:/predict cust churn/Telco-Customer-Churn.csv")
#churn
#churn <- read.csv("D:/predict cust churn/Telco-Customer-Churn.csv")

#We use sapply to check the number if missing values in each columns,We found that there are 11 missing values in “TotalCharges” columns
sapply(churn, function(x) sum(is.na(x)))
churn <- churn[complete.cases(churn), ]

#Look at the variables, we can see that we have some wranglings to do.

#1. We will change “No internet service” to “No” for six columns, they are: “OnlineSecurity”, “OnlineBackup”, 
#“DeviceProtection”, “TechSupport”, “streamingTV”, “streamingMovies”.
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#2. We will change “No phone service” to “No” for column “MultipleLines”
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
#3. Since the minimum tenure is 1 month and maximum tenure is 72 months, we can group them into five tenure groups: “0–12 Month”, “12–24 Month”, “24–48 Months”, “48–60 Month”, “> 60 Month”
min(churn$tenure); max(churn$tenure)
#[1] 1
#[1] 72

#####BLUM BISA
#group_tenure <- (tenure = 0 & tenure  12 & tenure  24 & tenure  48 & tenure  60){
#  return('> 60 Month')
#}


####ALTERNATIVE
churn1 <- churn %>%
  mutate(group_tenure=ifelse(between(tenure, 0, 12), '12',
                             ifelse(between(tenure, 13, 24), '24',
                                    ifelse(between(tenure, 25, 48), '48',
                                           ifelse(between(tenure, 49, 60), '60','70')))))

churn1
##########
#churn1$tenure_group <- sapply(churn1$tenure,group_tenure)
churn1$group_tenure <- as.factor(churn1$group_tenure)
#---4. Change the values in column “SeniorCitizen” from 0 or 1 to “No” or “Yes”.
churn1$SeniorCitizen <- as.factor(mapvalues(churn1$SeniorCitizen,
                                            from=c("0","1"),
                                            to=c("No", "Yes")))
#---5. Remove the columns we do not need for the analysis
churn1$customerID <- NULL
churn1$tenure <- NULL
###Correlation between numeric variables
numeric.var <- sapply(churn1, is.numeric)
corr.matrix <- cor(churn1[,numeric.var])

##           ender    SeniorCitizen          Partner       Dependents     PhoneService    MultipleLines  InternetService 
##           FALSE            FALSE            FALSE            FALSE            FALSE            FALSE            FALSE 
##  OnlineSecurity     OnlineBackup DeviceProtection      TechSupport      StreamingTV  StreamingMovies         Contract 
##           FALSE            FALSE            FALSE            FALSE            FALSE            FALSE            FALSE 
##PaperlessBilling    PaymentMethod   MonthlyCharges     TotalCharges            Churn     group_tenure 
##            ALSE            FALSE             TRUE             TRUE            FALSE            FALSE 

corr.matrix <- cor(churn1[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")
churn1$TotalCharges <- NULL
###The Monthly Charges and Total Charges are correlated. So one of them will be removed from the model. 
###We remove Total Charges.

####Bar plots of categorical variables
p1 <- ggplot(churn1, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p1
p2 <- ggplot(churn1, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p2
p3 <- ggplot(churn1, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() +
  theme_minimal()
p3
p4 <- ggplot(churn1, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() +
  theme_minimal()
p4
pg1 <- grid.arrange(p1, p2, p3, p4, ncol=1)
plot(pg1)
##tb1 <- churn1 %>%
## select(PhoneService,InternetService)
##tb1
p5 <- ggplot(churn1, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p5
p6 <- ggplot(churn1, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p6
p7 <- ggplot(churn1, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p7
p8 <- ggplot(churn1, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p8
pg2 <- grid.arrange(p5, p6, p7, p8, ncol=2)
plot(pg2)

##tb2 <- churn %>%
## select(PhoneService,InternetService,OnlineSecurity)
##tb2


p9 <- ggplot(churn1, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p9
p10 <- ggplot(churn1, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p10
p11 <- ggplot(churn1, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p11
p12 <- ggplot(churn1, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p12

pg3 <- grid.arrange(p9, p10, p11, p12, ncol=2)
plot(pg3)

##tb3 <- churn %>%
##select(PhoneService,InternetService,OnlineSecurity,StreamingMovies)
##tb3


p13 <- ggplot(churn1, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p13
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p14
p15 <- ggplot(churn1, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p15
p16 <- ggplot(churn1, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() +
  theme_minimal()
p16
p17 <- ggplot(churn1, aes(x=group_tenure)) + ggtitle("group_tenure") + xlab("group_tenure") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + 
  theme_minimal()
p17

pg4 <- grid.arrange(p13, p14, p15, p16, p17, ncol=2)
plot(pg4)

##tb4 <- churn %>%
##select(PhoneService,InternetService,OnlineSecurity,StreamingMovies,group_tenure)
##tb4

######## Analisa###################################
###Logistic Regression
#First, we split the data into training and testing sets
intrain <- createDataPartition(churn1$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn1[intrain,]
testing<- churn1[-intrain,]

###Confirm the splitting is correct
##dim(training); dim(testing)
##[1] 4924   19
##[1] 2108   19

###----Fitting the Logistic Regression Model

logmodel <- glm(Churn ~.,family=binomial(link="logit"),data=training)
#plot(logmodel)
psum <- print(summary(logmodel))
psum 


###---Feature Analysis

a1 <- anova(logmodel, test="Chisq")
a1sum <- print(a1)
a1sum
####----Calculate accuracy of model
predicted <- round(predict(logmodel,newdata=training,type="response"))
actual <- training$Churn
confusion_matrix_log <- ftable(actual,predicted)
confusion_matrix_log
accuracy <- sum(diag(confusion_matrix_log))*100/length(actual)
p_error <- print(paste('Logistic Regression Accuracy',accuracy))
p_error
#[1] "Logistic Regression Accuracy 80.8692120227457"

#####Logistic Regression Confusion Matrix
#print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)
## [1] "Confusion Matrix for Logistic Regression"

## FALSE TRUE
## 0  1392  156
## 1   273  287


####Odds Ratio
## One of the interesting performance measurements in logistic regression is Odds Ratio.Basically, 
## Odds ratio is what the odds of an event is happening.

p_or <- exp(cbind(OR=coef(logmodel), confint(logmodel))) #need more minute
p_or


###Decision Tree
#Decision Tree visualization
#For illustration purpose, we are going to use only three variables for
#plotting Decision Trees, they are “Contract”, “tenure_group” and “PaperlessBilling”.
intrain<- createDataPartition(churn1$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn1[intrain,]
testing<- churn1[-intrain,]
tree <- ctree(Churn ~Contract+group_tenure+PaperlessBilling, training)##ok
plot(tree, type='simple')
ptree <- tree

##1. Out of three variables we use, Contract is the most important variable to predict customer churn or not churn.
##2. If a customer in a one-year or two-year contract, no matter he (she) has PapelessBilling or not, he (she) is less likely to churn.
##3. On the other hand, if a customer is in a month-to-month contract, and in the tenure group of 0–12 month, and using PaperlessBilling, then this customer is more likely to churn.

###Decision Tree Confusion Matrix
##We are using all the variables to product confusion matrix table and make predictions.
pred_tree <- predict(tree, testing)
#print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)
confusion_matrix_tree <- table(Predicted = pred_tree, Actual = testing$Churn)
confusion_matrix_tree
##[1] "Confusion Matrix for Decision Tree"
##             Actual
##Predicted    0    1
##No        1391  345
##Yes        157  215


####----Decision Tree Accuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
ptree_error <- print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))
#[1] "Decision Tree Accuracy 0.763282732447818"

##The accuracy for Decision Tree has hardly improved. Let’s see if we can do better 
##using Random Forest.

####----Random Forest
#Random Forest Initial Model
set.seed(1) 
# randomly pick 70% of the number of observations (365)
index <- sample(1:nrow(churn1),size = 0.7*nrow(churn1)) 
# subset weather to include only the elements in the index
train <- churn1[index,] 
# subset weather to include all but the elements in the index
test <- churn1[-index,] 

###----Random Forest

rfModel <- randomForest(Churn ~ ., data = training)
#plot(rfModel)
psum_rf <- rfModel
psum_rf

imp <- as.data.frame(sort(importance(rfModel)[,1],decreasing = TRUE),optional = T)
p_imp <- print(imp)
p_imp
#rfModel <- randomForest(Churn ~., data = training)
#print(rfModel)

#--------Random Forest Prediction and Confusion Matrix
pred_rf <- predict(rfModel, testing)
confusionMatrix_rf<- confusionMatrix(pred_rf, testing$Churn)
confusionMatrix_rf

