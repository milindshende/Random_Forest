install.packages("randomForest")
library(randomForest)

fraud<-read.csv(file.choose()) # read fraud_data csv file
View(fraud)
str(fraud)
summary(fraud)
hist(fraud$Taxable.Income)
boxplot(fraud$Taxable.Income)

# factorise the Taxable Income (O/P Variable) into 2 levels

fraud$Taxable.Income<- cut(fraud$Taxable.Income,breaks = c(0,30000,100000),labels = c("Risky","Good"))
levels(fraud$Taxable.Income)
nlevels(fraud$Taxable.Income)
str(fraud)
table(fraud$Taxable.Income) # Risky=124, Good=476
summary(fraud)
hist(fraud$Taxable.Income)
boxplot(fraud$Taxable.Income)
attach(fraud)

###### Splitting data into training and testing by Random (70/30 ratio)#####

install.packages("caTools")
library(caTools)
split<-sample.split(fraud$Taxable.Income,SplitRatio = 0.70)
split
table(split)
fraud_train<-subset(fraud,split==TRUE)
fraud_test<-subset(fraud,split==FALSE)

prop.table(table(fraud_train$Taxable.Income)) # Risky=20.71% , Good=79.29%
prop.table(table(fraud_test$Taxable.Income)) # Risky=20.55% , Good=79.44%

# Building Random Forest model on training data 

fraud_forest <- randomForest(Taxable.Income~.,data=fraud_train, na.action=na.roughfix,importance=TRUE)
fraud_forest$ntree # 500 Decision Trees used default
fraud_forest$mtry # 2 variables (Columns) in each Decision tree used
fraud_forest$confusion

# Prediction & Accuracy on Training data
pred_train<-fraud_forest$predicted
table(pred_train,fraud_train$Taxable.Income) # 93 errors in predictions 
mean(pred_train==fraud_train$Taxable.Income) # 77.85% accuracy

library(caret)
# Confusion Matrix
confusionMatrix(fraud_train$Taxable.Income, pred_train)

# Visualization 
plot(fraud_forest,lwd=2)
legend("topright", colnames(fraud_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Crosstable
library(gmodels)
rf_perf<-CrossTable(fraud_train$Taxable.Income,fraud_forest$predicted,
                    prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
                    dnn=c('actual default','predicted default'))

# Prediction & Accuracy on Test Data
pred_test<-predict(fraud_forest,newdata = fraud_test)
table(pred_test,fraud_test$Taxable.Income) # 40 errors in predictions
mean(pred_test==fraud_test$Taxable.Income) # 77.77% accuracy

# Confusion Matrix
confusionMatrix(fraud_test$Taxable.Income, pred_test)

######### Splitting data into training and testing by Stratified (75/25 ratio)#####

fraud_train2<-fraud[1:450, ] # 75% data for Train
fraud_test2<-fraud[451:600, ] # 25% data for Test

prop.table(table(fraud_train2$Taxable.Income)) # Risky = 22.66% , Good= 77.33%
prop.table(table(fraud_test2$Taxable.Income)) # Risky = 14.66% , Good= 85.33%

# Building Random Forest model on training data 

fraud_forest2 <- randomForest(Taxable.Income~.,data=fraud_train2, na.action=na.roughfix,importance=TRUE)
fraud_forest2$ntree # 500 Decision Trees used default
fraud_forest2$mtry # 2 variables (Columns) in each Decision tree used
fraud_forest2$confusion

# Prediction & Accuracy on Training data
pred_train2<-fraud_forest2$predicted
table(pred_train2,fraud_train2$Taxable.Income) # 110 errors in predictions 
mean(pred_train2==fraud_train2$Taxable.Income) # 75.55% accuracy

library(caret)
# Confusion Matrix
confusionMatrix(fraud_train2$Taxable.Income, pred_train2)

# Visualization 
plot(fraud_forest2,lwd=2)
legend("topright", colnames(fraud_forest2$err.rate),col=1:4,cex=0.8,fill=1:4)

# Crosstable
library(gmodels)
rf_perf2<-CrossTable(fraud_train2$Taxable.Income,fraud_forest2$predicted,
                    prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
                    dnn=c('actual default','predicted default'))

##### Prediction & Accuracy on Test Data

pred_test2<-predict(fraud_forest2,newdata = fraud_test2)
table(pred_test2,fraud_test2$Taxable.Income) # 26 errors in predictions
mean(pred_test2==fraud_test2$Taxable.Income) # 82.66% accuracy

# Confusion Matrix
confusionMatrix(fraud_test2$Taxable.Income, pred_test2)

