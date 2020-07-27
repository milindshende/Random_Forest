install.packages("randomForest")
library(randomForest)
 
company<-read.csv(file.choose()) # read company_data csv file
View(company)
str(company)
summary(company)
hist(company$Sales)
boxplot(company$Sales)

# factorise the "Sales" (O/P Variable) into 2 levels , based on summary of "Sales" variable.
# 3rd Quartile is 9.32 means almost 75% values covered. Hence "Sales" above 9.32 defined as 'High Sales'

company$Sales<- cut(company$Sales,breaks = c(-1,9.32,17),labels = c("Sale","High Sale"))
levels(company$Sales) # 2 levels , Sales & High Sales
nlevels(company$Sales) # 2
str(company) # check "Sales" has converted to Factor from Num
table(company$Sales) # Sale = 301 , High Sale=99
summary(company)
attach(company)

###### Splitting data into training and testing by Random method (75/25 ratio)#####

install.packages("caTools")
library(caTools)
split<-sample.split(company$Sales,SplitRatio = 0.75)
split
table(split)
company_train<-subset(company,split==TRUE)
company_test<-subset(company,split==FALSE)

prop.table(table(company_train$Sales)) # Sale=75.33% , High Sale=24.66%
prop.table(table(company_test$Sales)) # Sale=75% , High Sale=25%

# Building Random Forest model on training data 

company_forest <- randomForest(Sales~.,data=company_train, na.action=na.roughfix,importance=TRUE)
company_forest$ntree # 500 Decision Trees used default
company_forest$mtry # 3 variables (Columns) in each Decision tree used
company_forest$confusion

# Prediction & Accuracy on Training data
pred_train<-company_forest$predicted
table(pred_train,company_train$Sales) # 44 errors in predictions 
mean(pred_train==company_train$Sales) # 85.33% accuracy

library(caret)
# Confusion Matrix
confusionMatrix(company_train$Sales, pred_train)

# Visualization 
plot(company_forest,lwd=2)
legend("topright", colnames(company_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Crosstable
library(gmodels)
rf_perf<-CrossTable(company_train$Sales,company_forest$predicted,
                    prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
                    dnn=c('actual default','predicted default'))

# Prediction & Accuracy on Test Data
pred_test<-predict(company_forest,newdata = company_test)
table(pred_test,company_test$Sales) # 12 errors in predictions
mean(pred_test==company_test$Sales) # 88% accuracy

# Confusion Matrix
confusionMatrix(company_test$Sales, pred_test)

###########

install.packages("party",dependencies = TRUE)
library(party)

company_tree1<-ctree(Sales~. , data = company_train)
summary(company_tree1)
plot(company_tree1)

# Based on above plot ; attributes for High Sales->
#ShelveLoc Good, Price<=108;then probability of High Sale @ 95%
#ShelveLoc Bad/Medium,Price>94,Advertising>10,Income>99,probability of High Sale @ 65%

######### Splitting data into training and testing by Stratified (75/25 ratio)#####

company_train2<-company[1:300, ] # 75% of data for Train
company_test2<-company[301:400,] # 25% of data for Test

prop.table(table(company_train2$Sales)) # Sales=76.67% High Sale=23.33%
prop.table(table(company_test2$Sales)) # Sales=71% High Sale=29%

# Building Random Forest model on training data 

company_forest2 <- randomForest(Sales~.,data=company_train2, na.action=na.roughfix,importance=TRUE)
company_forest2$ntree # 500 Decision Trees used default
company_forest2$mtry # 3 variables (Columns) in each Decision tree used
company_forest2$confusion # 40 errors

# Prediction & Accuracy on Training data
pred_t2<-company_forest2$predicted
table(pred_t2,company_train2$Sales) # 40 errors in predictions 
mean(pred_t2==company_train2$Sales) # 86.66% accuracy

library(caret)
# Confusion Matrix
confusionMatrix(company_train2$Sales, pred_t2)

# Visualization 
plot(company_forest2,lwd=2)
legend("topright", colnames(company_forest2$err.rate),col=1:4,cex=0.8,fill=1:4)

# Crosstable
library(gmodels)
rf_perf2<-CrossTable(company_train2$Sales,company_forest2$predicted,
                    prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
                    dnn=c('actual default','predicted default'))

# Prediction & Accuracy on Test Data
pred_test2<-predict(company_forest2,newdata = company_test2)
table(pred_test2,company_test2$Sales) # 20 errors in predictions
mean(pred_test2==company_test2$Sales) # 80% accuracy

# Confusion Matrix
confusionMatrix(company_test2$Sales, pred_test2)

#####################3

install.packages("party",dependencies = TRUE)
library(party)

company_tree2<-ctree(Sales~. , data = company_train2)
summary(company_tree2)
plot(company_tree2)

# Based on above plot ; attributes for High Sales->
#ShelveLoc Good, Price<=109;then probability of High Sale @ 95%
#ShelveLoc Bad/Medium,Price<=87,probability of High Sale @ 55%
