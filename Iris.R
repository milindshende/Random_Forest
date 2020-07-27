# Using Random Forest
install.packages("randomForest")
library(randomForest)

data(iris)
View(iris)

# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 

iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50

iris_train <- rbind(iris_setosa[1:30,],iris_versicolor[1:30,],iris_virginica[1:30,])
iris_test <- rbind(iris_setosa[31:50,],iris_versicolor[31:50,],iris_virginica[31:50,])

# Building a random forest model on training data 

iris.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,ntree=50,importance=TRUE)

iris.forest$ntree # 50 as we defined in model build
iris.forest$confusion
iris.forest$mtry # 2 variables considered in each DT

pred_train<-iris.forest$predicted
table(pred_train,iris_train$Species) # 5 errors
mean(iris_train$Species==pred_train) # 94.44% accuracy

library(caret)
# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)

# Visualization 
plot(iris.forest,lwd=2)
legend("topright", colnames(iris.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Crosstable
library(gmodels)
rf_perf<-CrossTable(iris_train$Species,iris.forest$predicted,
                    prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
                    dnn=c('actual default','predicted default'))

####### Predicting on Test Data
pred_test<-predict(iris.forest,newdata = iris_test)
table(pred_test,iris_test$Species) # 1 error
mean(pred_test==iris_test$Species) # 98.33% accuracy

# Confusion Matrix 

confusionMatrix(iris_test$Species, pred_test)

#######################################