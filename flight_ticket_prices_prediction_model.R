### 4. Data Pre-Processing
# Call dataset
airfare <- read.csv(file.choose(), na.string=c(""), stringsAsFactors = FALSE)


## 4.1. Data Exploration
# View dataset
View(airfare)
# strcture of dataset
str(airfare)
# Dimensions of an Object
dim(airfare) 
# The names of an Objects
names(airfare)
head(airfare)
describe(airfare)
summary(airfare)

## 4.2. Missing value detetion and replacement


### Counting Missing value
# 1. Check Missing value by dataset
is.na(airfare)
# 2. Check Missing value from dataset
table(is.na(airfare))
# 3. Check Missing value by variable
table(is.na(airfare$Route))
table(is.na(airfare$Total_Stops))
# 4. Display row which contain NA value
airfare[!complete.cases(airfare),]


## 4.3. Feature Selection
# 1) Replace missing value and re-check missing value
airfare_2 <- airfare
airfare_2$Route[is.na(airfare_2$Route)] <- "DEL ? MAA ? COK"
airfare_2$Total_Stops[is.na(airfare_2$Total_Stops)] <- "1 stop"

table(is.na(airfare_2))
table(is.na(airfare_2$Route))
table(is.na(airfare_2$Total_Stops))
# 2). Data transformation
# 2.1) Replacing total_stops as numeric category
airfare_3 <- airfare_2
airfare_3$Total_Stops = factor(airfare_3$Total_Stops,
                             levels = c("1 stop", "2 stops", "3 stops", "4 stops", "non-stop"),
                             labels = c(1,2,3,4,0))

airfare_3$Airline = factor(airfare_3$Airline,
                           levels = c("Air Asia", "Air India", "GoAir", "IndiGo", "Jet Airways", "Jet Airways Business",
                                      "Multiple carriers", "Multiple carriers Premium economy", "SpiceJet", "Trujet",
                                      "Vistara", "Vistara Premium economy"),
                           labels = c(1,2,3,4,5,6,7,8,9,10,11,12))


airfare_3$Source = factor(airfare_3$Source,
                          levels = c("Banglore", "Chennai", "Delhi", "Kolkata", "Mumbai"),
                          labels = c(1,2,3,4,5))


airfare_3$Destination = factor(airfare_3$Destination,
                               levels = c("Banglore", "Cochin", "Delhi", "Hyderabad", "Kolkata", "New Delhi"),
                               labels = c(1,2,3,4,5,6))



# 2.2) Convert Factor to numerical
airfare_3$Total_Stops <-as.numeric(factor(as.matrix(airfare_3$Total_Stops)))
airfare_3$Price <-as.numeric(factor(as.matrix(airfare_3$Price)))
airfare_3$Total_Stops <-as.numeric(factor(as.matrix(airfare_3$Total_Stops)))
airfare_3$Airline <-as.numeric(factor(as.matrix(airfare_3$Airline)))
airfare_3$Source <-as.numeric(factor(as.matrix(airfare_3$Source)))
airfare_3$Destination <-as.numeric(factor(as.matrix(airfare_3$Destination)))
airfare_3$Price <-as.numeric(factor(as.matrix(airfare_3$Price)))
# 2.3) Covert chr to Factor for Expensive 
airfare_3$Expensive <- factor(airfare_3$Expensive)
# 2.4) Convert chr to Date for Date_of_Journey
airfare_3$Date_of_Journey <-as.Date(factor(as.matrix(airfare_3$Date_of_Journey)), format = "%d/%m/%Y")
# 2.5) Drop unnecessary variables
airfare_3<-airfare_3[c(-5, -6, -7, -8, -10)]

str(airfare_3)



## 4.4. Data split / partitioning
set.seed(50)
pred<-sample(2, nrow(airfare_3),replace = TRUE,prob = c(0.7,0.3))
airfare_train<-airfare_3[pred==1,]
airfare_test<-airfare_3[pred==2,]
str(airfare_train)

### 5. Apply Machine Learning

## 5.1. Support Venctor Machine


########## SVM CODE 1

# 1. Install required package
library(e1071)
library(Epi)

str(airfare_train)
table(airfare_train$Expensive)
# Find out Optimal value for parmameter
tune.out <- tune.svm(as.factor(Expensive) ~., data = airfare_train, kernel='radial', cost=2^(-1:5), gamma = 2^(-1:1))
summary(tune.out)
summary(tune.out$best.model)

# Create SVM Model
svm_model <- svm(as.factor(Expensive)~ Airline + Date_of_Journey + Total_Stops, data = airfare_train, gamma=0.5, cost=32)
summary(svm_model)

# Find out Accuracy
svm_test <- predict(svm_model, airfare_test)
predict_table <- table(real = airfare_test$Expensive, predict = svm_test)
predict_table
(predict_table[1,1] + predict_table[2,2]) / sum(predict_table)

## 5.2. Random Forests

# 1. Install required package
install.packages("caret", dependencies = TRUE)
install.packages("randomForest")
library(caret)
library(randomForest)

head(airfare_train)
head(airfare_test)

table(airfare_train[,c('Expensive', 'Total_Stops')])
install.packages("fields")
library(fields)

###### STARTING IMPLEMENT RANDOM FOREST
# Set a random seed
set.seed(51)
# randome Forest method
# Model - 1
rf_model <- train(Expensive ~ Airline + Date_of_Journey + # 'train' common function in R. 
                 Source + Destination + Total_Stops + Price,
               data = airfare_train, # Use the airfare_train data frame as the airfare_train data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 5)) # Use 5 folds for cross-validation
# Model - 2
rf_model2 <- train(Expensive ~ Date_of_Journey + Total_Stops, 
               data = airfare_train, 
               method = 'rf',
               trControl = trainControl(method = 'cv', 
                                        number = 5))


summary(airfare_test)

#Predict with test_data set.
rf_pred <- predict(rf_model2, newdata = airfare_test) #'predict' - method
rf_pred
# Accuracy checking
confus_matrix<-confusionMatrix(rf_pred, airfare_test$Expensive)
confus_matrix

#predicted_table <- predict(rf_model, airfare_test[,-7])
#table(observed = airfare_test[,7],predicted = predicted_table)

#plot(margin(rf_model, airfare_test$Expensive))
#plot(Airline, Expensive.Date_of_Journey, Expensive.Total_Stops, colour=Price)


## 5.3. Neural Network
library(nnet)
library(caret)
library(ROCR)

#Check feature importance before modeling
library((MASS))
model_lda <- train(Expensive ~ .,data=airfare_train, method = "lda")
model_lda
plot(varImp(model_lda))

# Modeling
set.seed(123)
start <- proc.time()[3]
str(airfare_train)
nn_model <- train(Expensive~., data=airfare_train, method = "nnet")

nn_model2 <- train(Expensive~ Airline + Date_of_Journey + Total_Stops, data=airfare_train, method = "nnet")

nn_model3 <- train(Expensive~ Airline + Date_of_Journey + Source + Destination +
                     Total_Stops, data=airfare_train, method = "nnet")

plot(nn_model)

str(airfare_test)


actual <-airfare_test$Expensive
cfm <- table(actual, predictions)
cfm

# 
round(prop.table(cfm) * 100, digit =1)


# nueral network plot
library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

plot.nnet(nn_model2)
# estimate relative feature importance
library(NeuralNetTools)
garson(nn_model2)


# prediction and accuracy
predictions1 <- predict(nn_model, airfare_test[,1:6])
accuracy1 <- sum(predictions1 == airfare_test[,7])/length(airfare_test[,7])
print(accuracy1)

predictions <- predict(nn_model2, airfare_test[,1:6])
accuracy <- sum(predictions == airfare_test[,7])/length(airfare_test[,7])
print(accuracy)

# Check processing time
end <- proc.time()[3]
end
print(paste("This took ", round(end-start, digits = 1), " seconds", sep = ""))
