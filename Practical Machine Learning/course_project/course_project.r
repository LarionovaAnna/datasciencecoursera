#Load required libraries
library(caret)
library(rattle)
library(rpart)
library(randomForest)

#Set the seed for reproducibility
set.seed(12345)


#load data

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#read from downloaded files
#original_training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
#original_testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

original_training <- read.csv("C:/Documents and Settings/Администратор/Рабочий стол/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
original_testing <- read.csv("C:/Documents and Settings/Администратор/Рабочий стол/pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

#Splitting original_training data set into two data sets, 60% for training, 40% for testing:
inTrain <- createDataPartition(y=original_training$classe, p=0.6, list=FALSE)
training <- original_training[inTrain, ]; testing <- training[-inTrain, ]
dim(training); dim(testing)


#Cleaning the data
#1st cleaning NearZeroVariance Variables
NZVs <- nearZeroVar(training, saveMetrics=TRUE)

#subset without the NZVs:
NZVvars <- names(training) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
"kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
"max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
"var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
"stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
"kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
"max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
"kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
"skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
"amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
"skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
"max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
"amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
"avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
"stddev_yaw_forearm", "var_yaw_forearm")

training <- training[!NZVvars]
dim(training)
#[1] 11776   100

#2nd - delete first column of dataset - ID, so that it does not interfere with ML Algorithms:
training <- training[c(-1)]


#Transformation 3: cross out variables with too many NA - more than a 60% threshold of NA's:

temp_training <- training 
for(i in 1:length(training)) { 
        if( sum( is.na( training[, i] ) ) /nrow(training) >= .6 ) { 
		for(j in 1:length(temp_training)) {
			if( length( grep(names(training[i]), names(temp_training)[j]) ) ==1)  {
				temp_training <- temp_training[ , -j] 
			}	
		} 
	}
}

dim(temp_training)

training <- temp_training
rm(temp_training)
#same 3 transformations for original_testing and testing data sets: 

clean1 <- colnames(training)
clean2 <- colnames(training[, -58])
testing <- testing[clean1]
original_testing <- original_testing[clean2]

dim(testing); dim(original_testing)

#In order to ensure proper functioning of Decision Trees and especially RandomForest 
#Algorithm with the Test data set (data set provided), we need to coerce the data into the same type.
for (i in 1:length(original_testing) ) {
        for(j in 1:length(training)) {
		if( length( grep(names(training[i]), names(original_testing)[j]) ) ==1)  {
			class(original_testing[j]) <- class(training[i])
		}      
	}      
}

original_testing <- rbind(training[2, -58] , original_testing) #note row 2 does not mean anything, this will be removed right.. now:
original_testing <- original_testing[-1,]


# Decision Tree prediction

modFitA1 <- rpart(classe ~ ., data=training, method="class")
#see plot by following command
#fancyRpartPlot(modFitA1)

predictionsA1 <- predict(modFitA1, testing, type = "class")

confusionMatrix(predictionsA1, testing$classe)

# Overall Statistics
                                          
#                Accuracy : 0.8789          
#                  95% CI : (0.8715, 0.8861)
#     No Information Rate : 0.2845          
#     P-Value [Acc > NIR] : < 2.2e-16 


#Random Forests

modFitB1 <- randomForest(classe ~. , data=training)

predictionsB1 <- predict(modFitB1, testing, type = "class")

confusionMatrix(predictionsB1, testing$classe)
# Overall Statistics
                                          
#                Accuracy : 0.9986          
#                  95% CI : (0.9975, 0.9993)
#     No Information Rate : 0.2845          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.9982          
#  Mcnemar's Test P-Value : NA 

#Random Forests has better results!


#Generating Files to submit as answers for the Assignment:

#use best model - Random Forests for prediction of original_testing:
predictionsB2 <- predict(modFitB1, original_testing, type = "class")

#Function to generate files with predictions to submit for assignment:

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)