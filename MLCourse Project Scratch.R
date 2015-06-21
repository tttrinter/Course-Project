setwd("C:/Users/Tom/Documents/Coursera/Data Science/8. Practical Machine Learning/Course Project")
training=read.csv("pml-training.csv",header=TRUE)


table(training$classe)

#Lots of columns are filled with NA.  Find and remove!
NAcount=sapply(training,function(x) sum(is.na(x)))
goodColumns=names(NAcount[NAcount<1000])

#Other columns to remove

#Remove timestamp variables
goodColumns=goodColumns[-grep("timestamp",goodColumns)]

#Remove other mostly blank columns
badColumns=c("X",
             "kurtosis_yaw_belt",
             "num_window",
             "new_window",
             "skewness_yaw_belt",
             "amplitude_yaw_belt",
             "kurtosis_yaw_dumbbell",
             "skewness_yaw_dumbbell",
             "amplitude_yaw_dumbbell",
             "skewness_yaw_forearm",
             "kurtosis_roll_belt",
             "kurtosis_picth_belt",
             "skewness_roll_belt",
             "skewness_roll_belt.1",
             "max_yaw_belt",
             "min_yaw_belt",
             "kurtosis_roll_arm",
             "kurtosis_picth_arm",
             "kurtosis_yaw_arm",
             "skewness_roll_arm",
             "skewness_pitch_arm",
             "skewness_yaw_arm",
             "kurtosis_roll_dumbbell",
             "kurtosis_picth_dumbbell",
             "skewness_roll_dumbbell",
             "skewness_pitch_dumbbell",
             "max_yaw_dumbbell",
             "min_yaw_dumbbell",
             "kurtosis_roll_forearm",
             "kurtosis_picth_forearm",
             "kurtosis_yaw_forearm",
             "skewness_roll_forearm",
             "skewness_pitch_forearm",
             "max_yaw_forearm",
             "min_yaw_forearm",
             "amplitude_yaw_forearm")

goodColumns=goodColumns[!goodColumns %in% badColumns]

training=training[,goodColumns]

#Cut the data into a
library(caret)
inTrain=createDataPartition(y=training$classe,p=0.30, list=FALSE)
subTraining=training[inTrain,]
subTest=training[-inTrain,]

nsv=nearZeroVar(subTraining,saveMetrics=TRUE)
nsv

#Try GLM model
modelFitGLM=train(classe~., data=subTraining, method="glm")

#Try rpart model
modelFitRPart=train(classe~., data=subTraining, method="rpart")
library(rattle)
fancyRpartPlot(modelFitRPart$finalModel)

testRPart=predict(modelFit,newdata=subTest)
confusionMatrix(subTest$classe,predict(modelFit,newdata=subTest))

subTest$predRight<-testRPart==subTest$classe
#not very good results

#Try again with processed data
modelFit1=train(classe ~., data=subTraining,
               preProcess=c("center","scale"), method="rpart")

#Modeling with RandomForest
modelFit=train(classe~., data=subTraining, method="rf", prox=TRUE)
#high accuracy, but is it overfit?

#Predicting new values
pred=predict(modelFit,subTest)
#create a variable to show if the prediction was correct or now
subTest$predRight<-pred==subTest$classe
#show results
confusionMatrix(subTest$classe,pred)



#The test submissions
#get the data
testing=read.csv("pml-testing.csv",header=TRUE)

answers=predict(modelFit,testing)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers)