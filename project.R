# Human Activity Recognition
# use data from accelerometers on the belt, forearm, arm, and dumbell of 6
# participants in order to quantify how well they perform the exercise 

#---------------------------------
# write a report (with R markdown), words < 2000, figs < 5
# describe the model(s) used
# how cross validation was used in the data
# describe the out sample error
#---------------------------------
# create a github repo:
#   R markdown
#   compiled HTML file describing analysis
#---------------------------------


library(caret)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)
library(doMC)

library(GGally)

set.seed(2195324)

numCores <- detectCores()
registerDoMC(numCores / 2)

train_url = 
    'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'

if (!file.exists("training.csv")) {
    download.file(url = train_url, destfile = 'training.csv')
}

training_data = read.csv(file = 'training.csv', 
                         na.strings = c("NA", "", "#DIV/0!"))

# MISSING VALUES

na_col <- function(column_indx){
    sum(is.na(training_data[, column_indx]))
}

boolean <- function(indx){
    na_vector[indx] == 0
}

# create a boolean vector with the number of elements corresponding to 
# the number of columns in the dataset, that gives only TRUE for the
# columns I want to select

# use sapply
indx_vector = 1:ncol(training_data)
# create a vector where each element corresponds to a column in the dataset
# and the value is the total number of NA in that column
na_vector = sapply(indx_vector, na_col)
# create a boolean vector with the number of elements corresponding to 
# the number of columns in the dataset, that gives only TRUE in the
# columns I want to select
boolean_vector = sapply(indx_vector, boolean)

data = training_data[, boolean_vector]

# keep only the variables that show data collected from the accelerometers 
# (original sensor readings). this corresponds to selecting the features 
# that indicate the Euler angles (roll, pitch, yaw) and the 3-axis coordinates
# of the accelerometer, gyroscope, and magnetometer readings for each one
# of the four sensors
# eliminate features containing the following strings:
# "user", "timestamp", "window", "total"

# pipes dplyr
data <- data %>%
    select(-contains("user"), -contains("timestamp"), -contains("window"), 
           -contains("total")) %>%
    select(-starts_with("X")) %>%
    mutate("classe" = as.factor(classe))


# use cross validation in the train function, i.e., in this case use 10-fold
# cross validation with the random forest method

# separate data into training and testing sets
inTrain = createDataPartition(y = data$classe, p = 0.7, list = FALSE)
training = data[inTrain, ]
testing = data[-inTrain, ]


# exploratory data analysis
# construct a mesh-plot that shows some of the dependencies of the euler
# angle variables, i.e. roll, pitch, yaw, for a given position of the recording 
# devices, i.e. dumbbell, arm, foream, belt:
qplot(roll_dumbbell, pitch_dumbbell, colour = classe, data = training)
qplot(roll_dumbbell, color = classe, data = training) + geom_density()

# look ggpairs: matrix of plots
# correlation among arm Euler angles
data_pairs <- select(training,
                     contains("roll_arm"), 
                     contains("pitch_arm"), 
                     contains("yaw_arm"), 
                     contains("classe"))

# use color with the classe variable
# remove the classe variable from the matrix plot
ggallyDensityPlot <- function(data, mapping, ...){
    ggplot(data = data, mapping = mapping) + 
    geom_density(mapping = aes_string(color = "classe"), fill = NA)
}

ggpairs(data_pairs, columns = c(1:3), 
        mapping = ggplot2::aes(colour = classe),
        lower = list(continuous = wrap("points", alpha = 0.5)),
        diag = list(continuous = ggallyDensityPlot),
        title = 'Correlation among arm Euler angles' 
        )

# explain why is a good approach to this problem to apply rf to train the
# model

# random forest training method (week 3)
# boosting (week 3) plot predictions

# use class "centers" to see what the prediction would be at the center 
# of the class predictions

rf_file = "./rf10foldCV.rds"

if (file.exists(rf_file)){
    rf10fold_cv <- readRDS("./rf10foldCV.rds")
} else {
    rf10fold_cv <- train(classe ~ ., data = training, 
                    method = 'rf', prox = TRUE, 
                    trControl = trainControl(method = "cv", number = 10), 
                    tuneLength = 8)
}


pred <- predict(rf10fold_cv, testing)
accuracy_table <- table(pred, testing$classe)
accuracy_table
cM <- confusionMatrix(pred, testing$classe)

acc <- function(index){
    accuracy_table[index, index] / sum(accuracy_table[, index])
}

classes <- c(1:5)
pred_by_class <- sapply(classes, acc)
    
cat("Class A precision: ", acc(1))
cat("Class B precision: ", acc(2))
cat("Class C precision: ", acc(3))
cat("Class D precision: ", acc(4))
cat("Class E precision: ", acc(5))

for (i in seq(classes)) {
    cat("Class", classes[i], "precision: ", pred_by_class[i], "\n")
}

# prediction quiz portion
# Apply your machine learning algorithm to the 20 test cases available in the 
# test data above and submit your predictions in appropriate format to the 
# Course Project Prediction Quiz for automated grading

test_url = 
    'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

if (!file.exists("test.csv")) {
    download.file(url = test_url, destfile = 'test.csv')
}

test_data = read.csv(file = 'test.csv', na.strings = c("NA", "", "#DIV/0!"))

rf_predictions <- predict(rf10fold_cv, newdata = test_data)
rf_predictions









