library(tidyverse)
library(caret)
library(parallel)
library(doParallel)

# Download and import original data
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              "data/pml_training.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              "data/pml_testing.csv")
orig <- read_csv("data/pml_training.csv")
final_test <- read_csv("data/pml_testing.csv")

# Split into train/test sets
set.seed(42)
inTrain <- createDataPartition(orig$classe, p = 0.75)[[1]]
train <- orig[inTrain,]
test <- orig[-inTrain,]

#
# Random Forest Model -----
#

# Configure parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

# Configure train control object for cross-validation
rfFitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Fit random forest model
rfFit <- train(classe ~ roll_belt + pitch_belt + yaw_belt + roll_forearm + pitch_forearm +
                   yaw_forearm + roll_arm + pitch_arm + yaw_arm + roll_dumbbell + pitch_dumbbell +
                   yaw_dumbbell, method="rf", data=train, trControl=rfFitControl)

# Cleanup parallel processing
stopCluster(cluster)
registerDoSEQ()

# Apply model to test set, check results
rfRes <- data.frame(Obs=test$classe,
                    Pred=predict.train(rfFit, test))
rfRes$Correct <- ifelse(rfRes$Obs == rfRes$Pred, 1, 0)
table(rfRes$Correct) # 0.9838
confusionMatrix.train(rfFit)

# Save confusion matrix as plot
cm <- as.data.frame(confusionMatrix.train(rfFit)[[1]]) %>%
    rename(Frequency = Freq)
ggplot(cm, aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = Frequency)) +
    geom_text(aes(label = round(Frequency, 3)), size = 3) +
    scale_fill_gradient(low = "lightgoldenrod1", high = "darkgoldenrod4") + 
    labs(x = "Observation", title = "Random Forest Confusion Matrix",
         subtitle = "5-Fold Cross Validation")
ggsave("./images/rfFit_confusion_matrix.jpeg")

# Run model on final testing set
test_res <- data.frame(Sample=seq(1,20),
                       Pred=predict.train(rfFit, final_test))

#
# Boosted Random Forest Model -----
#

# Configure parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

# Configure train control object for cross-validation
gbmFitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Fit random forest model
gbmFit <- train(classe ~ roll_belt + pitch_belt + yaw_belt + roll_forearm + pitch_forearm +
                   yaw_forearm + roll_arm + pitch_arm + yaw_arm + roll_dumbbell + pitch_dumbbell +
                   yaw_dumbbell, method="gbm", data=train, trControl=gbmFitControl)

# Cleanup parallel processing
stopCluster(cluster)
registerDoSEQ()

# Apply model to test set, check results
gbmRes <- data.frame(Obs=test$classe,
                    Pred=predict.train(gbmFit, test))
gbmRes$Correct <- ifelse(gbmRes$Obs == gbmRes$Pred, 1, 0)
table(gbmRes$Correct) # 0.9305
confusionMatrix.train(gbmFit)
gbmCM <- as.data.frame(confusionMatrix.train(gbmFit)[[1]]) %>%
    rename(Frequency = Freq)
ggplot(gbmCM, aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = Frequency)) +
    geom_text(aes(label = round(Frequency, 3)), size = 3) +
    scale_fill_gradient(low = "lightgoldenrod1", high = "darkgoldenrod4") + 
    labs(x = "Observation", title = "Gradient Tree Boosting Confusion Matrix",
         subtitle = "5-Fold Cross Validation")
ggsave("./images/gbmFit_confusion_matrix.jpeg")
