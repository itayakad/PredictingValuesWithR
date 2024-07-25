#--SET UP--
install.packages("Metrics")
library(Metrics)
library(rpart)
adult_data <- read.csv("/Users/itayakad/Desktop/Data 101 HWs/Data 101 - HW4/adult.csv")
data <- na.omit(adult_data)
head(data)
#--FEATURE ENGINEERING--
# Binning Age into categories
data$AgeGroup <- cut(data$age, breaks=c(0, 25, 45, 65, Inf), labels=c("Young", "Middle-aged", "Senior", "Elder"))
# CapitalNet as capital.gain minus capital.loss
data$CapitalNet <- data$capital.gain - data$capital.loss
# HoursPerYear as hours.per.week multiplied by 52
data$HoursPerYear <- data$hours.per.week * 52
# AgeWorkclassInteract as interaction term (numeric example)
data$AgeWorkclassInteract <- data$age * as.numeric(as.factor(data$workclass))
# EduNumHours as product of education.num and hours.per.week
data$EduNumHours <- data$education.num * data$hours.per.week
# Simplifying Education into fewer categories
data$EducationLevel <- ifelse(data$education %in% c("Bachelors", "Masters", "Doctorate"), "Higher",
                                    ifelse(data$education %in% c("Some-college", "Assoc-acdm", "Assoc-voc", "Prof-school"), "Mid", "Lower"))
# OccupationHours categorizing hours per week
data$OccupationHours <- cut(data$hours.per.week, breaks=c(0, 20, 40, 60, Inf), labels=c("Part-time", "Full-time", "Overtime", "Extreme"))
# MaritalStatusRace as interaction term
data$MaritalStatusRace <- interaction(data$marital.status, data$race)
# AgeCategoryByWorkHours interaction of age categories and work hours
data$AgeCategoryByWorkHours <- interaction(cut(data$age, breaks=c(0, 25, 45, 65, Inf)), 
                                                 cut(data$hours.per.week, breaks=c(0, 20, 40, 60, Inf)))


#--PREDICTING NUM VALUE--
##--CREATING BENCHMARK--
  data$race <- as.factor(data$race)
  data$marital.status <- as.factor(data$marital.status)
  # Fit a linear model
  lm_model <- lm(HoursPerYear ~ race + marital.status, data = data)
  # Fit a regression tree model
  rpart_model <- rpart(HoursPerYear ~ race + marital.status, data = data, 
                             control=rpart.control(minsplit=50, cp=0.01))
  # Prediction using the linear model
  pred_lm <- predict(lm_model, newdata = data)
  # Prediction using the regression tree model
  pred_rpart <- predict(rpart_model, newdata = data)
  # Calculate MSE for both models
  mse_lm <- mse(data$HoursPerYear, pred_lm)
  mse_rpart <- mse(data$HoursPerYear, pred_rpart)
  # Compare MSEs and determine the better benchmark
  benchmark_mse <- min(mse_lm, mse_rpart)
  # Print the results
  cat("MSE from LM: ", mse_lm, "\n")
  cat("MSE from Rpart: ", mse_rpart, "\n")
  cat("Benchmark MSE is: ", benchmark_mse, "\n")
##--PREDICTION MODEL--
  # Segment data based on relationship status
  data$RelationshipSegment <- ifelse(data$relationship == "Husband", "Husband", "Other")
  # Update models
  model_husband <- lm(HoursPerYear ~ AgeGroup + education + sex + marital.status, 
                      data = data[data$RelationshipSegment == 'Husband',])
  model_other <- lm(HoursPerYear ~ AgeGroup + education + sex + marital.status, 
                    data = data[data$RelationshipSegment != 'Husband',])
  # Making predictions for each segment
  pred_husband <- predict(model_husband, newdata = data[data$RelationshipSegment == 'Husband',])
  pred_other <- predict(model_other, newdata = data[data$RelationshipSegment != 'Husband',])
  # Combine predictions back into one vector
  pred_combined <- rep(NA, nrow(data))
  pred_combined[data$RelationshipSegment == 'Husband'] <- pred_husband
  pred_combined[data$RelationshipSegment == 'Other'] <- pred_other
  # Calculating MSE
  mse_combined <- mean((pred_combined - data$HoursPerYear)^2)
  # Output the MSE
  cat("Updated Model MSE is", mse_combined, "Benchmark MSE is", benchmark_mse)
##--CROSS VALIDATION--
  # Scramble the data frame to randomize the order
  v <- sample(1:nrow(data))
  v[1:5]
  dataScrambled <- data[v, ]
  # One-step cross-validation
  # Define the size of the test sample
  n <- 100
  trainSample <- dataScrambled[(n+1):nrow(dataScrambled), ]  # All but the first n samples for training
  testSample <- dataScrambled[1:n,]  # First n samples for testing
  # Fit a linear model on the training sample
  # Adjust the formula to include only the relevant predictors
  model <- lm(HoursPerYear ~ age + education.num + sex + marital.status, data = trainSample)
  # Predict using the fitted model on the test sample
  pred <- predict(model, newdata = testSample)
  # Calculate MSE between the observed and predicted values in the test sample
  mse_value <- mse(testSample$HoursPerYear, pred)
  # Output the MSE
  cat("MSE:", mse_value)
  
  
#--PREDICTING CAT VALUE--
##--CREATING BENCHMARK--
  # Ensure factors are correctly specified
  data$workclass <- as.factor(data$workclass)
  data$occupation <- as.factor(data$occupation)
  data$race <- as.factor(data$race)
  data$marital.status <- as.factor(data$marital.status)
  # Build the decision tree model using different predictors
  marital_status_tree <- rpart(marital.status ~ workclass + occupation + race, data = data, method = "class")
  # Predict marital status using the model
  predictions <- predict(marital_status_tree, data, type = "class")
  # Calculate accuracy of the model
  accuracy <- sum(data$marital.status == predictions) / nrow(data)
  # Print the model accuracy
  cat("Accuracy of the decision tree model predicting marital status:", accuracy, "\n")
##--PREDICTION MODEL--
  # Segment data based on age
  data$AgeGroup <- ifelse(data$age >= 45, "Older", "Younger")
  # Factors must be correctly specified
  data$AgeGroup <- as.factor(data$AgeGroup)
  data$workclass <- as.factor(data$workclass)
  data$occupation <- as.factor(data$occupation)
  data$race <- as.factor(data$race)
  # Build decision tree models for each segment
  model_younger <- rpart(marital.status ~ workclass + occupation + race, 
                         data = data[data$AgeGroup == 'Younger',], method = "class")
  model_older <- rpart(marital.status ~ workclass + occupation + race, 
                       data = data[data$AgeGroup == 'Older',], method = "class")
  # Making predictions for each segment
  pred_younger <- predict(model_younger, newdata = data[data$AgeGroup == 'Younger',], type = "class")
  pred_older <- predict(model_older, newdata = data[data$AgeGroup == 'Older',], type = "class")
  # Combine predictions back into one vector
  predictions <- rep(NA, nrow(data))
  predictions[data$AgeGroup == 'Younger'] <- levels(data$marital.status)[pred_younger]
  predictions[data$AgeGroup == 'Older'] <- levels(data$marital.status)[pred_older]
  # Calculate accuracy of the combined model
  newAccuracy <- sum(data$marital.status == predictions) / nrow(data)
  # Print the combined model accuracy
  cat("Combined Model Accuracy:", newAccuracy, "vs Benchmark Accuracy:", accuracy)
##--CROSS VALIDATION--
  # Scramble the data frame to randomize the order
  v <- sample(1:nrow(data))
  v[1:5]
  dataScrambled <- data[v, ]
  # One-step cross-validation
  # Define the size of the test sample
  n <- 100
  trainSample <- dataScrambled[(n+1):nrow(dataScrambled), ]  # All but the first n samples for training
  testSample <- dataScrambled[1:n,]  # First n samples for testing
  # Fit a decision tree model on the training sample
  model <- rpart(marital.status ~ workclass + occupation + race, data = trainSample, method = "class")
  # Predict using the fitted model on the test sample, getting probabilities
  predictions <- predict(model, newdata = testSample, type = "prob")
  # Determine the most probable class for each prediction
  predicted_classes <- colnames(predictions)[max.col(predictions, ties.method = "first")]
  # Calculate accuracy
  accuracy_value <- sum(testSample$marital.status == predicted_classes) / n
  # Output the accuracy
  cat("Accuracy:", accuracy_value)
  