#--LOADING DATA--
library(rpart)
install.packages("devtools")
devtools::install_github("devanshagr/CrossValidation")
CrossValidation::cross_validate()
housing <- read.csv("/Users/itayakad/Desktop/Data 101 HWs/Data 101 - HW4/housing.csv")
data <- na.omit(housing)
head(data)

#--EDA--
# Histograms for numerical variables
hist(data$median_house_value, main="Distribution of Median House Value", xlab="Median House Value", col="blue")
hist(data$median_income, main="Distribution of Median Income", xlab="Median Income", col="green")
# Scatter plot of median_income vs median_house_value
plot(data$median_income, data$median_house_value, main="Median Income vs Median House Value",
     xlab="Median Income", ylab="Median House Value", pch=19, col=rgb(0,0,1,0.5))
# Bar plot for categorical variable 'ocean_proximity'
table_proximity <- table(data$ocean_proximity)
barplot(table_proximity, main="Frequency of Ocean Proximity Types", col="red", xlab="Ocean Proximity", ylab="Frequency")

#--CREATING NEW FEATURES--
data$rooms_per_household <- data$total_rooms / data$households
data$rooms_per_household
data$bedrooms_per_room <- data$total_bedrooms / data$total_rooms
data$bedrooms_per_room
data$population_per_household <- data$population / data$households
data$population_per_household
data$income_per_population <- data$median_income / data$population
data$income_per_population
data$age_category <- cut(data$housing_median_age, breaks=c(0, 10, 20, 30, 40, 50, Inf), labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50+"))
data$age_category
data$house_value_per_room <- data$median_house_value / data$total_rooms
data$house_value_per_room
# Binning 'median_house_value' into categories
data$median_house_value_cat <- cut(data$median_house_value, 
                                   breaks = quantile(data$median_house_value, probs = c(0, 0.25, 0.5, 0.75, 1)),
                                   labels = c("Low", "Below_Average", "Above_Average", "High"),
                                   include.lowest = TRUE)
# Using median_house_value (numerical) and ocean_proximity (categorical)

#--NUMERICAL PREDICTION--
# Establishing Benchmarks
# Linear Regression Model
lm_model <- lm(median_house_value ~ ., data = data)
lm_predictions <- predict(lm_model, data)
# Regression Tree Model for Numerical Prediction
rpart_numerical_model <- rpart(median_house_value ~ ., data = data)
rpart_predictions <- predict(rpart_numerical_model, data)
# Calculate MSE for both models using the entire data set
lm_mse <- mean((data$median_house_value - lm_predictions)^2)
rpart_mse <- mean((data$median_house_value - rpart_predictions)^2)
# Determine the lower MSE out of the two
if (lm_mse < rpart_mse) {
  print("Linear Regression has the lower MSE.")
  numBM <- lm_mse
  numBM
} else {
  print("Regression Tree has the lower MSE.")
  numBM <- rpart_mse
  numBM
}

# Building Models
# Create the Linear Regression Model
lm_model <- lm(median_house_value ~ ., data = data)
# Create the Regression Tree Model
rpart_model <- rpart(median_house_value ~ ., data = data)
# Generate predictions from both models on the same data set
lm_predictions <- predict(lm_model, data)
rpart_predictions <- predict(rpart_model, data)
# Combine the predictions: Simple averaging method
combined_predictions <- (lm_predictions + rpart_predictions) / 2
# Calculate MSE for each model and the combined model
lm_mse <- mean((data$median_house_value - lm_predictions)^2)
rpart_mse <- mean((data$median_house_value - rpart_predictions)^2)
combined_mse <- mean((data$median_house_value - combined_predictions)^2)
# Output the MSE results
print(paste("MSE for Linear Regression Model:", lm_mse))
print(paste("MSE for Regression Tree Model:", rpart_mse))
print(paste("MSE for Combined Model:", combined_mse))