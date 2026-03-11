experience <- c(1,2,3,4,5,6,7,8,9,10)
salary <- c(2500,2700,3000,3400,3900,4400,5000,5600,6200,6900)

data <- data.frame(experience, salary)

# View dataset
print(data)

# Split data into 70% training and 30% testing
set.seed(123)

train_index <- sample(1:nrow(data), size = 0.7 * nrow(data))

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Display training and testing data
print("Training Data:")
print(train_data)

print("Testing Data:")
print(test_data)

# Create simple linear regression model
model <- lm(salary ~ experience, data = train_data)

# Show model summary
summary(model)

# Predict salary using test data
predicted_salary <- predict(model, newdata = test_data)

# Show predictions
print("Predicted Salaries:")
print(predicted_salary)

# Compare actual vs predicted
comparison <- data.frame(
  Experience = test_data$experience,
  Actual_Salary = test_data$salary,
  Predicted_Salary = predicted_salary
)

print("Actual vs Predicted Salary:")
print(comparison)

# Visualization: Scatter plot with regression line
plot(data$experience, data$salary,
     main = "Linear Regression: Experience vs Monthly Salary",
     xlab = "Years of Experience",
     ylab = "Monthly Salary (RM)",
     pch = 19)

# Add regression line
abline(model, col = "blue", lwd = 2)

