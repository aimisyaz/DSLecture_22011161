ozone <- c(11,11,11,12,12,13,13,13,13,14)
solar <- c(290,44,320,149,120,137,112,27,238,274)
wind  <- c(9.2,9.7,16.6,12.6,11.5,10.3,11.5,10.3,12.6,10.9)
temp  <- c(66,62,73,74,73,76,71,76,64,68)

data <- data.frame(ozone, solar, wind, temp)

set.seed(123)

train_index <- sample(1:nrow(data), size = 0.7 * nrow(data))

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Build Multiple Linear Regression Model
model <- lm(ozone ~ solar + wind + temp, data = train_data)

# Model summary
summary(model)

# Prediction
predicted_ozone <- predict(model, newdata = test_data)

comparison <- data.frame(
  Actual_Ozone = test_data$ozone,
  Predicted_Ozone = predicted_ozone
)

print(comparison)


plot(comparison$Actual_Ozone, comparison$Predicted_Ozone,
     main = "Actual vs Predicted Ozone",
     xlab = "Actual Ozone",
     ylab = "Predicted Ozone",
     pch = 19)

abline(0,1, col="red", lwd=2)
```

