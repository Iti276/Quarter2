#install.packages('ISLR', repos='http://cran.us.r-project.org')
library('ISLR')
df <- College

#write.csv(df, file = "df.csv", row.names = FALSE)


library(corrplot)
#cor_matrix <- cor(df[, sapply(df,is.numeric)])
#dev.new()
#write.csv(cor_matrix, file = "cor_matrix.csv", row.names = FALSE)




#install.packages('caret')
library(caret)
set.seed(123)
index <- createDataPartition(df$Grad.Rate, p = 0.7, list = FALSE)
train_data <- College[index, ]
test_data <- College[-index, ]
#write.csv(test_data, file = "test_data.csv", row.names = FALSE)

X <- as.matrix(train_data[, -c(1, ncol(train_data))])
y <- train_data$Grad.Rate

#install.packages("glmnet")
library(glmnet)
cvfit <- cv.glmnet(X, y)
results <- data.frame(
  Lambda = c("min", "1se"),
  Index = c(0.2965, 1.5821),
  Measure_Value = c(39, 21),
  SE = c(170.3, 179.6),
  Nonzero = c(9.967, 11.026)
)
#write.csv(results, file = "results.csv", row.names = FALSE)


par(mar = c(2, 2, 2, 2))  # Adjust margins
plot(cvfit)
abline(v = log(cvfit$lambda.min), col = "red", lty = 2)
abline(v = log(cvfit$lambda.1se), col = "blue", lty = 2)

# Add labels for lambda.min and lambda.1se
text(log(cvfit$lambda.min), max(cvfit$cvm) + 0.2, expression(paste("lambda.min")), col = "red")
text(log(cvfit$lambda.1se), max(cvfit$cvm) + 0.2, expression(paste("lambda.1se")), col = "blue")

# Add labels and a legend
legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("red", "blue"), lty = 2)

ridge_model <- glmnet(X, y, alpha = 0, lambda = 0.2719)
coefficients <- coef(ridge_model, s = 0.2719)
print(coefficients)
coefficients_matrix <- as.matrix(coefficients)

coefficients_df <- as.data.frame(coefficients_matrix)
#write.csv(coefficients_df, file = "coefficients_df.csv", row.names = FALSE)


# Predict on the training set
predictions <- predict(ridge_model, newx = X)
rmse <- sqrt(mean((y - predictions)^2))
# Print the RMSE
cat("Root Mean Squared Error on the training set:", rmse, "\n")



X_test <- as.matrix(test_data[, -c(1, ncol(test_data))])
y_test <- test_data$Grad.Rate
predictions_test <- predict(ridge_model, newx = X_test)
rmse_test <- sqrt(mean((y_test - predictions_test)^2))
# Print the RMSE on the test set
cat("Root Mean Squared Error on the test set:", rmse_test, "\n")

# Compare training and test set RMSE [Ridge]
cat("Training set RMSE:", rmse, "\n")
cat("Test set RMSE:", rmse_test, "\n")
------------------------------------------------------------------------------------

lasso_cvfit <- cv.glmnet(X, y, alpha = 1)
lasso_lambda.min <- lasso_cvfit$lambda.min
lasso_lambda.1se <- lasso_cvfit$lambda.1se
mse_data <- data.frame(
  "Lambda Index" = c("min", "1se"),
  "Lambda" = c(0.3919, 1.7364),
  "Measure" = c(36, 20),
  "SE" = c(169.8, 180.8),
  "Nonzero" = c(11, 7)
)
#write.csv(mse_data, file = "mse_data.csv", row.names = FALSE)


lasso_model <- glmnet(X, y, alpha = 1, lambda = lasso_lambda.min)
results_lasso_model <- data.frame(
  "Model Name" = "Lasso Model",
  "Df" = 11,
  "%Dev" = 45.95,
  "Lambda" = 0.391
)
#write.csv(lasso_model, file = "lasso_model.csv", row.names = FALSE)



library(glmnet)

# Assuming lasso_cvfit is your cv.glmnet object
# cv.glmnet object is typically generated using cv.glmnet() function

# Plot the cross-validated error paths
plot(lasso_cvfit)

# Extract lambda.min and lambda.1se
lambda_min <- lasso_cvfit$lambda.min
lambda_1se <- lasso_cvfit$lambda.1se

# Get the coefficients for lambda.min and lambda.1se
coef_min <- coef(lasso_cvfit, s = lambda_min)
coef_1se <- coef(lasso_cvfit, s = lambda_1se)

# Convert sparse matrix to a regular matrix
coef_min_matrix <- as.matrix(coef_min)

# Convert matrix to data frame
coef_min_df <- as.data.frame(coef_min_matrix)

# Write data frame to CSV file
write.csv(coef_min_df, file = "coef_min.csv", row.names = FALSE)



write.csv(coef_min, file = "coef_min.csv", row.names = FALSE)

zero_coefficients <- which(coef_min == 0)

# Label the lines
abline(v = log(lambda_min), col = "red", lty = 2)
text(log(lambda_min), max(coef_min) + 0.1, expression(lambda.min), col = "red")

abline(v = log(lambda_1se), col = "blue", lty = 2)
text(log(lambda_1se), max(coef_1se) + 0.1, expression(lambda.1se), col = "blue")

# Add labels and a legend
legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("red", "blue"), lty = 2)


X <- as.matrix(train_data[, -c(1, ncol(train_data))])
y <- train_data$Grad.Rate
# Predict on the training set
predictions <- predict(lasso_model, newx = X)
rmse <- sqrt(mean((y - predictions)^2))
# Print the RMSE
cat("Root Mean Squared Error on the training set:", rmse, "\n")



X_test <- as.matrix(test_data[, -c(1, ncol(test_data))])
y_test <- test_data$Grad.Rate
predictions_test <- predict(lasso_model, newx = X_test)
rmse_test <- sqrt(mean((y_test - predictions_test)^2))
# Print the RMSE on the test set
cat("Root Mean Squared Error on the test set:", rmse_test, "\n")

# Compare training and test set RMSE [Lasso]
cat("Training set RMSE:", rmse, "\n")
cat("Test set RMSE:", rmse_test, "\n")
----------------------------------------------------------------------------
step_model <- step(lm(Grad.Rate ~ ., data = train_data), direction = "both")

# Print the summary of the stepwise selected model
summary(step_model)

# Predict on the training set
predictions_step <- predict(step_model, newdata = train_data)
rmse_step_train <- sqrt(mean((train_data$Grad.Rate - predictions_step)^2))
cat("Root Mean Squared Error on the training set (Stepwise Selection):", rmse_step_train, "\n")

# Predict on the test set
predictions_step_test <- predict(step_model, newdata = test_data)
rmse_step_test <- sqrt(mean((test_data$Grad.Rate - predictions_step_test)^2))
cat("Root Mean Squared Error on the test set (Stepwise Selection):", rmse_step_test, "\n")

#Stepwise RMSE values
cat("Root Mean Squared Error on the training set (Stepwise Selection):", rmse_step_train, "\n")
cat("Root Mean Squared Error on the test set (Stepwise Selection):", rmse_step_test, "\n")
----------------------------------------------------------------------------------------
  # Create a data frame for the RMSE values
  rmse_data <- data.frame(
    Method = c("Ridge Regression", "LASSO", "Stepwise Selection"),
    Training_RMSE = c(12.9952, 13.07481, 12.96018),
    Test_RMSE = c(11.80957, 11.89388, 12.00483)
  )

# Print the table
print(rmse_data)

write.csv(rmse_data, file = "rmse_data.csv", row.names = FALSE)



