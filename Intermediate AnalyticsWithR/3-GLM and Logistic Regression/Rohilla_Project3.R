#install.packages('ISLR', repos='http://cran.us.r-project.org')
library('ISLR')
#install.packages('ggplot2')

college_summary <- summary(College)
head(College)

#college_summary_table <- as.data.frame(college_summary)

library(dplyr)
College$Private <- case_when(
  College$Private == "Yes" ~ 1,
  College$Private == "No" ~ 0,
 # TRUE ~ NA_integer_  # This line handles any other values, if present
)



summary(College[c('Accept','Enroll',  'Grad.Rate', 'Apps', 'Top10perc')])
#install.packages("knitr")
#library(knitr)
selected_columns <- c('Accept','Enroll',  'Grad.Rate', 'Apps', 'Top10perc')
summary_table <- summary(College[selected_columns])
write.csv(College, file = "College.csv", row.names = FALSE)

library(ggplot2)
College$Private <- as.factor(College$Private)
plot <- ggplot(College, aes(x = Private, fill = Private)) +
  geom_bar() +
  labs(title = "Number of Applications Based on Private/Public Status", x = "Private/Public", y = "Number of Applications") +
  theme_minimal()


library(corrplot)
cor_matrix <- cor(College[, sapply(College,is.numeric)])
dev.new()
write.csv(cor_matrix, file = "cor_matrix.csv", row.names = FALSE)
corrplot(cor_matrix, method = "color")



plot2 <- ggplot(College, aes(x = Outstate, fill = Private)) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Outstate Tuition Based on Private/Public Status", x = "Outstate Tuition") +
  theme_minimal()

#install.packages('caret')
library(caret)
set.seed(123)
index <- createDataPartition(College$Accept, p = 0.7, list = FALSE)
train_data <- College[index, ]
test_data <- College[-index, ]


# Use the correct formula syntax
logistic_model <- glm(
  formula = Private ~ Outstate + perc.alumni + Room.Board + Grad.Rate,
  data = train_data,
  family = binomial  # Specify the family as binomial for logistic regression
)
summary(logistic_model)

predictions_train <- predict(logistic_model,train_data,type='response')

predicted_classes_train <- ifelse(predictions_train > 0.5, 1, 0)


predicted_classes_train <- as.factor(predicted_classes_train)
levels(predicted_classes_train)

conf_matrix_train <- confusionMatrix(predicted_classes_train, train_data$Private)

# Assuming 'conf_matrix_train' is a confusion matrix object
# Assuming 'conf_matrix_train' is a confusion matrix object

# Extract the confusion matrix values
conf_matrix_values <- as.matrix(conf_matrix_train)

# Extract additional statistics
accuracy <- conf_matrix_train$overall["Accuracy"]
sensitivity <- conf_matrix_train$byClass["Sensitivity"]
specificity <- conf_matrix_train$byClass["Specificity"]
pos_pred_value <- conf_matrix_train$byClass["Pos Pred Value"]
neg_pred_value <- conf_matrix_train$byClass["Neg Pred Value"]
kappa <- conf_matrix_train$overall["Kappa"]

# Create a data frame
conf_matrix_data <- data.frame(
  Prediction = rep(c(0, 1), each = nrow(conf_matrix_values)),
  Reference = rep(c(0, 1), times = ncol(conf_matrix_values)),
  Value = as.vector(conf_matrix_values),
  Metric = rep(c("Accuracy", "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Kappa"), each = nrow(conf_matrix_values)),
  Statistic = c(rep(accuracy, nrow(conf_matrix_values)),
                rep(sensitivity, nrow(conf_matrix_values)),
                rep(specificity, nrow(conf_matrix_values)),
                rep(pos_pred_value, nrow(conf_matrix_values)),
                rep(neg_pred_value, nrow(conf_matrix_values)),
                rep(kappa, nrow(conf_matrix_values)))
)

# Write the data frame to a CSV file
write.csv(conf_matrix_data, file = "conf_matrix_train.csv", row.names = FALSE)

# Assuming you have a confusion matrix named 'conf_matrix'
TN <- conf_matrix_values[1, 1]
FP <- conf_matrix_values[1, 2]
FN <- conf_matrix_values[2, 1]
TP <- conf_matrix_values[2, 2]

# Calculate metrics
accuracy <- (TN + TP) / sum(conf_matrix_values)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)

metrics_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity"),
  Value = c(accuracy, precision, recall, specificity)
)

write.csv(metrics_table, file = "metrics_table.csv", row.names = FALSE)

############################################################################################
# Make predictions on the test set
predictions_test <- predict(logistic_model, newdata = test_data, type = 'response')




# Convert predicted probabilities to binary classes (0 or 1) using a threshold of 0.5
predicted_classes_test <- ifelse(predictions_test > 0.5, 1, 0)
result_df <- data.frame(College = names(predicted_classes_test), Predicted_Class = (predicted_classes_test))

test_data$Private <- factor(test_data$Private, levels = c(0, 1))
predicted_classes_test<- factor(predicted_classes_test, levels = c(0, 1))
# Create a confusion matrix for the test set
conf_matrix_test <- confusionMatrix(predicted_classes_test, test_data$Private)
# Print the confusion matrix and associated metrics
summary(conf_matrix_test)

# Extract additional statistics
accuracy <- conf_matrix_test$overall["Accuracy"]
sensitivity <- conf_matrix_test$byClass["Sensitivity"]
specificity <- conf_matrix_test$byClass["Specificity"]
pos_pred_value <- conf_matrix_test$byClass["Pos Pred Value"]
neg_pred_value <- conf_matrix_test$byClass["Neg Pred Value"]
kappa <- conf_matrix_test$overall["Kappa"]

conf_matrix_values_test <- as.matrix(conf_matrix_test)

# Create a data frame
conf_matrix_data_test <- data.frame(
  Prediction = rep(c(0, 1), each = nrow(conf_matrix_values_test)),
  Reference = rep(c(0, 1), times = ncol(conf_matrix_values_test)),
  Value = as.vector(conf_matrix_values_test),
  Metric = rep(c("Accuracy", "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Kappa"), each = nrow(conf_matrix_values_test)),
  Statistic = c(rep(accuracy, nrow(conf_matrix_values_test)),
                rep(sensitivity, nrow(conf_matrix_values_test)),
                rep(specificity, nrow(conf_matrix_values_test)),
                rep(pos_pred_value, nrow(conf_matrix_values_test)),
                rep(neg_pred_value, nrow(conf_matrix_values_test)),
                rep(kappa, nrow(conf_matrix_values_test)))
)

# Write the data frame to a CSV file
write.csv(conf_matrix_data_test, file = "conf_matrix_test.csv", row.names = FALSE)
---------------------------------------------------------------------------------------

# Convert factor to numeric
predicted_classes_numeric <- as.numeric(as.character(predicted_classes_test))

# Load the pROC library
library(pROC)

# Create the ROC curve
roc_curve <- roc(test_data$Private, predicted_classes_numeric)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

auc_value <- auc(roc_curve)
abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal line (random classifier)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)





















