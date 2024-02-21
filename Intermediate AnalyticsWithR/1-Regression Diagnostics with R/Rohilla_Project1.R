
# Load necessary libraries
library(ggplot2)
library(corrplot)
library(car)
library(leaps)
library(dplyr)

ames_data <- read.csv('AmesHousing.csv')

# Impute Missing Values
# For numeric columns, replace NA with mean
numerical_cols <- sapply(ames_data, is.numeric)
ames_data[numerical_cols] <- lapply(ames_data[numerical_cols], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

summary(ames_data)
head(ames_data)
str(ames_data)
numerical_columns <- ames_data[sapply(ames_data, is.numeric)]
# Subset the dataset with the specified variables
selected_vars <- ames_data[, c("Order", "PID", "Garage.Cars", "Lot.Frontage", "Lot.Area",
                               "Overall.Qual", "Overall.Cond", "Open.Porch.SF")]

# Summary for selected variables
summary_table_selected <- data.frame(
  Variable = character(),
  Min = numeric(),
  Q1 = numeric(),
  Median = numeric(),
  Mean = numeric(),
  Q3 = numeric(),
  Max = numeric(),
  stringsAsFactors = FALSE  # Ensure character columns are not converted to factors
)

# Calculate summary statistics for each variable
for (variable in colnames(selected_vars)) {
  min_val <- min(selected_vars[[variable]], na.rm = TRUE)
  q1_val <- quantile(selected_vars[[variable]], 0.25, na.rm = TRUE)
  median_val <- median(selected_vars[[variable]], na.rm = TRUE)
  mean_val <- mean(selected_vars[[variable]], na.rm = TRUE)
  q3_val <- quantile(selected_vars[[variable]], 0.75, na.rm = TRUE)
  max_val <- max(selected_vars[[variable]], na.rm = TRUE)
  summary_table_selected <- rbind(summary_table_selected, 
                                  c(variable, min_val, q1_val, median_val, mean_val, q3_val, max_val))
}
colnames(summary_table_selected) <- c("Variable", "Min", "1st Quartile", "Median", "Mean",
                                      "3rd Quartile", "Max")
# Print the summary table for selected variables
print(summary_table_selected)
write.csv(summary_table_selected, file = "summary_table_selected.csv", row.names = FALSE)

library(dplyr)
library(ggplot2)

# Explore the distribution of SalePrice
plot_distributions_SalePrice <- ggplot(ames_data, aes(x = SalePrice)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of SalePrice", x = "Sale Price", y = "Frequency")
ggsave("plot_distributions_SalePrice.png", plot_distributions_SalePrice, width = 8, height = 6, units = "in", dpi = 300)

# Explore the relationship between SalePrice and Gr Liv Area using a scatter plot
plotSalePrice_Gr.Liv.Area <-ggplot(ames_data, aes(x = Gr.Liv.Area, y = SalePrice)) +
  geom_point(color = "coral") +
  labs(title = "SalePrice vs. Gr Liv Area", x = "Above Grade Living Area", y = "Sale Price")
ggsave("plotSalePrice_Gr.Liv.Area.png", plotSalePrice_Gr.Liv.Area, width = 8, height = 6, units = "in", dpi = 300)



# Correlation Matrix of Numeric Values
cor_matrix <- cor(ames_data[, numerical_cols], use = "complete.obs")
rounded_cor_matrix <- round(cor_matrix, 2)
rounded_cor_matrix
write.csv(rounded_cor_matrix, file = "rounded_cor_matrix.csv", row.names = FALSE)


# Plot the Correlation Matrix
corplot <- corrplot(cor_matrix, method = "color",tl.cex = 0.7, tl.srt = 75)
png("corplot.png", width = 800, height = 600, units = "px", res = 300)
dev.off()


# Scatter Plots for Variables with High, Low, and ~0.5 Correlation with SalePrice
# Assuming SalePrice is  target variable

cor_threshold <- 0.5
high_corr_vars <- names(cor_matrix['SalePrice', abs(cor_matrix['SalePrice',]) > cor_threshold])

high_corr_var <- names(sort(cor_matrix['SalePrice',], decreasing = TRUE)[2])
low_corr_var <- names(sort(cor_matrix['SalePrice',], decreasing = FALSE)[2])
mid_corr_var <- names(sort(abs(cor_matrix['SalePrice',] - 0.5), decreasing = FALSE)[1])

high_corr_plot <- ggplot(ames_data, aes_string(x=high_corr_var, y='SalePrice')) + geom_point() + ggtitle('High Correlation Scatter Plot')
ggsave("high_corr_plot.png", high_corr_plot, width = 8, height = 6, units = "in", dpi = 300)

mid_corr_plot <- ggplot(ames_data, aes_string(x=mid_corr_var, y='SalePrice')) + geom_point() + ggtitle('Mid Correlation Scatter Plot')
ggsave("mid_corr_plot.png", mid_corr_plot, width = 8, height = 6, units = "in", dpi = 300)


low_corr_plot <- ggplot(ames_data, aes_string(x=low_corr_var, y='SalePrice')) + geom_point() + ggtitle('Low Correlation Scatter Plot')
ggsave("low_corr_plot.png", low_corr_plot, width = 8, height = 6, units = "in", dpi = 300)



# Fit a Regression Model
model <- lm(SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area + Garage.Cars + Full.Bath, data=ames_data)
summary_model <- summary(model)
plot(model)

# Extract relevant information
residual_standard_error <- summary_model$sigma
multiple_r_squared <- summary_model$r.squared
adjusted_r_squared <- summary_model$adj.r.squared
f_statistic <- summary_model$fstatistic[1]

# Create a table
table_data <- data.frame(
  "Residual Standard Error" = residual_standard_error,
  "Multiple R-squared" = multiple_r_squared,
  "Adjusted R-squared" = adjusted_r_squared,
  "F-statistic" = f_statistic
)

write.csv(table_data, file = "table_data.csv", row.names = FALSE)


# Plot Regression Model
par(mfrow=c(2,2))
plot(model)


# Check for Multicollinearity
vif_values <- vif(model)
write.csv(vif_values, file = "vif_values.csv", row.names = TRUE)

# Check for Outliers
# Using Cook's Distance
plot(model, which=4)



library(ggplot2)

# Check for missing values in Total.Bsmt.SF
if (any(is.na(ames_data$Total.Bsmt.SF))) {
  stop("Missing values in Total.Bsmt.SF.")
}

# Check for zero or negative values in Total.Bsmt.SF
if (any(ames_data$Total.Bsmt.SF <= 0)) {
  # Handle zero or negative values by replacing them with a small constant
  constant <- 0.001  # Adjust as needed
  ames_data$Total.Bsmt.SF <- ifelse(ames_data$Total.Bsmt.SF <= 0, constant, ames_data$Total.Bsmt.SF)
}

# Add a small constant to avoid issues with zero values
ames_data$log_Total.Bsmt.SF <- log(ames_data$Total.Bsmt.SF)
ames_data$sqrt_Gr.Liv.Area <- sqrt(ames_data$Gr.Liv.Area)

# Re-run the model with transformed variables
transformed_model <- lm(SalePrice ~ log_Total.Bsmt.SF + sqrt_Gr.Liv.Area + Garage.Cars + Full.Bath, data = ames_data)
summary(transformed_model)

# 2. Outlier Handling: Evaluate and Remove Outliers
outliers <- which(abs(residuals(model)) > 3 * sd(residuals(model)))
ames_data_no_outliers <- ames_data[-outliers, ]

# Re-run the model without outliers
model_no_outliers <- lm(SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area + Garage.Cars + Full.Bath, data = ames_data_no_outliers)
summary(model_no_outliers)

par(mfrow=c(2,2))
plot(model_no_outliers)

summary_model_no_outliers <- summary(model_no_outliers)

# Extract relevant information
residual_standard_error_no_outliers <- summary_model_no_outliers$sigma
multiple_r_squared_no_outliers <- summary_model_no_outliers$r.squared
adjusted_r_squared_no_outliers <- summary_model_no_outliers$adj.r.squared
f_statistic_no_outliers <- summary_model_no_outliers$fstatistic[1]

# Create a table
table_data_no_outliers <- data.frame(
  "Residual Standard Error" = residual_standard_error_no_outliers,
  "Multiple R-squared" = multiple_r_squared_no_outliers,
  "Adjusted R-squared" = adjusted_r_squared_no_outliers,
  "F-statistic" = f_statistic_no_outliers
)
write.csv(table_data_no_outliers, file = "table_data_no_outliers.csv", row.names = FALSE)
# 3. Heteroscedasticity: Weighted Least Squares Regression
weighted_model <- lm(SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area + Garage.Cars + Full.Bath, 
                     data = ames_data, weights = 1 / residuals(model)^2)

# 4. Assess Influential Points
influential_points <- which(hatvalues(model) > 2 * mean(hatvalues(model)))

# Re-run the model without influential points
model_no_influential <- lm(SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area + Garage.Cars + Full.Bath, 
                           data = ames_data[-influential_points, ])
summary(model_no_influential)



# 5. Model Validation: Cross-Validation
# Consider using a cross-validation method suitable for your data, e.g., k-fold cross-validation
# Evaluate the performance of the model using appropriate metrics

# Display diagnostic plots for the transformed model
par(mfrow=c(2,2))
plot(transformed_model)

# Additional plots for the model without outliers and without influential points
# You can customize these plots based on your needs
par(mfrow=c(2,2))
plot(model_no_outliers)

par(mfrow=c(2,2))
plot(model_no_influential, which = 1)


library(leaps)

# Specify the model formula
formula <- SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area + Garage.Cars + Full.Bath

# Run subset regression
model_subsets <- regsubsets(formula, data = ames_data)

# Explore results
summary(model_subsets)

