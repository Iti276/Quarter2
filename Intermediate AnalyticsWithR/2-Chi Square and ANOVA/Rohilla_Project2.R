observed <- c(12, 8, 24, 6)
expected <- c(0.2, 0.28, 0.36, 0.16)

frequency_table <- rbind(Observed = observed, Expected = expected)
colnames(frequency_table) <- c("A","B","O","AB")

chi_squared_test <- chisq.test(observed, p = expected)

test_statistic <- chi_squared_test$statistic
p_value <- chi_squared_test$p.value
df <- chi_squared_test$parameter

critical_value <- qchisq(1 - 0.10, df)

if (test_statistic > critical_value) {
  decision <- "Reject H₀ & Accept the Alternative H1"
} else {
  decision <- "Accept H₀ & Reject Alternative H1"
  
  results <- data.frame(
    Test_Statistic = test_statistic,
    Critical_Value = critical_value,
    
    P_Value = p_value,
    Decision = decision
  )
}

library(kableExtra)

# Display the results table using kable
results_table <- kable(results, format = "html", col.names = c("Test Statistic", "Critical Value", "P-Value", "Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")

# Print the table
print(results_table)
########################################################################################################

observed_2 <- c(125, 40, 10, 25)
expected_2 <- c(0.708,0.082, 0.09, 0.12)

frequency_table_2 <- rbind(Observed = observed_2, Expected = expected_2)
colnames(frequency_table_2) <- c("On_Time","System_Delay","Arriving_Late","Other")

chi_square_test_2 <- chisq.test(x = observed_2, p = expected_2)

test_statistic_2 <- chi_square_test_2$statistic
p_value_2 <- chi_square_test_2$p.value
df_2 <- chi_square_test_2$parameter

critical_value_2 <- qchisq(1-0.05, df_2)

if (test_statistic_2 > critical_value_2) {
  decision_2 <- "Reject H₀ & Accept the Alternative H1"
} else {
  decision_2 <- "Accept H₀ & Reject Alternative H1"
  
  result_2 <- data.frame(
    Test_Statistic = test_statistic_2,
    Critical_Value = critical_value_2,
    P_Value = format(p_value_2,digits=4),
    Decision = decision_2
  )
}

library(kableExtra)

# Display the results table using kable
results_table_2 <- kable(result_2, format = "html", col.names = c("Test Statistic", "Critical Value", "P-Value", "Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")

# Print the table
print(results_table_2)
#################################################
admissions <- matrix(c(724, 335, 174, 107, 370, 292, 152,140), nrow = 2, byrow = TRUE)
rownames(admissions) <- c("2013", "2014")
colnames(admissions) <- c("Caucasian", "Hispanic", "African American", "Other")

chi_square_test_3 <- chisq.test(admissions)
df <- (nrow(admissions) - 1) * (ncol(admissions) - 1)

critical_value_3 <- qchisq(0.95, df)

if (chi_square_test_3$statistic > critical_value_3) {
  decision_3 <- "Reject H₀ & Accept the Alternative H1"
} else {
  decision_3 <- " Accept H₀ & Reject Alternative H1"
}

result_summary_3 <- data.frame(
  TestStatistic = chi_square_test_3$statistic,
  CriticalValue = critical_value_3,
    PValue = format(chi_square_test_3$p.value,digits = 4),
  Decision = decision_3
)

library(kableExtra)

# Display the results table using kable
results_table_3 <- kable(result_summary_3, format = "html", col.names = c("Test Statistic", "Critical Value", "P-Value", "Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")

# Print the table
print(results_table_3)
#########################################################
Rank <-matrix(c(10791,62491,7816,42750,932,9525,11819,54344),nrow = 4, byrow = TRUE)
rownames(Rank) <- c("Army","Navy","Marine Corps","Marine Corps")
colnames(Rank) <- c("Officers","Enlisted")

chi_square_test_4 <- chisq.test(Rank)

df_4 <- chi_square_test_4$parameter
critical_value_4 <- qchisq(0.05,df_4)

if (chi_square_test_3$statistic > critical_value_3) {
  decision_4 <- "Reject H₀ & Accept the Alternative H1"
} else {
  decision_4 <- " Accept H₀ & Reject Alternative H1"
}

result_summary_4 <- data.frame(
test_statistic_4 = chi_square_test_4$statistic,
CriticalValue = critical_value_4,
p_value_4 = format(chi_square_test_4$p.value,digits = 4),
Decision = decision_4)

library(kableExtra)

# Display the results table using kable
results_table_4 <- kable(result_summary_4, format = "html", col.names = c("Test Statistic", "Critical Value", "P-Value", "Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")

# Print the table
print(results_table_4)
######################################################
condiments <- c(270, 130, 230, 180, 80, 70, 200, 160)
cereals <- c(260, 220, 290, 290, 200, 320, 140)
desserts <- c(100, 180, 250, 250, 300, 360, 300)

data <- data.frame(
  Sodium = c(condiments, cereals, desserts),
  Group = rep(c("Condiments", "Cereals", "Desserts"), times = c(length(condiments), length(cereals), length(desserts)))
)

# Perform one-way ANOVA
result_anova_5 <- aov(Sodium ~ Group, data = data)

# Print ANOVA table
anova_summary_5 <- summary(result_anova_5)

# Significance level
alpha <- 0.05


p_value_5 <- summary(result_anova_5)[[1]]$`Pr(>F)`[1]

# Decision-making based on ANOVA
if (p_value_5 < alpha) {
  Decision_5 <-"Reject the null hypothesis (ANOVA) is significant"
} else {
  Decision_5 <-"The one-way ANOVA result is not significant"
}

# Creating a data frame for ANOVA summary
anova_summary_table_5 <- data.frame(
  S_NO <- c(1,2),
  Df = c(2, 19),
  Sum_Sq = c(34379, 102257),
  Mean_Sq = c(17190, 5382),
  F_value = c(3.194, NA),
  `Pr(>F)` = c(0.0637, NA),
  Decision = Decision_5
)


library(kableExtra)

# Display the results table using kable
results_table_5 <- kable(anova_summary_table_5, format = "html", col.names = c("S_NO","Df", "Sum_Sq","Mean_Sq", "F-Value", "Pr..F","Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")

# Check if there are significant differences between groups using Tukey's HSD
posthoc <- TukeyHSD(result_anova)
posthoc_summary <- summary(posthoc)

# Print the Tukey HSD results
print(posthoc_summary)

# Extract significant pairs
significant_pairs <- coef(posthoc)
significant_pairs_data <- significant_pairs[, 4] < alpha

if (any(significant_pairs_data)) {
  cat("Reject the null hypothesis (Tukey's HSD) for the following pairwise comparisons:\n")
  print(significant_pairs[significant_pairs_data, 1:2])
} else {
  cat("Fail to reject the null hypothesis (Tukey's HSD) - No significant pairwise differences detected.\n")
}
######################################################################################################
# Sales data with missing value
cereal <- c(578, 320, 264, 249, 237)
chocolate_candy <- c(311, 106, 109, 125, 173)
coffee <- c(261, 185, 302, 689, NA)  # Assuming there's a missing value in the data
coffee[is.na(coffee)] <- mean(coffee, na.rm = TRUE)

# Combine data into a data frame
data_6 <- data.frame(
  group = rep(c("Cereal", "Chocolate_Candy", "Coffee"), each = c(length(cereal), length(chocolate_candy), length(coffee))),
  sales = c(cereal, chocolate_candy, coffee)
)

# Convert 'group' to factor
data_6$group <- factor(data_6$group)

# Perform one-way ANOVA
result_anova_6 <- aov(sales ~ group, data = data_6)

# Summary of ANOVA
anova_summary_6 <- summary(result_anova_6)


p_value_6 <- summary(result_anova_6)[[1]]$`Pr(>F)`[1]

if (p_value_6 < 0.01) {
  Decision_6 <-"Reject the null hypothesis (ANOVA) is significant"
} else {
  Decision_6 <- "The one-way ANOVA result is not significant"
}

# Creating a data frame for ANOVA summary
anova_summary_table_6 <- data.frame(
  S_NO <- c(1,2),
  Df = c(2, 12),
  Sum_Sq = c(109748, 262795),
  Mean_Sq = c(54874, 21900),
  F_value = c(2.506, NA),
  `Pr(>F)` = c(0.123, NA),
  Decision = Decision_6
)


library(kableExtra)

# Display the results table using kable
results_table_6 <- kable(anova_summary_table_6, format = "html", col.names = c("S_NO","Df", "Sum_Sq","Mean_Sq", "F-Value", "Pr..F","Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")
##################################################
# Expenditure data
eastern <- c(4946, 5953, 6202, 7243, 6113)
middle <- c(6149, 7451, 6000, 6479)
western <- c(5282, 8605, 6528, 6911)

# Combine data into a single data frame
expenditure_data <- data.frame(
  Expenditure = c(eastern, middle, western),
  Group = rep(c("Eastern", "Middle", "Western"), times = c(length(eastern), length(middle), length(western)))
)

# One-way ANOVA
anova_result_7 <- aov(Expenditure ~ Group, data = expenditure_data)

# Print ANOVA summary
summary(anova_result_7)

# Check p-value
p_value <- summary(anova_result_7)[[1]]$`Pr(>F)`[1]

# Compare p-value to significance level (e.g., 0.05)
if (p_value < 0.05) {
  Decision_7 <- "ANOVA is significant. Reject the null hypothesis."
  # Perform post hoc test (Tukey) if ANOVA is significant
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
} else {
  Decision_7 <- "ANOVA is not significant. Fail to reject the null hypothesis."
}

# Creating a data frame for ANOVA summary
anova_summary_table_7 <- data.frame(
  S_NO <- c(1,2),
  Df = c(2, 10),
  Sum_Sq = c(1244588, 9591145),
  Mean_Sq = c(622294, 959114),
  F_value = c(0.649, NA),
  `Pr(>F)` = c(0.543, NA),
  Decision = Decision_7
)


library(kableExtra)

# Display the results table using kable
results_table_7 <- kable(anova_summary_table_7, format = "html", col.names = c("S_NO","Df", "Sum_Sq","Mean_Sq", "F-Value", "Pr..F","Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")
######################################################################################
# Plant growth data
grow_light_1 <- c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9)
grow_light_2 <- c(7.1, 7.2, 8.5, 5.5, 5.8, 7.6)
plant_food_A <- c(rep("A", 3), rep("A", 3))
plant_food_B <- c(rep("B", 3), rep("B", 3))

# Combine data into a data frame
plant_growth_data <- data.frame(
  Growth = c(grow_light_1, grow_light_2),
  Grow_light = rep(c("Grow-light 1", "Grow-light 2"), each = 6),
  Plant_food = c(plant_food_A, plant_food_B)
)

# Two-way ANOVA
anova_result <- aov(Growth ~ Grow_light * Plant_food, data = plant_growth_data)

# Print ANOVA summary
summary(anova_result)
### check if Tukey_test is to be done
####################
df <-read.csv('baseball.csv')
colnames(df)
summary(df)

summary(df[c('RS','RankSeason',  'RA', 'SLG', 'W')])
#install.packages("knitr")
library(knitr)
selected_columns <- c('RS','RankSeason',  'RA', 'SLG', 'W')
summary_table <- summary(df[selected_columns])
kable(summary_table, format = "markdown", digits = 3)

write.csv(summary_table, file = "summary_table.csv", row.names = FALSE)

library(ggplot2)

histograms <- df %>%
  ggplot(aes(x = W)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Wins")

library(dplyr)

df$Year <- as.factor(df$Year)

agg_data <- df %>%
  group_by(Year) %>%
  summarise(total_RS = sum(RS)) %>%
  mutate(Decade = as.factor(paste0(floor(as.numeric(Year) / 10) * 10, "s")))

time_series_plot <- agg_data %>%
  ggplot(aes(x = Decade, y = total_RS, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Time Series of Aggregated Runs Scored",
       x = "Decade",
       y = "Total Runs Scored")

print(time_series_plot)

decade_data <- data.frame(
  Decade = factor(c("20s", "30s", "40s")),  # Replace with your actual decade values
  Wins = c(10, 15, 20)  # Replace with your actual wins data
)

chi_square_test_n <- chisq.test(decade_data$Wins)

test_statistic <- chi_square_test_n$statistic
p_value <- chi_square_test_n$p.value
df <- chi_square_test_n$parameter

df <- 2 
critical_value <- qchisq(0.95, df)

if (test_statistic > critical_value) {
  decision <- "Reject H₀ & Accept the Alternative H1"
} else {
  decision <- "Accept H₀ & Reject Alternative H1"
  
  results <- data.frame(
    Test_Statistic = test_statistic,
    Critical_Value = critical_value,
    P_Value = p_value,
    Decision = decision
  )
}
library(kableExtra)

# Display the results table using kable
results_table_l <- kable(results, format = "html", col.names = c("Test Statistic", "Critical Value", "P-Value", "Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")

# Print the table
print(results_table_l)
###########################################################################################
data <- read.csv('crop_data.csv')

data$density <- as.factor(data$density)
data$fertilizer <- as.factor(data$fertilizer)
data$block <- as.factor(data$block)

# Assuming 'yield' is the dependent variable
anova_result_7 <- aov(yield ~ density * fertilizer, data = data)

anova_summary_table_7  <- summary(anova_result_7)

# Set the significance level
alpha <- 0.05

# Remove NA values from p-values
p_values <- na.omit(p_values)
p_value_density <- summary(anova_result_7)[[1]]$`Pr(>F)`[1]
p_value_fertilizer <- summary(anova_result_7)[[1]]$`Pr(>F)`[2]
p_value_Densityfertilizer <- summary(anova_result_7)[[1]]$`Pr(>F)`[3]


# Make decision for each factor and interaction
if (p_value_density < alpha) {
  Decision_7 <- ("Reject the null hypothesis for density (significant effect")
} else {
  Decision_7 <- ("Fail to reject the null hypothesis for density (no significant effect")
}

if (p_value_fertilizer < alpha) {
  Decision_8 <- ("Reject the null hypothesis for fertilizer (significant effect")
} else {
  Decision_8 <- ("Fail to reject the null hypothesis for fertilizer (no significant effect)")
}

if (p_value_Densityfertilizer < alpha) {
  Decision_9 <- ("Reject the null hypothesis for interaction (significant interaction effect)")
} else {
  Decision_9 <- ("Fail to reject the null hypothesis for interaction (no significant interaction effect")
}


# Creating a data frame for ANOVA summary
anova_summary_table_7 <- data.frame(
  S_NO = c('density', 'fertilizer', 'density:fertilizer', 'Residuals', 'Decision'),
  Df = c(1, 2, 2, 90, NA),
  Sum_Sq = c(5.122, 6.068, 0.428, 30.337, NA),
  Mean_Sq = c(5.122, 3.034, 0.214, 0.337, NA),
  F_value = c(15.195, 9.001, 0.635, NA, NA),
  'Pr(>F)' = c(0.000186, 0.000273, 0.532500, NA, NA),
  Decision = c(Decision_7, Decision_8, Decision_9, NA, NA)
)
anova_summary_table_7 <- anova_summary_table_7[anova_summary_table_7$S_NO != "Decision", ]


library(kableExtra)

# Display the results table using kable
results_table_7 <- kable(anova_summary_table_7, format = "html", col.names = c("S_NO","Df", "Sum_Sq","Mean_Sq", "F-Value", "Pr..F","Decision")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")


# Perform Tukey's test for density and fertilizer
tukey_density <- TukeyHSD(aov(yield ~ density, data = data))
tukey_fertilizer <- TukeyHSD(aov(yield ~ fertilizer, data = data))

# Display the Tukey's test results for density
tukey_density

tukey_density_result <- data.frame(
  S_NO = c("comparison between group2-group1"),
  Diff = c(0.461956),
  lwr = c(0.2082555),
  upr = c(0.7156566),
  p_adj = c(0.0004845)
)

library(kableExtra)

# Display the results table using kable
tukey_density_result_9 <- kable(tukey_density_result, format = "html", col.names = c("S_NO", "Diff", "lwr", "upr", "p_adj")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")

# Display the Tukey's test results for fertilizer
tukey_fertilizer

tukey_fertilizer_result <- data.frame(
  S_NO = c("comparison between group2-group1", "comparison between group3-group1", "comparison between group3-group2"),
  Diff = c(0.1761687, 0.5991256, 0.4229569),
  lwr = c(-0.19371896, 0.22923789, 0.05306916),
  upr = c(0.5460564, 0.9690133, 0.7928445),
  p_adj = c(0.4954705, 0.0006125, 0.0208735)
)

library(kableExtra)

# Display the results table using kable
tukey_fertlizer_result_10 <- kable(tukey_fertilizer_result, format = "html", col.names = c("S_NO", "Diff", "lwr", "upr", "p_adj")) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#000080") %>%
  row_spec(1, extra_css = "border-bottom: 1px solid #ddd;")
