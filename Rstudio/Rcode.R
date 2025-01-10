# Load necessary packages
# dplyr for data manipulation.
# ggplot2 for creating informative and attractive visualizations.
library(dplyr)
library(ggplot2)

# Load the dataset
# Load the dataset from a CSV file located within the 'data' directory.
data <- read.csv("data/DataSet.csv")

# Steps: Create a Primary_Position column by extracting the first listed position
# Step 1: Extract the primary position from the 'Positions' column.
# This is the first listed position in the comma-separated values.
data <- data %>%
  mutate(Primary_Position = sapply(strsplit(as.character(Positions), ","), `[`, 1))

# Step 2: Remove any leading or trailing whitespace in the Primary_Position column
data$Primary_Position <- trimws(data$Primary_Position)

# Step 3: Summarize points by primary position after cleaning
summary_stats_clean <- data %>%
  group_by(Primary_Position) %>%
  summarize(mean_pts = mean(PTS, na.rm = TRUE),
            sd_pts = sd(PTS, na.rm = TRUE),
            count = n())
print(summary_stats_clean)

# Step 4: Perform a Spearman correlation test between Assists (AST) and Points Scored (PTS)
cor_test <- cor.test(data$AST, data$PTS, method = "spearman", exact = FALSE)

# Print the results of the correlation test
print(cor_test)

# Extract test statistic and p-value
test_statistic <- cor_test$statistic
p_value <- cor_test$p.value

# Step 5: Create a scatter plot with a linear trendline
ggplot(data, aes(x = AST, y = PTS)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points for visualization
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add trendline (linear regression)
  labs(
    title = "Relationship Between Assists (AST) and Points Scored (PTS)",
    x = "Assists (AST)",
    y = "Points Scored (PTS)"
  ) +
  theme_minimal()

# Step 6: Generate a histogram for Points Scored (PTS) with a bell curve overlay.
# The overlay represents the normal distribution fit to the histogram, illustrating the distribution's shape.
ggplot(data, aes(x = PTS)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$PTS, na.rm = TRUE), 
                            sd = sd(data$PTS, na.rm = TRUE)), 
                color = "red", linewidth = 1) +
  labs(
    title = "Histogram of Points Scored (PTS) with Normal Curve Overlay",
    x = "Points Scored (PTS)",
    y = "Density"
  ) +
  theme_minimal()

# Output test statistic and p-value for reporting
# Output the test statistic and p-value from the correlation test to help interpret the significance of the findings.

cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")

