# Load necessary packages
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read.csv("data/DataSet.csv")

# Step 1: Create a Primary_Position column by extracting the first listed position
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

# Step 4: Perform ANOVA test
anova_result_clean <- aov(PTS ~ Primary_Position, data = data)
summary(anova_result_clean)

# Step 5: Post-hoc analysis with Tukey's HSD test
tukey_result_clean <- TukeyHSD(anova_result_clean)
print(tukey_result_clean)

# Step 6: Create a boxplot for points by primary position (cleaned data)
ggplot(data, aes(x = Primary_Position, y = PTS)) +
  geom_boxplot() +
  labs(title = "Points Scored by Position (Cleaned Data)",
       x = "Primary Position",
       y = "Points (PTS)") +
  theme_minimal()
