library(brms)
library(readr)
library(ggplot2)
library(reshape2)

# 1. Data Preparation
df <- read_csv("product_info.csv")
df <- df[, c("product_name", "price_usd", "rating", "loves_count", "primary_category", "secondary_category")]
df <- na.omit(df)

# 2. Feature Engineering
# Convert continuous variables to categorical levels
df$price_level <- cut(df$price_usd, breaks = c(0, 30, 70, 200, Inf),
                      labels = c("Low", "Medium", "High", "Luxury"),
                      include.lowest = TRUE)
df$price_level <- factor(df$price_level, levels = c("Low", "Medium", "High", "Luxury"))

df$rating_level <- cut(df$rating, breaks = c(0, 3.5, 4.2, 5),
                       labels = c("Low", "Medium", "High"),
                       include.lowest = TRUE)
df$rating_level <- factor(df$rating_level, levels = c("Low", "Medium", "High"))

df$loves_level <- cut(df$loves_count, breaks = c(0, 1000, 5000, 10000, Inf),
                      labels = c("Few", "Medium", "Popular", "Hot"),
                      include.lowest = TRUE)
df$loves_level <- factor(df$loves_level, levels = c("Few", "Medium", "Popular", "Hot"))

# 3. Load Existing Model
# Load the pre-trained model instead of training a new one
fit <- readRDS("brms_model_final.rds")
cat("✅ Model loaded successfully\n")

# 4. Generate Visualizations

# 4.1 Feature Distribution Plots
# Price Distribution
p1 <- ggplot(df, aes(x = price_level)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Price Level Distribution", x = "Price Level", y = "Count") +
  theme_minimal()

# Rating Distribution
p2 <- ggplot(df, aes(x = rating_level)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Rating Level Distribution", x = "Rating Level", y = "Count") +
  theme_minimal()

# Loves Count Distribution
p3 <- ggplot(df, aes(x = loves_level)) +
  geom_bar(fill = "purple") +
  labs(title = "Loves Count Distribution", x = "Loves Level", y = "Count") +
  theme_minimal()

# Save Feature Distribution Plots
ggsave("Bayesian_price_distribution.png", p1, width = 8, height = 6)
ggsave("Bayesian_rating_distribution.png", p2, width = 8, height = 6)
ggsave("Bayesian_loves_distribution.png", p3, width = 8, height = 6)

# 4.2 Category Distribution
p4 <- ggplot(df, aes(x = primary_category)) +
  geom_bar(fill = "orange") +
  labs(title = "Primary Category Distribution", x = "Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Bayesian_category_distribution.png", p4, width = 12, height = 6)

# 4.3 Model Prediction Visualization
# Create test data for prediction
test_data <- expand.grid(
  price_level = levels(df$price_level),
  rating_level = levels(df$rating_level),
  loves_level = levels(df$loves_level)
)

# Get prediction probabilities
pred_probs <- predict(fit, newdata = test_data, summary = FALSE)
posterior_means <- colMeans(pred_probs)

# Create prediction probability plot
prob_matrix <- melt(posterior_means)
colnames(prob_matrix) <- c("Category", "Probability")

p5 <- ggplot(prob_matrix, aes(x = Category, y = Probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Predicted Category Probabilities", x = "Category", y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Bayesian_prediction_probabilities.png", p5, width = 12, height = 6)

cat("✅ Visualizations generated successfully\n")
