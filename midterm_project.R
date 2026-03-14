# ==============================================================================
# Project: Spatiotemporal Forecasting of Air Quality Index in Dhaka
# Section: H, Midterm Project, Group 8
# ==============================================================================


# List of only the required packages used in the script
packages <- c("ggplot2", "dplyr", "readr", "corrplot", "moments")

# Install any packages that are not already installed
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(moments)

# --- A. DATA UNDERSTANDING ---

# 1. Load the dataset
options(timeout = 300)
url <- "https://raw.githubusercontent.com/salman-sayeed/introduction-to-datascience-midterm-project/main/dhaka_air_quality_2000_2025.csv"
data <- read_csv(url)

# 2. Display the first few rows
print("First 6 rows of the dataset:")
head(data)

# 3. Show shape
cat("Dataset Shape: ", nrow(data), " rows x ", ncol(data), "\n")

# 4. Display data types
print("Data Types of each column:")
glimpse(data)

# 5. Descriptive statistics
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

cat("\n--- Descriptive Statistics ---\n")
stats_summary <- data %>%
  summarise(across(where(is.numeric), list(
    Mean = ~mean(.x, na.rm = TRUE),
    Median = ~median(.x, na.rm = TRUE),
    Std_Dev = ~sd(.x, na.rm = TRUE),
    Min = ~min(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    Skewness = ~skewness(.x, na.rm = TRUE)
  )))
print(t(stats_summary))
cat("Mode of AQI:", get_mode(data$AQI), "\n")


# --- B. DATA EXPLORATION & VISUALIZATION ---

# Create Categorical Variables
data <- data %>%
  mutate(
    AQI_Category = case_when(
      AQI <= 50  ~ "Good",
      AQI <= 100 ~ "Moderate",
      AQI <= 150 ~ "Unhealthy (Sensitive)",
      AQI <= 200 ~ "Unhealthy",
      TRUE       ~ "Very Unhealthy/Hazardous"
    ),
    Season = case_when(
      format(datetime, "%m") %in% c("12", "01", "02") ~ "Winter",
      format(datetime, "%m") %in% c("03", "04", "05") ~ "Pre-Monsoon",
      format(datetime, "%m") %in% c("06", "07", "08", "09") ~ "Monsoon",
      TRUE ~ "Post-Monsoon"
    )
  )

# 1. Univariate Analysis
ggplot(data, aes(x = AQI)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  labs(title = "Distribution of AQI", x = "AQI Value", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = AQI_Category, fill = AQI_Category)) +
  geom_bar() + coord_flip() +
  labs(title = "Frequency of AQI Categories") + theme_minimal()

# 2. Bivariate Analysis
numeric_cols <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_cols, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", number.cex = 0.7)

ggplot(data, aes(x = Season, y = AQI, fill = Season)) +
  geom_boxplot() + labs(title = "AQI Variation by Season") + theme_minimal()


# --- C. DATA PREPROCESSING ---

# 1. Handling Missing Values
set.seed(42)
data[sample(1:nrow(data), 50), "PM2.5"] <- NA
cat("\nMissing Values Detected:\n")
print(colSums(is.na(data)))

data$PM2.5[is.na(data$PM2.5)] <- median(data$PM2.5, na.rm = TRUE)
data$Temperature[is.na(data$Temperature)] <- median(data$Temperature, na.rm = TRUE)

# 2. Handling Outliers (Capping)
Q1 <- quantile(data$AQI, 0.25)
Q3 <- quantile(data$AQI, 0.75)
IQR_val <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR_val
data$AQI <- ifelse(data$AQI > upper_bound, upper_bound, data$AQI)

# 3. Encoding
data$Season_Encoded <- as.numeric(factor(data$Season))

# 4. Data Transformation
data$PM2.5_log <- log1p(data$PM2.5) 
data$Temp_Zscore <- as.vector(scale(data$Temperature)) 
min_max_norm <- function(x) { (x - min(x)) / (max(x) - min(x)) }
data$Humidity_Norm <- min_max_norm(data$Humidity)

# 5. Feature Selection
correlations <- cor(data %>% select(where(is.numeric)))[,"AQI"]
print("\nFeature Correlation with AQI:")
print(sort(correlations, decreasing = TRUE))

final_data <- data %>%
  select(AQI, PM2.5_log, PM10, Temp_Zscore, Humidity_Norm, Season_Encoded)

cat("\nPreprocessing Complete. Final Shape:", dim(final_data)[1], "x", dim(final_data)[2], "\n")
