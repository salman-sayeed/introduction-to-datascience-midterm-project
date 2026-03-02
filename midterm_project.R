# Necessary libraries
library(readr)
library(dplyr)


# Loading the dataset from Github using raw url
url <- "https://raw.githubusercontent.com/salman-sayeed/introduction-to-datascience-midterm-project/main/dhaka_air_quality_2000_2025.csv"
data <- read_csv(url)

# Printing the first few rows
print("First 6 rows of the dataset:")
head(data)


# 4. Show shape (rows x columns)
cat("Dataset Shape: ", nrow(data), " rows x ", ncol(data), "\n")


# 5. Display data types and Identify Features
print("Data Types of each column:")
glimpse(data)

