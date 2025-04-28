### Step 1: Load Required Libraries ###
library(tidyverse)  # For data manipulation
library(data.table) # For efficient data manipulation
library(stringr)    # For string operations
library(skimr)      # For data summary statistics
library(broom)      # For tidying regression model outputs

### Step 2: Set Working Directory ###
# Get the current working directory
current_directory <- getwd()
print(current_directory)

# Set the working directory to the desired path
setwd("/Users/carlovilloresi/Documents/Coding/R/Projects/2. Supply Chain Regression")

### Step 3: Data Loading and Preparation ###
# Define file paths
sales_file <- file.path(current_directory, "reg_dati_vendite.txt")
products_file <- file.path(current_directory, "reg_dati_anag_prodotti.txt")

# Read the data files into R
sales_data <- read.table(sales_file, sep = ";", header = TRUE)
product_data <- read.table(products_file, sep = ";", header = TRUE)

# Merge the two datasets by PROD_ID
merged_data <- sales_data %>%
  left_join(product_data, by = "PROD_ID") %>%
  relocate(PROD_ID, MEINS)

# Convert MEINS to a factor
merged_data$MEINS <- as.factor(merged_data$MEINS)

# Summarize the dataset by product ID to check the number of rows per product
product_summary <- merged_data %>%
  group_by(PROD_ID) %>%
  summarise(num_rows = n())

# Summarize and explore the dataset
skim(merged_data)
hist(merged_data$QTA_ORD)

# Identify negative quantities (returns) and handle them
negative_sales <- merged_data %>% filter(QTA_ORD < 0)

# According to business rules, set negative quantities to zero
merged_data <- merged_data %>%
  mutate(QTA_ORD = ifelse(QTA_ORD < 0, 0, QTA_ORD))

### Step 4: Regression Modeling ###
# Filter products that have varying discounts
products_with_varying_discounts <- merged_data %>%
  group_by(PROD_ID) %>%
  summarise(num_sales = n(), unique_discounts = n_distinct(SCONTO)) %>%
  filter(unique_discounts > 1)

# Subset the dataset for products with varying discounts
regression_data <- merged_data %>%
  filter(PROD_ID %in% products_with_varying_discounts$PROD_ID)

# Split the dataset into a list of data frames, one for each product
split_data_by_product <- regression_data %>%
  group_split(PROD_ID)

# Apply linear regression for each product and collect results
regression_results <- map_dfr(split_data_by_product, function(product_data) {
  model <- lm(QTA_ORD ~ SCONTO, data = product_data)
  glance(model) %>%
    mutate(PROD_ID = product_data$PROD_ID[1])
})

# Filter out models with significant results (p-value <= 0.05)
significant_models <- regression_results %>%
  filter(p.value <= 0.05, nobs > 10)

### Step 5: Model Evaluation ###
# Check results for a specific product (example PROD_ID = 238)
selected_product_id <- 238
product_check_data <- regression_data %>% filter(PROD_ID == selected_product_id)

# Fit a regression model for the selected product
product_model <- lm(QTA_ORD ~ SCONTO, data = product_check_data)

# Display model summary and plot results
summary(product_model)
plot(product_check_data$SCONTO, product_check_data$QTA_ORD)
abline(product_model)
