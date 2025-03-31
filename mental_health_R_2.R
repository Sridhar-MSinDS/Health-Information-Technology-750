# ============================
# Install and Load Required Packages
# ============================

# Install required packages (if not already installed)
install.packages(c("dplyr", "ggplot2", "tidyr", "data.table", "readr", "gridExtra"))
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")  # Required for handling spatial data

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(data.table)
library(gridExtra)
library(grid)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# Suppress warnings
options(warn = -1)

# ============================
# Load and Explore the Dataset
# ============================

# Read the CSV file (update file path if necessary)
csv_data <- read.csv("C:\\Users\\Sridhar Reddy\\Desktop\\HIT_750\\Mental_health_Depression_disorder_Data.csv", stringsAsFactors = FALSE)

# Display the first 5 rows of the data
head(csv_data)

# Display column names
colnames(csv_data)

# Display the structure of the dataset
str(csv_data)

# ============================
# Data Type Conversion
# ============================

# Specify the columns to convert to numeric
columns_to_convert <- c("Year", "Schizophrenia....", "Bipolar.disorder....", "Eating.disorders....")

# Convert specified columns to numeric
csv_data[columns_to_convert] <- lapply(csv_data[columns_to_convert], as.numeric)

# Check the structure again after conversion
str(csv_data)

# ============================
# Check for Duplicates
# ============================

# Count and display total duplicates
total_duplicates <- sum(duplicated(csv_data))
cat("Total duplicate rows:", total_duplicates, "\n")

# Remove duplicates (uncomment to apply)
# csv_data <- distinct(csv_data)

# Display dimensions of the dataset after removing duplicates
dim(csv_data)

# ============================
# Handle Missing Values
# ============================

# Count missing values per column
missing_values <- colSums(is.na(csv_data))
cat("Missing values per column:\n")
print(missing_values)

# Drop rows with missing values in selected columns
selected_columns <- c("Code", "Year", 
                      "Schizophrenia....", "Bipolar.disorder....", 
                      "Eating.disorders....", "Anxiety.disorders....", 
                      "Drug.use.disorders....", "Depression....", 
                      "Alcohol.use.disorders....")

# Drop rows where data is missing in these columns
csv_data <- csv_data[complete.cases(csv_data[, selected_columns]), ]

# Display missing values after handling
missing_values_after_handling <- colSums(is.na(csv_data))
cat("Missing values after handling:\n")
print(missing_values_after_handling)

# Display dimensions of the cleaned dataset
dim(csv_data)

# ============================
# Remove Unnecessary Columns
# ============================

# Remove 'index' column if it exists
if ("index" %in% colnames(csv_data)) {
  csv_data$index <- NULL
}

# Display first 5 rows after column removal
head(csv_data)

# ============================
# Boxplots Before Handling Outliers
# ============================

# Function to create boxplots for numeric columns
plot_boxplots <- function(df, title) {
  # Identify numeric columns (excluding 'Year')
  numeric_columns <- sapply(df, is.numeric)
  numeric_columns <- names(df)[numeric_columns]
  numeric_columns <- setdiff(numeric_columns, "Year")
  
  # Convert data into long format for plotting
  df_long <- df[, numeric_columns] %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
  
  # Create and display boxplot
  p <- ggplot(df_long, aes(x = Variable, y = Value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = title, x = "Variables", y = "Values") +
    theme_minimal()
  
  print(p)
}

# Plot boxplots before handling outliers
plot_boxplots(csv_data, "Boxplots Before Handling Outliers")

# ============================
# Visualizing Trends & Insights
# ============================

# Trend of depression over time
depression_trend <- csv_data %>%
  group_by(Year) %>%
  summarise(avg_depression = mean(Depression...., na.rm = TRUE))

ggplot(depression_trend, aes(x = Year, y = avg_depression, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Average Depression Rates Over Time",
       x = "Year",
       y = "Average Depression Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Choropleth Map: Global Depression Rates (2017)
world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- csv_data %>%
  filter(Year == 2017) %>%
  right_join(world, by = c("Code" = "iso_a3")) %>%
  st_as_sf()

ggplot(map_data) +
  geom_sf(aes(fill = Depression....)) +
  scale_fill_viridis_c(option = "plasma", name = "Depression Rate (%)") +
  labs(title = "Global Depression Rates (2017)") +
  theme_void()

# Animated Bubble Chart
dynamic_plot <- ggplot(csv_data, 
                       aes(x = Depression...., y = Anxiety.disorders...., 
                           size = Bipolar.disorder...., color = Entity)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  labs(title = "Year: {frame_time}", 
       x = "Depression Rate", 
       y = "Anxiety Rate") +
  transition_time(as.integer(Year)) +
  ease_aes("linear")

animate(dynamic_plot, fps = 10, width = 800, height = 600)

# ============================
# Handle Outliers Using IQR
# ============================

# Function to remove outliers using IQR
remove_outliers <- function(df) {
  df_clean <- df
  numeric_columns <- sapply(df_clean, is.numeric)
  numeric_columns <- names(df_clean)[numeric_columns]
  numeric_columns <- setdiff(numeric_columns, "Year")
  
  # Loop through numeric columns to remove outliers
  for (col in numeric_columns) {
    Q1 <- quantile(df_clean[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df_clean[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Filter outliers
    df_clean <- df_clean %>%
      filter(df_clean[[col]] >= lower_bound & df_clean[[col]] <= upper_bound)
  }
  return(df_clean)
}

# Iteratively remove outliers until no more are detected
data_clean <- csv_data  # Copy of the original data
repeat {
  previous_rows <- nrow(data_clean)  # Rows before removing outliers
  data_clean <- remove_outliers(data_clean)
  
  # Break loop if no more outliers are removed
  if (nrow(data_clean) == previous_rows) {
    break
  }
}

# Plot boxplots after handling outliers
plot_boxplots(data_clean, "Boxplots After Handling Outliers")

# Display dimensions after handling outliers
dim(data_clean)

# ============================
# Plot Histograms Before & After Handling Outliers
# ============================

# Load required packages
library(ggplot2)
library(gridExtra)

# Function to create histograms with mean lines
plot_histograms_grid <- function(df, title) {
  numeric_columns <- sapply(df, is.numeric)
  numeric_columns <- names(df)[numeric_columns]
  numeric_columns <- setdiff(numeric_columns, "Year")
  
  # List to store plots
  plot_list <- list()
  
  for (col in numeric_columns) {
    # Create the histogram
    p <- ggplot(df, aes_string(x = col)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      geom_vline(aes_string(xintercept = paste0("mean(", col, ", na.rm = TRUE)")),
                 color = "red", linetype = "dashed", size = 1) +
      ggtitle(paste("Distribution of", col)) +
      theme_minimal() +
      labs(caption = paste("Mean:", round(mean(df[[col]], na.rm = TRUE), 2)))
    
    # Append the plot to the list
    plot_list[[col]] <- p
  }
  
  # Arrange all plots in a grid
  grid.arrange(grobs = plot_list, ncol = 3, top = title)
}

# Plot histograms before handling outliers
plot_histograms_grid(csv_data, "Distribution Before Handling Outliers")

# Plot histograms after handling outliers
plot_histograms_grid(data_clean, "Distribution After Handling Outliers")

# ============================
# Trend Analysis & Correlation
# ============================
library(corrplot)
library(dplyr)
# Summary statistics for disorders by year
disorder_summary <- data_clean %>%
  group_by(Year) %>%
  summarise(
    avg_depression = mean(Depression...., na.rm = TRUE),
    avg_anxiety = mean(Anxiety.disorders...., na.rm = TRUE),
    avg_bipolar = mean(Bipolar.disorder...., na.rm = TRUE),
    avg_schizophrenia = mean(Schizophrenia...., na.rm = TRUE),
    avg_eating = mean(Eating.disorders...., na.rm = TRUE),
    avg_drug = mean(Drug.use.disorders...., na.rm = TRUE),
    avg_alcohol = mean(Alcohol.use.disorders...., na.rm = TRUE)
  )

# Correlation matrix
cor_matrix <- cor(data_clean %>%
                    select(Schizophrenia...., Bipolar.disorder...., Eating.disorders....,
                           Anxiety.disorders...., Drug.use.disorders...., Depression....,
                           Alcohol.use.disorders....), use = "complete.obs")

# Plot correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Correlation Between Different Mental Health Disorders",
         mar = c(0,0,1,0))

