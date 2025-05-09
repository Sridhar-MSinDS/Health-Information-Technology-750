# ===============================
# 01. Install necessary packages
# ===============================
install.packages(c("readr", "dplyr", "ggplot2", "tidyr", "tidyverse", "corrplot", "sf", "rnaturalearth", "rnaturalearthdata", "viridis", "gridExtra", "reshape2", "patchwork", "knitr", "fmsb"))

# ===============================
# 02. Import libraries
# ===============================
library(readr)           # for reading data files
library(dplyr)           # for data manipulation (filtering, grouping, summarizing)
library(ggplot2)         # for creating data visualizations
library(tidyr)           # for reshaping and tidying data (e.g., pivoting)
library(tidyverse)       # for loading a collection of data science packages including ggplot2, dplyr, tidyr, etc.
library(corrplot)        # for visualizing correlation matrices
library(sf)              # for handling and analyzing spatial data
library(rnaturalearth)   # for downloading natural earth map data (country boundaries, etc.)
library(rnaturalearthdata) # for providing the actual natural earth datasets used in mapping
library(viridis)         # for printer-friendly visualizations
library(gridExtra)       # for arranging multiple ggplot plots in a grid layout
library(reshape2)        # for reshaping data between wide and long formats (e.g., melt, dcast)
library(patchwork)       # for combining multiple ggplot2 plots using a simple and intuitive syntax
library(knitr)           # for creating tables from dataframe
library(fmsb)            # for creating Radar Chart
# ===============================
# 03. Load the Data
# ===============================
data <- read.csv("C:\\Users\\Sridhar Reddy\\Desktop\\HIT_750\\Mental_health_Depression_disorder_Data.csv", stringsAsFactors = FALSE)

# stringsAsFactors = TRUE	 :: Converts strings to factors(Good for categorization)
# stringsAsFactors = FALSE :: Keeps strings as character(Good for text analysis)

# ===============================
# 04. Explore the Dataset
# ===============================
### Basic structure
str(data)
#It is used to display the internal structure of an R object
#i) Shows the data type of each column (e.g., numeric, factor, character)
#ii) Lists the first few entries in each column
#iii) Displays the number of observations (rows) and variables (columns)

dim(data)                   
# Returns the dimension i.e., Number of rows and columns

colnames(data)
# Returns the names of the columns
# It is essential for selecting, renaming, or transforming specific columns.

head(data) # returns top 6 rows of the dataset

unique(data$Entity) # returns all unique Entity names in the dataset

unique(data$Year) # returns all unique Years in the dataset

# Summary statistics for disorders by year
disorder_summary <- data %>%
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
kable(disorder_summary, caption = "Average Disorder Rates by Year")

# Summary statistics for disorders by year (United States only)
us_disorder_summary <- data %>%
  filter(Entity == "United States") %>%
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

# Display as formatted table
kable(us_disorder_summary, caption = "Average Disorder Rates in the United States by Year")

# ===============================
# 05. Data Cleaning
# ===============================
# *****
# Datatype Conversion :
# *****
# i) Specify the columns to convert to numeric
columns_to_convert <- c("Year", "Schizophrenia....", "Bipolar.disorder....", "Eating.disorders....")

# ii) Convert specified columns to numeric
data[columns_to_convert] <- lapply(data[columns_to_convert], as.numeric)

# iii) Check the structure again after conversion
str(data)

# *****
# Statistical summary :
# *****
summary(data)
# Useful for spotting Outliers and distribution

# *****
# Check and remove duplicates :
# *****
# Count duplicates
sum(duplicated(data))

# Remove duplicates, if available
# data <- distinct(data)

# *****
# Handling missing values :
# *****
# Check for Missing values
colSums(is.na(data))

# Drop rows with NA in the selected columns, in case missing values are present
# required_cols <- names(data)[1:11]
# data <- data[complete.cases(data[, required_cols]), ]
# str(data) # 6468 rows, 11 columns
# data[, required_cols] extracts only the columns specified in required_cols from the dataframe.
# complete.cases() returns a logical vector indicating which rows have no missing values in the specified columns

# *****
# Handling inconsistent columns and rows :
# *****
### Remove unnecessary columns
# Drop index column
data$index <- NULL

# Drop Code Column
data$Code <- NULL

### Remove unnecessary rows
# i) Filter out non-country entities from the dataset
non_countries <- c(
  "World", "High SDI", "Low SDI", "Middle SDI", "High-income", "Low-middle SDI", "High-middle SDI",
 "Andean Latin America", "Latin America and Caribbean", "Caribbean", "Central Latin America",
  "Eastern Europe", "Western Europe", "Central Asia", "East Asia", "South Asia","Central Sub-Saharan Africa",
  "Southeast Asia", "Oceania", "Sub-Saharan Africa", "Northern Ireland", "Southern Sub-Saharan Africa",
  "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
  "Central Europe", "Central Europe, Eastern Europe, and Central Asia", "North America",
  "North Africa and Middle East", "Southern Latin America", "Tropical Latin America", "Southeast Asia, East Asia, and Oceania",
   "Asia", "Europe", "Africa"
)
 
data <- data[!(data$Entity %in% non_countries), ]

str(data)         # 5600 rows, 9 columns
unique(data$Year) # Returns from 1990 to 2017

# *****
# Handling Outliers using Interquartile Range Method :
# *****
# Define a function to remove outliers
  
clean_outliers_columnwise <- function(df, cols) {
  for (col in cols) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    
    # Replace outliers with NA
    df[[col]][ df[[col]] < lower | df[[col]] > upper ] <- NA
  }
  return(df)
}

# Apply to Mental Disorder columns
cols <- names(data)[3:9]
data_clean <- clean_outliers_columnwise(data, cols)
colSums(is.na(data_clean))

# Descriptive summary 
summary(data_clean)

# Select columns by their position 
required_cols <- names(data_clean)[3:9]

# Drop rows with NA in the selected columns
data_clean <- data_clean[complete.cases(data_clean[, required_cols]), ]
colSums(is.na(data_clean))

# ===============================
# 06. Exploratory Data Analysis
# ===============================

# *****
# Boxplots Before Outlier Removal
# *****
data_before_outliers <- melt(data, id.vars = c("Entity", "Year"))
# All the mental disorders melted and resulted in single column with its respective percentage values

ggplot(data_before_outliers, aes(x = variable, y = value)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot Before Outlier Removal", x = "Disorder", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# print(data_before_outliers)
# It give the mental disorder columns melted into a single column with its respecttive assigned values

# *****
# Boxplots After Outlier Removal
# *****
data_after_outliers <- melt(data_clean, id.vars = c("Entity", "Year"))

ggplot(data_after_outliers, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +  
  labs(title = "Boxplot After Outlier Removal",
       x = "Disorder", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# *****
# Distribution Plots Before and After Handling Outliers
# *****
# Density plots distribution for each mental disorder 
plot_density_comparison <- function(disorder_name) {
  p1 <- ggplot(filter(data_before_outliers, variable == disorder_name), aes(x = value)) +
    geom_density(fill = "skyblue", alpha = 0.6) +
    geom_vline(aes(xintercept = median(value, na.rm = TRUE)), color = "blue", linetype = "dashed") +
    labs(title = paste(disorder_name, "- Before Outlier Removal"), x = "Value", y = "Density") +
    theme_minimal()
  
  p2 <- ggplot(filter(data_after_outliers, variable == disorder_name), aes(x = value)) +
    geom_density(fill = "lightgreen", alpha = 0.6) +
    geom_vline(aes(xintercept = median(value, na.rm = TRUE)), color = "darkgreen", linetype = "dashed") +
    labs(title = paste(disorder_name, "- After Outlier Removal"), x = "Value", y = "Density") +
    theme_minimal()
  
  return(p1 + p2)  
}

# Loop through each disorder column
unique_disorders <- unique(data_before_outliers$variable)

for (disorder in unique_disorders) {
  print(plot_density_comparison(disorder))
}

# *****
# Distribution Plots Before and After Handling Outliers
# *****
# Histogram plots distribution for each mental disorder
plot_histogram_comparison <- function(disorder_name, binwidth = 0.5) {
  p1 <- ggplot(filter(data_before_outliers, variable == disorder_name), aes(x = value)) +
    geom_histogram(fill = "skyblue", color = "black", binwidth = binwidth, alpha = 0.7) +
    geom_vline(aes(xintercept = median(value, na.rm = TRUE)), color = "blue", linetype = "dashed") +
    labs(title = paste(disorder_name, "- Before Outlier Removal"), x = "Value", y = "Count") +
    theme_minimal()
  
  p2 <- ggplot(filter(data_after_outliers, variable == disorder_name), aes(x = value)) +
    geom_histogram(fill = "lightgreen", color = "black", binwidth = binwidth, alpha = 0.7) +
    geom_vline(aes(xintercept = median(value, na.rm = TRUE)), color = "darkgreen", linetype = "dashed") +
    labs(title = paste(disorder_name, "- After Outlier Removal"), x = "Value", y = "Count") +
    theme_minimal()
  
  return(p1 + p2)  
}

# Loop through each disorder variable and plot histograms
for (disorder in unique_disorders) {
  print(plot_histogram_comparison(disorder, binwidth = 0.5))
}

# *****
# World map with Distribution of Mental disorders
# *****
plot_world_map <- function(disorder, year) {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # Filter data
  map_data <- data %>%
    filter(Year == year) %>%
    select(Entity, Year, all_of(disorder)) %>%
    rename(Disorder_Value = all_of(disorder))
  
  
  map_data$Entity[map_data$Entity == "United States"] <- "United States of America"
  map_data$Entity[map_data$Entity == "Democratic Republic of Congo"] <- "Dem. Rep. Congo"
  map_data$Entity[map_data$Entity == "Czech Republic"] <- "Czechia"
  map_data$Entity[map_data$Entity == "Cote d'Ivoire"] <- "Ivory Coast"
  map_data$Entity[map_data$Entity == "Swaziland"] <- "Eswatini"
  map_data$Entity[map_data$Entity == "Timor"] <- "Timor-Leste"
  map_data$Entity[map_data$Entity == "Micronesia (country)"] <- "Micronesia"
  map_data$Entity[map_data$Entity == "Macedonia"] <- "North Macedonia"
  map_data$Entity[map_data$Entity == "Cape Verde"] <- "Cabo Verde"
  map_data$Entity[map_data$Entity == "Sao Tome and Principe"] <- "São Tomé and Príncipe"
  map_data$Entity[map_data$Entity == "United States Virgin Islands"] <- "Virgin Islands (U.S.)"
  map_data$Entity[map_data$Entity == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
  map_data$Entity[map_data$Entity == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
  map_data$Entity[map_data$Entity == "Solomon Islands"] <- "Solomon Is."
  map_data$Entity[map_data$Entity == "Central African Republic"] <- "Central African Rep."
  map_data$Entity[map_data$Entity == "Dominican Republic"] <- "Dominican Rep."
  map_data$Entity[map_data$Entity == "South Sudan"] <- "S. Sudan"
  map_data$Entity[map_data$Entity == "England"] <- "United Kingdom"
  map_data$Entity[map_data$Entity == "Scotland"] <- "United Kingdom"
  map_data$Entity[map_data$Entity == "Wales"] <- "United Kingdom"

  # Join with map data
  world_map <- left_join(world, map_data, by = c("name" = "Entity"))
  
  # Plot
  ggplot(world_map) +
   geom_sf(aes(fill = Disorder_Value), color = "white") +
    scale_fill_viridis(option = "C", name = paste0(disorder, " (%)"), na.value = "grey90") +
    labs(title = paste(disorder, "Prevalence in", year)) +
    theme_minimal()
}

# Example usage:
plot_world_map("Schizophrenia....", 2015)
plot_world_map("Bipolar.disorder....", 2015)
plot_world_map("Eating.disorders....", 2015)
plot_world_map("Anxiety.disorders....", 2015)
plot_world_map("Drug.use.disorders....", 2015)
plot_world_map("Depression....", 2015)
plot_world_map("Alcohol.use.disorders....", 2015)

# *****
# Radarchart for Average Global Disorder rates for 2017
# *****
radar_data <- data %>%
  filter(Year == 2017) %>%
  summarise(
    Depression = mean(Depression...., na.rm = TRUE),
    Anxiety = mean(Anxiety.disorders...., na.rm = TRUE),
    Bipolar = mean(Bipolar.disorder...., na.rm = TRUE),
    Schizophrenia = mean(Schizophrenia...., na.rm = TRUE),
    Eating = mean(Eating.disorders...., na.rm = TRUE),
    Drug = mean(Drug.use.disorders...., na.rm = TRUE),
    Alcohol = mean(Alcohol.use.disorders...., na.rm = TRUE)
  )

# Add max and min rows for radar chart format
radar_data <- rbind(
  rep(10, 7),  # max values
  rep(0, 7),   # min values
  radar_data   # actual data
)

# Plot radar chart
radarchart(radar_data,
           axistype = 1,
           pcol = "blue",
           pfcol = scales::alpha("blue", 0.3),
           plwd = 2,
           title = "Average Global Disorder Rates (2017)")

# *****
# Histogram plots for Global trends of Mental disorders
# *****
plot_all_disorders_horizontal <- function(year) {
  # Filter and compute average for each disorder
  year_data <- data %>%
    filter(Year == year) %>%
    summarise(across(3:9, mean, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "Disorder", values_to = "Prevalence")
  
  # Plot
  ggplot(year_data, aes(y = Disorder, x = Prevalence)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Prevalence of Mental Disorders -", year),
         x = "Prevalence (%)", y = "") +
    theme_minimal()
}

plot_all_disorders_horizontal(2017)

# *****
# Line plots for Global trends of Mental disorders
# *****
# calculate global average for each disorder per year
line_data <- data %>%
  group_by(Year) %>%
  summarise(across(2:8, mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -Year, names_to = "Disorder", values_to = "Average")

ggplot(line_data, aes(x = Year, y = Average, color = Disorder)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  labs(title = "Global Trends of Mental Disorders Over Time",
       x = "Year", y = "Average Prevalence (%)") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))
# ===============================
# 07. Time Series Analysis
# ===============================
# *****
# Global Distribution of Mental disorders
# *****
disorders <- c("Schizophrenia....", "Bipolar.disorder....", "Eating.disorders....",
               "Anxiety.disorders....", "Drug.use.disorders....", 
               "Depression....", "Alcohol.use.disorders....")

for (d in disorders) {
  trend <- data %>%
    group_by(Year) %>%
    summarise(avg_rate = mean(.data[[d]], na.rm = TRUE))
  
  print(
    ggplot(trend, aes(x = Year, y = avg_rate)) +
      geom_line(color = "steelblue") +
      geom_point() +
      labs(title = paste("Trend of", d),
           x = "Year", y = "Average Prevalence (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# *****
# Regional Distribution (USA) of Mental disorders
# *****
us_data <- data %>%
  filter(Entity == "United States")

us_long <- us_data %>%
  pivot_longer(cols = c(Schizophrenia...., Bipolar.disorder...., 
                        Depression...., Anxiety.disorders....,
                        Eating.disorders...., Drug.use.disorders....,
                        Alcohol.use.disorders....),
               names_to = "Disorder",
               values_to = "Rate")

ggplot(us_long, aes(x = Year, y = Rate, color = Disorder, group = Disorder)) +
  geom_line() +
  labs(title = "Mental Health Disorder Trends in the United States",
       x = "Year",
       y = "Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ===============================
# 08. Correlation Analysis
# ===============================
# Calculate correlations
cor_data <- data %>%
  select(3:9) %>%
  cor(use = "pairwise.complete.obs")

# Plot
corrplot(cor_data, method = "color", type = "upper",
         col = viridis::viridis(100), 
         addCoef.col = "black", tl.col = "black", number.cex = 0.7,
         title = "Correlation Matrix of Mental Disorders", mar = c(0,0,1,0))

# ===============================
# 09. Key Insights
# ===============================
# Select only disorder columns and Entity
disorder_cols <- names(data)[3:9]

# Function to get top 2 countries for each mental disorder
top <- function(col) {
  data %>%
    group_by(Entity) %>%
    summarise(avg = mean(.data[[col]], na.rm = TRUE)) %>%
    arrange(desc(avg)) %>%
    head(2) %>%
    mutate(Disorder = col)
}

# Apply to all disorders and combine results
top_all_disorders <- bind_rows(lapply(disorder_cols, top))

# Reorder columns for readability
top_all_disorders <- top_all_disorders %>%
  select(Disorder, Entity, avg)

print(top_all_disorders, n= 20)

# ===============================
# 10. Policy Recommendations and Conclusion
# ===============================

# - Bipolar disorder and Eating disorder are highly correlated
# - U.S. and certain regions show elevated trends in mental disorders
# - Policy focus should be on youth, high-risk nations, substance abuse support

