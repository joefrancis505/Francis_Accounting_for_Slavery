# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages needed
packages <- c("readODS", "dplyr", "kableExtra", "knitr")

# Check which packages are not installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(new_packages)) install.packages(new_packages)

# Load all required packages
library(readODS)
library(dplyr)
library(kableExtra)
library(knitr)
cat("\014")  # Clear console

# Set working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Read the data
data <- read_ods("GDP_1839_1899_v1.0.ods", sheet = "data")

# Helper function to convert columns to numeric
convert_to_numeric <- function(data, cols) {
  for(col in cols) {
    if(col %in% names(data)) {
      data[[col]] <- as.numeric(as.character(data[[col]]))
    }
  }
  return(data)
}

# Define all numeric columns
numeric_cols <- c("gdp", "agriculture", "agriculture_crops_and_livestock", 
                  "agriculture_farm_improvments", "construction", "mining", 
                  "manufacturing", "manufacturing_factory", "manufacturing_home", 
                  "services", "services_education", "services_finance", 
                  "services_government", "services_hand_trades", "services_personal", 
                  "services_professional", "services_shelter", "services_trade", 
                  "services_transportation_and_public_utilities", "population", 
                  "gdp_per_capita", "wealth", "enslaved_population", 
                  "urban_population", "labor_force", "agricultural_labor_force",
                  "easterlin_1957", "easterlin_1960", "klein",
                  "agriculture_crops_and_livestock_livestock_and_feed_crops",
                  "agriculture_crops_and_livestock_food_grains", 
                  "agriculture_crops_and_livestock_cotton",
                  "agriculture_crops_and_livestock_other")

# Convert all necessary columns to numeric
data <- convert_to_numeric(data, numeric_cols)

# Table 1: GDP by sector, 1839-1899
gdp_agg <- data %>%
  filter(year %in% c(1839, 1849, 1859, 1869, 1879, 1889, 1899)) %>%
  group_by(year) %>%
  summarize(
    GDP = sum(gdp, na.rm = TRUE),
    Agriculture = sum(agriculture, na.rm = TRUE),
    `Crops and livestock` = sum(agriculture_crops_and_livestock, na.rm = TRUE),
    `Farm improvements` = sum(agriculture_farm_improvments, na.rm = TRUE),
    Construction = sum(construction, na.rm = TRUE),
    Mining = sum(mining, na.rm = TRUE),
    Manufacturing = sum(manufacturing, na.rm = TRUE),
    `Factory` = sum(manufacturing_factory, na.rm = TRUE),
    `Home` = sum(manufacturing_home, na.rm = TRUE),
    Services = sum(services, na.rm = TRUE),
    `Education` = sum(services_education, na.rm = TRUE),
    `Finance` = sum(services_finance, na.rm = TRUE),
    `Government` = sum(services_government, na.rm = TRUE),
    `Hand trades` = sum(services_hand_trades, na.rm = TRUE),
    `Personal` = sum(services_personal, na.rm = TRUE),
    `Professional` = sum(services_professional, na.rm = TRUE),
    `Shelter` = sum(services_shelter, na.rm = TRUE),
    `Trade` = sum(services_trade, na.rm = TRUE),
    `Transportation` = sum(services_transportation_and_public_utilities, na.rm = TRUE)
  )

# Create table
gdp_table <- data.frame(
  Sector = c(
    "GDP", 
    "Agriculture", 
    "\\quad Crops and livestock", 
    "\\quad Farm improvements", 
    "Construction", 
    "Mining", 
    "Manufacturing", 
    "\\quad Factory", 
    "\\quad Home", 
    "Services", 
    "\\quad Education", 
    "\\quad Finance", 
    "\\quad Government", 
    "\\quad Hand trades", 
    "\\quad Personal", 
    "\\quad Professional", 
    "\\quad Shelter", 
    "\\quad Trade", 
    "\\quad Transportation"
  )
)

# Add year columns
for(y in c(1839, 1849, 1859, 1869, 1879, 1889, 1899)) {
  year_data <- gdp_agg %>% filter(year == y)
  if(nrow(year_data) > 0) {
    gdp_table[[as.character(y)]] <- c(
      year_data$GDP,
      year_data$Agriculture,
      year_data$`Crops and livestock`,
      year_data$`Farm improvements`,
      year_data$Construction,
      year_data$Mining,
      year_data$Manufacturing,
      year_data$Factory,
      year_data$Home,
      year_data$Services,
      year_data$Education,
      year_data$Finance,
      year_data$Government,
      year_data$`Hand trades`,
      year_data$Personal,
      year_data$Professional,
      year_data$Shelter,
      year_data$Trade,
      year_data$Transportation
    )
  } else {
    gdp_table[[as.character(y)]] <- rep(NA, nrow(gdp_table))
  }
}

# Round values to whole numbers
gdp_table[, -1] <- round(gdp_table[, -1])

# Print the table
table_output <- kable(gdp_table, caption = "GDP by sector, 1839--1899", format.args = list(big.mark = ","))
print(table_output)

# Table 2: Correlation Coefficients for Old and New Estimates

# Function to calculate correlations for a specific year and measure
calc_corr <- function(year_val, measure_col) {
  year_data <- data %>% 
    filter(year == year_val) %>%
    filter(!is.na(gdp) & !is.na(!!sym(measure_col)))
  
  # Check if we have enough data points
  if(nrow(year_data) >= 3) {
    # Total correlation
    total_corr <- cor(year_data$gdp, year_data[[measure_col]], 
                      method = "pearson", use = "complete.obs")
    
    # Per capita correlation
    per_capita_val <- if(all(year_data$population > 0)) {
      year_data[[measure_col]] / year_data$population
    } else {
      year_data[[measure_col]]
    }
    
    per_capita_corr <- cor(year_data$gdp_per_capita, per_capita_val,
                           method = "pearson", use = "complete.obs")
    
    return(c(total_corr, per_capita_corr))
  } else {
    return(c(NA, NA))
  }
}

# Initialize results dataframe
correlation_results <- data.frame(
  Year = numeric(),
  Measure = character(),
  Total = numeric(),
  Per_Capita = numeric(),
  stringsAsFactors = FALSE
)

# Calculate correlations for each measure and year
measures <- list(
  "Easterlin (1957)" = list(years = c(1879, 1899), col = "easterlin_1957"),
  "Easterlin (1960)" = list(years = c(1839, 1879, 1899), col = "easterlin_1960"),
  "Klein (2013)" = list(years = c(1879, 1889, 1899), col = "klein")
)

for(measure_name in names(measures)) {
  measure_info <- measures[[measure_name]]
  
  for(year in measure_info$years) {
    corrs <- tryCatch({
      calc_corr(year, measure_info$col)
    }, error = function(e) {
      return(c(NA, NA))
    })
    
    correlation_results <- rbind(correlation_results, 
                                 data.frame(Year = year, 
                                            Measure = measure_name,
                                            Total = corrs[1], 
                                            Per_Capita = corrs[2],
                                            stringsAsFactors = FALSE))
  }
}

# Format correlation table
corr_table <- correlation_results %>%
  filter(!is.na(Total) & !is.na(Per_Capita)) %>%
  mutate(Total = round(Total, 2),
         Per_Capita = round(Per_Capita, 2)) %>%
  arrange(Measure, Year)

# Split by measure
corr_table_formatted <- data.frame(
  Measure = character(),
  Year = numeric(),
  Total = numeric(),
  Per_Capita = numeric(),
  stringsAsFactors = FALSE
)

# Add each measure section
for(measure_name in unique(corr_table$Measure)) {
  measure_rows <- corr_table %>% filter(Measure == measure_name)
  measure_letter <- switch(measure_name,
                           "Easterlin (1957)" = "a",
                           "Easterlin (1960)" = "b",
                           "Klein (2013)" = "c")
  
  if(nrow(measure_rows) > 0) {
    corr_table_formatted <- rbind(
      corr_table_formatted,
      data.frame(
        Measure = paste0("(", measure_letter, ") ", measure_name),
        Year = NA,
        Total = NA,
        Per_Capita = NA,
        stringsAsFactors = FALSE
      ),
      data.frame(
        Measure = "",
        Year = measure_rows$Year,
        Total = measure_rows$Total,
        Per_Capita = measure_rows$Per_Capita,
        stringsAsFactors = FALSE
      )
    )
  }
}

# Print the correlation table
if(nrow(corr_table_formatted) > 0) {
  correlation_table <- kable(corr_table_formatted[, c("Year", "Total", "Per_Capita")], 
                             col.names = c("", "Total", "Per capita"),
                             align = c("r", "c", "c"), 
                             caption = "Pearson correlation coefficients")
  
  print(correlation_table)
}

# Table 3: GDP and Wealth per Capita, 1859-1860
# Filter data for 1859
filtered_data <- data %>% filter(year == 1859)

# Function to calculate metrics for region
calculate_metrics <- function(region_values, region_name, is_region = TRUE) {
  if (is_region) {
    filtered <- filtered_data %>% filter(region %in% region_values)
  } else {
    filtered <- filtered_data %>% filter(division %in% region_values)
  }
  
  agg_data <- filtered %>%
    summarize(
      population = sum(population, na.rm = TRUE),
      gdp = sum(gdp, na.rm = TRUE),
      wealth = sum(wealth, na.rm = TRUE)
    )
  
  gdp_per_capita <- (agg_data$gdp * 1000) / agg_data$population
  wealth_per_capita <- agg_data$wealth / agg_data$population
  
  return(data.frame(
    Region = region_name,
    population = agg_data$population,
    gdp = agg_data$gdp,
    gdp_per_capita = gdp_per_capita,
    wealth_per_capita = wealth_per_capita
  ))
}

# Calculate North as reference (regions 1 and 2)
north_data <- calculate_metrics(c(1, 2), "North", TRUE)
north_data <- north_data %>% 
  mutate(
    GDP_index = 100,
    Wealth_index = 100
  )

# Create results dataframe
results <- data.frame(
  Region = character(),
  GDP_per_capita = numeric(),
  Wealth_per_capita = numeric(),
  GDP_index = numeric(),
  Wealth_index = numeric(),
  stringsAsFactors = FALSE
)

# Add US total
us_data <- filtered_data %>%
  summarize(
    population = sum(population, na.rm = TRUE),
    gdp = sum(gdp, na.rm = TRUE),
    wealth = sum(wealth, na.rm = TRUE)
  )

us_gdp_per_capita <- (us_data$gdp * 1000) / us_data$population
us_wealth_per_capita <- us_data$wealth / us_data$population

results <- rbind(results, data.frame(
  Region = "United States",
  GDP_per_capita = us_gdp_per_capita,
  Wealth_per_capita = us_wealth_per_capita,
  GDP_index = us_gdp_per_capita / north_data$gdp_per_capita * 100,
  Wealth_index = us_wealth_per_capita / north_data$wealth_per_capita * 100
))

# Add North row
results <- rbind(results, data.frame(
  Region = "North",
  GDP_per_capita = north_data$gdp_per_capita,
  Wealth_per_capita = north_data$wealth_per_capita,
  GDP_index = 100,
  Wealth_index = 100
))

# Add regions and divisions
regions <- list(
  c(1, "Northeast", TRUE),
  c(1, "\\quad New England", FALSE),
  c(2, "\\quad Middle Atlantic", FALSE),
  c(2, "Midwest", TRUE),
  c(3, "\\quad East North Central", FALSE),
  c(4, "\\quad West North Central", FALSE),
  c(3, "South", TRUE),
  c(5, "\\quad South Atlantic", FALSE),
  c(6, "\\quad East South Central", FALSE),
  c(7, "\\quad West South Central", FALSE),
  c(4, "West", TRUE),
  c(8, "\\quad Mountain", FALSE),
  c(9, "\\quad Pacific", FALSE)
)

for(region in regions) {
  region_metrics <- calculate_metrics(region[[1]], region[[2]], region[[3]])
  
  results <- rbind(results, data.frame(
    Region = region[[2]],
    GDP_per_capita = region_metrics$gdp_per_capita,
    Wealth_per_capita = region_metrics$wealth_per_capita,
    GDP_index = region_metrics$gdp_per_capita / north_data$gdp_per_capita * 100,
    Wealth_index = region_metrics$wealth_per_capita / north_data$wealth_per_capita * 100
  ))
}

# Round values
results$GDP_per_capita <- round(results$GDP_per_capita * 1000)
results$Wealth_per_capita <- round(results$Wealth_per_capita)
results$GDP_index <- round(results$GDP_index)
results$Wealth_index <- round(results$Wealth_index)

# Print the table
table_output <- kable(results, 
                      col.names = c("", "GDP", "Wealth", "GDP", "Wealth"),
                      align = c("l", "r", "r", "r", "r"),
                      caption = "GDP and Wealth per Capita, 1859–1860")

print(table_output)

# Table 4: Structure of Southern GDP, 1839-1899

# Filter data for the Southern region and aggregate by year
southern_data <- data %>%
  filter(region == 3) %>%  # Region 3 is the South
  filter(year %in% c(1839, 1849, 1859, 1869, 1879, 1889, 1899)) %>%
  group_by(year) %>%
  summarize(
    GDP = sum(gdp, na.rm = TRUE),
    Agriculture = sum(agriculture, na.rm = TRUE),
    `Crops and livestock` = sum(agriculture_crops_and_livestock, na.rm = TRUE),
    `Livestock and feed crops` = sum(agriculture_crops_and_livestock_livestock_and_feed_crops, na.rm = TRUE),
    `Food grains` = sum(agriculture_crops_and_livestock_food_grains, na.rm = TRUE),
    `Cotton` = sum(agriculture_crops_and_livestock_cotton, na.rm = TRUE),
    `Other` = sum(agriculture_crops_and_livestock_other, na.rm = TRUE),
    `Farm improvements` = sum(agriculture_farm_improvments, na.rm = TRUE),
    Construction = sum(construction, na.rm = TRUE),
    Mining = sum(mining, na.rm = TRUE),
    Manufacturing = sum(manufacturing, na.rm = TRUE),
    `Factory` = sum(manufacturing_factory, na.rm = TRUE),
    `Home` = sum(manufacturing_home, na.rm = TRUE),
    Services = sum(services, na.rm = TRUE),
    `Trade` = sum(services_trade, na.rm = TRUE),
    `Transport and public utilities` = sum(services_transportation_and_public_utilities, na.rm = TRUE),
    `Finance` = sum(services_finance, na.rm = TRUE),
    `Professional` = sum(services_professional, na.rm = TRUE),
    `Personal` = sum(services_personal, na.rm = TRUE),
    `Government` = sum(services_government, na.rm = TRUE),
    `Education` = sum(services_education, na.rm = TRUE),
    `Hand trades` = sum(services_hand_trades, na.rm = TRUE),
    `Shelter` = sum(services_shelter, na.rm = TRUE)
  )

# Create table data frame
southern_gdp_table <- data.frame(
  Sector = c(
    "Agriculture", 
    "\\quad Crops and livestock", 
    "\\quad\\quad Livestock and feed crops", 
    "\\quad\\quad Food grains", 
    "\\quad\\quad Cotton", 
    "\\quad\\quad Other", 
    "\\quad Farm improvements", 
    "Construction", 
    "Mining", 
    "Manufacturing", 
    "\\quad Factory", 
    "\\quad Home", 
    "Services", 
    "\\quad Trade", 
    "\\quad Transport and public utilities", 
    "\\quad Finance", 
    "\\quad Professional", 
    "\\quad Personal", 
    "\\quad Government", 
    "\\quad Education", 
    "\\quad Hand trades", 
    "\\quad Shelter"
  )
)

# Calculate percentages for each year
for(y in c(1839, 1849, 1859, 1869, 1879, 1889, 1899)) {
  year_data <- southern_data %>% filter(year == y)
  if(nrow(year_data) > 0) {
    southern_gdp_table[[as.character(y)]] <- c(
      year_data$Agriculture / year_data$GDP * 100,
      year_data$`Crops and livestock` / year_data$GDP * 100,
      year_data$`Livestock and feed crops` / year_data$GDP * 100,
      year_data$`Food grains` / year_data$GDP * 100,
      year_data$Cotton / year_data$GDP * 100,
      year_data$Other / year_data$GDP * 100,
      year_data$`Farm improvements` / year_data$GDP * 100,
      year_data$Construction / year_data$GDP * 100,
      year_data$Mining / year_data$GDP * 100,
      year_data$Manufacturing / year_data$GDP * 100,
      year_data$Factory / year_data$GDP * 100,
      year_data$Home / year_data$GDP * 100,
      year_data$Services / year_data$GDP * 100,
      year_data$Trade / year_data$GDP * 100,
      year_data$`Transport and public utilities` / year_data$GDP * 100,
      year_data$Finance / year_data$GDP * 100,
      year_data$Professional / year_data$GDP * 100,
      year_data$Personal / year_data$GDP * 100,
      year_data$Government / year_data$GDP * 100,
      year_data$Education / year_data$GDP * 100,
      year_data$`Hand trades` / year_data$GDP * 100,
      year_data$Shelter / year_data$GDP * 100
    )
  } else {
    southern_gdp_table[[as.character(y)]] <- rep(NA, nrow(southern_gdp_table))
  }
}

# Round values to one decimal place
southern_gdp_table[, -1] <- round(southern_gdp_table[, -1], 1)

# Print the table
table_output <- kable(southern_gdp_table, 
                      caption = "Structure of Southern GDP, 1839--1899", 
                      align = rep("r", ncol(southern_gdp_table)),
                      format.args = list(decimal.mark = "."))

print(table_output)

# Table 5: GDP per Worker, 1859

# Filter data for 1859
worker_data <- data %>% filter(year == 1859)

# Identify Missouri
missouri_rows <- worker_data %>% filter(tolower(state) %in% c("missouri", "mo"))
if(nrow(missouri_rows) > 0) {
  missouri_name <- missouri_rows$state[1]
} else {
  missouri_name <- "NOT_FOUND"
}

# Define slave states (South plus Missouri)
slave_states <- worker_data %>%
  filter(region == 3 | state == missouri_name)

# Calculate for United States
us_stats <- worker_data %>%
  summarize(
    labor_force = sum(labor_force, na.rm = TRUE) / 1000000,
    gdp_total = sum(gdp, na.rm = TRUE),
    gdp_per_worker = (gdp_total / labor_force)
  )

# Calculate for free states
free_states_stats <- worker_data %>%
  filter(!(region == 3 | state == missouri_name)) %>%
  summarize(
    labor_force = sum(labor_force, na.rm = TRUE) / 1000000,
    gdp_total = sum(gdp, na.rm = TRUE),
    gdp_per_worker = (gdp_total / labor_force)
  )

# Calculate for slave states
slave_states_stats <- slave_states %>%
  summarize(
    labor_force = sum(labor_force, na.rm = TRUE) / 1000000,
    gdp_total = sum(gdp, na.rm = TRUE),
    gdp_per_worker = (gdp_total / labor_force)
  )

# Define enslaved labor force (using given values)
enslaved_stats <- data.frame(
  labor_force = 2.5,
  gdp_total = 500,
  gdp_per_worker = (500 / 2.5)
)

# For free workers in slave states
free_stats <- data.frame(
  labor_force = slave_states_stats$labor_force - enslaved_stats$labor_force,
  gdp_total = slave_states_stats$gdp_total - enslaved_stats$gdp_total,
  gdp_per_worker = ((slave_states_stats$gdp_total - enslaved_stats$gdp_total) / 
                      (slave_states_stats$labor_force - enslaved_stats$labor_force))
)

# Create the results table
gdp_worker_table <- data.frame(
  Region = c("United States", "Free states", "Slave states", "\\quad Enslaved", "\\quad Free"),
  Labor_force = c(us_stats$labor_force, free_states_stats$labor_force, 
                  slave_states_stats$labor_force, enslaved_stats$labor_force, 
                  free_stats$labor_force),
  GDP_billion = c(us_stats$gdp_total/1000, free_states_stats$gdp_total/1000, 
                  slave_states_stats$gdp_total/1000, enslaved_stats$gdp_total/1000, 
                  free_stats$gdp_total/1000),
  GDP_per_worker = c(us_stats$gdp_per_worker, free_states_stats$gdp_per_worker, 
                     slave_states_stats$gdp_per_worker, enslaved_stats$gdp_per_worker, 
                     free_stats$gdp_per_worker)
)

# Round values
gdp_worker_table$Labor_force <- round(gdp_worker_table$Labor_force, 1)
gdp_worker_table$GDP_billion <- round(gdp_worker_table$GDP_billion, 1)
gdp_worker_table$GDP_per_worker <- round(gdp_worker_table$GDP_per_worker, 0)

# Print the table
table_output <- kable(gdp_worker_table,
                      col.names = c("", "Labor force (millions)", "GDP (billion $)", "GDP ($ per worker)"),
                      align = c("l", "r", "r", "r"),
                      caption = "GDP per Worker, 1859")

print(table_output)