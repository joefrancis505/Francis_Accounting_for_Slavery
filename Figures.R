# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages needed
packages <- c("readODS", "dplyr")

# Check which packages are not installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(new_packages)) install.packages(new_packages)

# Load all required packages
library(readODS)
library(dplyr)
cat("\014")  # Clear console

# Set working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Create Figures directories
dir.create("Figures", showWarnings = FALSE)

# Try to load package for writing ODS
if (!require(readODS) || !exists("write_ods", where = asNamespace("readODS"), mode = "function")) {
  # If write_ods is not available in readODS, try to use writexl
  if (!require(writexl)) {
    install.packages("writexl")
    library(writexl)
  }
  use_ods_direct <- FALSE
} else {
  use_ods_direct <- TRUE
}

# Function to set up plot parameters
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, left_margin = 1, right_margin = 1) {
  # Convert inches to points (1 inch = 72 points)
  width_pt <- width * 72
  height_pt <- height * 72
  
  # Set the plot area size
  par(pin = c(width - left_margin - right_margin, height - top_margin - bottom_margin))
  
  # Set margins in inches
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  
  # Set other plot parameters
  par(family = "sans",
      cex = 1.3,
      cex.axis = 1.3,
      cex.lab = 1.3,
      tck = 0.01,
      lwd = 0.5,
      las = 1,
      mgp = c(2.8, 0.8, 0))
}

Figure_1 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  # Process data
  raw_data <- read_ods("GDP_1839_1899_v1.0.ods", sheet = "data")
  names(raw_data) <- tolower(names(raw_data))
  
  raw_data$gdp[raw_data$gdp == "NA"] <- NA
  raw_data$gdp <- as.numeric(raw_data$gdp)
  
  working_data <- raw_data[c("year", "region", "gdp")]
  working_data <- na.omit(working_data)
  
  data_summary <- aggregate(gdp ~ year + region, data = working_data, sum)
  
  total_gdp <- aggregate(gdp ~ year, data = data_summary, sum)
  names(total_gdp)[2] <- "total_gdp"
  
  data_summary <- merge(data_summary, total_gdp, by = "year")
  data_summary$percentage <- (data_summary$gdp / data_summary$total_gdp) * 100
  
  data_wide <- reshape(data_summary[c("year", "region", "percentage")],
                       idvar = "year",
                       timevar = "region",
                       direction = "wide")
  
  data_wide[is.na(data_wide)] <- 0
  names(data_wide) <- gsub("percentage.", "", names(data_wide))
  
  data_wide$west_cum <- data_wide$`4`
  data_wide$south_cum <- data_wide$`4` + data_wide$`3`
  data_wide$midwest_cum <- data_wide$`4` + data_wide$`3` + data_wide$`2`
  data_wide$northeast_cum <- data_wide$`4` + data_wide$`3` + data_wide$`2` + data_wide$`1`
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    plot(data_wide$year, data_wide$northeast_cum,
         type = "n",
         xlab = " ",
         ylab = "% of total",
         xlim = c(1839, 1899),
         ylim = c(0, 100),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    polygon(c(data_wide$year, rev(data_wide$year)), 
            c(data_wide$northeast_cum, rep(0, nrow(data_wide))), 
            col = "gray55", border = NA)
    polygon(c(data_wide$year, rev(data_wide$year)), 
            c(data_wide$midwest_cum, rep(0, nrow(data_wide))), 
            col = "gray75", border = NA)
    polygon(c(data_wide$year, rev(data_wide$year)), 
            c(data_wide$south_cum, rep(0, nrow(data_wide))), 
            col = "gray95", border = NA)
    polygon(c(data_wide$year, rev(data_wide$year)), 
            c(data_wide$west_cum, rep(0, nrow(data_wide))), 
            col = "white", border = NA)
    
    lines(data_wide$year, data_wide$northeast_cum, col = "black", lwd = 0.5)
    lines(data_wide$year, data_wide$midwest_cum, col = "black", lwd = 0.5)
    lines(data_wide$year, data_wide$south_cum, col = "black", lwd = 0.5)
    lines(data_wide$year, data_wide$west_cum, col = "black", lwd = 0.5)
    
    axis(1, at = seq(1839, 1899, by = 10), lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 100, by = 20), lwd = 0, lwd.ticks = 0.5, padj = 0.4)
    box(lwd = 0.5)
    
    text(1885, 3.83, "West", adj = c(0, 0.5), cex = 1.3)
    text(1885, 16.96, "South", adj = c(0, 0.5), cex = 1.3)
    text(1885, 44.82, "Midwest", adj = c(0, 0.5), cex = 1.3)
    text(1885, 81.7, "Northeast", adj = c(0, 0.5), cex = 1.3)
  }
  
  # Console plot
  plot_function()
  
  # PDF plot
  pdf("Figures/gdp.pdf", width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  
  # Return the data used for plotting
  return(data_wide)
}

Figure_2 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  # Process data
  raw_data <- read_ods("GDP_1839_1899_v1.0.ods", sheet = "data")
  names(raw_data) <- tolower(names(raw_data))
  
  raw_data$gdp[raw_data$gdp == "NA"] <- NA
  raw_data$population[raw_data$population == "NA"] <- NA
  raw_data$gdp <- as.numeric(raw_data$gdp)
  raw_data$population <- as.numeric(raw_data$population)
  
  raw_data$area <- ifelse(raw_data$region %in% c(1, 2), "North", 
                          ifelse(raw_data$region == 3, "South", NA))
  
  working_data <- raw_data[!is.na(raw_data$area), ]
  working_data <- na.omit(working_data[c("year", "area", "gdp", "population")])
  
  gdp_sums <- aggregate(gdp ~ year + area, data = working_data, sum)
  pop_sums <- aggregate(population ~ year + area, data = working_data, sum)
  
  data_combined <- merge(gdp_sums, pop_sums)
  
  data_combined$gdp_per_capita <- data_combined$gdp / data_combined$population
  
  yearly_natl_avg <- aggregate(
    gdp_per_capita ~ year, 
    data = data_combined, 
    FUN = function(x) weighted.mean(x, w = data_combined$population[data_combined$year == data_combined$year[1]])
  )
  
  data_final <- merge(data_combined, yearly_natl_avg, by = "year", 
                      suffixes = c("", "_natl"))
  data_final$relative_gdp_pc <- (data_final$gdp_per_capita / 
                                   data_final$gdp_per_capita_natl) * 100
  
  data_wide <- reshape(data_final[c("year", "area", "relative_gdp_pc")],
                       idvar = "year",
                       timevar = "area",
                       direction = "wide")
  names(data_wide) <- gsub("relative_gdp_pc.", "", names(data_wide))
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    plot(data_wide$year, data_wide$North,
         type = "n",
         xlab = " ",
         ylab = "National average = 100",
         xlim = c(1839, 1899),
         ylim = c(0, 160),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    axis(1, at = seq(1839, 1899, by = 10), lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 160, by = 40), lwd = 0, lwd.ticks = 0.5, padj = 0.4)
    box(lwd = 0.5)
    
    lines(data_wide$year, data_wide$North, col = "black", lwd = 2)
    lines(data_wide$year, data_wide$South, col = "black", lwd = 1)
    
    text(1888, 134.9, "North", adj = c(0, 0.5), cex = 1.3)
    text(1888, 53, "South", adj = c(0, 0.5), cex = 1.3)
  }
  
  # Console plot
  plot_function()
  
  # PDF plot
  pdf("Figures/gdp_per_capita.pdf", width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  
  # Return the data used for plotting
  return(data_wide)
}

Figure_3 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  # Process data
  data <- read_ods("GDP_1839_1899_v1.0.ods", sheet = "data")
  
  to_numeric <- function(x) {
    suppressWarnings(as.numeric(x))
  }
  
  data <- data %>%
    mutate(
      gdp = to_numeric(gdp),
      population = to_numeric(population),
      agriculture_crops_and_livestock_cotton = to_numeric(agriculture_crops_and_livestock_cotton),
      region = as.numeric(region),
      year = as.numeric(year)
    )
  
  regional_data <- data %>%
    group_by(year) %>%
    summarize(
      north_gdp = sum(gdp[region %in% c(1, 2)], na.rm = TRUE),
      south_gdp = sum(gdp[region == 3], na.rm = TRUE),
      north_pop = sum(population[region %in% c(1, 2)], na.rm = TRUE),
      south_pop = sum(population[region == 3], na.rm = TRUE),
      north_cotton = sum(agriculture_crops_and_livestock_cotton[region %in% c(1, 2)], na.rm = TRUE),
      south_cotton = sum(agriculture_crops_and_livestock_cotton[region == 3], na.rm = TRUE)
    )
  
  regional_data <- regional_data %>%
    mutate(
      north_pc = north_gdp / north_pop,
      south_pc = south_gdp / south_pop,
      north_pc_no_cotton = (north_gdp - north_cotton) / north_pop,
      south_pc_no_cotton = (south_gdp - south_cotton) / south_pop,
      w_cotton = (south_pc / north_pc) * 100,
      w_o_cotton = (south_pc_no_cotton / north_pc_no_cotton) * 100
    )
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    plot(c(1839, 1899), c(0, 100),
         type = "n",
         xlab = " ",
         ylab = "North = 100",
         xlim = c(1839, 1899),
         ylim = c(0, 100),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    axis(1, at = seq(1839, 1899, by = 10), lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 100, by = 20), lwd = 0, lwd.ticks = 0.5, padj = 0.4)
    box(lwd = 0.5)
    
    lines(regional_data$year, regional_data$w_o_cotton, col = "black", lwd = 1)
    lines(regional_data$year, regional_data$w_cotton, col = "black", lwd = 2)
    
    text(1879, 35, "without cotton", adj = c(0.5, 0.5), cex = 1.3)
  }
  
  # Console plot
  plot_function()
  
  # PDF plot
  pdf("Figures/cotton_gdp.pdf", width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  
  # Return the data used for plotting
  return(regional_data[, c("year", "w_cotton", "w_o_cotton")])
}

Figure_4 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  # Process data
  raw_data <- read_ods("GDP_1839_1899_v1.0.ods", sheet = "data")
  names(raw_data) <- tolower(names(raw_data))
  
  raw_data$labor_force[raw_data$labor_force == "NA"] <- NA
  raw_data$population[raw_data$population == "NA"] <- NA
  raw_data$labor_force <- as.numeric(raw_data$labor_force)
  raw_data$population <- as.numeric(raw_data$population)
  
  raw_data$area <- ifelse(raw_data$region %in% c(1, 2), "North", 
                          ifelse(raw_data$region == 3, "South", NA))
  
  raw_data$display_year <- raw_data$year + (10 - raw_data$year %% 10)
  
  working_data <- raw_data[!is.na(raw_data$area), ]
  working_data <- na.omit(working_data[c("display_year", "area", "labor_force", "population")])
  
  labor_sums <- aggregate(labor_force ~ display_year + area, data = working_data, sum)
  pop_sums <- aggregate(population ~ display_year + area, data = working_data, sum)
  
  data_combined <- merge(labor_sums, pop_sums)
  
  data_combined$participation_rate <- (data_combined$labor_force / data_combined$population) * 100
  
  data_wide <- reshape(data_combined[c("display_year", "area", "participation_rate")],
                       idvar = "display_year",
                       timevar = "area",
                       direction = "wide")
  names(data_wide) <- gsub("participation_rate.", "", names(data_wide))
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    plot(data_wide$display_year, data_wide$North,
         type = "n",
         xlab = " ",
         ylab = "% of population in labor force",
         xlim = c(1840, 1900),
         ylim = c(0, 50),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    axis(1, at = seq(1840, 1900, by = 10), lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 50, by = 10), lwd = 0, lwd.ticks = 0.5, padj = 0.4)
    box(lwd = 0.5)
    
    lines(data_wide$display_year, data_wide$North, col = "black", lwd = 2)
    lines(data_wide$display_year, data_wide$South, col = "black", lwd = 1)
    
    text(1850, 44.4, "South", adj = c(0, 0.5), cex = 1.3)
    text(1850, 28.2, "North", adj = c(0, 0.5), cex = 1.3)
  }
  
  # Console plot
  plot_function()
  
  # PDF plot
  pdf("Figures/participation.pdf", width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  
  # Return the data used for plotting
  return(data_wide)
}

Figure_5 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  # Process data
  raw_data <- read_ods("GDP_1839_1899_v1.0.ods", sheet = "data")
  names(raw_data) <- tolower(names(raw_data))
  
  raw_data$labor_force_agricultural[raw_data$labor_force_agricultural == "NA"] <- NA
  raw_data$labor_force[raw_data$labor_force == "NA"] <- NA
  raw_data$labor_force_agricultural <- as.numeric(raw_data$labor_force_agricultural)
  raw_data$labor_force <- as.numeric(raw_data$labor_force)
  
  raw_data$area <- ifelse(raw_data$region %in% c(1, 2), "North", 
                          ifelse(raw_data$region == 3, "South", NA))
  
  raw_data$display_year <- raw_data$year + (10 - raw_data$year %% 10)
  
  working_data <- raw_data[!is.na(raw_data$area), ]
  working_data <- na.omit(working_data[c("display_year", "area", "labor_force_agricultural", "labor_force")])
  
  agr_sums <- aggregate(labor_force_agricultural ~ display_year + area, data = working_data, sum)
  labor_sums <- aggregate(labor_force ~ display_year + area, data = working_data, sum)
  
  data_combined <- merge(agr_sums, labor_sums)
  
  data_combined$agr_share <- (data_combined$labor_force_agricultural / data_combined$labor_force) * 100
  
  data_wide <- reshape(data_combined[c("display_year", "area", "agr_share")],
                       idvar = "display_year",
                       timevar = "area",
                       direction = "wide")
  names(data_wide) <- gsub("agr_share.", "", names(data_wide))
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    plot(data_wide$display_year, data_wide$North,
         type = "n",
         xlab = " ",
         ylab = "% agricultural",
         xlim = c(1840, 1900),
         ylim = c(0, 100),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    axis(1, at = seq(1840, 1900, by = 10), lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 100, by = 20), lwd = 0, lwd.ticks = 0.5, padj = 0.4)
    box(lwd = 0.5)
    
    lines(data_wide$display_year, data_wide$North, col = "black", lwd = 2)
    lines(data_wide$display_year, data_wide$South, col = "black", lwd = 1)
    
    text(1892, 22, "North", adj = c(0, 0.5), cex = 1.3)
    text(1892, 65.8, "South", adj = c(0, 0.5), cex = 1.3)
  }
  
  # Console plot
  plot_function()
  
  # PDF plot
  pdf("Figures/agricultural_share.pdf", width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  
  # Return the data used for plotting
  return(data_wide)
}

Figure_6 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  # Process data
  raw_data <- read_ods("GDP_1839_1899_v1.0.ods", sheet = "data")
  names(raw_data) <- tolower(names(raw_data))
  
  raw_data$agriculture[raw_data$agriculture == "NA"] <- NA
  raw_data$labor_force_agricultural[raw_data$labor_force_agricultural == "NA"] <- NA
  raw_data$agriculture <- as.numeric(raw_data$agriculture)
  raw_data$labor_force_agricultural <- as.numeric(raw_data$labor_force_agricultural)
  
  raw_data$area <- ifelse(raw_data$region %in% c(1, 2), "North", 
                          ifelse(raw_data$region == 3, "South", NA))
  
  working_data <- raw_data[!is.na(raw_data$area), ]
  working_data <- na.omit(working_data[c("year", "area", "agriculture", "labor_force_agricultural")])
  
  agriculture_sums <- aggregate(agriculture ~ year + area, data = working_data, sum)
  labor_sums <- aggregate(labor_force_agricultural ~ year + area, data = working_data, sum)
  
  data_combined <- merge(agriculture_sums, labor_sums)
  data_combined$agriculture_per_capita <- data_combined$agriculture / data_combined$labor_force_agricultural
  
  yearly_natl_avg <- aggregate(
    agriculture_per_capita ~ year, 
    data = data_combined, 
    FUN = function(x) weighted.mean(x, w = data_combined$labor_force_agricultural[data_combined$year == data_combined$year[1]])
  )
  
  data_final <- merge(data_combined, yearly_natl_avg, by = "year", suffixes = c("", "_natl"))
  data_final$relative_agriculture_pc <- (data_final$agriculture_per_capita / data_final$agriculture_per_capita_natl) * 100
  
  data_wide <- reshape(data_final[c("year", "area", "relative_agriculture_pc")],
                       idvar = "year",
                       timevar = "area",
                       direction = "wide")
  names(data_wide) <- gsub("relative_agriculture_pc.", "", names(data_wide))
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    plot(data_wide$year, data_wide$North,
         type = "n",
         xlab = " ",
         ylab = "National average = 100",
         xlim = c(1839, 1899),
         ylim = c(0, 160),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    axis(1, at = seq(1839, 1899, by = 10), lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 160, by = 40), lwd = 0, lwd.ticks = 0.5, padj = 0.4)
    box(lwd = 0.5)
    
    lines(data_wide$year, data_wide$North, col = "black", lwd = 2)
    lines(data_wide$year, data_wide$South, col = "black", lwd = 1)
    
    text(1888, 144.5, "North", adj = c(0, 0.5), cex = 1.3)
    text(1888, 58.3, "South", adj = c(0, 0.5), cex = 1.3)
  }
  
  # Console plot
  plot_function()
  
  # PDF plot
  pdf("Figures/agricultural_productivity.pdf", width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  
  # Return the data used for plotting
  return(data_wide)
}

# New function to save figure data to ODS or Excel
save_figures_data <- function(fig1_data, fig2_data, fig3_data, fig4_data, fig5_data, fig6_data) {
  if (use_ods_direct) {
    # Using readODS to save directly to ODS format
    write_ods(fig1_data, "Figures.ods", sheet = "Figure_1")
    write_ods(fig2_data, "Figures.ods", sheet = "Figure_2", append = TRUE)
    write_ods(fig3_data, "Figures.ods", sheet = "Figure_3", append = TRUE)
    write_ods(fig4_data, "Figures.ods", sheet = "Figure_4", append = TRUE)
    write_ods(fig5_data, "Figures.ods", sheet = "Figure_5", append = TRUE)
    write_ods(fig6_data, "Figures.ods", sheet = "Figure_6", append = TRUE)
    
    cat("Saved figure data to Figures.ods\n")
  } else {
    # Using writexl to save to Excel format
    data_list <- list(
      "Figure_1" = fig1_data,
      "Figure_2" = fig2_data,
      "Figure_3" = fig3_data,
      "Figure_4" = fig4_data,
      "Figure_5" = fig5_data,
      "Figure_6" = fig6_data
    )
    
    write_xlsx(data_list, "Figures.xlsx")
    
    cat("Saved figure data to Figures.xlsx\n")
    cat("Note: To convert to ODS format, you can use LibreOffice or OpenOffice to convert the Excel file.\n")
    cat("      Alternatively, update the readODS package to version 2.0.0 or later for direct ODS support.\n")
  }
}

# Generate figures and collect their data
fig1_data <- Figure_1()
fig2_data <- Figure_2()
fig3_data <- Figure_3()
fig4_data <- Figure_4()
fig5_data <- Figure_5()
fig6_data <- Figure_6()

# Save all figure data to ODS or Excel
save_figures_data(fig1_data, fig2_data, fig3_data, fig4_data, fig5_data, fig6_data)