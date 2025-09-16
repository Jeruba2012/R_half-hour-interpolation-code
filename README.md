# R_half-hour-interpolation-code
This R code helps to interpolate data into half hourly for data at hourly scale
# Title: Temporal Interpolation of Time Series Data
# Description: This script interpolates a time series from a lower frequency (e.g., hourly)
#              to a higher frequency (e.g., 30-minute) using linear interpolation.
#              It includes a sanity check plot to compare original and interpolated data.
# Author: Your Name
# Date: YYYY-MM-DD

# Load required packages
# install.packages("data.table") # Uncomment to install if needed
library(data.table)

# ----- User Configuration: Modify these variables for your use case -----

# Input: Path to your CSV file with the original time series data.
in_path  <- "path/to/your/input_data_hourly.csv"

# Output: Path where the interpolated data will be saved.
out_path <- "path/to/your/output_data_subhourly.csv"

# Time Interval: The desired time step for the output data (in minutes).
# Examples: 30 for half-hourly, 15 for quarter-hourly, 10 for 10-minute data.
interval_minutes <- 30

# ----- 1) Read Data and Parse Timestamps -----

# Read the input data file
ts_data <- fread(in_path)

# Parse the datetime column.
# IMPORTANT: Adjust the 'format' argument to match your input file's datetime structure.
# Common formats:
# - "%Y-%m-%d %H:%M:%S" for "2024-01-15 13:30:00" (24-hour)
# - "%m/%d/%Y %I:%M:%S %p" for "1/15/2024 01:30:00 PM" (12-hour with AM/PM)
# - "%d.%m.%Y %H:%M" for "15.01.2024 13:30" (24-hour)
ts_data[, time_utc := as.POSIXct(
  paste(Date_Column, Time_Column), # Replace 'Date_Column' and 'Time_Column' with your actual column names if they are separate.
  format = "%m/%d/%Y %I:%M:%S %p", # <-- CHANGE THIS FORMAT STRING TO MATCH YOUR DATA
  tz = "UTC" # Set the timezone. Use "UTC", "EST", etc., as appropriate.
)]

# Alternatively, if your file already has a single combined datetime column (e.g., "DateTime"):
# ts_data[, time_utc := as.POSIXct(DateTime_Column, format="%Y-%m-%d %H:%M:%S", tz="UTC")]

# Keep just the necessary columns and ensure the value column is numeric
ts_data <- ts_data[, .(
  time_utc,
  value = as.numeric(value_column) # Replace 'value_column' with the name of your data column (e.g., 'temperature', 'blh')
)]

# Data Cleaning: Remove rows with missing time or values, sort by time, remove duplicate timestamps.
ts_data <- ts_data[!is.na(time_utc) & !is.na(value)]
setorder(ts_data, time_utc)
ts_data <- unique(ts_data, by = "time_utc")

# Check if there is enough data to interpolate
if (nrow(ts_data) < 2L) {
  stop("Aborting: Not enough valid data points (n < 2) to perform interpolation.")
}

# ----- 2) Create a Regular High-Frequency Time Grid -----

# Find the start and end time of the original dataset
t_start <- min(ts_data$time_utc, na.rm = TRUE)
t_end   <- max(ts_data$time_utc, na.rm = TRUE)

# Generate a sequence of timestamps at the desired interval
grid <- data.table(
  time_utc = seq(from = t_start,
                 to   = t_end,
                 by   = sprintf("%d min", interval_minutes)) # Creates the regular grid
)

# ----- 3) Perform Linear Interpolation -----

# Convert POSIXct timestamps to numeric (seconds since epoch) for the approx() function
original_times_numeric <- as.numeric(ts_data$time_utc)
original_values <- ts_data$value
grid_times_numeric <- as.numeric(grid$time_utc)

# Interpolate! 'rule = 2' means extrapolate as a constant value beyond the data range.
grid[, value_interp := approx(x = original_times_numeric,
                              y = original_values,
                              xout = grid_times_numeric,
                              method = "linear",
                              rule = 2)$y]

# ----- 4) Save the Result -----

fwrite(grid, out_path)
cat("Interpolation complete.\n")
cat("Original data points: ", nrow(ts_data), "\n")
cat("Interpolated points:  ", nrow(grid), "\n")
cat("Output file saved to:\n", out_path, "\n")

# ----- 5) (Optional) Sanity Check Plot -----

# This block creates a plot to visually verify the interpolation.
# Try to use ggplot2 if installed; otherwise, uses base R graphics.

plot_check <- function(original_dt, interpolated_dt, plot_days = 7) {
  # Subset to the first 'plot_days' days for a clear view
  t_cutoff <- min(original_dt$time_utc) + (plot_days * 24 * 3600)
  plot_original <- original_dt[time_utc <= t_cutoff]
  plot_interp <- interpolated_dt[time_utc <= t_cutoff]
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    p <- ggplot() +
      geom_line(data = plot_interp, aes(x = time_utc, y = value_interp),
                linewidth = 0.5, alpha = 0.8, color = "blue") +
      geom_point(data = plot_original, aes(x = time_utc, y = value),
                 size = 2, color = "red", shape = 1) + # Use open circles
      labs(title = "Time Series Interpolation Sanity Check",
           subtitle = sprintf("Red points: Original data. Blue line: %d-minute interpolation.", interval_minutes),
           x = "Time (UTC)",
           y = "Value") +
      theme_minimal()
    print(p)
  } else {
    message("Package 'ggplot2' not installed. Using base R for plotting.")
    # Use base R plotting
    plot(plot_interp$time_utc, plot_interp$value_interp,
         type = "l", col = "blue",
         xlab = "Time (UTC)", ylab = "Value",
         main = sprintf("Interpolation Check (%d-min line vs original points)", interval_minutes))
    points(plot_original$time_utc, plot_original$value, col = "red", pch = 1)
    legend("topright", legend = c("Interpolated", "Original"),
           col = c("blue", "red"), lty = c(1, NA), pch = c(NA, 1))
  }
}

# Generate the plot (first 7 days by default)
plot_check(ts_data, grid, plot_days = 7)
