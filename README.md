# R_half-hour-interpolation-code
This R code helps to interpolate data into half hourly for data at hourly scale

To use this script:

Install R and the data.table package.

Prepare your data in a CSV file. Ensure you have a datetime column (or separate date and time columns) and a column with the numeric values you want to interpolate.

Configure the script:

Set in_path and out_path.

Set interval_minutes to your desired output frequency.

Crucially, modify the format string in the as.POSIXct() call to exactly match the datetime format in your CSV file.

Replace Date_Column, Time_Column, and value_column with your actual column names.

Run the script. It will save the interpolated data and show a plot for verification.

