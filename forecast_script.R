# forecast_script.R

# Load only the required libraries for forecasting
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(zoo))

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript forecast_script.R <input_csv> <periods> <output_csv> <summary_txt>", call. = FALSE)
}

# Assign arguments to variables
input_file <- args[1]
forecast_periods <- as.integer(args[2])
output_file <- args[3]
summary_file <- args[4]

# Read the data and create a time series object
tryCatch({
  # Read data from the input CSV
  data <- read.csv(input_file)
  
  # Get the start year and month from the first row
  start_year <- as.integer(substr(data$Month_Year[1], 1, 4))
  start_month <- as.integer(substr(data$Month_Year[1], 6, 7))

  # Create the time series object
  sales_ts <- ts(data$Total_Sales, start = c(start_year, start_month), frequency = 12)

  # Clean the time series using tsclean
  cleaned_ts <- tsclean(sales_ts)

  # Create Fourier terms for seasonality
  m <- frequency(cleaned_ts)
  k_fourier <- floor(m / 2)
  xreg <- fourier(cleaned_ts, K = k_fourier)
  future_xreg <- fourier(cleaned_ts, K = k_fourier, h = forecast_periods)

  # Fit the auto.arima model
  fit <- auto.arima(
    cleaned_ts,
    xreg = xreg,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE,
    lambda = "auto"
  )

  # Generate the forecast
  fc <- forecast(fit, h = forecast_periods, xreg = future_xreg)
  
  # Prepare and write the forecast output to CSV
  forecast_output <- data.frame(
    Point.Forecast = as.numeric(fc$mean),
    Lo.80 = as.numeric(fc$lower[, 1]),
    Hi.80 = as.numeric(fc$upper[, 1]),
    Lo.95 = as.numeric(fc$lower[, 2]),
    Hi.95 = as.numeric(fc$upper[, 2])
  )
  write.csv(forecast_output, file = output_file, row.names = FALSE)
  
  # Write the model summary to a text file
  summary_text <- capture.output(summary(fit))
  writeLines(summary_text, con = summary_file)
  
}, error = function(e) {
  # If an error occurs, write it to the summary file
  writeLines(paste("Error in R script:", e$message), con = summary_file)
  file.create(output_file)
})