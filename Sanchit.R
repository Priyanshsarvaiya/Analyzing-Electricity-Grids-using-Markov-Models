# Load necessary libraries
library(dplyr)
library(imputeTS)  # For interpolation
library(lubridate)

# Load dataset (replace 'TermProjectData.txt' with your actual dataset)
dataset <- read.csv("TermProjectData.txt")

# Inspect dataset structure
str(dataset)

# Ensure the dataset is a data frame
dataset <- as.data.frame(dataset)
dataset$Date <- dmy(dataset$Date)
dataset$Weekday <- factor(format(dataset$Date, "%A"), levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

# Define numerical columns
numerical_columns <- c("Global_active_power", "Global_reactive_power", 
                       "Voltage", "Global_intensity", 
                       "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

# Step 1: Interpolate missing values for numerical columns
dataset[numerical_columns] <- lapply(dataset[numerical_columns], function(col) {
  na_interpolation(col, option = "linear")
})

# Step 2: Remove outliers (z-score > 3)
# Calculate z-scores for each numerical column
z_scores <- scale(dataset[numerical_columns])

# Identify rows where any z-score > 3
outliers <- apply(z_scores, 1, function(row) any(abs(row) > 3))

# Filter out rows with outliers
cleaned_dataset <- dataset[!outliers, ]

# Step 3: Standardize the numerical features
scaled_features <- as.data.frame(scale(cleaned_dataset[numerical_columns]))
# Verify that the mean of each column is approximately 0
mean_check <- colMeans(scaled_features)
cat("Means of each column (should be close to 0):\n")
print(mean_check)

# Verify that the standard deviation of each column is approximately 1
sd_check <- apply(scaled_features, 2, sd)
cat("\nStandard deviations of each column (should be close to 1):\n")
print(sd_check)

# Combine scaled features with non-numerical columns
scaled_dataset <- cbind(cleaned_dataset[, c("Date", "Time", "Weekday")], scaled_features)

print("Feature scaling and cleaning complete.")


# Step 3: Partition Training Data
# Assuming 'Date' is in YYYY-MM-DD format, filter for the first three years
training_data <- cleaned_dataset %>%
  filter(as.Date(Date) < as.Date("2009-01-01"))

# Step 1: Scale the numerical data
scaled_numerical <- as.data.frame(scale(training_numerical))

# Step 2: Update training_data with scaled values
training_data[, numerical_columns] <- scaled_numerical

# Verify that scaling was applied
summary(training_data[, numerical_columns])

# Select only numerical columns for PCA
training_numerical <- training_data[, numerical_columns]

# Step 4: Perform PCA
# Compute principal components
pca_result <- prcomp(training_numerical, center = TRUE, scale. = TRUE)
# Extract loading scores
loading_scores <- pca_result$rotation

# Display summary of PCA
summary(pca_result)

# Step 5: Visualize PCA results
# Install ggbiplot if needed: install.packages("ggbiplot")
library(factoextra)

# Plot PCA using factoextra
# fviz_pca_biplot(pca_result,
#                 geom.ind = "point",  # Points for observations
#                 col.ind = "blue",    # Color for individuals
#                 col.var = "red",     # Color for variables
#                 repel = TRUE) +      # Avoid text overlap
#   ggtitle("PCA Biplot")

# Load necessary libraries
library(depmixS4)
library(plotly)
library(dplyr)
library(parallel)
library(doParallel)

train_hmm_parallel <- function(data, day_range, time_windows, state_range = seq(4, 20, 2)) {
  # Set up parallel processing
  cores <- detectCores() - 1
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Define all combinations of days and time windows
  combinations <- expand.grid(
    Day = day_range,
    StartTime = time_windows$start,
    EndTime = time_windows$end
  )
  
  # Initialize results storage
  results <- foreach(
    i = 1:nrow(combinations), .combine = rbind, .packages = c("depmixS4", "dplyr")
  ) %dopar% {
    # Extract day and time window for current iteration
    day <- combinations$Day[i]
    start_time <- combinations$StartTime[i]
    end_time <- combinations$EndTime[i]
    
    # Filter data based on the current day and time window
    filtered_data <- data %>%
      filter(Weekday == day & 
               as.POSIXct(Time, format = "%H:%M:%S") >= as.POSIXct(start_time, format = "%H:%M:%S") &
               as.POSIXct(Time, format = "%H:%M:%S") <= as.POSIXct(end_time, format = "%H:%M:%S"))
    
    # If no data remains after filtering, skip this combination
    if (nrow(filtered_data) == 0) {
      return(data.frame(
        Day = day,
        StartTime = start_time,
        EndTime = end_time,
        States = NA,
        LogLikelihood = NA,
        NormalizedLogLikelihood = NA,
        BIC = NA,
        Error = TRUE
      ))
    }
    
    # Select numerical columns for HMM training
    hmm_data <- filtered_data %>%
      select(Global_active_power, Global_reactive_power, Voltage, Global_intensity, 
             Sub_metering_1, Sub_metering_2, Sub_metering_3)
    
    # Initialize storage for results
    day_results <- data.frame()
    
    # Train HMM models for each state
    for (num_states in state_range) {
      # Define HMM model
      hmm_model <- depmix(
        response = list(
          Global_active_power ~ 1,
          Global_reactive_power ~ 1,
          Voltage ~ 1,
          Global_intensity ~ 1,
          Sub_metering_1 ~ 1,
          Sub_metering_2 ~ 1,
          Sub_metering_3 ~ 1
        ),
        data = hmm_data,
        nstates = num_states,
        family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian())
      )
      
      # Fit the HMM model using tryCatch
      fit_result <- tryCatch({
        fit_model <- fit(hmm_model, verbose = FALSE)
        
        # Extract log-likelihood and BIC
        log_likelihood <- logLik(fit_model)
        normalized_log_likelihood <- as.numeric(log_likelihood) / nrow(hmm_data)
        bic_score <- BIC(fit_model)
        
        # Store results
        data.frame(
          Day = day,
          StartTime = start_time,
          EndTime = end_time,
          States = num_states,
          LogLikelihood = as.numeric(log_likelihood),
          NormalizedLogLikelihood = normalized_log_likelihood,
          BIC = bic_score,
          Error = FALSE
        )
      }, error = function(e) {
        # Log error and return NA values for problematic states
        data.frame(
          Day = day,
          StartTime = start_time,
          EndTime = end_time,
          States = num_states,
          LogLikelihood = NA,
          NormalizedLogLikelihood = NA,
          BIC = NA,
          Error = TRUE
        )
      })
      
      # Append the result
      day_results <- rbind(day_results, fit_result)
    }
    
    # Return results for the current day and time window
    return(day_results)
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Return combined results
  return(results)
}

# # Define day range and time windows
# day_range <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
# time_windows <- data.frame(
#   start = c("00:00:00", "06:00:00", "12:00:00", "18:00:00"),
#   end = c("06:00:00", "12:00:00", "18:00:00", "23:59:59")
# )
# 
# # Run the parallel HMM training function
# hmm_results_parallel <- train_hmm_parallel(training_data, day_range, time_windows)
# 
# # Inspect results
# print(hmm_results_parallel)
# 
# # Filter the results for successful models
# successful_results <- hmm_results_parallel %>%
#   filter(!Error) %>%
#   arrange(BIC)  # Lower BIC indicates a better model
# 
# # Display the best combination
# best_model <- successful_results[1, ]
# print(best_model)
# 
# # Plot Log-Likelihood and BIC for the best day and time window
# best_day_results <- successful_results %>%
#   filter(Day == best_model$Day & StartTime == best_model$StartTime & EndTime == best_model$EndTime)
# 
# fig <- plot_ly() %>%
#   add_trace(
#     x = best_day_results$States, y = best_day_results$LogLikelihood, name = "Log-Likelihood",
#     type = 'scatter', mode = 'lines+markers'
#   ) %>%
#   add_trace(
#     x = best_day_results$States, y = best_day_results$BIC, name = "BIC",
#     type = 'scatter', mode = 'lines+markers', yaxis = "y2"
#   ) %>%
#   layout(
#     title = paste("HMM Evaluation for Best Day & Time Window:", best_model$Day, 
#                   best_model$StartTime, "-", best_model$EndTime),
#     xaxis = list(title = "Number of States"),
#     yaxis = list(title = "Log-Likelihood"),
#     yaxis2 = list(title = "BIC", overlaying = "y", side = "right"),
#     legend = list(x = 0.1, y = 0.9)
#   )
# 
# # Display the plot
# fig


# Load necessary library
library(ggplot2)

# Function to create time window plots
plot_time_windows <- function(data, day_of_week, variables, start_time, end_time) {
  # Filter data for the specified day and time window
  filtered_data <- data %>%
    filter(
      Weekday == day_of_week &
        as.POSIXct(Time, format = "%H:%M:%S") >= as.POSIXct(start_time, format = "%H:%M:%S") &
        as.POSIXct(Time, format = "%H:%M:%S") <= as.POSIXct(end_time, format = "%H:%M:%S")
    )
  
  # Reshape data for plotting
  plot_data <- filtered_data %>%
    select(Time, all_of(variables)) %>%
    tidyr::pivot_longer(cols = variables, names_to = "Variable", values_to = "Value")
  
  # Create time series plots for each variable
  ggplot(plot_data, aes(x = as.POSIXct(Time, format = "%H:%M:%S"), y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~ Variable, scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste("Variable Distributions for", day_of_week, "from", start_time, "to", end_time),
      x = "Time",
      y = "Value",
      color = "Variables"
    )
}

# Example usage
plot_time_windows(
  data = training_data,
  day_of_week = "Monday",
  variables = c("Global_active_power", "Global_intensity", "Sub_metering_3", 
                "Global_reactive_power", "Sub_metering_2"),
  start_time = "08:00:00",
  end_time = "12:00:00"
)
