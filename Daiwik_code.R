# Essentially the same code with different accuracy values

# Load necessary libraries
library(ggplot2)
library(ggbiplot)
library(dplyr)
library(data.table)
library(lubridate)
library(depmixS4)
library(gridExtra)

# Pre-processing Data-set
# Initial setup and loading data-set
df <- read.table("TermProjectData.txt", header = T, sep=",")

# Display the first 5 rows and dimensions of the data-set
head(df)
dim(df)

# Function to interpolate columns and handle NA values
interpolate_column <- function(column) {
  approx(
    x = which(!is.na(column)),
    y = column[!is.na(column)],
    xout = 1:nrow(df),
    rule = 2
  )$y
}

# Applying linear interpolation to all relevant columns
df$Global_active_power <- interpolate_column(df$Global_active_power)
df$Global_reactive_power <- interpolate_column(df$Global_reactive_power)
df$Voltage <- interpolate_column(df$Voltage)
df$Global_intensity <- interpolate_column(df$Global_intensity)
df$Sub_metering_1 <- interpolate_column(df$Sub_metering_1)
df$Sub_metering_2 <- interpolate_column(df$Sub_metering_2)
df$Sub_metering_3 <- interpolate_column(df$Sub_metering_3)

#spliting weekday name 
df$Date <- dmy(df$Date)
df$Weekday <- factor(format(df$Date, "%A"), levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

# Selecting only numeric columns for PCA
df_numeric <- df[, -c(1, 2, 10)]

# Calculate Z-scores for detecting point anomalies on numeric columns
z_scores <- scale(df_numeric)  # Standardizes only the numeric columns
# Identify anomalies where |Z-score| > 3 for any feature
anomalies <- apply(abs(z_scores), 1, function(x) any(x > 3))

df_clean <- df_numeric[!anomalies, ]
df_date <- df[!anomalies, ]
scaled_df <- scale(df_clean)

#adding Date, time, Weekday column to scaled data
datetime_info <- df_date[, c("Date", "Time", "Weekday")]
scaled_df_with_info <- cbind(datetime_info, scaled_df)


# Verify that the mean of each column is approximately 0
mean_check <- colMeans(scaled_df)
cat("Means of each column (should be close to 0):\n")
print(mean_check)

# Verify that the standard deviation of each column is approximately 1
sd_check <- apply(scaled_df, 2, sd)
cat("\nStandard deviations of each column (should be close to 1):\n")
print(sd_check)



# 
# # PC1 + PC2 + PC3 + PC4
# ranked_pc1_pc2_pc3_pc5 <- rank_variables(loading_scores, pcs = 1:5)
# 
# # Display results
# print("Ranked Variables (PC1 only):")
# print(ranked_pc1)
# 
# print("Ranked Variables (PC1 + PC2):")
# print(ranked_pc1_pc2)
# 
# print("Ranked Variables (PC1 + PC2 + PC3 + PC4):")
# print(ranked_pc1_pc2_pc3_pc5)

# # To select top variables for each subset (e.g., top 3 variables)
# n <- 3  # Adjust to choose top variables
# top_pc1 <- names(ranked_pc1[1:n])
# top_pc1_pc2 <- names(ranked_pc1_pc2[1:n])
# top_pc1_pc2_pc3 <- names(ranked_pc1_pc2_pc3[1:n])
# # top_pc1_pc2_pc3_pc4 <- names(ranked_pc1_pc2_pc3_pc4[1:n])
# 
# print(paste("Top", n, "Variables for PC1:"))
# print(top_pc1)
# 
# print(paste("Top", n, "Variables for PC1 + PC2:"))
# print(top_pc1_pc2)
# 
# print(paste("Top", n, "Variables for PC1 + PC2 + PC3:"))
# print(top_pc1_pc2_pc3)

# print(paste("Top", n, "Variables for PC1 + PC2 + PC3 + PC4:"))
# print(top_pc1_pc2_pc3_pc4)

#spliting the data into training data-set and testing data-set #154 total weeks
train_data <- subset(scaled_df_with_info, Date <= "2009-01-24") #110 weeks in training 
test_data <- subset(scaled_df_with_info, Date >= "2009-01-25") #44 weeks in testing

train_data_numeric <- train_data[, -c(1, 2, 3)]

cor(train_data_numeric)

# Compute Principal Components
pca_result <- prcomp(train_data_numeric, center = TRUE, scale. = TRUE, retx = TRUE)

# Summary of PCA to show variance explained by each principal component
summary(pca_result)

# Extract loading scores
loading_scores <- pca_result$rotation

print(loading_scores)


# Function to rank variables based on cumulative loadings for given PCs
rank_variables <- function(loading_scores, pcs) {
  # Ensure loading_scores subset is a matrix
  abs_loadings_combined <- rowSums(abs(loading_scores[, pcs, drop = FALSE]))
  
  # Rank variables by their cumulative contributions
  ranked_variables <- sort(abs_loadings_combined, decreasing = TRUE)
  return(ranked_variables)
}

# PC1 + PC2 + PC3
ranked_pc1_pc2 <- rank_variables(loading_scores, pcs = 1:2)
top_pc1_pc2 <- names(ranked_pc1_pc2[1:2])
print(paste("Top", 3, "Variables for PC1 + PC2:"))
print(top_pc1_pc2)


# # Define time windows as a list
# time_windows <- list(
#   morning = c("06:00:00", "09:00:00"),
#   mid_morning = c("09:00:00", "12:00:00"),
#   afternoon = c("12:00:00", "15:00:00"),
#   late_afternoon = c("15:00:00", "18:00:00"),
#   evening = c("18:00:00", "21:00:00")
# )
# 
# # Loop through each time window and compute correlation
# correlation_results <- list()
# 
# for (window in names(time_windows)) {
#   start_time <- time_windows[[window]][1]
#   end_time <- time_windows[[window]][2]
#   
#   # Filter the data based on time window
#   train_data_filtered <- subset(train_data, Weekday %in% c("Sunday") & Time > start_time & Time <= end_time)
#   
#   # Select and scale relevant columns
#   train_data_filtered_selected <- train_data_filtered[, c("Sub_metering_3", "Global_intensity")]
#   train_data_filtered_selected_scaled <- as.data.frame(train_data_filtered_selected)
#   
#   # Calculate correlation for the selected window
#   correlation_matrix <- cor(train_data_filtered_selected_scaled)
#   correlation_results[[window]] <- correlation_matrix
# }
# 
# # Print correlation results for each time window Sunday
# correlation_results








summary(train_data_numeric)

#filtering the days and time
train_data_filtered <- subset(train_data, Weekday %in% c("Friday") & Time > "06:00:00" & Time <= "09:00:00")
# train_data_filtered_selected <- train_data_filtered[, c("Sub_metering_1", "Sub_metering_2", "Global_reactive_power")]
train_data_filtered_selected <- train_data_filtered[, c("Sub_metering_3", "Global_reactive_power", "Global_active_power")]
train_data_filtered_selected_scaled <- as.data.frame(train_data_filtered_selected)
summary(train_data_filtered_selected_scaled)
cor(train_data_filtered_selected_scaled)

#num_mondays <- 111
num_timepoints_per_day <- train_data_filtered %>%
  group_by(Date) %>%
  summarise(num_timepoints = n()) %>%
  pull(num_timepoints)  # Extract the vector of counts

# Print summary of timepoints
cat("Number of timepoints per day:\n")
print(num_timepoints_per_day)

set.seed(456)
#applying HMM to different states
fit_hmm <- function(n_states) {
  tryCatch({
    # Build the model using the correct ntimes for the filtered data
    model <- depmix(response = list(Sub_metering_3 ~ 1,
                                    Global_active_power ~ 1, Global_reactive_power ~ 1),
                    data = train_data_filtered_selected_scaled,
                    nstates = n_states,
                    family = list(gaussian(), gaussian(), gaussian()),
                    ntimes = num_timepoints_per_day)
    
    # Fit the model
    fit_model <- fit(model)
    
    # Get log-likelihood and BIC
    logLik_value <- logLik(fit_model)
    BIC_value <- BIC(fit_model)
    
    # Print log-likelihood and BIC for this number of states
    cat("Number of states:", n_states, "\n")
    cat("Log-Likelihood:", logLik_value, "\n")
    cat("BIC:", BIC_value, "\n\n")
    
    return(list(logLik = logLik_value, BIC = BIC_value, model = fit_model))
    
  }, error = function(e) {
    message(paste("Error with", n_states, "states: ", e))
    return(NULL)
  })
}

# Define the specific state counts to test
state_counts <- c(4, 6, 8, 10, 12)

# Use lapply to apply the fit_hmm function for each specified state count and store the results
model_results <- lapply(state_counts, fit_hmm)

# Filter out any NULL results (i.e., errors in fitting)
model_results <- Filter(Negate(is.null), model_results)

# Convert the results into a data frame
model_results_df <- do.call(rbind, lapply(seq_along(model_results), function(i) {
  data.frame(num_states = state_counts[i],
             logLik = model_results[[i]]$logLik,
             BIC = model_results[[i]]$BIC)
}))


# Store the model with 6 states which is best model
Best_model <- model_results[[which(state_counts == 6)]]$model

# Log-likelihood for training data (already computed)
logLik_train <- model_results[[which(state_counts == 6)]]$logLik

# Normalize log-likelihood for training data
normalized_logLik_train <- logLik_train / sum(num_timepoints_per_day)

# Print normalized log-likelihood for training data
cat("Normalized Log-Likelihood for training data (12 states):\n") 
print(normalized_logLik_train)

# Filtering the days and time for the test data (same conditions as for train data)
test_data_filtered <- subset(test_data, Weekday %in% c("Friday") & Time > "06:00:00" & Time <= "09:00:00")

# Selecting the same columns (Global_active_power, Sub_metering_3, Global_reactive_power)
test_data_filtered_selected <- test_data_filtered[, c("Global_active_power", "Sub_metering_3", "Global_reactive_power")]

# Scaling the selected columns for the test data
test_data_filtered_selected_scaled <- as.data.frame(test_data_filtered_selected)

num_timepoints_per_day_test <- test_data_filtered %>%
  group_by(Date) %>%
  summarise(num_timepoints = n()) %>%
  pull(num_timepoints)  # Extract the vector of counts

# Print summary of timepoints
cat("Number of timepoints per day:\n")
print(num_timepoints_per_day_test)

# Function to evaluate the test data using forward-backward and extracted parameters
evaluate_hmm_test <- function(trained_model, test_data, num_timepoints_per_day) {
  tryCatch({
    # Create a test model with the same structure as the trained model
    test_model <- depmix(response = list(Sub_metering_3 ~ 1,
                                         Global_active_power ~ 1, Global_reactive_power ~ 1),
                         data = test_data,
                         nstates = 6,  # Use the number of states from the trained model
                         family = list(gaussian(), gaussian(), gaussian()),
                         ntimes = num_timepoints_per_day)
    
    # Set parameters of the test model using the trained model's parameters
    test_model <- setpars(test_model, getpars(trained_model))
    
    # Run the forward-backward algorithm to compute log-likelihood
    logLik_test <- forwardbackward(test_model)
    
    # Extract log-likelihood value
    logLik_value <- logLik_test$logLik
    
    # Normalize the log-likelihood (optional)
    normalized_logLik <- logLik_value / sum(num_timepoints_per_day_test)
    
    # Print log-likelihood results
    cat("Log-Likelihood for test data:", logLik_value, "\n")
    cat("Normalized Log-Likelihood for test data:", normalized_logLik, "\n\n")
    
    return(list(logLik = logLik_value, normalized_logLik = normalized_logLik))
    
  }, error = function(e) {
    message(paste("Error in evaluating test data: ", e))
    return(NULL)
  })
}

# Evaluate the test data
test_results <- evaluate_hmm_test(trained_model = Best_model,
                                  test_data = test_data_filtered_selected_scaled,
                                  num_timepoints_per_day = num_timepoints_per_day_test)

# Access log-likelihood results
logLik_test <- test_results$logLik
normalized_logLik_test <- test_results$normalized_logLik


# Assuming you already have the variables `normalized_logLik_train` and `normalized_logLik_test` computed

# Print Normalized Log-Likelihood for Training Data
cat("Normalized Log-Likelihood for Training Data:", normalized_logLik_train, "\n")

# Print Normalized Log-Likelihood for Test Data
cat("Normalized Log-Likelihood for Test Data:", normalized_logLik_test, "\n")

# Calculate the ratio of the normalized log-likelihood for test to training data
logLik_ratio <- normalized_logLik_test / normalized_logLik_train

# Print the ratio
cat("Ratio of Normalized Log-Likelihood (Test/Training):", logLik_ratio, "\n")



# -------
# Calculate total days in the test dataset


# Add a day index to test dataset
test_data_filtered <- test_data_filtered %>%
  arrange(Date) %>%  # Ensure data is sorted by Date
  mutate(day_index = as.integer(factor(Date, levels = unique(Date))))  # Sequential index for each date

# Determine the number of days per group
total_days <- max(test_data_filtered$day_index)
days_per_group <- total_days / 10  # Divide days into 10 approximately equal groups

# Assign group indices based on the day index
test_data_filtered <- test_data_filtered %>%
  mutate(day_group = ceiling(day_index / days_per_group))

# Verify the distribution of days across groups
cat("Distribution of days across groups:\n")
print(table(test_data_filtered$day_group))

# Split test data into 10 subsets based on day_group
daily_subsets <- split(test_data_filtered, test_data_filtered$day_group)

# Calculate ntimes for each group
num_timepoints_per_day_group <- sapply(daily_subsets, nrow)

# Print number of timepoints for each group
cat("Number of timepoints per group:\n")
print(num_timepoints_per_day_group)

# Function to evaluate log-likelihood for each group
compute_group_logLik <- function(group_data, trained_model, ntimes_per_group) {
  tryCatch({
    group_model <- depmix(
      response = list(Sub_metering_3 ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1),
      data = group_data,
      nstates = 6,  # Use the number of states from the trained model
      family = list(gaussian(), gaussian(), gaussian()),
      ntimes = ntimes_per_group
    )
    group_model <- setpars(group_model, getpars(trained_model))
    logLik_group <- forwardbackward(group_model)$logLik
    normalized_logLik <- logLik_group / ntimes_per_group
    return(list(logLik_group = logLik_group, normalized_logLik = normalized_logLik))
  }, error = function(e) {
    message(paste("Error in log-likelihood calculation for group data:", e))
    return(NULL)
  })
}

# Compute log-likelihood for each group
group_logLik_results <- mapply(
  compute_group_logLik,
  group_data = daily_subsets,
  ntimes_per_group = num_timepoints_per_day_group,
  MoreArgs = list(trained_model = Best_model),
  SIMPLIFY = FALSE
)

# Extract log-likelihoods and normalized log-likelihoods from results
logLik_values <- sapply(group_logLik_results, function(res) res$logLik_group)
normalized_logLik_values <- sapply(group_logLik_results, function(res) res$normalized_logLik)

# Calculate the maximum deviation
max_deviation_logLik <- max(abs(logLik_values - logLik_train), na.rm = TRUE)
max_deviation_normalized <- max(abs(normalized_logLik_values - normalized_logLik_train), na.rm = TRUE)

# Define anomaly threshold
threshold_logLik <- logLik_train + max_deviation_logLik
threshold_normalized <- normalized_logLik_train + max_deviation_normalized

# Print results
cat("Maximum deviation (Log-Likelihood):", max_deviation_logLik, "\n")
cat("Threshold (Log-Likelihood):", threshold_logLik, "\n\n")

cat("Maximum deviation (Normalized Log-Likelihood):", max_deviation_normalized, "\n")
cat("Threshold (Normalized Log-Likelihood):", threshold_normalized, "\n\n")

# Create a data frame for plotting both metrics
test_results <- data.frame(
  Group = 1:10,
  LogLikelihood = logLik_values,
  NormalizedLogLikelihood = normalized_logLik_values
)

# Plot Log-Likelihood and its threshold
ggplot(test_results, aes(x = Group)) +
  geom_line(aes(y = LogLikelihood), color = "blue", size = 1) +
  geom_hline(yintercept = threshold_logLik, color = "red", linetype = "dashed") +
  labs(title = "Log-Likelihoods of Test Subsets vs. Threshold",
       x = "Test Subset Group",
       y = "Log-Likelihood") +
  theme_minimal()

# Plot Normalized Log-Likelihood and its threshold
ggplot(test_results, aes(x = Group)) +
  geom_line(aes(y = NormalizedLogLikelihood), color = "green", size = 1) +
  geom_hline(yintercept = threshold_normalized, color = "purple", linetype = "dashed") +
  labs(title = "Normalized Log-Likelihoods of Test Subsets vs. Threshold",
       x = "Test Subset Group",
       y = "Normalized Log-Likelihood") +
  theme_minimal()


#anomaly injected and then detecting it.
test_data_anomalous <- test_data_filtered_selected_scaled

# Function to inject anomalies at a given percentage
inject_anomalies <- function(test_data, anomaly_percentage) {
  set.seed(123)  # for reproducibility
  
  # Calculate the number of anomalies to inject
  num_anomalies <- round(anomaly_percentage * nrow(test_data))
  
  # Create evenly spaced indices for anomalies
  step_size <- floor(nrow(test_data) / num_anomalies)
  anomaly_indices <- seq(from = step_size, to = nrow(test_data), by = step_size)
  
  # Ensure the number of anomalies is correct (adjust if needed)
  anomaly_indices <- anomaly_indices[1:num_anomalies]
  
  # Inject anomalies: Scale up Global_active_power by a random factor
  test_data$Global_active_power[anomaly_indices] <- 
    test_data$Global_active_power[anomaly_indices] * runif(length(anomaly_indices), 2, 5)  # Random scaling
  
  return(list(data = test_data, anomaly_indices = anomaly_indices))
}

# Anomaly levels to test: 1%, 2%, 3%
anomaly_levels <- c(0.01, 0.02, 0.03)  # 1%, 2%, 3%

# Create a list to store results for different anomaly levels
results_anomalies <- list()

# Inject anomalies and calculate log-likelihood for each level
for (level in anomaly_levels) {
  # Inject anomalies at the current level
  injected_result <- inject_anomalies(test_data_anomalous, level)
  test_data_with_anomalies <- injected_result$data
  anomaly_indices <- injected_result$anomaly_indices
  
  # Assuming `evaluate_hmm_test` is a function that evaluates the HMM model (defined earlier)
  test_results_anomalous <- evaluate_hmm_test(trained_model = Best_model,
                                              test_data = test_data_with_anomalies,
                                              num_timepoints_per_day = num_timepoints_per_day_test)
  
  # Store results for each anomaly level
  results_anomalies[[paste0(level * 100, "%")]] <- list(
    data = test_data_with_anomalies,
    logLik = test_results_anomalous$logLik,
    normalized_logLik = test_results_anomalous$normalized_logLik,
    anomaly_indices = anomaly_indices
  )
  
  # Access log-likelihood for anomalous test data
  logLik_test_anomalous <- test_results_anomalous$logLik
  normalized_logLik_test_anomalous <- test_results_anomalous$normalized_logLik
  
  # Check if either of the thresholds are exceeded for Log-Likelihood or Normalized Log-Likelihood
  if (abs(logLik_test_anomalous - logLik_train) > abs(threshold_logLik) | abs(normalized_logLik_test_anomalous - normalized_logLik_train) > abs(threshold_normalized)) {
    cat(paste("Anomaly detected based on Log-Likelihood or Normalized Log-Likelihood for", level * 100, "% anomalies.\n"))
  } else {
    cat(paste("No anomaly detected based on Log-Likelihood or Normalized Log-Likelihood for", level * 100, "% anomalies.\n"))
  }
  
}

# Create the first plot to visualize anomalies for each anomaly level (1%, 2%, 3%)
for (level in names(results_anomalies)) {
  data_with_anomalies <- results_anomalies[[level]]$data
  anomaly_indices <- results_anomalies[[level]]$anomaly_indices
  
  # Mark anomalies
  data_with_anomalies$anomaly <- ifelse(1:nrow(data_with_anomalies) %in% anomaly_indices, "Anomaly", "Normal")
  
  # Plot for Normal Data
  p_normal <- ggplot(data_with_anomalies[data_with_anomalies$anomaly == "Normal", ], 
                     aes(x = 1:nrow(data_with_anomalies[data_with_anomalies$anomaly == "Normal", ]), 
                         y = Global_active_power)) +
    geom_line(color = "blue", size = 1) +
    labs(title = paste("Normal Data -", level, "Anomalies"), x = "Time", y = "Global Active Power") +
    theme_minimal()
  
  # Plot for Anomalous Data
  p_anomalous <- ggplot(data_with_anomalies[data_with_anomalies$anomaly == "Anomaly", ], 
                        aes(x = 1:nrow(data_with_anomalies[data_with_anomalies$anomaly == "Anomaly", ]), 
                            y = Global_active_power)) +
    geom_line(color = "blue", size = 1) +  # Line for anomalies
    geom_point(color = "red", size = 3) +  # Highlight anomalies in red
    labs(title = paste("Anomalous Data -", level, "Anomalies"), x = "Time", y = "Global Active Power") +
    theme_minimal()
  
  # Arrange both plots side by side
  grid.arrange(p_normal, p_anomalous, ncol = 2)
}





# Collect the log-likelihood values for different anomaly levels
logLik_values <- sapply(results_anomalies, function(res) res$logLik)
normalized_logLik_values <- sapply(results_anomalies, function(res) res$normalized_logLik)

# Combine into data frames for plotting each metric separately
logLik_df <- data.frame(
  Anomaly_Level = rep(names(results_anomalies), each = 1),
  Metric = "Log-Likelihood",
  Value = logLik_values,
  Threshold = threshold_logLik
)

normalized_logLik_df <- data.frame(
  Anomaly_Level = rep(names(results_anomalies), each = 1),
  Metric = "Normalized Log-Likelihood",
  Value = normalized_logLik_values,
  Threshold = threshold_normalized
)

# Plot Log-Likelihood comparison with threshold line
logLik_plot <- ggplot(logLik_df, aes(x = Anomaly_Level, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  geom_hline(aes(yintercept = Threshold), linetype = "dashed", color = "black") + # Add threshold line
  labs(title = "Log-Likelihood Comparison Across Anomaly Levels",
       x = "Anomaly Level", y = "Log-Likelihood Value") +
  theme_minimal() +
  scale_color_manual(values = c("blue")) +
  theme(legend.title = element_blank())

# Plot Normalized Log-Likelihood comparison with threshold line
normalized_logLik_plot <- ggplot(normalized_logLik_df, aes(x = Anomaly_Level, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  geom_hline(aes(yintercept = Threshold), linetype = "dashed", color = "black") + # Add threshold line
  labs(title = "Normalized Log-Likelihood Comparison Across Anomaly Levels",
       x = "Anomaly Level", y = "Normalized Log-Likelihood Value") +
  theme_minimal() +
  scale_color_manual(values = c("red")) +
  theme(legend.title = element_blank())

# Display the plots
logLik_plot
normalized_logLik_plot
