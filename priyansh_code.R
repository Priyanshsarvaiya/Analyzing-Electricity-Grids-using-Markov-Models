# Load necessary libraries
library(ggplot2)
library(ggbiplot)
library(dplyr)
library(data.table)
library(lubridate)
library(depmixS4)


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


# Selecting only numeric columns for PCA
df_numeric <- df[, -c(1, 2)]

# Feature Scaling: Standardization (z-score normalization)
scaled_df <- scale(df_numeric)

#spliting weekday name 
df$Date <- dmy(df$Date)
df$Weekday <- factor(format(df$Date, "%A"), levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))


#adding Date, time, Weekday column to scaled data
datetime_info <- df[, c("Date", "Time", "Weekday")]
scaled_df_with_info <- cbind(datetime_info, scaled_df)


# Verify that the mean of each column is approximately 0
mean_check <- colMeans(scaled_df)
cat("Means of each column (should be close to 0):\n")
print(mean_check)

# Verify that the standard deviation of each column is approximately 1
sd_check <- apply(scaled_df, 2, sd)
cat("\nStandard deviations of each column (should be close to 1):\n")
print(sd_check)

# Compute Principal Components
pca_result <- prcomp(scaled_df, center = TRUE, scale. = TRUE, retx = TRUE)

# Summary of PCA to show variance explained by each principal component
summary(pca_result)

# Perform PCA (Assuming 'scaled_df' is your scaled dataset)
pca_result <- prcomp(scaled_df, center = TRUE, scale. = TRUE, retx = TRUE)

# Extract loading scores
loading_scores <- pca_result$rotation

# Function to rank variables based on cumulative loadings for given PCs
rank_variables <- function(loading_scores, pcs) {
  # Ensure loading_scores subset is a matrix
  abs_loadings_combined <- rowSums(abs(loading_scores[, pcs, drop = FALSE]))
  
  # Rank variables by their cumulative contributions
  ranked_variables <- sort(abs_loadings_combined, decreasing = TRUE)
  return(ranked_variables)
}

# PC1 only
ranked_pc1 <- rank_variables(loading_scores, pcs = 1)

# PC1 + PC2
ranked_pc1_pc2 <- rank_variables(loading_scores, pcs = 1:2)

# PC1 + PC2 + PC3
ranked_pc1_pc2_pc3 <- rank_variables(loading_scores, pcs = 1:3)

# PC1 + PC2 + PC3 + PC4
ranked_pc1_pc2_pc3_pc4 <- rank_variables(loading_scores, pcs = 1:4)

# Display results
print("Ranked Variables (PC1 only):")
print(ranked_pc1)

print("Ranked Variables (PC1 + PC2):")
print(ranked_pc1_pc2)

print("Ranked Variables (PC1 + PC2 + PC3):")
print(ranked_pc1_pc2_pc3)

print("Ranked Variables (PC1 + PC2 + PC3 + PC4):")
print(ranked_pc1_pc2_pc3_pc4)

# To select top variables for each subset (e.g., top 3 variables)
n <- 3  # Adjust to choose top variables
top_pc1 <- names(ranked_pc1[1:n])
top_pc1_pc2 <- names(ranked_pc1_pc2[1:n])
top_pc1_pc2_pc3 <- names(ranked_pc1_pc2_pc3[1:n])
top_pc1_pc2_pc3_pc4 <- names(ranked_pc1_pc2_pc3_pc4[1:n])

print(paste("Top", n, "Variables for PC1:"))
print(top_pc1)

print(paste("Top", n, "Variables for PC1 + PC2:"))
print(top_pc1_pc2)

print(paste("Top", n, "Variables for PC1 + PC2 + PC3:"))
print(top_pc1_pc2_pc3)

print(paste("Top", n, "Variables for PC1 + PC2 + PC3 + PC4:"))
print(top_pc1_pc2_pc3_pc4)

#spliting the data into training data-set and testing data-set
train_data <- subset(scaled_df_with_info, Date <= "2009-01-31")
test_data <- subset(scaled_df_with_info, Date >= "2009-02-01")


#filtering the days and time
train_data_filtered <- subset(train_data, Weekday %in% c("Monday") & Time >= "09:00:00" & Time <= "12:00:00")
train_data_filtered_selected <- train_data_filtered[, c("Sub_metering_2", "Global_reactive_power", "Sub_metering_1")]
train_data_filtered_selected_scaled <- scale(train_data_filtered_selected)

# Assuming train_data_filtered is already defined and contains the 'Date' column

# Count unique days
unique_days_count <- length(unique(train_data_filtered$Date))

# Print the number of unique days
cat("Number of unique days in the dataset:", unique_days_count, "\n")

# 
# set.seed(100)
# #applying HMM to different states
# fit_hmm <- function(n_states) {
#   tryCatch({
#     # Build the model using the correct ntimes for the filtered data
#     model <- depmix(response = list(Sub_metering_2 ~ 1,
#                                     Global_reactive_power ~ 1, Sub_metering_1 ~ 1),
#                     data = train_data_filtered_selected_scaled,
#                     nstates = n_states,
#                     family = list(gaussian(), gaussian(), gaussian()),
#                     ntimes = nrow(train_data_filtered_selected_scaled))
# 
#     # Fit the model
#     fit_model <- fit(model)
# 
#     # Get log-likelihood and BIC
#     logLik_value <- logLik(fit_model)
#     BIC_value <- BIC(fit_model)
# 
#     # Print log-likelihood and BIC for this number of states
#     cat("Number of states:", n_states, "\n")
#     cat("Log-Likelihood:", logLik_value, "\n")
#     cat("BIC:", BIC_value, "\n\n")
# 
#     return(list(logLik = logLik_value, BIC = BIC_value, model = fit_model))
# 
#   }, error = function(e) {
#     message(paste("Error with", n_states, "states: ", e))
#     return(NULL)
#   })
# }
# 
# # Define the specific state counts to test
# state_counts <- c(4, 6, 8, 10)
# 
# # Use lapply to apply the fit_hmm function for each specified state count and store the results
# model_results <- lapply(state_counts, fit_hmm)
# 
# # Filter out any NULL results (i.e., errors in fitting)
# model_results <- Filter(Negate(is.null), model_results)
# 
# # Convert the results into a data frame
# model_results_df <- do.call(rbind, lapply(seq_along(model_results), function(i) {
#   data.frame(num_states = state_counts[i],
#              logLik = model_results[[i]]$logLik,
#              BIC = model_results[[i]]$BIC)
# }))
