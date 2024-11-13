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

# # Plotting PCA results using ggbiplot
# ggbiplot(pca_result, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE) +
#   ggtitle("PCA Biplot") +
#   theme_minimal()

# library(ggfortify)
# autoplot(pca_result, x = 2, y = 3, loadings = TRUE, loadings.label = TRUE) +
#   ggtitle("PCA Biplot (PC3 vs PC4)") +
#   theme_minimal()


#spliting the data into training data-set and testing data-set
train_data <- subset(scaled_df_with_info, Date <= "2009-01-31")
test_data <- subset(scaled_df_with_info, Date >= "2009-02-01")


#filtering the days and time
train_data_filtered <- subset(train_data, Weekday %in% c("Tuesday") & Time >= "09:00:00" & Time <= "14:00:00")
train_data_filtered_selected <- train_data_filtered[, c("Global_active_power", "Global_reactive_power")]

set.seed(457)
#applying HMM to different states
fit_hmm <- function(n_states) {
  tryCatch({
    # Build the model using the correct ntimes for the filtered data
    model <- depmix(response = list(Global_active_power ~ 1, 
                                    Global_reactive_power ~ 1), 
                    data = train_data_filtered_selected, 
                    nstates = n_states, 
                    family = list(gaussian(), gaussian()),
                    ntimes = nrow(train_data_filtered_selected))
    
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
state_counts <- c(4, 6, 8, 10)

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
