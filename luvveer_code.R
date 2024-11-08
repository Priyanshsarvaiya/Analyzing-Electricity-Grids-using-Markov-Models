# Preprocessing (Code taken from Assignment 1)
# Initial setup and loading dataset

df <- read.table("TermProjectData.txt", header = T, sep=",")
# Prints 1st 5 rows
head(df)
# Prints dimension of dataset -> how many rows & columns are there in the 
# dataset
dim(df)

# Step 1: Apply linear interpolation for each column with NA values
df$Global_active_power <- approx(x = 1:nrow(df), y = df$Global_active_power, rule = 2)$y
df$Global_reactive_power <- approx(x = 1:nrow(df), y = df$Global_reactive_power, rule = 2)$y
df$Voltage <- approx(x = 1:nrow(df), y = df$Voltage, rule = 2)$y
df$Global_intensity <- approx(x = 1:nrow(df), y = df$Global_intensity, rule = 2)$y
df$Sub_metering_1 <- approx(x = 1:nrow(df), y = df$Sub_metering_1, rule = 2)$y
df$Sub_metering_2 <- approx(x = 1:nrow(df), y = df$Sub_metering_2, rule = 2)$y
df$Sub_metering_3 <- approx(x = 1:nrow(df), y = df$Sub_metering_3, rule = 2)$y

# Verify if NAs are handled
sum(is.na(df))  # Should return 0 if no missing values remain

"Part 2:- "
# Step 2: Remove non-numeric columns for scaling (Date and Time)
df_numeric <- df[, -c(1, 2)]  # Remove the first and second columns which are 'Date' and 'Time'

# Calculate Z-scores for detecting point anomalies on numeric columns
z_scores <- scale(df_numeric)  # Standardizes only the numeric columns
# Identify anomalies where |Z-score| > 3 for any feature
anomalies <- apply(abs(z_scores), 1, function(x) any(x > 3))

# Remove rows with anomalies
df_clean <- df[!anomalies, ]

# Check the dimensions after removing anomalies
dim(df_clean)

# Taken from Assignment 2
library(dplyr)
library(lubridate)

# Step 1: Ensure the dataset starts from Mis.naeonday and extract complete weeks (Monday-Sunday)
# Convert Date to Date object without timezone
df_clean$Date <- as.Date(df_clean$Date, format = "%d/%m/%Y")  

# Ensure Time is character (no timezone specification)
df_clean$Time <- as.character(df_clean$Time)

# Combine Date and Time into datetime without timezone
df_clean$datetime <- as.POSIXct(paste(df_clean$Date, df_clean$Time), format="%Y-%m-%d %H:%M:%S", tz="")

# Adjust for daylight savings time
df_clean$datetime <- with_tz(df_clean$datetime, tzone = "Canada/Pacific")


# Dropping week where timezone changes from PST to PDT (2007-03-11 02:00:00 to 02:59:00)
df_clean <- df_clean %>%
  filter(!is.na(datetime))

na_rows <- df_clean[is.na(df_clean$datetime), ]
print(na_rows)

# Extract the weekday names
df_clean$Weekday <- factor(format(df_clean$datetime, "%A"), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Create week index
df_clean <- df_clean %>%
  mutate(week_start = floor_date(datetime, "week", week_start = 1),  # Start week on Monday
         week_index = as.integer(as.factor(week_start)))  # Create a week index






#Assignment 3 starting here
library(depmixS4)
library(dplyr)

# Filter for the specific time period (9 AM to 12 PM) for all Mondays of the week
df_clean_filtered <- df_clean %>%
  filter(hour(datetime) >= 9 & hour(datetime) < 12) %>%    # Select 9 AM to 12 PM
  filter(Weekday %in% c("Monday"))  %>%     # only Mondays
  filter(week_index >= 1 & week_index <= 52)

# Group the data by 'week_index' and collect the 'Global_active_power'
week_data <- df_clean_filtered %>%
  group_by(week_index) %>%
  summarise(Global_active_power = list(Global_active_power))

# Unlist the filtered Global_active_power from all weeks into a single vector for modeling
power_data_filtered <- unlist(week_data$Global_active_power)

# Get the number of observations per week for the filtered data
ntimes_vec_filtered <- sapply(week_data$Global_active_power, length)

# Vector of different number of states to try
nstates_vec <- c(4, 6, 8, 10, 12, 14, 16)

# Function to fit HMM for a specific number of states and print log-likelihood & BIC
fit_hmm <- function(n_states) {
  tryCatch({
    #seeding to get same values of Log-lik and BIC every-time to compare 
    set.seed(457)
    
    # Build the model using the correct ntimes for the filtered data
    model <- depmix(response = power_data_filtered ~ 1, data = df_clean_filtered, nstates = n_states, ntimes = ntimes_vec_filtered)
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

# Use lapply to fit HMMs for different numbers of states
hmm_results_filtered <- lapply(nstates_vec, fit_hmm)

# Extract log-likelihoods and BIC values from the results
log_likelihoods_filtered <- sapply(hmm_results_filtered, function(res) if (!is.null(res)) res$logLik else NA)
bic_values_filtered <- sapply(hmm_results_filtered, function(res) if (!is.null(res)) res$BIC else NA)

valid_indices <- !is.na(log_likelihoods_filtered) & !is.na(bic_values_filtered)
log_likelihoods_filtered <- log_likelihoods_filtered[valid_indices]
bic_values_filtered <- bic_values_filtered[valid_indices]
nstates_vec_filtered <- nstates_vec[valid_indices]  # Ensure this matches the valid indices


# Plot Log-Likelihood
plot(nstates_vec_filtered, log_likelihoods_filtered, type="b", col="blue", xlab="Number of States", ylab="Log-Likelihood", main="Log-Likelihood vs Number of States (continuous Data)")

# Plot BIC
plot(nstates_vec_filtered, bic_values_filtered, type="b", col="red", xlab="Number of States", ylab="BIC", main="BIC vs Number of States (continuous Data)")

#plotting both graph in one to compare
# Plot Log-Likelihood with custom y-axis limits
plot(nstates_vec_filtered, log_likelihoods_filtered, type="b", col="blue",
     xlab="Number of States", ylab="Values",
     main="Log-Likelihood and BIC vs Number of States (Continuous Data)",
     ylim=c(-95000, 95000))  # Set y-axis limits

# Add BIC values to the same graph
lines(nstates_vec_filtered, bic_values_filtered, type="b", col="red", lty=2)

# Add a legend to differentiate between Log-Likelihood and BIC
legend("topright", legend=c("Log-Likelihood", "BIC"), col=c("blue", "red"), lty=c(1, 2), lwd=2)



#Assignmnet 3 Part-2
# adding a column to the df_clean_filtered set as Global_active_power_des
#after converting the continuous values into discrete values.
df_clean_filtered <- df_clean_filtered %>%
  mutate(global_active_power_des = round(df_clean_filtered$Global_active_power * 2) /2)

#checking new column for na values
sum(is.na(df_clean_filtered$global_active_power_des))

# Group the data by 'week_index' and collect the 'Global_active_power'
week_data_des <- df_clean_filtered %>%
  group_by(week_index) %>%
  summarise(Global_active_power_des = list(global_active_power_des))

# Unlist the filtered Global_active_power from all weeks into a single vector for modeling
power_data_filtered_des <- as.factor(unlist(week_data_des$Global_active_power_des))

# Get the number of observations per week for the filtered data
ntimes_vec_filtered_des <- sapply(week_data_des$Global_active_power_des, length)
nstates_vec_des <- c(4, 6, 8, 10, 12, 14, 16)

fit_hmm_des <- function(n_states) {
  tryCatch({
    
    #seeding to get same values of Log-lik and BIC every-time to compare 
    set.seed(457)
    
    # Build the model using the correct ntimes for the filtered data
    model_des <- depmix(response = power_data_filtered_des ~ 1, data = df_clean_filtered, nstates = n_states, family = multinomial(), ntimes = ntimes_vec_filtered_des)
    fit_model_des <- fit(model_des)
    
    # Get log-likelihood and BIC
    logLik_value_des <- logLik(fit_model_des)
    BIC_value_des <- BIC(fit_model_des)
    
    # Print log-likelihood and BIC for this number of states
    cat("Number of states:", n_states, "\n")
    cat("Log-Likelihood Descrete:", logLik_value_des, "\n")
    cat("BIC Descrete:", BIC_value_des, "\n\n")
    
    return(list(logLik = logLik_value_des, BIC = BIC_value_des, model = fit_model_des))
    
  }, error = function(e) {
    message(paste("Error with", n_states, "states: ", e))
    return(NULL)
  })
}

# Use lapply to fit HMMs for different numbers of states
hmm_results_filtered_des <- lapply(nstates_vec_des, fit_hmm_des)

# Extract log-likelihoods and BIC values from the results
log_likelihoods_filtered_des <- sapply(hmm_results_filtered_des, function(res) if (!is.null(res)) res$logLik else NA)
bic_values_filtered_des <- sapply(hmm_results_filtered_des, function(res) if (!is.null(res)) res$BIC else NA)

valid_indices <- !is.na(log_likelihoods_filtered_des) & !is.na(bic_values_filtered_des)
if(any(valid_indices))
{
  log_likelihoods_filtered_des <- log_likelihoods_filtered_des[valid_indices]
  bic_values_filtered_des <- bic_values_filtered_des[valid_indices]
  nstates_vec_filtered_des <- nstates_vec[valid_indices]  # Ensure this matches the valid indices
  
  
  # Plot Log-Likelihood
  plot(nstates_vec_filtered_des, log_likelihoods_filtered_des, type="b", col="blue", xlab="Number of States", ylab="Log-Likelihood", main="Log-Likelihood vs Number of States (Discrete Data)")
  
  # Plot BIC
  plot(nstates_vec_filtered_des, bic_values_filtered_des, type="b", col="red", xlab="Number of States", ylab="BIC", main="BIC vs Number of States (Discrete Data)")
} else{
  cat("No valid results for plotting")
}


#plotting both graph in one to compare
# Plot Log-Likelihood with custom y-axis limits
plot(nstates_vec_filtered_des, log_likelihoods_filtered_des, type="b", col="blue",
     xlab="Number of States", ylab="Values",
     main="Log-Likelihood and BIC vs Number of States (Discrete Data)",
     ylim=c(-95000, 95000))  # Set y-axis limits

# Add BIC values to the same graph
lines(nstates_vec_filtered_des, bic_values_filtered_des, type="b", col="red", lty=2)

# Add a legend to differentiate between Log-Likelihood and BIC
legend("topright", legend=c("Log-Likelihood", "BIC"), col=c("blue", "red"), lty=c(1, 2), lwd=2)


