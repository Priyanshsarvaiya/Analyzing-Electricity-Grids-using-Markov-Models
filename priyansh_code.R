# Load necessary libraries
library(ggplot2)
library(ggbiplot)




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

# Compute Principal Components
pca_result <- prcomp(scaled_df, center = TRUE, scale. = TRUE)

# Summary of PCA to show variance explained by each principal component
summary(pca_result)

# Plotting PCA results using ggbiplot
ggbiplot(pca_result, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE) +
  ggtitle("PCA Biplot") +
  theme_minimal()