# Pre-processing Data-set
# Initial setup and loading data-set
df <- read.table("TermProjectData.txt", header = T, sep=",")

# Prints 1st 5 rows
head(df)

# Prints dimension of data-set -> how many rows & columns are there in the data-set
dim(df)

# Linear interpolation for each column, ignoring NA values
df$Global_active_power <- approx(x = which(!is.na(df$Global_active_power)), 
                                 y = df$Global_active_power[!is.na(df$Global_active_power)], 
                                 xout = 1:nrow(df), rule = 2)$y

df$Global_reactive_power <- approx(x = which(!is.na(df$Global_reactive_power)), 
                                   y = df$Global_reactive_power[!is.na(df$Global_reactive_power)], 
                                   xout = 1:nrow(df), rule = 2)$y

df$Voltage <- approx(x = which(!is.na(df$Voltage)), 
                     y = df$Voltage[!is.na(df$Voltage)], 
                     xout = 1:nrow(df), rule = 2)$y

df$Global_intensity <- approx(x = which(!is.na(df$Global_intensity)), 
                              y = df$Global_intensity[!is.na(df$Global_intensity)], 
                              xout = 1:nrow(df), rule = 2)$y

df$Sub_metering_1 <- approx(x = which(!is.na(df$Sub_metering_1)), 
                            y = df$Sub_metering_1[!is.na(df$Sub_metering_1)], 
                            xout = 1:nrow(df), rule = 2)$y

df$Sub_metering_2 <- approx(x = which(!is.na(df$Sub_metering_2)), 
                            y = df$Sub_metering_2[!is.na(df$Sub_metering_2)], 
                            xout = 1:nrow(df), rule = 2)$y

df$Sub_metering_3 <- approx(x = which(!is.na(df$Sub_metering_3)), 
                            y = df$Sub_metering_3[!is.na(df$Sub_metering_3)], 
                            xout = 1:nrow(df), rule = 2)$y
