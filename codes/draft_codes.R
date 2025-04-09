
# # Furnished × Condition Interaction Effect
# ggplot(window_data, aes(x = condition_grouped, y = log_price_m2, fill = furnished)) +
#   geom_boxplot() +
#   labs(title = "Interaction Effect: Furnished × Condition",
#        x = "Condition Grouped", y = "Log Price per m²") +
#   theme_minimal() +
#   scale_fill_manual(values = c("blue", "red"))  # Assign colors to distinguish categories
# 
# 
# ggplot(window_data, aes(x = condition_grouped, y = log_price_m2, fill = factor(room_category))) +
#   geom_boxplot() +
#   labs(title = "Interaction Effect: Room Category × Condition",
#        x = "Condition Grouped", y = "Log Price per m²") +
#   theme_minimal() +
#   scale_fill_manual(name = "Room Category", values = c("green", "orange", "purple"))  # Set manual colors


# # Define the weights for each amenity
# weights <- c(
#   school = 0.20,
#   kindergarten = 0.20,
#   hospital = 0.20,
#   park = 0.20,
#   supermarket = 0.20
# )
# 
# # Convert logical variables (TRUE/FALSE) to numeric (1/0) and compute weighted proximity
# cleaned_data <- cleaned_data %>%
#   mutate(
#     weighted_proximity = 
#       as.numeric(school) * weights["school"] +
#       as.numeric(kindergarten) * weights["kindergarten"] +
#       as.numeric(hospital) * weights["hospital"] +
#       as.numeric(park) * weights["park"] +
#       as.numeric(supermarket) * weights["supermarket"]
#   )
# 
# # Categorizing proximity levels into Low, Medium, and High
# cleaned_data <- cleaned_data %>%
#   mutate(proximity_category = case_when(
#     weighted_proximity <= 0.2 ~ "Low",
#     weighted_proximity > 0.2 & weighted_proximity <= 0.6 ~ "Medium",
#     weighted_proximity > 0.6 ~ "High"
#   ))




# not using Cook's distance

# Step 4: Run Regression for Initial Window with Outlier Detection -----------------------------
if (nrow(window_data) < 5) {
  cat("Not enough data points for the initial window.\n")
} else {
  # Fit initial model
  initial_model <- lm(log_price_m2 ~ furnished + condition_grouped + room_category
                      + build_type_grouped + build_plan_grouped + 
                        + month_factor, 
                      data = window_data)
  
  # Compute Cook’s Distance
  cooks_d <- cooks.distance(initial_model)
  threshold <- 4 / nrow(window_data)  # Cook's Distance threshold
  window_data <- window_data[cooks_d < threshold, ]  # Remove influential points
  
  # Refit model on cleaned data
  model <- lm(log_price_m2 ~ furnished + condition_grouped + room_category
              + build_type_grouped + build_plan_grouped + 
                + month_factor, 
              data = window_data)
  
  coeffs <- coef(model)
  time_dummies <- exp(coeffs[grep("^month_factor", names(coeffs))])  # Extract month dummies
  
  # Since January is the base, its coefficient is implicitly 1 (exp(0) = 1)
  jan_value <- 1  
  all_values <- c(jan_value, time_dummies)
  
  # Compute the base year average including January
  base_year_avg <- mean(all_values, na.rm = TRUE)
  
  # Normalize to January = 100
  initial_index <- 100 * all_values / base_year_avg
  
  # Assign month names (January is missing in time_dummies, so we manually add it)
  names(initial_index) <- c("month_factorянв 2022", names(time_dummies))
  
  print("Initial Index Values (Outliers Removed & January Included):")
  print(initial_index)
}

# Step 5: Create Rolling YearMonth Sequence -----------------------------
max_month <- max(data_subset$YearMonth, na.rm = TRUE)
month_sequence <- sort(unique(data_subset$YearMonth))

print("Month Sequence:")
print(month_sequence)

index_values <- rep(NA, length(month_sequence))  # Preallocate with NA
index_values[1:length(initial_index)] <- initial_index  # Store initial index values

# Rolling window calculation with Cook's Distance filtering
for (i in (length(initial_index) + 1):length(month_sequence)) {  
  current_end_month <- month_sequence[i]
  current_start_month <- current_end_month - 11/12  # Define rolling 12-month window
  
  window_data <- data_subset %>%
    filter(YearMonth >= current_start_month & YearMonth <= current_end_month) %>%
    mutate(month_factor = factor(YearMonth, levels = unique(YearMonth)))
  
  if (nrow(window_data) < 5) {
    cat(paste("Not enough data points for window ending:", current_end_month, "\n"))
    index_values[i] <- index_values[i - 1]  # Carry forward last index value
    next
  }
  
  # Fit the model on the rolling window
  model <- lm(log_price_m2 ~ furnished + condition_grouped + room_category
              + build_type_grouped + build_plan_grouped + 
                + month_factor, 
              data = window_data)
  
  # Compute Cook’s Distance for outlier detection
  cooks_d <- cooks.distance(model)
  threshold <- 4 / nrow(window_data)  # Cook’s Distance threshold
  window_data <- window_data[cooks_d < threshold, ]  # Remove influential observations
  
  # Refit model after removing outliers
  model <- lm(log_price_m2 ~ furnished + condition_grouped + room_category
              + build_type_grouped + build_plan_grouped + 
                + month_factor, 
              data = window_data)
  
  model_coeffs <- coef(model)
  time_dummy_indices <- grep("^month_factor", names(model_coeffs))
  
  if (length(time_dummy_indices) < 2) {
    cat(paste("Not enough time dummies for window ending:", current_end_month, "\n"))
    index_values[i] <- index_values[i - 1]  # Carry forward last index value
    next
  }
  
  current_time_dummies <- exp(model_coeffs[time_dummy_indices])
  
  if (any(is.na(current_time_dummies)) || length(current_time_dummies) < 2) {
    cat(paste("NA detected in time dummies for window ending:", current_end_month, "\n"))
    index_values[i] <- index_values[i - 1]  # Carry forward last index value
    next
  }
  
  # Compute growth rate from the last two dummy values in the current window
  last_month_index <- length(current_time_dummies)
  growth_rate <- current_time_dummies[last_month_index] / current_time_dummies[last_month_index - 1]
  
  # Chain forward the index value
  index_values[i] <- index_values[i - 1] * growth_rate
}



# Step 7: Review the Resulting Index ------------------------------------
result <- data.frame(
  YearMonth = month_sequence,
  index = index_values, 
  
)

result <- result %>%
  arrange(YearMonth) %>%
  mutate(
    monthly_growth = (index / lag(index) - 1) * 100  # Calculate month-over-month percentage change
  )

print("Final Rolling Index (Cook's Distance Applied):")
print(result)


write_xlsx(full_index_df, "housing_price_index_9.xlsx")





# drafts with smoothing effect

# Step 4: Run Regression for Initial Window with Outlier Detection -----------------------------
if (nrow(window_data) < 5) {
  cat("Not enough data points for the initial window.\n")
} else {
  # Fit initial model
  initial_model <- lm(log_price_m2 ~ furnished + condition_grouped + room_category
                      + build_type_grouped + build_plan_grouped + 
                        + month_factor, 
                      data = window_data)
  
  # Compute Cook’s Distance
  cooks_d <- cooks.distance(initial_model)
  threshold <- 4 / nrow(window_data)  # Cook's Distance threshold
  window_data <- window_data[cooks_d < threshold, ]  # Remove influential points
  
  # Refit model on cleaned data
  model <- lm(log_price_m2 ~ furnished + condition_grouped + room_category
              + build_type_grouped + build_plan_grouped + 
                + month_factor, 
              data = window_data)
  
  coeffs <- coef(model)
  time_dummies <- exp(coeffs[grep("^month_factor", names(coeffs))])  # Extract month dummies
  
  # Since January is the base, its coefficient is implicitly 1 (exp(0) = 1)
  jan_value <- 1  
  all_values <- c(jan_value, time_dummies)
  
  # Compute the base year average including January
  base_year_avg <- mean(all_values, na.rm = TRUE)
  
  # Normalize to January = 100
  initial_index <- 100 * all_values / base_year_avg
  
  # Assign month names (January is missing in time_dummies, so we manually add it)
  names(initial_index) <- c("month_factorянв 2022", names(time_dummies))
  
  print("Initial Index Values (Outliers Removed & January Included):")
  print(initial_index)
}

# Step 5: Create Rolling YearMonth Sequence -----------------------------
max_month <- max(data_subset$YearMonth, na.rm = TRUE)
month_sequence <- sort(unique(data_subset$YearMonth))

print("Month Sequence:")
print(month_sequence)

index_values <- rep(NA, length(month_sequence))  # Preallocate with NA
index_values[1:length(initial_index)] <- initial_index  # Store initial index values

# Rolling window calculation with Cook's Distance filtering
for (i in (length(initial_index) + 1):length(month_sequence)) {  
  current_end_month <- month_sequence[i]
  current_start_month <- current_end_month - 11/12  # Define rolling 12-month window
  
  window_data <- data_subset %>%
    filter(YearMonth >= current_start_month & YearMonth <= current_end_month) %>%
    mutate(month_factor = factor(YearMonth, levels = unique(YearMonth)))
  
  if (nrow(window_data) < 5) {
    cat(paste("Not enough data points for window ending:", current_end_month, "\n"))
    index_values[i] <- index_values[i - 1]  # Carry forward last index value
    next
  }
  
  # Fit the model on the rolling window
  model <- lm(log_price_m2 ~ furnished + condition_grouped + room_category
              + build_type_grouped + build_plan_grouped + 
                + month_factor, 
              data = window_data)
  
  # Compute Cook’s Distance for outlier detection
  cooks_d <- cooks.distance(model)
  threshold <- 4 / nrow(window_data)  # Cook’s Distance threshold
  window_data <- window_data[cooks_d < threshold, ]  # Remove influential observations
  
  # Refit model after removing outliers
  model <- lm(log_price_m2 ~ furnished + condition_grouped + room_category
              + build_type_grouped + build_plan_grouped + 
                + month_factor, 
              data = window_data)
  
  model_coeffs <- coef(model)
  time_dummy_indices <- grep("^month_factor", names(model_coeffs))
  
  if (length(time_dummy_indices) < 2) {
    cat(paste("Not enough time dummies for window ending:", current_end_month, "\n"))
    index_values[i] <- index_values[i - 1]  # Carry forward last index value
    next
  }
  
  current_time_dummies <- exp(model_coeffs[time_dummy_indices])
  
  if (any(is.na(current_time_dummies)) || length(current_time_dummies) < 2) {
    cat(paste("NA detected in time dummies for window ending:", current_end_month, "\n"))
    index_values[i] <- index_values[i - 1]  # Carry forward last index value
    next
  }
  
  # Compute growth rate from the last two dummy values in the current window
  last_month_index <- length(current_time_dummies)
  growth_rate <- current_time_dummies[last_month_index] / current_time_dummies[last_month_index - 1]
  
  # Chain forward the index value
  index_values[i] <- index_values[i - 1] * growth_rate
}



# Step 1: Create a complete time sequence from Jan 2022 to the last available month
full_month_sequence <- seq(from = as.yearmon("2022-01"), to = max(month_sequence, na.rm = TRUE), by = 1/12)

# Step 2: Merge with index_values, ensuring missing months are filled with NA
index_df <- data.frame(YearMonth = month_sequence, index = index_values)
full_index_df <- merge(data.frame(YearMonth = full_month_sequence), index_df, by = "YearMonth", all.x = TRUE)

# Step 3: Interpolate missing months (May 2022 and any other gaps)
full_index_df$index <- na.approx(full_index_df$index, na.rm = FALSE)  # Keeps leading NAs

# Step 4: Convert to time series (ts) object, ensuring frequency = 12 for monthly data
index_ts <- ts(full_index_df$index, start = c(2022, 1), frequency = 12)

# Step 5: Apply Holt-Winters Smoothing
index_values_smooth <- HoltWinters(index_ts, alpha = 0.3, beta = 0.3, gamma = FALSE)  # gamma = FALSE for trend smoothing

# Step 6: Extract smoothed values
index_smooth <- as.numeric(index_values_smooth$fitted[,1])  # Extract level component

# Step 1: Verify lengths before assignment
print(length(full_index_df$index))      # Check the length of original index values
print(length(index_smooth))


index_smooth <- as.numeric(index_values_smooth$fitted[, "xhat"])  # Extract smoothed index values

# Step 2: Identify the starting month of smoothed values
start_smooth_month <- as.yearmon("2022-03")  # First available month from Holt-Winters output

# Step 3: Initialize smoothed index column in full_index_df
full_index_df$index_smooth <- NA  

# Step 4: Assign smoothed values only from March 2022 onwards
matching_indices <- which(full_index_df$YearMonth >= start_smooth_month)  # Find where smoothing starts
full_index_df$index_smooth[matching_indices] <- index_smooth  # Assign smoothed values


# Step 6: Plot Original vs. Smoothed Index
ggplot(full_index_df, aes(x = YearMonth)) +
  geom_line(aes(y = index, color = "Original Index"), size = 1) +
  geom_line(aes(y = index_smooth, color = "Smoothed Index"), size = 1, linetype = "dashed") +
  labs(title = "Comparison of Original vs. Smoothed Index",
       x = "Year-Month",
       y = "Index Value") +
  scale_color_manual(name = "Index Type", values = c("Original Index" = "blue", "Smoothed Index" = "red")) +
  theme_minimal()

# Step 7: Review the Resulting Index ------------------------------------
result <- data.frame(
  YearMonth = month_sequence,
  index = index_values, 
  
)

result <- result %>%
  arrange(YearMonth) %>%
  mutate(
    monthly_growth = (index / lag(index) - 1) * 100  # Calculate month-over-month percentage change
  )

print("Final Rolling Index (Cook's Distance Applied):")
print(result)


write_xlsx(full_index_df, "housing_price_index_9.xlsx")