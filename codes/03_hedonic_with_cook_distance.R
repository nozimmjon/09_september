# Step 4: Run Regression for Initial Window with Outlier Detection -----------------------------
if (nrow(window_data) < 5) {
  cat("Not enough data points for the initial window.\n")
} else {
  # Fit initial model
  initial_model <- lm(log_price_m2 ~ 
                        furnished * condition_grouped +  # Interaction of Furnished × Condition
                        room_category  +  # Interaction of Room Category × Condition
                        build_type_grouped + build_plan_grouped + month_factor, 
                      data = window_data)
  
  # Compute Cook’s Distance
  cooks_d <- cooks.distance(initial_model)
  threshold <- 4 / nrow(window_data)  # Cook's Distance threshold
  window_data <- window_data[cooks_d < threshold, ]  # Remove influential points
  
  # Refit model on cleaned data
  model <- lm(log_price_m2 ~ 
                furnished * condition_grouped +  # Interaction of Furnished × Condition
                room_category  +  # Interaction of Room Category × Condition
                build_type_grouped + build_plan_grouped + month_factor, 
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
  model <- lm(log_price_m2 ~ 
                furnished * condition_grouped +  # Interaction of Furnished × Condition
                room_category  +  # Interaction of Room Category × Condition
                build_type_grouped + build_plan_grouped + month_factor, 
              data = window_data)
  
  # Compute Cook’s Distance for outlier detection
  cooks_d <- cooks.distance(model)
  threshold <- 4 / nrow(window_data)  # Cook’s Distance threshold
  window_data <- window_data[cooks_d < threshold, ]  # Remove influential observations
  
  # Refit model after removing outliers
  model <- lm(log_price_m2 ~ 
                furnished * condition_grouped +  # Interaction of Furnished × Condition
                room_category  +  # Interaction of Room Category × Condition
                build_type_grouped + build_plan_grouped + month_factor, 
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
  index = index_values)

result <- result %>%
  arrange(YearMonth) %>%
  mutate(
    monthly_growth = (index / lag(index) - 1) * 100  # Calculate month-over-month percentage change
  )

print("Final Rolling Index (Cook's Distance Applied):")
print(result)


write_xlsx(result, "housing_price_index_10.xlsx")