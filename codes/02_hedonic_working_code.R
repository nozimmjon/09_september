# Load required packages
library(dplyr)
library(tidyr)
library(writexl)

#############################################
# Helper Function: run_clean_model
#############################################
# This function fits a regression model, computes Cook’s distances,
# removes observations that exceed the threshold, and refits the model.
run_clean_model <- function(data, formula, outlier_threshold = 4 / nrow(data)) {
  initial_model <- lm(formula, data = data)
  cooks_d <- cooks.distance(initial_model)
  data_clean <- data[cooks_d < outlier_threshold, ]
  
  # Ensure that after outlier removal, there are enough observations
  if (nrow(data_clean) < 5) return(NULL)
  
  # Refit the model on the cleaned data
  lm(formula, data = data_clean)
}

#############################################
# Function: calculate_price_index
#############################################
# This function computes a quality-adjusted housing price index
# for a given market segment and district using a rolling-window
# hedonic regression method.
calculate_price_index <- function(data_processed, district_name, window_months = 12, min_obs = 5) {
  
  # Filter data for the specified district and sort chronologically
  data_subset <- data_processed %>%
    filter(district == district_name) %>%
    arrange(YearMonth)
  
  # Get the complete sequence of unique months
  month_sequence <- sort(unique(data_subset$YearMonth))
  
  # Define the initial window: starting at the earliest month
  start_month <- min(month_sequence)
  end_month <- start_month + (window_months - 1) / 12  # assuming YearMonth is numeric or Date
  
  cat(paste("Market:", unique(data_subset$home_type),
            "- Start Month:", start_month, "End Month:", end_month, "\n"))
  
  # Extract initial window data and create a time factor variable
  window_data <- data_subset %>%
    filter(YearMonth >= start_month & YearMonth <= end_month) %>%
    mutate(month_factor = factor(YearMonth, levels = unique(YearMonth)))
  
  if (nrow(window_data) < min_obs) {
    warning("Not enough data points for the initial window.")
    return(NULL)
  }
  
  # Define the hedonic regression formula
  reg_formula <- as.formula("log_price_m2 ~ furnished * condition_grouped +
                             room_category +
                             build_type_grouped + build_plan_grouped + month_factor")
  
  # Fit and clean the model using our helper function
  model <- run_clean_model(window_data, reg_formula)
  if (is.null(model)) {
    warning("Not enough observations after outlier removal in initial window.")
    return(NULL)
  }
  
  # Extract the coefficients for the time dummies.
  # Since the factor variable uses the first level as the base (omitted in the model),
  # we set that base to 1 manually.
  coeffs <- coef(model)
  time_dummy_indices <- grep("^month_factor", names(coeffs))
  # Exponentiate the time-dummy coefficients to get multiplicative contributions
  time_dummies <- exp(coeffs[time_dummy_indices])
  
  # Normalization: set the base (January or first month) to 1
  # then form a vector of all time dummy values: base=1 + the estimated values
  all_values <- c(1, time_dummies)
  base_year_avg <- mean(all_values, na.rm = TRUE)
  initial_index <- 100 * all_values / base_year_avg
  
  # Name the initial indices
  names(initial_index) <- c(paste0("month_factor", start_month), names(time_dummies))
  
  # Initialize the index vector for the complete month sequence
  index_values <- rep(NA, length(month_sequence))
  index_values[1:length(initial_index)] <- initial_index
  
  # Loop over subsequent months to update the index using a rolling window
  for (i in (length(initial_index) + 1):length(month_sequence)) {
    current_end_month <- month_sequence[i]
    current_start_month <- current_end_month - (window_months - 1) / 12
    
    window_data <- data_subset %>%
      filter(YearMonth >= current_start_month & YearMonth <= current_end_month) %>%
      mutate(month_factor = factor(YearMonth, levels = unique(YearMonth)))
    
    if (nrow(window_data) < min_obs) {
      warning(paste("Not enough data points for window ending:", current_end_month))
      index_values[i] <- index_values[i - 1]  # Carry forward the previous index value
      next
    }
    
    model <- run_clean_model(window_data, reg_formula)
    if (is.null(model)) {
      warning(paste("Model could not be estimated for window ending:", current_end_month))
      index_values[i] <- index_values[i - 1]
      next
    }
    
    model_coeffs <- coef(model)
    time_dummy_indices <- grep("^month_factor", names(model_coeffs))
    if (length(time_dummy_indices) < 2) {
      warning(paste("Not enough time dummies for window ending:", current_end_month))
      index_values[i] <- index_values[i - 1]
      next
    }
    
    current_time_dummies <- exp(model_coeffs[time_dummy_indices])
    if (any(is.na(current_time_dummies)) || length(current_time_dummies) < 2) {
      warning(paste("NA detected in time dummies for window ending:", current_end_month))
      index_values[i] <- index_values[i - 1]
      next
    }
    
    # Compute the growth rate using the last two time dummy values
    last_idx <- length(current_time_dummies)
    growth_rate <- current_time_dummies[last_idx] / current_time_dummies[last_idx - 1]
    
    # Chain the index forward
    index_values[i] <- index_values[i - 1] * growth_rate
  }
  
  # Create the resulting data frame with the index and monthly growth rates
  result <- data.frame(
    YearMonth = month_sequence,
    index = index_values,
    market_type = unique(data_subset$home_type)
  ) %>%
    arrange(YearMonth) %>%
    mutate(monthly_growth = (index / lag(index) - 1) * 100)
  
  return(result)
}

#############################################
# Function: combine_indices
#############################################
# This function merges the secondary and new market indices over the full date range,
# fills in any missing values using the last observation carried forward,
# and computes the combined index (equal weights) as well as growth rates.
combine_indices <- function(secondary_index, new_index) {
  all_months <- sort(unique(c(secondary_index$YearMonth, new_index$YearMonth)))
  
  secondary_complete <- secondary_index %>%
    complete(YearMonth = all_months) %>%
    arrange(YearMonth) %>%
    fill(index, .direction = "down")
  
  new_complete <- new_index %>%
    complete(YearMonth = all_months) %>%
    arrange(YearMonth) %>%
    fill(index, .direction = "down")
  
  combined_index <- data.frame(
    YearMonth = all_months,
    secondary_index = secondary_complete$index,
    new_index = new_complete$index
  ) %>%
    mutate(
      combined_index = (secondary_index + new_index) / 2,
      secondary_growth = (secondary_index / lag(secondary_index) - 1) * 100,
      new_growth = (new_index / lag(new_index) - 1) * 100,
      combined_growth = (combined_index / lag(combined_index) - 1) * 100
    )
  
  return(combined_index)
}

#############################################
# Main Function: calculate_combined_housing_index
#############################################
# This function coordinates the calculation of the indices for two market segments
# and then combines them into a final housing price index.
calculate_combined_housing_index <- function(secondary_data_pre, new_apartments_pre, district_name = "Chilonzor") {
  
  # Calculate indices for each market segment
  secondary_index <- calculate_price_index(secondary_data_pre, district_name)
  new_index <- calculate_price_index(new_apartments_pre, district_name)
  
  if (is.null(secondary_index) || is.null(new_index)) {
    stop("One of the indices could not be calculated due to insufficient data.")
  }
  
  # Combine the indices
  combined_index <- combine_indices(secondary_index, new_index)
  
  # Select and order the final columns
  final_result <- combined_index %>%
    select(YearMonth, secondary_index, secondary_growth, 
           new_index, new_growth, 
           combined_growth)
  
  cat("\nFinal Combined Housing Price Index:\n")
  print(final_result)
  
  # Save the result to an Excel file
  write_xlsx(final_result, "combined_housing_price_index.xlsx")
  
  return(final_result)
}

#############################################
# Example Usage
#############################################
# Assume df_all_process is a preprocessed data frame containing:
#   - YearMonth (numeric or Date)
#   - district
#   - home_type (with values such as "Вторичный рынок" for secondary and "Новостройки" for new apartments)
#   - log_price_m2, furnished, condition_grouped, room_category, build_type_grouped, build_plan_grouped, etc.

# Separate the data by market type
secondary_data <- df_all_process %>% filter(home_type == "Вторичный рынок")
new_apartments <- df_all_process %>% filter(home_type == "Новостройки")

# Calculate the combined housing price index for a specified district (e.g., "Chilonzor")
combined_index <- calculate_combined_housing_index(secondary_data, new_apartments, district_name = "Yakkasaroy")
