library(dplyr)
library(readxl)
library(writexl)
library(lmtest)
library(sandwich)

# Function to Compute Hedonic Index and Growth Rates
compute_hedonic_index <- function(data, district_name, market_type) {
  data_subset <- data %>% filter(district == district_name) %>% arrange(YearMonth)
  
  start_month <- min(data_subset$YearMonth, na.rm = TRUE)
  month_sequence <- sort(unique(data_subset$YearMonth))
  
  if (length(month_sequence) < 2) {
    return(NULL)
  }
  
  index_values <- rep(NA, length(month_sequence))
  index_values[1] <- 100  # Set initial index value to 100
  
  for (i in 2:length(month_sequence)) {
    current_end_month <- month_sequence[i]
    current_start_month <- current_end_month - 11/12
    window_data <- data_subset %>%
      filter(YearMonth >= current_start_month & YearMonth <= current_end_month) %>%
      mutate(month_factor = factor(YearMonth, levels = unique(YearMonth)))
    
    if (nrow(window_data) < 5) {
      index_values[i] <- index_values[i - 1]
      next
    }
    
    model <- tryCatch({
      lm(log_price_m2 ~ furnished * condition_grouped + room_category + build_type_grouped + build_plan_grouped + month_factor, data = window_data)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(model)) {
      index_values[i] <- index_values[i - 1]
      next
    }
    
    model_coeffs <- coef(model)
    time_dummy_indices <- grep("^month_factor", names(model_coeffs))
    
    if (length(time_dummy_indices) < 2) {
      index_values[i] <- index_values[i - 1]
      next
    }
    
    current_time_dummies <- exp(model_coeffs[time_dummy_indices])
    
    if (any(is.na(current_time_dummies)) || length(current_time_dummies) < 2) {
      index_values[i] <- index_values[i - 1]
      next
    }
    
    last_month_index <- length(current_time_dummies)
    growth_rate <- current_time_dummies[last_month_index] / current_time_dummies[last_month_index - 1]
    index_values[i] <- index_values[i - 1] * growth_rate
  }
  
  result <- data.frame(YearMonth = month_sequence, index = index_values, growth_rate = c(NA, diff(index_values) / index_values[-length(index_values)] * 100))
  result$market_type <- market_type
  return(result)
}

# Load the data for both markets
secondary_data <- df_all_process %>% filter(home_type == "Вторичный рынок")
new_apartments <- df_all_process %>% filter(home_type == "Новостройки")

districts <- unique(df_all_process$district)

final_results <- list()

for (district_name in districts) {
  secondary_index <- compute_hedonic_index(secondary_data, district_name, "Secondary")
  new_index <- compute_hedonic_index(new_apartments, district_name, "New")
  
  if (!is.null(secondary_index) & !is.null(new_index)) {
    combined_index <- left_join(secondary_index, new_index, by = "YearMonth", suffix = c("_secondary", "_new"))
    combined_index <- combined_index %>%
      mutate(overall_index = (index_secondary + index_new) / 2,
             overall_growth = (growth_rate_secondary + growth_rate_new) / 2)
    final_results[[district_name]] <- combined_index
  }
}

final_results_df <- bind_rows(final_results, .id = "district")

# Keep only index levels and growth rates
final_results_df <- final_results_df %>% select(district, YearMonth, index_secondary, index_new, overall_index, growth_rate_secondary, growth_rate_new, overall_growth)

# Save final results
write_xlsx(final_results_df, "district_housing_index.xlsx")

print("Final Hedonic Housing Index Computed for All Districts Successfully!")
