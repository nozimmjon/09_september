library(dplyr)
library(readxl)
library(writexl)
library(lmtest)
library(sandwich)

# Function to Compute Hedonic Growth Rates for a Given Market Type
compute_hedonic_growth <- function(data, district_name, market_type) {
  data_pre <- data %>%
    mutate(
      condition_grouped = factor(case_when(
        condition %in% c("Авторский проект", "Евроремонт") ~ "Excellent",
        TRUE ~ "Average"
      )),
      build_type_grouped = factor(case_when(
        build_type == "Кирпичный" ~ "Brick",
        TRUE ~ "Modern"
      )),
      build_plan_grouped = factor(case_when(
        build_plan == "Студия" ~ "Studio",
        TRUE ~ "Regular"
      )),
      room_category = factor(case_when(
        num_rooms <= 2 ~ "Small",
        num_rooms %in% c(3, 4) ~ "Medium",
        num_rooms >= 5 ~ "Large"
      )),
      log_price_m2 = log(price_m2)
    )
  
  data_subset <- data_pre %>% filter(district == district_name) %>% arrange(YearMonth)
  
  # Ensure each categorical variable has at least 2 levels
  categorical_vars <- c("condition_grouped", "build_type_grouped", "build_plan_grouped", "room_category")
  for (var in categorical_vars) {
    if (length(unique(data_subset[[var]])) < 2) {
      data_subset[[var]] <- factor(data_subset[[var]], levels = c(levels(data_subset[[var]]), "Fallback"))
    }
  }
  
  start_month <- min(data_subset$YearMonth, na.rm = TRUE)
  month_sequence <- sort(unique(data_subset$YearMonth))
  
  if (length(month_sequence) < 2) {
    return(NULL)
  }
  
  index_values <- rep(NA, length(month_sequence))
  
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
    
    model <- lm(log_price_m2 ~ furnished * condition_grouped + room_category + build_type_grouped + build_plan_grouped + month_factor, data = window_data)
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
  
  result <- data.frame(YearMonth = month_sequence, growth_rate = c(NA, diff(index_values) / index_values[-length(index_values)] * 100))
  result$market_type <- market_type
  return(result)
}


# Load the data for both markets
secondary_data <- df_all_process %>% filter(home_type == "Вторичный рынок")
new_apartments <- df_all_process %>% filter(home_type == "Новостройки")

districts <- unique(df_all_process$district)

final_results <- list()

for (district_name in districts) {
  secondary_growth <- compute_hedonic_growth(secondary_data, district_name, "Secondary")
  new_growth <- compute_hedonic_growth(new_apartments, district_name, "New")
  
  if (!is.null(secondary_growth) & !is.null(new_growth)) {
    combined_growth <- full_join(secondary_growth, new_growth, by = "YearMonth", suffix = c("_secondary", "_new"))
    combined_growth <- combined_growth %>%
      mutate(overall_growth = rowMeans(select(., starts_with("growth_rate")), na.rm = TRUE))
    final_results[[district_name]] <- combined_growth
  }
}

final_results_df <- bind_rows(final_results, .id = "district")

# Save final results
write_xlsx(final_results_df, "district_housing_growth.xlsx")

print("Final Hedonic Housing Growth Computed for All Districts Successfully!")
