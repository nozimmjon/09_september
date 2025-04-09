library(dplyr)
library(tidyr)
library(writexl)

#############################################
# New Function: calculate_median_price_index
#############################################
# This function computes a median price index for a given district.
# It groups the data by YearMonth, computes the median of price_m2 and count (nobs),
# then normalizes the series so that the first available month is set to 100.
# Finally, it computes the monthly percentage growth.
calculate_median_price_index <- function(data, district_name) {
  # Filter data for the specified district and arrange by YearMonth
  data_subset <- data %>% 
    filter(district == district_name) %>% 
    arrange(YearMonth)
  
  # Group by YearMonth and compute median price and number of observations.
  # Replace "price_m2" with exp(log_price_m2) if price_m2 is not available.
  median_data <- data_subset %>%
    group_by(YearMonth) %>%
    summarise(
      median_price = median(price_m2, na.rm = TRUE),
      nobs = n(),
      .groups = "drop"
    ) %>%
    arrange(YearMonth)
  
  if(nrow(median_data) == 0) return(NULL)
  
  # Use the first available month as the base period.
  base_price <- median_data$median_price[1]
  
  # Compute the index: base period set to 100.
  median_data <- median_data %>%
    mutate(
      index = 100 * median_price / base_price,
      monthly_growth = (index / lag(index) - 1) * 100,
      district = district_name
    )
  
  return(median_data)
}

#############################################
# New Function: calculate_market_median_index_all_districts
#############################################
# This function loops over all unique districts in a market dataset (either secondary or new),
# calls calculate_median_price_index for each district, and returns a combined long-format data frame.
calculate_market_median_index_all_districts <- function(df_market) {
  districts <- unique(df_market$district)
  results <- list()
  
  for(d in districts) {
    cat("Processing district:", d, "\n")
    df_d <- df_market %>% filter(district == d)
    median_df <- calculate_median_price_index(df_d, district_name = d)
    if(is.null(median_df)) {
      warning(paste("Skipping district", d, "due to insufficient data."))
      next
    }
    results[[d]] <- median_df
  }
  
  combined <- bind_rows(results)
  return(combined)
}

#############################################
# New Function: aggregate_region_index_median
#############################################
# This function aggregates district-level median index data into a regional index.
# For each month, it computes a weighted average of the district indices using nobs as weights.
# Then it calculates the month-over-month growth rate of the regional index.
aggregate_region_index_median <- function(district_indices) {
  region <- district_indices %>%
    group_by(YearMonth) %>%
    summarise(
      total_nobs = sum(nobs, na.rm = TRUE),
      region_index = sum(index * nobs, na.rm = TRUE) / total_nobs,
      .groups = "drop"
    ) %>%
    arrange(YearMonth) %>%
    mutate(
      region_growth = (region_index / lag(region_index) - 1) * 100
    )
  return(region)
}

#############################################
# Example Usage for Median Price Index
#############################################
# Start from the filtered data:
secondary_data <- df_all_process %>% filter(home_type == "Вторичный рынок", district != "Tashkent")
new_apartments <- df_all_process %>% filter(home_type == "Новостройки", district != "Tashkent")

# Calculate district-level median indices for the secondary market:
sec_median_indices <- calculate_market_median_index_all_districts(secondary_data)
# Calculate district-level median indices for the new (primary) market:
new_median_indices <- calculate_market_median_index_all_districts(new_apartments)

# (Optional) Inspect the long data frames:
View(sec_median_indices)
View(new_median_indices)

# Aggregate across districts to get the Tashkent regional index using median prices:
region_secondary_median <- aggregate_region_index_median(sec_median_indices)
region_primary_median <- aggregate_region_index_median(new_median_indices)

# Optionally, export the regional indices to Excel:
write_xlsx(region_secondary_median, "Tashkent_Secondary_Region_Median_Index.xlsx")
write_xlsx(region_primary_median, "Tashkent_Primary_Region_Median_Index.xlsx")

# Print the aggregated regional indices:
print(region_secondary_median)
print(region_primary_median)



# Pivot the long data frame into wide format (each row is a district, each column is a YearMonth)
sec_median_wide <- sec_median_indices %>%
  select(district, YearMonth, median_price) %>%
  pivot_wider(
    id_cols = district,
    names_from = YearMonth,
    values_from = median_price,
    names_prefix = "median_"
  )

new_median_wide <- new_median_indices %>%
  select(district, YearMonth, median_price) %>%
  pivot_wider(
    id_cols = district,
    names_from = YearMonth,
    values_from = median_price,
    names_prefix = "median_"
  )


write_xlsx(sec_median_wide, "Secondary_Median_Price_Index_Wide.xlsx")
write_xlsx(new_median_wide, "Primary_Median_Price_Index_Wide.xlsx")
