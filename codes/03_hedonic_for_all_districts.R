

# -- Existing code remains the same up to "Example Usage" --
# We assume the following functions are already in your environment:
# run_clean_model(), calculate_price_index(), combine_indices(), 
# calculate_combined_housing_index()

#############################################
# New Function: calculate_combined_housing_index_all_districts
#############################################
# This function:
# 1. Identifies all unique districts in your dataset.
# 2. For each district, calls calculate_combined_housing_index() 
#    on the relevant subset of secondary_data and new_apartments.
# 3. Keeps only (YearMonth, secondary_growth, new_growth, combined_growth),
#    adding a column 'district'.
# 4. Stacks all district results into a single data frame.
# 5. Pivots the data so each row is a district and columns represent each time period's growth rates.
calculate_combined_housing_index_all_districts <- function(df_secondary, df_new) {
  # Identify all districts that appear in both data frames 
  # (so we can actually compute a combined index for them).
  common_districts <- intersect(unique(df_secondary$district), unique(df_new$district))
  
  # A list to store the final results from each district
  all_results <- list()
  
  # Loop over each district and compute the combined index
  for (d in common_districts) {
    cat("\n------------------------\nProcessing district:", d, "\n")
    
    # Filter each dataset to the current district
    sec_district <- df_secondary %>% filter(district == d)
    new_district <- df_new %>% filter(district == d)
    
    # If there is insufficient data for either segment, skip
    if (nrow(sec_district) < 5 || nrow(new_district) < 5) {
      warning(paste("Skipping district", d, "- not enough observations."))
      next
    }
    
    # Calculate the combined index for this district
    final_result <- calculate_combined_housing_index(
      sec_district,
      new_district,
      district_name = d
    )
    
    # Keep only the columns of interest + add district name
    final_result <- final_result %>%
      select(YearMonth, secondary_growth, new_growth, combined_growth) %>%
      mutate(district = d)
    
    # Store in the list
    all_results[[d]] <- final_result
  }
  
  # Bind all district results into a single long data frame
  df_long <- bind_rows(all_results)
  
  # If you want each row = district, and each date is a set of columns, pivot wider:
  # (This will create columns like secondary_growth_2022-01, new_growth_2022-01, etc.)
  df_wide <- df_long %>%
    tidyr::pivot_wider(
      id_cols = "district",          # each row is a district
      names_from = "YearMonth",     # each column is a YearMonth
      values_from = c("secondary_growth", "new_growth", "combined_growth")
    )
  
  # Return both the long format (df_long) and the wide format (df_wide)
  list(
    long_format = df_long,
    wide_format = df_wide
  )
}

#############################################
# Example Usage
#############################################

# Suppose your master dataset is df_all_process
# Filter it into secondary and new segments as before:
secondary_data <- df_all_process %>% filter(home_type == "Вторичный рынок", district != "Tashkent")
new_apartments <- df_all_process %>% filter(home_type == "Новостройки", district != "Tashkent")

# Now call the new function to process ALL districts
all_district_results <- calculate_combined_housing_index_all_districts(secondary_data, new_apartments)

# You get a list with two elements:
# 1) all_district_results$long_format -> long data frame
# 2) all_district_results$wide_format -> wide data frame

# Inspect the wide version
View(all_district_results$wide_format)


wide_format <- all_district_results$wide_format

write_xlsx(wide_format, "housing_growth.xlsx")
# The wide_format table will have:
# - One row per district
# - A group of columns for each YearMonth, e.g.:
#   secondary_growth_2022-01, new_growth_2022-01, combined_growth_2022-01, ...
#   secondary_growth_2022-02, new_growth_2022-02, combined_growth_2022-02, etc.


observation_shares <- df_all_process %>%
  filter(district != "Tashkent") %>% 
  group_by(home_type, district) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  group_by(home_type) %>%
  mutate(total_obs = sum(n_obs),
         share = n_obs / total_obs) %>%
  ungroup()

write_xlsx(observation_shares, "district_shares.xlsx")