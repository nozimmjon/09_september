

secondary_data <- df_all_process %>% 
  filter(home_type == "Вторичный рынок") %>% 
  group_by(YearMonth, district, price_m2, num_rooms, area, apart_floor, 
           home_floor, home_type, build_year, build_type) %>%
  slice(1L) %>% 
  ungroup()
  
# secondary_data <-  secondary_data %>% filter(price_m2 < 4500)
 
# view(dfSummary(secondary_data))


# secondary_data %>%
#   ggplot(aes(x = price_m2),
#          y = 2)+
#   geom_boxplot(fill = "lightblue", alpha = 0.7) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# secondary_data %>%
#   ggplot(aes(y = price_m2,
#          x = fct_reorder(build_year, price_m2, .fun = mean, na.rm = TRUE)))+
#   geom_boxplot(fill = "lightblue", alpha = 0.7) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


secondary_data_pre <- secondary_data %>%
  mutate(condition_grouped = case_when(
    condition %in% c("Авторский проект", "Евроремонт") ~ "Excellent",
    TRUE ~ "average"
  )) %>% 
  mutate(build_type_grouped = case_when(
    build_type == "Кирпичный" ~ "Brick",
    TRUE ~ "Modern"
  )) %>% 
  mutate(build_plan_grouped = case_when(
    build_plan == "Студия" ~ "Studio",
    TRUE ~ "Regular"
  )) %>% 
  mutate(room_category = case_when(
    num_rooms <= 2 ~ "Small",
    num_rooms %in% c(3, 4) ~ "Medium",
    num_rooms >= 5 ~ "Large"
  )) %>% 
  # Convert build_year to a single numeric value
  mutate(build_year_decade = case_when( 
    build_year == "1960 - 1979" ~ "upto10",
    build_year == "1980 - 1989" ~ "upto10",
    build_year == "1990 - 2000" ~ "upto10",
    build_year == "старше 1960" ~ "upto10", 
    build_year == "2001 - 2010" ~ "10x",
    build_year == "2011 - 2014" ~ "10x",
    build_year == "2015" ~ "10x",
    build_year == "2016" ~ "10x",
    build_year == "2017" ~ "new",
    build_year == "2018" ~ "new",
    build_year == "2019" ~ "new",
    build_year == "2020" ~ "new",
    build_year == "2021" ~ "new",
    build_year == "2022" ~ "new",
    build_year == "2023" ~ "new",
    build_year == "Сдача в 2017" ~ "new",
    build_year == "Сдача в 2018" ~ "new",
    is.na(build_year) ~ "upto10",
    TRUE ~ NA_character_  # Keep NA for now
  )) %>% 
  mutate(
    condition_grouped = as.factor(condition_grouped),
    build_type_grouped = as.factor(build_type_grouped),
    build_plan_grouped = as.factor(build_plan_grouped),
    room_category = as.factor(room_category),
    log_price_m2 = log(price_m2)
    # proximity_category = as.factor(proximity_category)
  )


  # ggplot(aes(y = price_m2,
  #        x = fct_reorder(park, price_m2, .fun = mean, na.rm = TRUE)))+
  # geom_boxplot(fill = "lightblue", alpha = 0.7) +
  # theme_minimal() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Step 2: Filter for one district and home type -------------------------
district_name <- "Chilonzor"  # Replace with an actual district

data_subset <- secondary_data_pre %>%
  filter(district == district_name) %>%
  arrange(YearMonth) 

# Step 3: Define rolling periods using YearMonth ------------------------
start_month <- min(data_subset$YearMonth)  # Earliest available month
end_month <- start_month + 11/12  # 12-month rolling window

print(paste("Start Month:", start_month, "End Month:", end_month))

# Step 4: Run Regression for Initial Window -----------------------------
window_data <- data_subset %>%
  filter(YearMonth >= start_month & YearMonth <= end_month) %>%
  mutate(month_factor = factor(YearMonth, levels = unique(YearMonth)))
