# Compute the median price per m2 for each month
median_prices <- data_subset %>%
  group_by(YearMonth) %>%
  summarize(median_price = median(price_m2, na.rm = TRUE)) %>%
  arrange(YearMonth)

# Set the base month as the first available month (e.g., January 2022)
base_price <- median_prices$median_price[1]
median_index <- 100 * median_prices$median_price / base_price

# Merge the median index with your hedonic index result
comparison <- merge(data.frame(YearMonth = median_prices$YearMonth, median_index = median_index),
                    result[, c("YearMonth", "index")],
                    by = "YearMonth", all = TRUE)

# Rename columns for clarity
names(comparison)[names(comparison)=="index"] <- "hedonic_index"

print("Comparison of Hedonic Index vs. Median Index:")
print(comparison)

# Plot the two indices
library(ggplot2)
ggplot(comparison, aes(x = YearMonth)) +
  geom_line(aes(y = hedonic_index, color = "Hedonic Index"), size = 1) +
  geom_line(aes(y = median_index, color = "Median Price Index"), size = 1, linetype = "dashed") +
  labs(title = "Comparison of Hedonic vs. Median Price Indices",
       y = "Index (Base = 100)",
       x = "Year-Month") +
  scale_color_manual(name = "Index Type", values = c("Hedonic Index" = "blue", "Median Price Index" = "red")) +
  theme_minimal()



# Create a median index; base it on the first month available
base_median <- median_prices$median_price[1]
median_prices <- median_prices %>%
  mutate(median_index = 100 * median_price / base_median)

# Calculate month-over-month growth rates for the median index
median_prices <- median_prices %>%
  mutate(median_growth = (median_index / lag(median_index) - 1) * 100)



result <- result %>%
  arrange(YearMonth) %>%
  rename(hedonic_index = index,
         hedonic_growth = monthly_growth)

# Merge the two series by YearMonth:
merged_data <- merge(result, median_prices, by = "YearMonth", all = TRUE)



data_subset %>% filter(YearMonth == "дек 2024") %>% 
  ggplot(aes(x = price_m2,
         y = condition_grouped))+
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



median_prices0 <- data_subset %>%
  filter(condition != "Евроремонт",
         condition != "Авторский проект") %>% 
  group_by(YearMonth) %>%
  summarize(median_price = median(price_m2, na.rm = TRUE)) %>%
  arrange(YearMonth)


# Create a median index; base it on the first month available
base_median <- median_prices0$median_price[1]
median_prices0 <- median_prices0 %>%
  mutate(median_index = 100 * median_price / base_median)

# Calculate month-over-month growth rates for the median index
median_prices0 <-median_prices0 %>%
  mutate(median_growth = (median_index / lag(median_index) - 1) * 100)








