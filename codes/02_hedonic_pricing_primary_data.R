


# Converting categorical variables to dummy variables
reg_data <- cleaned_data %>%
  select(-title_text, -post_text) %>% 
  mutate(build_year = as.numeric(build_year)) %>% 
  mutate(age = 2024- build_year) %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric) 
  

write_xlsx(reg_data, "reg_data.xlsx")

dfSummary(reg_data) %>% summarytools::stview()

reg_data %>% filter(apart_floor < 20) %>% 
ggplot(aes(y = apart_floor, x = log(price_02))) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(title = "Relationship between Home Floor and Price",
       y = "apart Floor",
       x = "Price")

# Reorder based on median price
reg_data$apart_floor <- with(reg_data, reorder(apart_floor, price_02, FUN = median))

# Create the box plot
ggplot(reg_data, aes(x = apart_floor, y = price_02)) + 
  geom_boxplot() + 
  labs(title = "Price Distribution by Home Floor",
       x = "Home Floor",
       y = "Price")

correlations <- cor(reg_data %>% select_if(is.numeric), use = "complete.obs")
corrplot::corrplot(correlations, method = "circle")

# Pairwise scatter plots for numeric variables
pairs(reg_data %>% select_if(is.numeric))

 tashkent_data <- reg_data %>% filter(region == "Тошкент")  

 model <- lm(log(price_02) ~ city + age + furnished + condition, data = tashkent_data)
 
 
 # Obtain predicted prices
 tashkent_data$predicted_price <- exp(predict(model, tashkent_data))
 
 
 geometric_mean_price <- exp(sum(log(tashkent_data$predicted_price)) / nrow(tashkent_data))
 
 
 ggplot(tashkent_data %>% filter(city == "Bektemir"), aes(x = price_02, y = predicted_price)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
   labs(x = "Actual Price", y = "Predicted Price", title = "Actual vs Predicted Prices") +
   theme_minimal()
 