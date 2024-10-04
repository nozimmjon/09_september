
# #importing telegram data on secondary market
# cleaned_telegram <- read_excel(here("cleaned_telegram.xlsx"))
# cleaned_olx <- read_excel(here("cleaned_olx.xlsx"))

#joining telegram and olx data on secondary market 

final_data <- bind_rows(cleaned_data, cleaned_telegram) %>% 
  filter(month != "август") %>% 
  filter(price_m2 > 100) %>% 
  group_by(region) %>% 
  arrange(desc(price_m2)) %>% 
  slice(-1:-5) %>% 
  ungroup() %>% 
  filter(!(region %in% c("Сирдарё", "Жиззах") & price_m2 > 900))
  

final_data %>%
  filter(region == "Сирдарё") %>%
  ggplot(aes(x = price_m2, y = 1)) +
  geom_jitter(width = 0, alpha = 0.5) +
  scale_y_discrete() +
  # facet_wrap(~city)+
  labs(y = "", x = "price")
# 
# 
# dfSummary(final_data) %>% summarytools::stview()


price_reg <- final_data %>% 
  group_by(region) %>% 
  summarise(mean_apr = mean(price_m2),
            median_apr= median(price_m2),
            num_obs_apr = n(),
            min_apr = min(price_m2), 
            max_apr= max(price_m2)) 

write_xlsx(price_reg, "price_reg.xlsx")




# Define the arbitrary numbers for each district as a named vector
price_numbers <- c("Bektemir" = 2000, "Sergeli" = 2000, 
                   "Yashnobod" = 2000, "Olmazor" = 2000, "Uchtepa" = 2000, 
                   "Yunusobod" = 3000, "Chilonzor" = 2500, "Mirzo-Ulugbek" = 2000,
                   "Shayhontohur" = 2000, "Yakkasaroy" = 3000, "Mirobod" = 3000) # and so on...


price_tashkent <-  final_data %>%
  filter(region == "Тошкент", condition != "Евроремонт",
        condition != "Авторский проект") %>%  
  group_by(city) %>% 
  mutate(high_price_m2 = price_m2 > price_numbers[city]) %>%
  ungroup() %>%
  filter(!high_price_m2) %>%
  select(-high_price_m2) %>%
  group_by(city) %>%  # Drop the helper column
  summarise(mean = mean(price_m2),
            median= median(price_m2),
            num_obs = n(),
            min = min(price_m2), 
            max= max(price_m2)) 



write_xlsx(price_tashkent, "price_tash.xlsx")


price_tashkent %>% 
  #filter(city == "Mirobod") %>% 
  ggplot(aes(x = price_m2, y = 1)) +
  geom_jitter(width = 0, alpha = 0.5) +
  facet_wrap(~city)+
  scale_y_discrete() +
  labs(y = "", x = "price")
