

rent <- read_excel(here("rent", "arenda", "tashkent.xlsx")) %>% 
  clean_names() 
 
  # select(-district_28) %>% 
  # rename("district" = district_5)
  
rent_01 <- rent %>% 
  filter(!str_detect(title_text, "kvartirani 1 xonasi")) %>% 
  filter(!str_detect(title_text,"1 хона берилади")) %>% 
  filter(!str_detect(title_text,"комноту")) %>% 
  filter(!str_detect(title_text,"хаозяйка билан")) %>% 
  filter(!str_detect(title_text,"шерилика")) %>% 
  filter(!str_detect(title_text,"Сдаются комната")) %>% 
  filter(!str_detect(title_text,"Сдаю комнату")) %>% 
  filter(!str_detect(title_text,"с хоз.")) %>%
  filter(!str_detect(post_text,"bita honasini beraman")) %>%
  filter(!str_detect(post_text,"бир хонасига")) %>%
  drop_na(price_m2) %>% 
  # group_by(title_text, price) %>% 
  # slice(1L) %>% 
  # ungroup() %>% 
  group_by(post_text, price) %>%
  slice(1L) %>%
  ungroup() %>%
  group_by(district) %>%
  filter(price <= quantile(price, 0.99), 
          price >= quantile(price, 0.01)) %>% 
  summarise(mean = mean(price_m2, na.rm = TRUE),
            median= median(price_m2, na.rm = TRUE),
            num_obs = n(),
            min = min(price_m2, na.rm = TRUE), 
            max= max(price_m2, na.rm = TRUE)) 

write_xlsx(rent_01, "rent_tash.xlsx")