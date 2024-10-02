

all_reg <- dir(here::here("data"), pattern=".xlsx", full.names=TRUE) %>%
  purrr::map_dfr(readxl::read_excel, na="*", .id = "region")


#Removing unnecessary columns

all_reg_01 <- all_reg %>% select(-2, -link) %>% 
  mutate(region = recode(region,
                         "1" = "Андижон",
                         "2" = "Бухоро",
                         "3" = "Жиззах",
                         "4" = "Фарғона",
                         "5" = "Хоразм",
                         "6" = "Қоракалпоғистон",
                         "7" = "Қашқадарё",
                         "8" = "Наманган",
                         "9" = "Навоий",
                         "10" = "Самарқанд",
                         "11" = "Сурхондарё",
                         "12" = "Сирдарё",
                         "13" = "Тошкент вилояти"   ,
                         "14" = "Тошкент")) %>% 
  mutate(area2 = case_when(num_rooms == 1 & area < 10 ~ 35,
                           num_rooms == 1 & area > 100 ~ 35,
                           num_rooms == 2 & area < 10 ~ 55,
                           num_rooms == 2 & area > 150 ~ 55,
                           num_rooms == 3 & area < 10 ~ 80,
                           num_rooms == 3 & area > 150 ~ 80,
                           num_rooms == 4 & area > 200 ~ 90,
                           num_rooms == 4 & area < 10 ~ 95,
                           num_rooms == 5 & area < 10 ~ 115,
                           num_rooms == 5 & area > 250 ~ 115,
                           TRUE ~ area)) %>% 
  mutate(price = round(price, 0)) %>% 
  mutate(price_m2_02 = price/area2) %>% 
  mutate(price_02 = if_else(price < 3000, price, price_m2_02)) %>% 
  distinct(across(everything())) %>% # takrorlangan qatorlarni olib tashlaydi va bitta unique obs qoladi 
  filter(home_type != "Вторичный рынок")


dfSummary(all_reg_01) %>% summarytools::stview()


exclude_phrases <- c("qolgan to`lovi bor", "to`langan", "тўланган", "imtiyozlik", 
                     "tanidan", "танидан", "krediti", "qolgan", "kvartira kerak",
                     "hovli", "ховли", "нотурар", "noturar", "ipoteka", "ипотека",
                     "subsidiya", "субсидия", "subsidiyasi", "kredit", "kreditga", 
                     "krediti", "кредит", "кредитга", "кредити", "имтиёзлик", "имтиёзли",
                     "imtiyozlik", "imtiyozli", "qarzi qolgan", "qarzi bor", "карзи бор",
                     "қарзи бор", "қарзи қолган", "карзи колган", "ойлик тўлов", "ойлик тулов",
                     "ойига", "бўлиб тўлаш", "oylik to'lov", "oylik tulov", "oyiga", "bulib tulash", 
                     "bo'lib to'lash", "uchastka", "участка", "катеж", "Noturar", "но турар", "сотик",
                     )

cleaned_data <- all_reg_01 %>% 
   mutate(post_text = str_trim(post_text),
         post_text = str_squish(post_text),
         post_text = str_to_lower(post_text),
         title_text = str_trim(title_text),
         title_text = str_squish(title_text),
         title_text = str_to_lower(title_text)
  ) %>% 
  filter(!(region == "Тошкент вилояти" &
             str_detect(post_text, regex("qoraqamish|metro|yashnobod|yunusobod|algoritm|algoritmda|algaritm|algаritm|algаritmda|algаritimda|chilonzor|chilanzor|Chorsu|shayxontoxur|shayxantoxur|uchtepa|do'mbiraobod|sergeli|yangihayot"))
  )) %>% 
  filter(!(region == "Тошкент вилояти" & 
             str_detect(post_text, 
                        regex("кара камиш|метро|яшнабад|метро ойбек|алгоритм|алгоритим|алгаритм|алгаритме|чилонзор|чиланзар|чиланзор|чорсу|шайхонтохур|шайхантохур|шайхонтохурский|юнусабад|юнусабадский|учтепинский|учтепа|мирабадский|мирабатский|шайхантахурский|дўмбираобод|сергели|сергелийский|янгихаёт")) 
  )) %>% 
  filter(!(region == "Тошкент вилояти" & 
             str_detect(title_text, regex("qoraqamish|metro|yashnobod|algoritm|algoritmda|algaritm|algаritm|algаritmda|algаritimda|chilonzor|chilanzor|Chorsu|shayxontoxur|shayxantoxur|uchtepa|do'mbiraobod|sergeli|yangihayot"))
  )) %>% 
  filter(!(region == "Тошкент вилояти" &
             str_detect(title_text, 
                        regex("кара камиш|метро|яшнабад|метро ойбек|алгоритм|алгаритм|алгоритим|алгаритме|чилонзор|чиланзар|чиланзор|чорсу|шайхонтохур|шайхантохур|шайхонтохурский|юнусабад|юнусабадский|учтепинский|учтепа|мирабадский|мирабатский|шайхантахурский|дўмбираобод|сергели|сергелийский|янгихаёт"))
  )) %>% 
  filter(!reduce(exclude_phrases, ~ .x | str_detect(post_text, .y), .init = FALSE)) %>%
  filter(!reduce(exclude_phrases, ~ .x | str_detect(title_text, .y), .init = FALSE)) %>%
  group_by(region) %>% 
  filter(!is.na(price_02)) %>% 
  filter(area2 < 300, area2 > 15, num_rooms < 11) %>% 
  filter(price_02 <= quantile(price_02, 0.99), 
         price_02 >= quantile(price_02, 0.01)) %>%
  filter(build_year > 2015) %>%
  filter(build_year != "старше 1960") %>% 
  ungroup() %>% 
  mutate(date = dmy(date), 
         month = month(date, label = TRUE)) %>% 
  group_by(num_rooms, price_02, apart_floor, home_floor, area, city) %>%
  slice(1L) %>% 
  ungroup() %>% 
  select(-home_type,-price,-commission,-area,-price_m2,-price_m2_02, -date)


dfSummary(cleaned_data) %>% summarytools::stview()




skimr::skim(cleaned_data)


#write_xlsx(cleaned_data, "cleaned_olx.xlsx")



check <-  cleaned_data %>% 
  group_by(region) %>%
  summarise(mean_dec = mean(price_02),
            median_dec= median(price_02),
            num_obs_dec = n(),
            min_dec = min(price_02),
            max_dec= max(price_02))


cleaned_data %>% 
  #filter(region == "Тошкент") %>% 
  ggplot(aes(x = price_02, y = 1)) +
  geom_jitter(width = 0, alpha = 0.5) +
  scale_y_discrete() +
  facet_wrap(~region, scales = "free_y", strip.position = "top")+
  labs(y = "", x = "price")

