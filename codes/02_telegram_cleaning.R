
#importing data
namangan_telegram <- read_excel(here("data", "telegram", "namangan.txt_uzbek.xlsx"))


exclude_phrases <- c("qolgan to`lovi bor", "to`langan", "тўланган", "imtiyozlik", 
                     "tanidan", "танидан", "krediti", "qolgan", "kvartira kerak",
                     "hovli", "ховли", "нотурар", "noturar", "ipoteka", "ипотека",
                     "subsidiya", "субсидия", "subsidiyasi", "kredit", "kreditga", 
                     "krediti", "кредит", "кредитга", "кредити", "имтиёзлик", "имтиёзли",
                     "imtiyozlik", "imtiyozli", "qarzi qolgan", "qarzi bor", "карзи бор",
                     "қарзи бор", "қарзи қолган", "карзи колган", "ойлик тўлов", "ойлик тулов",
                     "ойига", "бўлиб тўлаш", "oylik to'lov", "oylik tulov", "oyiga", "bulib tulash", 
                     "bo'lib to'lash", "uchastka", "участка", "kvartira olaman", "sotildi", "ijara", "ijaraga",
                     "sotix", "qarzi bor", "subsidiyaga", "tanidan", "navastroyka", "novostroyka", "qolgan", 
                     "kvartira kerak", "kvartira beriladi", "muborak umra", "biznes uchun"
)


cleaned_namangan <- namangan_telegram %>% 
  select(-channel, -post_id, -views) %>% 
  mutate(date_format = ymd_hms(date)) %>% 
  mutate(month = month(date_format, label = TRUE)) %>% 
  mutate(post = str_trim(post),
         post = str_squish(post),
         post = str_to_lower(post)) %>% 
  filter(!str_detect(post, "🏠")) %>% 
  filter(!str_detect(post, "🏡")) %>% 
  filter(!reduce(exclude_phrases, ~ .x | str_detect(post, .y), .init = FALSE)) %>%
  mutate(area = str_extract(post, "(\\d+[\\.,]?\\d*)+(?= m²)")) %>% 
  mutate(price_post = str_replace(post, "💰", "price ")) %>%
  mutate(price_post = str_replace(price_post, "💵", "price ")) %>%
  mutate(price_post = str_replace_all(price_post, "[[:punct:]]", "")) %>% 
  mutate(price_post = str_replace_all(price_post, " ", "")) %>% 
  mutate(price = str_extract(price_post, "(?<=price?\\s{0,10})(\\d+[\\.,]?\\d*)")) %>% 
  mutate(num_rooms = str_extract(price_post, "\\d?+(?=xona)")) %>% 
  mutate(num_rooms = na_if(num_rooms, "")) %>% 
  group_by(price_post) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  mutate(narxi_02 = str_extract(price_post, "(?<=narxi?\\s{0,10})(\\d+[\\.,]?\\d*)")) %>% 
  mutate(final_price = if_else(is.na(price), narxi_02, price)) %>% 
  drop_na(final_price) %>% 
  select(-date, -date_format, -price, -narxi_02) %>% 
  mutate(subset = str_sub(price_post, 1, 20)) %>% 
  group_by(num_rooms, area, final_price, month, subset) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  select(-subset) %>% 
  mutate(region = "Наманган", final_price = parse_number(final_price)) %>%
  mutate(price = if_else(final_price < 1000, 1000 * final_price, final_price)) %>% 
  filter(price != 6900, price < 100000) %>% 
  mutate(area = if_else(is.na(area) & num_rooms == 1, "35", area)) %>% 
  mutate(area = if_else(is.na(area) & num_rooms == 2, "55", area)) %>%
  mutate(area = if_else(is.na(area) & num_rooms == 3, "75", area)) %>%
  mutate(area = if_else(is.na(area) & num_rooms == 4, "105", area)) %>% 
  mutate(area = if_else(is.na(area) & num_rooms == 5, "125", area)) %>% 
  mutate(area = parse_number(area)) %>% 
  mutate(price_m2 = price/area) %>% 
  filter(area> 10) %>%
  filter(price_m2 > 100) %>%
  #filter(month == "дек") %>% 
  select(-price_post, -price, -post) %>% 
  rename("price" = final_price) %>% 
  drop_na(price_m2, num_rooms) %>% 
  distinct() %>% 
  mutate(month = as.character(month),
         num_rooms = as.integer(num_rooms), 
         price_m2 = as.double(price_m2),
         price = as.integer(price),
         area = as.double(area))


#importing data for regions with limited data 
data_hand <- read_excel(here("by_hand.xlsx")) %>% 
  mutate(month = as.character(month),
         num_rooms = as.integer(num_rooms), 
         price_m2 = as.double(price_m2),
         price = as.integer(price),
         area = as.double(area))

#merging them into one data frame
cleaned_telegram <- bind_rows(cleaned_namangan,
                                  data_hand
)

# write_xlsx(cleaned_telegram, "cleaned_telegram.xlsx")  