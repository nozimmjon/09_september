

# 1) List Excel files and read them in
file_paths <- list.files(
  path = here::here("data"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)
df_list <- lapply(file_paths, read_excel)



# 2) Get the union of all columns across every file
all_cols_union <- Reduce(union, lapply(df_list, names))


# 3) Remove columns you do NOT want to keep
#    Here, we exclude "...1", "day", "month", and "year" entirely
cols_to_exclude <- c("...1", "day", "month", "year")
all_cols_union <- setdiff(all_cols_union, cols_to_exclude)


# 4) Align each data frame so they have the same columns
df_list_aligned <- lapply(df_list, function(df) {
  # Drop any unwanted columns if they exist
  df <- df %>% 
    select(-any_of(cols_to_exclude))
  
  # Identify which columns from all_cols_union are missing in this particular df
  missing_cols <- setdiff(all_cols_union, names(df))
  
  # Add missing columns as NA
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  # Reorder columns to the standard arrangement
  df <- df[all_cols_union]
  df
})


# 5) Verify all aligned data frames have identical column sets in the same order
all_identical <- all(
  sapply(df_list_aligned, function(x) identical(names(x), all_cols_union))
)
if (all_identical) {
  cat("All data frames now share the same column names in the same order.\n")
} else {
  cat("WARNING: Some data frames still differ in their column names/order.\n")
}


df_list_aligned <- lapply(df_list_aligned, function(df) {
  # Convert these columns to numeric (or integer) across all data frames
  numeric_cols <- c("home_floor", "apart_floor")
  
  for (col in numeric_cols) {
    # Only convert if this col exists
    if (col %in% names(df)) {
      # Safely coerce to numeric (invalid strings become NA)
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  
  df
})

# Then bind
df_all <- dplyr::bind_rows(df_list_aligned)


df_all <- df_all %>%
  mutate(district = if_else(is.na(district), city, district)) %>%
  select(-city, -link) %>% 
  mutate(
    district = case_when(
      district == "Алмазарский"       ~ "Olmazor",
      district == "Алмазарский район"       ~ "Olmazor",
      district == "Бектемирский"      ~ "Bektemir",
      district == "Бектемирский район"      ~ "Bektemir",
      district == "Мирабадский"       ~ "Mirobod",
      district == "Мирабадский район"       ~ "Mirobod",
      district == "Мирзо-Улугбекский район" ~ "Mirzo-Ulugbek",
      district == "Мирзо-Улугбекский" ~ "Mirzo-Ulugbek",
      district == "Сергелийский"      ~ "Sergeli",
      district == "Сергелийский район"      ~ "Sergeli",
      district == "Ташкент"           ~ "Tashkent",
      district == "Учтепинский"       ~ "Uchtepa",
      district == "Учтепинский район"       ~ "Uchtepa",
      district == "Чиланзарский район"      ~ "Chilonzor",
      district == "Чиланзарский"      ~ "Chilonzor",
      district == "Chilonozor"      ~ "Chilonzor",
      district == "Шайхантахурский район"   ~ "Shayhontohur",
      district == "Шайхантахурский"   ~ "Shayhontohur",
      district == "Юнусабадский"      ~ "Yunusobod",
      district == "Юнусабадский район"      ~ "Yunusobod",
      district == "Яккасарайский"     ~ "Yakkasaroy",
      district == "Яккасарайский район"     ~ "Yakkasaroy",
      district == "Яшнабадский"       ~ "Yashnobod",
      district == "Яшнабадский район"       ~ "Yashnobod",
      TRUE ~ district  # Leave all other values (including existing Latin names, NA) as is
    )
    )

exclude_phrases <- c("qolgan to`lovi bor", "to`langan", "тўланган", "imtiyozlik", 
                     "tanidan", "танидан", "krediti", "qolgan", "kvartira kerak",
                     "hovli", "ховли", "нотурар", "noturar", "ipoteka", "ипотека",
                     "subsidiya", "субсидия", "subsidiyasi", "kredit", "kreditga", 
                     "krediti", "кредит", "кредитга", "кредити", "имтиёзлик", "имтиёзли",
                     "imtiyozlik", "imtiyozli", "qarzi qolgan", "qarzi bor", "карзи бор",
                     "қарзи бор", "қарзи қолган", "карзи колган", "ойлик тўлов", "ойлик тулов",
                     "ойига", "бўлиб тўлаш", "oylik to'lov", "oylik tulov", "oyiga", "bulib tulash", 
                     "bo'lib to'lash", "uchastka", "участка", "катеж", "Noturar", "но турар", "сотик"
)

#Removing unnecessary columns

df_all_process <- df_all %>% 
  # mutate(region = recode(region,
  #                        "1" = "Андижон",
  #                        "2" = "Бухоро",
  #                       "3" = "Жиззах",
  #                      "4" = "Фарғона",
  #                       "5" = "Хоразм",
  #                       "6" = "Қоракалпоғистон",
  #                       "7" = "Қашқадарё",
  #                       "8" = "Наманган",
  #                      "9" = "Навоий",
  #                       "10" = "Самарқанд",
  #                        "11" = "Сурхондарё",
  #                     "12" = "Сирдарё",
  #                      "13" = "Тошкент вилояти"   ,
  #                        "14" = "Тошкент")) %>%
  mutate(area = case_when(num_rooms == 1 & area < 10 ~ 35,
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
  mutate(price_m2_02 = price/area) %>% 
  distinct(across(everything())) %>% 
  group_by(district) %>% 
  filter(area < 300, area > 15, num_rooms < 10, apart_floor< 20, 
         home_floor<20, ceil_height<4, price_m2 > 100) %>% 
  filter(!reduce(exclude_phrases, ~ .x | str_detect(post_text, .y), .init = FALSE)) %>%
  filter(!reduce(exclude_phrases, ~ .x | str_detect(title_text, .y), .init = FALSE)) %>%
  mutate(date = dmy(date), 
         month = month(date, label = TRUE)) %>% 
  mutate(price_m2_02 = if_else(price < 1000, price, price_m2_02)) %>% 
  filter(!is.na(price_m2_02)) %>% 
  mutate(month = as.character(month),
         num_rooms = as.integer(num_rooms), 
         price_m2 = as.double(price_m2_02),
         price = as.integer(price),
         area = as.double(area), 
         YearMonth = format(date, "%Y-%m"),  # Convert date to YearMonth (character)
         YearMonth = as.yearmon(YearMonth)  # Convert to yearmon for easier rolling calculations
  ) %>% 
  dplyr::select(-price_m2_02, -commission) %>% 
  filter(!is.na(district))



df_all_process <- df_all_process %>%
  group_by(YearMonth, district, price_m2, num_rooms, area, apart_floor, 
           home_floor, home_type, build_year, build_type) %>%
  slice(1L) %>% 
  ungroup() %>%
  mutate(
    condition_grouped = case_when(
      condition %in% c("Авторский проект", "Евроремонт") ~ "Excellent",
      TRUE ~ "average"
    ),
    build_type_grouped = case_when(
      build_type == "Кирпичный" ~ "Brick",
      TRUE ~ "Modern"
    ),
    build_plan_grouped = case_when(
      build_plan == "Студия" ~ "Studio",
      TRUE ~ "Regular"
    ),
    room_category = case_when(
      num_rooms <= 2 ~ "Small",
      num_rooms %in% c(3, 4) ~ "Medium",
      num_rooms >= 5 ~ "Large"
    ),
    condition_grouped = as.factor(condition_grouped),
    build_type_grouped = as.factor(build_type_grouped),
    build_plan_grouped = as.factor(build_plan_grouped),
    room_category = as.factor(room_category),
    log_price_m2 = log(price_m2)
  ) %>% 
  filter(price_m2 > 300, price_m2 < 5000)
