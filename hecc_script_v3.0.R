#Packages

pks <- c("fingertipsR", "tidyverse", "purrr", "tibble", 
         "spatstat", "lubridate", "readODS", "janitor", "progressr", "patchwork")

sapply(pks, library, character.only = T)

"%ni%" <- Negate("%in%")

handlers(global = TRUE)

# GP practice workforce data ----------------------------------------------

#Load CSV files

wf_march24 <- read.csv("1 General Practice – March 2024 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2024, month = 3)

wf_feb24 <- read.csv("1 General Practice – February 2024 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2024, month = 2)

wf_jan24 <- read.csv("1 General Practice – January 2024 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2024, month = 1)

wf_dec23 <- read.csv("1 General Practice – December 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 12)

wf_nov23 <- read.csv("1 General Practice – November 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 11)

wf_oct23 <- read.csv("1 General Practice – October 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 10)

wf_sep23 <- read.csv("1 General Practice – September 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 09)

wf_aug23 <- read.csv("1 General Practice – August 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 8)

wf_jul23 <- read.csv("1 General Practice – July 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 7)

wf_june23 <- read.csv("1 General Practice – June 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 6)

wf_may23 <- read.csv("1 General Practice – May 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 5)

wf_apr23 <- read.csv("1 General Practice – April 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 4)

wf_mar23 <- read.csv("1 General Practice – March 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 3)

wf_feb23 <- read.csv("1 General Practice – February 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 2)

wf_jan23 <- read.csv("1 General Practice – January 2023 Practice Level - Detailed.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate_each(funs(as.numeric), starts_with("TOTAL")) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2023, month = 1)

wf_dec22 <- read.csv("40. General Practice – December 2022 Practice Level.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate_each(funs(as.numeric), starts_with("TOTAL")) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2022, month = 12) %>% 
  mutate_each(funs(as.numeric), starts_with("TOTAL"))


wf_nov22 <- read.csv("39. General Practice – November 2022 Practice Level.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate_each(funs(as.numeric), starts_with("TOTAL")) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2022, month = 11) %>% 
  mutate_each(funs(as.numeric), starts_with("TOTAL"))

wf_oct22 <- read.csv("38. General Practice – October 2022 Practice Level.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate_each(funs(as.numeric), starts_with("TOTAL")) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2022, month = 10) %>% 
  mutate_each(funs(as.numeric), starts_with("TOTAL"))

wf_sep22 <- read.csv("37. General Practice – September 2022 Practice Level.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate_each(funs(as.numeric), starts_with("TOTAL")) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2022, month = 9) %>% 
  mutate_each(funs(as.numeric), starts_with("TOTAL"))

wf_aug22 <- read.csv("36. General Practice – August 2022 Practice Level.csv") %>%
  select(PRAC_CODE, PRAC_NAME,
         PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS,
         SUB_ICB_CODE, SUB_ICB_NAME, 
         ICB_CODE, ICB_NAME, 
         TOTAL_PATIENTS, 
         TOTAL_GP_FTE,
         TOTAL_GP_EXTG_FTE,
         TOTAL_GP_LOCUM_VAC_HC,
         TOTAL_GP_LOCUM_ABS_HC,
         TOTAL_GP_LOCUM_OTH_HC,
         TOTAL_NURSES_FTE, 
         TOTAL_DPC_FTE, 
         TOTAL_ADMIN_FTE) %>%
  mutate_each(funs(as.numeric), starts_with("TOTAL")) %>%
  mutate(TOTAL_GP_TR_GR = TOTAL_GP_FTE - TOTAL_GP_EXTG_FTE) %>%
  mutate(across(TOTAL_GP_FTE:TOTAL_ADMIN_FTE, ~ (.x*10000)/TOTAL_PATIENTS)) %>%
  add_column(year = 2022, month = 8) %>% 
  mutate_each(funs(as.numeric), starts_with("TOTAL"))

#Clean data

wf_all <- bind_rows(wf_march24, wf_feb24, wf_jan24, 
          wf_dec23, wf_nov23, wf_oct23, 
          wf_sep23, wf_aug23, wf_jul23, 
          wf_june23, wf_may23, wf_apr23, 
          wf_mar23, wf_feb23, wf_jan23, 
          wf_dec22, wf_nov22, wf_oct22, 
          wf_sep22, wf_aug22)

#Load deprivation lookup

depriv <- read.csv("gp_depriv_lookup.csv") %>%
  select(Area.Code, Area.Name, Time.period, Value) %>%
  filter(Time.period == 2019) %>%
  rename(PRAC_CODE = Area.Code)

workforce_df <- left_join(wf_all, depriv, by = "PRAC_CODE")

#Remove unnecessary datasets from environment

rm(list = ls(pattern = "^wf"))

#names(wf_depriv)
# PCN workforce data ------------------------------------------------------

#Load data

pcn_mar24 <- read.csv("Primary Care Networks - March 2024 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_feb24 <- read.csv("Primary Care Networks - February 2024 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_jan24 <- read.csv("Primary Care Networks - January 2024 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_dec23 <- read.csv("Primary Care Networks - December 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_nov23 <- read.csv("Primary Care Networks - December 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_oct23 <- read.csv("Primary Care Networks - October 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_sep23 <- read.csv("Primary Care Networks - September 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_aug23 <- read.csv("Primary Care Networks - August 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_jul23 <- read.csv("Primary Care Networks - July 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_jun23 <- read.csv("Primary Care Networks – June 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_may23 <- read.csv("Primary Care Networks – May 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_apr23 <- read.csv("Primary Care Networks – April 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_mar23 <- read.csv("Primary Care Networks – March 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_feb23 <- read.csv("Primary Care Networks – February 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_jan23 <- read.csv("13.Primary Care Networks – January 2023 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_dec22 <- read.csv("12.Primary Care Networks – December 2022 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_sep22 <- read.csv("11.Primary Care Networks – September 2022 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_jun22 <- read.csv("10.Primary Care Networks – June 2022 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_mar22 <- read.csv("9.Primary Care Networks – March 2022 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_dec21 <- read.csv("8.Primary Care Networks – December 2021 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_sep21 <- read.csv("7.Primary Care Networks – September 2021 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_jun21 <- read.csv("6.Primary Care Networks – June 2021 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_mar21 <- read.csv("5.Primary Care Networks – March 2021 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_dec20 <- read.csv("4.Primary Care Networks – December 2020 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_sep20 <- read.csv("3.Primary Care Networks – September 2020 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_jun20 <- read.csv("2.Primary Care Networks – June 2020 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

pcn_mar20 <- read.csv("1.Primary Care Networks – March 2020 Individual Level.csv") %>%
  mutate(UNIQUE_IDENTIFIER = as.integer(UNIQUE_IDENTIFIER))

#Bind rows and create aggregated staff group data

pcn_all <- grep("pcn", ls(), value = TRUE) |>
  mget() |>
  bind_rows() %>%
  select(CENSUS_YEAR, CENSUS_MONTH, 
         PCN_CODE, PCN_NAME, 
         ICB_CODE, ICB_NAME, STAFF_GROUP, 
         FTE) %>%
  group_by(CENSUS_YEAR, CENSUS_MONTH, PCN_CODE, PCN_NAME, STAFF_GROUP) %>%
  mutate(FTE = sum(FTE))

#Generate PCN deprivation data, from GP practice level

names(pcn_all)

prac_pcn_depriv <- workforce_df %>%
  select(year, month, PRAC_CODE, PCN_CODE, PCN_NAME, 
         TOTAL_PATIENTS, Value)

pcn_depriv_lookup <- prac_pcn_depriv %>% 
  filter(year == 2024 & month == 3) %>%
  drop_na() %>%
  group_by(PCN_NAME, PCN_CODE) %>%
  dplyr::summarise(med_depriv = weighted.median(Value, TOTAL_PATIENTS),
                   .groups = 'drop',
                   TOTAL_PATIENTS = sum(TOTAL_PATIENTS)) 

primarycarenetwork_df <- left_join(pcn_all, pcn_depriv_lookup, 
          by = "PCN_CODE") %>%
  select(-PCN_NAME.y) %>%
  filter(STAFF_GROUP == "Other Direct Patient Care staff") %>%
  mutate(FTE_denom = (FTE*30000)/TOTAL_PATIENTS)

#remove unnecessary pcn objects

rm(list = ls(pattern = "^pcn"))

# GP appointment data -----------------------------------------------------

#Load CSVs
#Data structures and file names match so can load in a function
  
data_path <- "C:/Users/jonny/OneDrive/HCB Associates/HECC Ineq Analysis/hecc_ineq_analysis/Appointments/Extracted"

files <- dir(data_path, pattern = "*.csv") 

appt_df <- files %>%
  map(~ read_csv(file.path(data_path, .))) %>% 
  bind_rows()

appt_df %>% head()

appt_dep <- left_join(appt_df, depriv, by = c("GP_CODE"="PRAC_CODE"))


#appt_dep %>%
#  mutate(month = month(APPOINTMENT_MONTH_START_DATE))

head(appt_dep)

names(appt_dep)

appt_dep$NATIONAL_CATEGORY

appt_dep_v2 <- appt_dep %>%
  select(APPOINTMENT_MONTH_START_DATE, GP_CODE, GP_NAME, 
         APPT_MODE, APPT_STATUS, 
         HCP_TYPE, COUNT_OF_APPOINTMENTS, Value) %>%
  mutate(APPOINTMENT_MONTH_START_DATE = dmy(APPOINTMENT_MONTH_START_DATE))

rm(appt_df)
rm(appt_dep)

# GP practice payments ----------------------------------------------------

#Load CSVs

payment_2223 <- read.csv("nhspaymentsgp-22-23-prac-csv.csv") %>%
  add_column(year = 2022) %>%
  select(Practice.Code, Practice.Name, 
         Dispensing.Practice,
         Average.Number.of.Registered.Patients, 
         Average.Number.of.Weighted.Patients,
         Average.payments.per.registered.patient, 
         Average.payments.per.weighted.patient,
         Global.Sum,
         year) %>%
  mutate(Global.Sum_wp = Global.Sum / Average.Number.of.Weighted.Patients) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9, 
         global_sum_wp = 10)

payment_2223_cats <- read.csv("nhspaymentsgp-22-23-prac-csv.csv") %>%
  add_column(year = 2022) %>%
  select(Practice.Code, Practice.Name, 
         Dispensing.Practice,
         Average.Number.of.Registered.Patients, 
         Average.Number.of.Weighted.Patients,
         Average.payments.per.registered.patient, 
         Average.payments.per.weighted.patient,
         Global.Sum,
         year,
         MPIG.Correction.factor,
         Balance.of.PMS.Expenditure,
         Premises.Payments,
         Information.Management.and.Technology,
         Total.QOF.Payments,
         Prescribing.Fee.Payments,
         Dispensing.Fee.Payments,
         Reimbursement.of.Drugs,
         Seniority,
         Doctors.Retainer.Scheme.Payments,
         Total.Locum.Allowances,
         General.Practice.Transformation,
         PCN.Participation,
         winter.Access.Fund,
         Other.Payments,
         Non.DES.Item.Pneumococcal.Vaccine..Childhood.Immunisation.Main.Programme,
         Influenza.and.Pneumococcal.Immunisations,
         Learning.Disabilities,
         Meningitis,
         Minor.Surgery,
         Out.Of.Area.in.Hours.Urgent.Care,
         Pertussis,
         Rotavirus.and.Shingles.Immunisation,
         Services.for.Violent.Patients,
         Weight.Management.Service,
         Local.Incentive.Schemes,
         GP.Extended.Hours.Access,
         Medical.Assessment.Reviews) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9)

payment_2122 <- read.csv("nhspaymentsgp-21-22-prac-csv-v2.csv") %>%
  add_column(year = 2021) %>%
  select(Practice.Code, Practice.Name, 
         Dispensing.Practice,
         Number.of.Registered.Patients..Last.Known.Figure., 
         Number.of.Weighted.Patients..Last.Known.Figure.,
         Average.payments.per.registered.patient, 
         Average.payments.per.weighted.patient,
         Global.Sum,
         year) %>%
  mutate(Global.Sum_wp = Global.Sum / Number.of.Weighted.Patients..Last.Known.Figure.) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9, 
         global_sum_wp = 10)

payment_2021 <- read.csv("nhspaymentsgp-20-21-prac-csv.csv") %>%
  add_column(year = 2020) %>%
  select(Practice.Code, Practice.Name, 
         Dispensing.Practice,
         Number.of.Registered.Patients..Last.Known.Figure., 
         Number.of.Weighted.Patients..Last.Known.Figure.,
         Average.payments.per.registered.patient, 
         Average.payments.per.weighted.patient,
         Global.Sum,
         year) %>%
  mutate(Global.Sum_wp = Global.Sum / Number.of.Weighted.Patients..Last.Known.Figure.) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9, 
         global_sum_wp = 10)

payment_1920 <- read.csv("nhspaymentsgp-19-20-prac-csv.csv") %>%
  add_column(year = 2019) %>%
  select(Practice.Code, Practice.Name, 
         Dispensing.Practice,
         Average.number.of.registered.patients1, 
         Average.number.of.weighted.patients1,
         Average.payments.per.registered.patient1, 
         Average.payments.per.weighted.patient1,
         Global.Sum,
         year) %>%
  mutate(Global.Sum_wp = Global.Sum / Average.number.of.weighted.patients1) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9, 
         global_sum_wp = 10) %>%
  mutate(payment_rp = as.numeric(payment_rp),
         payment_wp = as.numeric(payment_wp))

payment_1819 <- read.csv("nhspaymentsgp-18-19-csv.csv") %>%
  add_column(year = 2018)  %>%
  select(Practice.Code, Practice.Name, 
         Dispensing.Practice,
         Average.Number.of.Registered.Patients, 
         Average.Number.of.Weighted.Patients,
         Average.payments.per.registered.patient, 
         Average.payments.per.weighted.patient,
         Global.Sum,
         year) %>%
  mutate(Global.Sum_wp = Global.Sum / Average.Number.of.Weighted.Patients) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9, 
         global_sum_wp = 10)

payment_1718 <- read.csv("nhspaymentsgp-17-18-csv.csv") %>%
  add_column(year = 2017) %>%
  select(Practice.Code, Practice.Name, 
         Dispensing.Practice,
         Number.of.Registered.Patients..Last.Known.Figure., 
         Number.of.Weighted.Patients..Last.Known.Figure.,
         Average.payments.per.registered.patient, 
         Average.payments.per.weighted.patient,
         Global.Sum,
         year) %>%
  mutate(Global.Sum_wp = Global.Sum / Number.of.Weighted.Patients..Last.Known.Figure.) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9, 
         global_sum_wp = 10) %>%
  mutate(payment_rp = as.numeric(payment_rp),
         payment_wp = as.numeric(payment_wp))

payment_1617 <- read.csv("nhspaymentsgp-16-17-csv.csv", check.names = F) %>%
  add_column(year = 2016) %>%
  select(PracticeCode, PracticeName, 
         DispensingPractice,
         NumberofRegisteredPatientsLastKnownFigure, 
         NumberofWeightedPatientsLastKnownFigure,
         'AveragePaymentsPerRegisteredPatient_\xa3', 
         'AveragePaymentsPerWeightedPatient_\xa3',
         'GlobalSum_\xa3',
         year) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9) %>%
    mutate(Global.Sum_wp = global_sum / wt_pts) %>%
  rename(global_sum_wp = Global.Sum_wp) %>%
  mutate(payment_rp = as.numeric(payment_rp),
         payment_wp = as.numeric(payment_wp))

payment_1516 <- read.csv("nhspaymentsgp-15-16-csv.csv", check.names = F) %>%
  add_column(year = 2015) %>%
  select(PracticeCode, PracticeName, 
         DispensingPractice,
         NumberOfRegisteredPatientsLastKnownFigure, 
         NumberOfWeightedPatientsLastKnownFigure,
         'AveragePaymentPerRegisteredPatient_\xa3', 
         'AveragePaymentPerWeightedPatient_\xa3',
         'GlobalSum_\xa3',
         year) %>%
  rename(prac_code = 1, prac_name = 2, 
         disp_prac = 3, reg_pts = 4, wt_pts = 5, 
         payment_rp = 6, payment_wp = 7, 
         global_sum = 8, year = 9) %>%
  mutate(Global.Sum_wp = global_sum / wt_pts) %>%
  rename(global_sum_wp = Global.Sum_wp)

payment_all <- bind_rows(payment_1516, payment_1617, payment_1718, 
          payment_1819, payment_1920, payment_2021, 
          payment_2122, payment_2223)

payment_dep <- left_join(payment_all, depriv, by = c("prac_code" = "PRAC_CODE"))

#Add ICB data

prac_icb_lookup <- workforce_df %>%
  select(PRAC_CODE, 
         ICB_NAME, ICB_CODE) %>% distinct()

payment_dep_icb <- left_join(payment_dep, prac_icb_lookup, by = c("prac_code" = "PRAC_CODE"))

#Merge category dataset with lookup

pay_cats_dep <- left_join(payment_2223_cats, depriv, by = c("prac_code" = "PRAC_CODE"))

pay_cats_dep_icb <- left_join(pay_cats_dep, prac_icb_lookup, by = c("prac_code" = "PRAC_CODE"))

pay_cats_dep_icb %>% write.csv("payment_2223_cats.csv")


# GP Practice Survey data -------------------------------------------------


#Load CSVs

#Q18_12pct = overall good experience making appt
#Q89_12pct = trust in HCP during appt
#Q9_12pct = usually see preferred GP
#Q28_12pct = overall satisfied

surv_24 <- read.csv("GPPS_2024_Practice_data_(weighted)_(csv)_PUBLIC.csv") %>%
  select(ad_practicecode, ad_practicename, 
         gpcontactoverall.pcteval, lastgpapptconf.pcteval,
         localgpservicesprefhpsee.pcteval, overallexp.pcteval) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, trust_hcp = 4,
         see_pref_gp = 5, overall_sat = 6) %>%
  add_column(year = 2024)

surv_23 <- read.csv("GPPS_2023_Practice_data_(weighted)_(csv)_PUBLIC.csv") %>%
  select(Practice_Code, Practice_Name, 
         Q18_12pct, Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, trust_hcp = 4,
        see_pref_gp = 5, overall_sat = 6) %>%
  add_column(year = 2023)

surv_22 <- read.csv("GPPS_2022_Practice_data_(weighted)_(csv)_PUBLIC.csv") %>%
  select(Practice_Code, Practice_Name, 
         Q18_12pct, Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, trust_hcp = 4,
         see_pref_gp = 5, overall_sat = 6) %>%
  add_column(year = 2022)

surv_21 <- read.csv("GPPS_2021_Practice_data_(weighted)_(csv)_PUBLIC.csv") %>%
  select(Practice_Code, Practice_Name, 
         Q18_12pct, Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, trust_hcp = 4,
         see_pref_gp = 5, overall_sat = 6) %>%
  add_column(year = 2021)

surv_20 <- read.csv("GPPS_2020_Practice_data_(weighted)_(csv)_PUBLIC.csv") %>%
  select(Practice_Code, Practice_Name, 
         Q18_12pct, Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, trust_hcp = 4,
         see_pref_gp = 5, overall_sat = 6) %>%
  add_column(year = 2020)

surv_19 <- read.csv("GPPS_2019_Practice_data_(weighted)_(csv)_PUBLIC.csv") %>%
  select(Practice_Code, Practice_Name, 
         Q18_12pct, Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, trust_hcp = 4,
         see_pref_gp = 5, overall_sat = 6) %>%
  add_column(year = 2019)

surv_18 <- read.csv("GPPS 2018 Practice data (weighted) (csv) PUBLIC.csv") %>%
  select(Practice_Code, Practice_Name, 
         Q18_12pct, Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, trust_hcp = 4,
         see_pref_gp = 5, overall_sat = 6) %>%
  add_column(year = 2018)

surv_17 <- read.csv("GPPS 2017 Practice data (weighted) (csv) PUBLIC.csv") %>%
  select(Practice_code, Practice_Name, 
         Q18_12pct, #Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, #trust_hcp = 4,
         see_pref_gp = 4, overall_sat = 5) %>%
  add_column(year = 2017)

surv_16 <- read.csv("July 2016 Practice level data weighted CSV.csv") %>%
  select(Practice_code, Practice_Name, 
         Q18_12pct, #Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, #trust_hcp = 4,
         see_pref_gp = 4, overall_sat = 5) %>%
  add_column(year = 2016)

surv_15 <- read.csv("July 2015 Practice level data weighted.csv") %>%
  select(Practice_code, Practice_Name, 
         Q18_12pct, #Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, #trust_hcp = 4,
         see_pref_gp = 4, overall_sat = 5) %>%
  add_column(year = 2015)

surv_14 <- read.csv("July 2014 Practice level data weighted.csv") %>%
  select(Practice_code, Practice_Name, 
         Q18_12pct, #Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, #trust_hcp = 4,
         see_pref_gp = 4, overall_sat = 5) %>%
  add_column(year = 2014)

surv_13 <- read.csv("June 2013 Practice Level Data weighted.csv") %>%
  select(Practice_Code, Practice_Name, 
         Q18_12pct, #Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, #trust_hcp = 4,
         see_pref_gp = 4, overall_sat = 5) %>%
  add_column(year = 2013)

surv_12 <- read.csv("June 2012 Practice Level Data weighted.csv") %>%
  select(Practice_Code, Practice_Name, 
         Q18_12pct, #Q89_12pct,
         Q9_12pct, Q28_12pct) %>%
  rename(prac_code = 1, prac_name = 2, 
         appt_exp = 3, #trust_hcp = 4,
         see_pref_gp = 4, overall_sat = 5) %>%
  add_column(year = 2012)

#Bind data

surveys_all <- bind_rows(surv_24, surv_23, surv_22, surv_21, 
                      surv_20, surv_19, surv_18,
                      surv_17, surv_16, surv_15, 
                      surv_14, surv_13, surv_12)

#Add deprivation data

surveys_depriv <- left_join(surveys_all, depriv, 
          by = c("prac_code" = "PRAC_CODE"))

surveys_depriv %>%
  write.csv("surveys.csv")


#Remove unnecessary objects

rm(list = ls(pattern = "^surv_"))


# CQC data ----------------------------------------------------------------

#Load ODS data

cqc_df <- read_ods("CQC_ratings_april_24.ods", 
         sheet = 2) %>% clean_names()

unique(cqc_df$location_primary_inspection_category)

cqc_gp <- cqc_df %>%
  filter(location_primary_inspection_category == "GP Practices") %>%
  select(location_ods_code, location_name, domain, latest_rating, publication_date, inherited_rating_y_n, 
         )

unique(cqc_gp$domain)

cqc_dep <- left_join(cqc_gp, depriv, 
          by = c("location_ods_code" = "PRAC_CODE")) %>%
  mutate(publication_date = dmy(publication_date)) %>%
  mutate(year = year(publication_date)) %>% select(-publication_date)


# National GP Profile data (Fingertips) -----------------------------------

#total QOF points achieved

qof_points <- read.csv("total_qof_points.csv") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

#cervical cancer screening

cervic_2449 <- read.csv("cervic_2425.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Age, Time.period, Value)

cervic_5064 <- read.csv("cervic_5064.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Age, Time.period, Value)

cervic_all <- bind_rows(cervic_2449, cervic_5064) %>%
  select(-Age)

#Emergency cancer admissions

emer_canc <- read.csv("cancer_admissions.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

#Hospital admissions among children aged 0-4 years

emerg_u5_admissions <- read.csv("emer_under5_admissions.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

#A&E attendances among children aged 0-4 years

ed_u5 <- read.csv("ed_under5.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

#Child immunisation data

vaccine_2223 <- read_csv("qof_vaccine_2223.csv") %>%
  clean_names()

#mmr <- read.csv("mmr_2yo.csv") %>%
 # filter(Area.Type == "GPs") %>%
  #select(Indicator.Name, Area.Code, Area.Name, Age, Time.period, Value)

#Flu imms

#seems unavailable

#Smoking

smok_prev <- read.csv("smok_qof_prev.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

#Obesity

obes_prev <- read.csv("obesity_qof_prev.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

#Hypertension

bp_prev <- read.csv("hypertension_qof_prevalence.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

#Prevalence of LTCs

data_path_ltc <- "C:/Users/jonny/OneDrive/HCB Associates/HECC Ineq Analysis/hecc_ineq_analysis/LTCs"

files <- dir(data_path_ltc, pattern = "*.csv") 

ltc_prev <- files %>%
  map(~ read_csv(file.path(data_path_ltc, .))) %>% 
  bind_rows()

ltc_prev_v2 <- ltc_prev %>% clean_names() %>%
  filter(area_type == "GPs") %>%
  select(indicator_name, area_code, area_name, time_period, value) %>%
  rename(Indicator.Name = indicator_name, 
         Area.Code = area_code, 
         Area.Name = area_name,
         Time.period = time_period, 
         Value = value)

unique(ltc_prev$indicator_name)

#Quality of management of LTCs

managed_bp <- read.csv("managed_bp_qof.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

managed_dm <- read.csv("managed_dm_qof.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

#Merge QOF and Fingertips datasets

names(qof_points)

names(vaccine_2223)

#Need to alter variable names for vaccine data

vaccine_2223_v2 <- vaccine_2223 %>%
  select(practice_code, practice_name, perc_achievement) %>%
  add_column(Indicator.Name = "child_imms",
             Time.period = "2022/23") %>%
  rename(Area.Code = practice_code, 
         Area.Name = practice_name, 
         Value = perc_achievement) %>%
  mutate(Value = as.numeric(Value))

qof_df <- bind_rows(qof_points, bp_prev, cervic_all, ed_u5, emer_canc, emerg_u5_admissions, 
          ltc_prev_v2, managed_bp, managed_dm, 
          obes_prev, smok_prev, vaccine_2223_v2) %>%
  rename(indicator = 1, PRAC_CODE = 2, year = 4, indicator_value = 5, 
         )

qof_dep <- left_join(qof_df, depriv, by = c("PRAC_CODE" = "PRAC_CODE", 
                                 "Area.Name" = "Area.Name"))

#Next steps to create unified dataset then visuals

head(workforce_df)
head(appt_dep_v2)
head(payment_dep_icb)
head(primarycarenetwork_df)
head(surveys_depriv)
head(cqc_dep)
head(qof_dep)

#Remove all objects but these

rm(list = ls()[!ls() %in% c("workforce_df", "appt_dep_v2",
                            "payment_dep_icb", "primarycarenetwork_df",
                            "surveys_depriv",
                            "cqc_dep",
                            "qof_dep")])

#try and save to CSV

workforce_df %>%
  write.csv("workforce.csv")

appt_dep_v2 %>%
  write.csv("appt.csv")

payment_dep_icb %>%
  write.csv("payments.csv")

primarycarenetwork_df %>%
  write.csv("pcn_payments.csv")

surveys_depriv %>%
  write.csv("surveys.csv")

cqc_dep %>%
  write.csv("cqc_df.csv")

qof_dep %>% write.csv("qof_df.csv")

###

"%ni%" <- Negate("%in%")

#Data

workforce_df <- read.csv("workforce.csv")

appt_dep_v2 <- read.csv("appt.csv")

payment_dep_icb <- read.csv("payments.csv")

primarycarenetwork_df <- read.csv("pcn_payments.csv")

surveys_depriv <- read.csv("surveys.csv")

cqc_dep <- read.csv("cqc_df.csv")

qof_dep <- read.csv("qof_df.csv")

#Final steps to finalise data

#workforce

workforce_df <- workforce_df %>% 
  mutate(total_locum_gp = TOTAL_GP_LOCUM_VAC_HC + TOTAL_GP_LOCUM_ABS_HC + TOTAL_GP_LOCUM_OTH_HC) %>%
  select(-c("SUB_ICB_CODE", "SUB_ICB_NAME", "Area.Name", "Time.period", "TOTAL_PATIENTS", "TOTAL_GP_LOCUM_VAC_HC", "TOTAL_GP_LOCUM_ABS_HC", "TOTAL_GP_LOCUM_OTH_HC"))

#Create quintiles by ICB

prac_quintiles <- workforce_df %>%
  select(PRAC_CODE, PRAC_NAME, ICB_CODE, ICB_NAME, Value) %>%
  group_by(ICB_CODE, ICB_NAME) %>%
  mutate(quintile = ntile(x=Value, 5))

wf_quin <- left_join(workforce_df, prac_quintiles, 
                     by = c("PRAC_CODE", "PRAC_NAME", "ICB_CODE", "ICB_NAME", 
                            "Value"), 
                     relationship = "many-to-many")

#check distribution

wf_quin %>%
  ggplot(aes(x=TOTAL_ADMIN_FTE)) + 
  geom_density()

#Variables are all highly right skewed

final_wf <- wf_quin %>% 
  select(-X) %>%
  distinct() %>%
  select(-c("month", "Value")) %>%
  group_by(year, ICB_CODE, ICB_NAME, quintile) %>%
  summarise_at(vars(TOTAL_GP_FTE:total_locum_gp), median, na.rm=T)

#appts

workforce_df <- read.csv("workforce.csv")

prac_icb_lookup <- workforce_df %>%
  filter(year == 2024 & month == 3) %>%
  select(PRAC_CODE, PRAC_NAME, ICB_CODE, ICB_NAME, TOTAL_PATIENTS)

appt_dep_v3 <- left_join(appt_dep_v2, prac_icb_lookup, by = 
                           c("GP_CODE" = "PRAC_CODE", 
                             "GP_NAME" = "PRAC_NAME"), 
                         relationship = "many-to-many")

#add quintile lookup

prac_quintiles <- appt_dep_v3 %>%
  select(GP_CODE, GP_NAME, ICB_CODE, ICB_NAME, Value) %>%
  distinct() %>%
  group_by(ICB_CODE, ICB_NAME) %>%
  mutate(quintile = ntile(x=Value, 5)) %>% distinct()

appts_quins <- left_join(appt_dep_v3, prac_quintiles,
                              by = c("GP_CODE", "GP_NAME", "ICB_CODE", "ICB_NAME"), 
                              relationship = "many-to-many")

#add patient list size lookup

pt_lookup <- appts_quins %>% 
  select(GP_CODE, GP_NAME, ICB_CODE, ICB_NAME, TOTAL_PATIENTS, quintile) %>%
  distinct() %>%
  group_by(ICB_CODE, ICB_NAME, quintile) %>%
  summarise(TOTAL_PATIENTS = sum(TOTAL_PATIENTS, na.rm = T))

appts_quins_pts <- left_join(appts_quins, pt_lookup, by = 
                                     c("ICB_CODE", "ICB_NAME", "quintile")) %>%
  select(-TOTAL_PATIENTS.x)

appts <- appts_quins_pts %>%
  distinct() %>%
  group_by(GP_CODE, GP_NAME, HCP_TYPE, APPT_STATUS, APPT_MODE, 
           APPOINTMENT_MONTH_START_DATE, ICB_NAME, ICB_CODE) %>%
  summarise(total_appts = sum(COUNT_OF_APPOINTMENTS)) %>%
  ungroup()

"%ni%" <- Negate("%in%")

final_appts <- left_join(appts, pt_lookup, 
          by = c("ICB_NAME", "ICB_CODE")) %>%
  mutate(APPT_MODE = case_when(
    APPT_STATUS == "DNA" ~ "DNA",
    TRUE ~ as.character(APPT_MODE)
  )) %>%
  mutate(year = year(APPOINTMENT_MONTH_START_DATE)) %>%
  group_by(quintile, year, HCP_TYPE, ICB_NAME, ICB_CODE, APPT_MODE, TOTAL_PATIENTS) %>%
  summarise(median_appts = sum(total_appts)) %>%
  filter(HCP_TYPE == "GP") %>%
  mutate(total_appts_1k = median_appts*1000/TOTAL_PATIENTS) %>%
  select(-c("HCP_TYPE", "TOTAL_PATIENTS", "median_appts")) %>%
  ungroup() %>%
  rename(variable = APPT_MODE, value = total_appts_1k)

#rm(list = ls()[!ls() %in% c("final_wf", "final_appts")])

#payments

#quintile lookup

prac_quintiles <- payment_dep_icb %>%
  select(prac_code, prac_name, ICB_CODE, ICB_NAME, Value) %>%
  group_by(ICB_CODE, ICB_NAME) %>%
  mutate(quintile = ntile(x=Value, 5))

pament_quins <- left_join(payment_dep_icb, prac_quintiles, 
                        by = c("prac_code", "prac_name", "ICB_CODE", "ICB_NAME", "Value"),
                        relationship = "many-to-many") %>%
  drop_na()

final_payment <- pament_quins %>%
  filter(disp_prac %ni% c("UNKNOWN", "Unknown")) %>%
  group_by(year, ICB_CODE, ICB_NAME, quintile, disp_prac) %>%
  summarise(med_pp_wp = median(payment_wp, na.rm = T), 
            med_pp_rp = median(payment_rp, na.rm = T), 
            med_gsum_wp = median(global_sum_wp, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = disp_prac, 
              values_from = c(med_pp_rp, med_pp_wp, med_gsum_wp)) %>%
  pivot_longer(cols = -c(year, ICB_CODE, ICB_NAME, quintile), 
               names_to = "variable", 
               values_to = "value") %>%
  pivot_wider(values_from = "value",
              names_from = c("variable")) %>%
  rowwise() %>%
  mutate(med_pp_wp_Total = median(c(med_pp_wp_No, med_pp_wp_Yes), na.rm = T)) %>%
  select(-c("med_pp_rp_No", 
            "med_pp_rp_Yes",
            "med_gsum_wp_No",
            "med_gsum_wp_Yes")) %>%
  pivot_longer(cols = -c("year", "ICB_CODE", "ICB_NAME", "quintile"),
               names_to = "variable", 
               values_to = "value")

#rm(list = ls()[!ls() %in% c("final_wf", "final_appts", "final_payment")])


#PCN workforce data

primarycarenetwork_df <- read.csv("pcn_payments.csv")

#create deprivation quintile lookup

pcn_quintiles <- primarycarenetwork_df %>%
  select(PCN_CODE, PCN_NAME.x, ICB_CODE, ICB_NAME, med_depriv) %>%
  group_by(ICB_CODE, ICB_NAME) %>%
  mutate(quintile = ntile(x=med_depriv, 5))

pcn_df_quins <- left_join(primarycarenetwork_df, pcn_quintiles,
                          by = c("PCN_CODE", "PCN_NAME.x", "ICB_CODE", "ICB_NAME", "med_depriv"), 
                          relationship = "many-to-many")

final_pcn_wf <- pcn_df_quins %>%
  group_by(ICB_CODE, ICB_NAME, quintile, CENSUS_YEAR) %>%
  summarise(med_dpc = median(FTE_denom)) %>%
  ungroup()

#rm(list = ls()[!ls() %in% c("final_wf", "final_appts", "final_pcn_wf", "final_payment")])

#surveys

surveys_depriv <- read.csv("surveys.csv")

workforce_df <- read.csv("workforce.csv")

prac_icb_lookup <- workforce_df %>%
  filter(year == 2024 & month == 3) %>%
  select(PRAC_CODE, PRAC_NAME, ICB_CODE, ICB_NAME, TOTAL_PATIENTS)

surv_icb <- left_join(surveys_depriv, prac_icb_lookup, by = 
                        c("prac_name" = "PRAC_NAME", 
                          "prac_code" = "PRAC_CODE"))

#generate quintiles

surv_icb_quins <- surv_icb %>%
  group_by(ICB_CODE, ICB_NAME) %>%
  mutate(quintile = ntile(x=Value, 5))

final_surv <- surv_icb_quins %>%
  mutate(trust_hcp = case_when(
    trust_hcp == -97 ~ NA,
    TRUE ~ as.numeric(trust_hcp)
  )) %>%
  filter(trust_hcp %ni% NA) %>%
  group_by(year, ICB_CODE, ICB_NAME, quintile) %>%
  summarise(appt_exp = median(appt_exp),
            trust_hcp = median(trust_hcp, na.rm = T),
            see_pref_gp = median(see_pref_gp),
            overall_sat = median(overall_sat)) %>%
  pivot_longer(cols = -c(year, ICB_NAME, ICB_CODE, quintile),
               names_to = "variable",
               values_to = "value") %>%
  ungroup()

#rm(list = ls()[!ls() %in% c("final_wf", "final_appts", "final_pcn_wf", 
                      #      "final_surv", "final_payment")])

#CQC data

cqc_dep <- read.csv("cqc_df.csv")

#need to link with ICB lookup

workforce_df <- read.csv("workforce.csv")

prac_icb_lookup <- workforce_df %>%
  filter(year == 2024 & month == 3) %>%
  select(PRAC_CODE, PRAC_NAME, ICB_CODE, ICB_NAME, TOTAL_PATIENTS)

cqc_icb <- left_join(cqc_dep, prac_icb_lookup, 
                     by = c("location_ods_code" = "PRAC_CODE"))

final_cqc <- cqc_icb %>%
  group_by(ICB_CODE, ICB_NAME) %>%
  mutate(quintile = ntile(x=Value, 5)) %>%
  ungroup() %>%
  filter(domain == "Overall") %>%
  group_by(ICB_CODE, ICB_NAME, quintile, latest_rating) %>%
  dplyr::count() %>%
  ungroup() %>%
  group_by(ICB_CODE, ICB_NAME, quintile) %>%
  mutate(per = n/sum(n)) %>% 
  select(-n) %>%
  pivot_wider(names_from = latest_rating, 
              values_from = per) %>%
  mutate(good_out = Good + Outstanding) %>%
  select(ICB_CODE, ICB_NAME, quintile, good_out)

#rm(list = ls()[!ls() %in% c("final_wf", "final_appts", "final_pcn_wf", 
                     #       "final_surv", "final_cqc", "final_payment")])

#QOF

qof_dep <- read.csv("qof_df.csv")

#add ICb lookup

workforce_df <- read.csv("workforce.csv")

prac_icb_lookup <- workforce_df %>%
  filter(year == 2024 & month == 3) %>%
  select(PRAC_CODE, PRAC_NAME, ICB_CODE, ICB_NAME, TOTAL_PATIENTS)

qof_icb <- left_join(qof_dep, prac_icb_lookup, 
                     by = "PRAC_CODE")

#Quality of care indicators

final_qof <- qof_icb %>%
  group_by(ICB_CODE, ICB_NAME, year) %>%
  mutate(quintile = ntile(x=Value, 5)) %>%
  group_by(ICB_CODE, ICB_NAME, year, quintile, indicator) %>%
  summarise(indicator_value = median(indicator_value)) %>%
  ungroup() %>%
  select(indicator, year, indicator_value, 
         ICB_CODE, ICB_NAME, quintile)

#rm(list = ls()[!ls() %in% c("final_wf", "final_appts", "final_pcn_wf", 
                      #      "final_surv", "final_cqc", "final_payment", 
                      #      "final_qof")])

#Compare dataframes

head(final_wf)
head(final_appts)
head(final_pcn_wf)
head(final_surv)
head(final_cqc)
head(final_payment)
head(final_qof)

#Convert each to long and bind

final_wf <- final_wf %>%
  pivot_longer(cols = -c(year, ICB_CODE, ICB_NAME, quintile)) %>%
  rename(indicator = name) %>%
  mutate(year = as.character(year))
  
final_appts <- final_appts %>%
  select(-HCP_TYPE) %>%
  rename(indicator = variable, 
         value = value)

final_pcn_wf <- final_pcn_wf %>%
  add_column(indicator = "med_dpc") %>%
  rename(value = med_dpc)

final_surv <- final_surv %>%
  rename(indicator = variable)

final_cqc <- final_cqc %>%
  add_column(indicator = "good_out") %>%
  rename(value = good_out)

final_payment <- final_payment %>%
  rename(indicator = variable)

final_qof <- final_qof %>%
  rename(value = indicator_value)

final_wf <- final_wf %>%
  mutate(year = as.character(year))

final_appts <- final_appts %>%
  mutate(year = as.character(year))

final_pcn_wf <- final_pcn_wf %>%
  rename(year = CENSUS_YEAR) %>%
  mutate(year = as.character(year))

final_surv <- final_surv %>%
  mutate(year = as.character(year))

final_cqc <- final_cqc %>%
  add_column(year = 2023) %>%
  mutate(year = as.character(year))

final_payment <- final_payment %>%
  mutate(year = as.character(year))

all_df <- bind_rows(final_wf, final_appts, final_pcn_wf, 
          final_surv, final_cqc, final_payment, 
          final_qof)

#rm(list = ls()[!ls() %in% c("all_df")])

write.csv(all_df, "all_data.csv")

###

#Data

hecc_df <- read.csv("all_data.csv") %>%
  select(-X)

unique(hecc_df$indicator)

hecc_df_v2 <- hecc_df %>%
  mutate(value = case_when(
    indicator == "med_dpc" ~ value/3, 
    TRUE ~ as.numeric(value)
  ))

#Create England wide dataset

names(hecc_df_v2)

eng_df <- hecc_df_v2 %>%
  group_by(year, quintile, indicator) %>%
  summarise(value = median(value, na.rm = T)) %>%
  add_column(ICB_CODE = "England", 
             ICB_NAME = "England") %>%
  ungroup()

#add this to original dataset

hecc_df_v3 <- bind_rows (hecc_df_v2, eng_df)

cc_df <- hecc_df_v3 %>%
  #select(-X) %>%
  group_by(year, ICB_CODE, ICB_NAME, indicator) %>%
  filter(!is.na(quintile)) %>%
  dplyr::summarise(cor(value, quintile, use="pairwise.complete.obs")) %>%
  rename(corr_coeff = 5)

final_df <- left_join(hecc_df_v3, cc_df, 
                      by = c("year", "ICB_CODE", "ICB_NAME", "indicator"))

#Put in final structure for charts

final_df <- final_df %>%
  mutate(logic = case_when(
    corr_coeff > 0.2 | corr_coeff < -0.2 ~ TRUE,
    corr_coeff < 0.2 | corr_coeff > -0.2 ~ FALSE)) %>%
  pivot_wider(names_from = quintile) %>%
  rename(quin_1 = `1`, 
         quin_5 = `5`)

write.csv(final_df, "final_data.csv")