library(tidyverse)
library(INLA)


dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agehiv_data_extract_240925.rds") 

KEN1993 <- dat %>% 
  filter(survey_id == "KEN1993ACA_FSW") %>% 
  filter(visit == 0) %>% 
  separate(survey_day, into = c(NA, NA, "survey_year"), sep = "/") %>% 
  mutate(year = paste0(19, survey_year),
         survey_id = paste0(survey_id, "_", survey_year)) %>% 
  select(survey_year, everything()) %>% 
  type.convert(as.is = T) %>% 
  mutate(year = ifelse(year < 1993, year + 100, year))

fsw_hiv_age_dat <- dat %>% 
  filter(kp == "FSW", !survey_id %in% c("ETH2020ACA_FSW")) %>% 
  filter(sex == 1 | is.na(sex)) %>% 
  filter(!survey_id == "KEN1993ACA_FSW") %>% 
  bind_rows(KEN1993) %>% 
  mutate(survey_id = ifelse(str_detect(survey_id, "KEN1993"), "KEN1993ACA_FSW", survey_id)) %>% 
  filter(!(age <10 | age > 70)) %>% 
  select(survey_id, iso3, year, age, hiv) %>% 
  group_by(survey_id, iso3, year, age, hiv) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(!is.na(hiv),
         !is.na(age)) %>% 
  left_join(moz.utils::region() %>% select(-four_region)) %>%
  pivot_wider(
    names_from = hiv,
    values_from = n,
    names_prefix = "hiv_",
    values_fill = 0  # fill missing with 0 if a group is missing hiv==1 or hiv==0
  ) 



pred_cfsw <- spec_hiv %>% 
  # kitchen.sink::single_year_to_five_year() %>% 
  group_by(iso3, year, sex, age) %>% 
  summarise(hivpop = sum(hivpop),
            totpop = sum(totpop),
            tot_prev = hivpop/totpop) %>% 
  ungroup() %>% 
  filter( iso3 %in% cfsw_dat_iso3,
          sex == "male",
          # age %in% 15:49,
          year %in% 2002:2025) %>%
  mutate(model = "countries") %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>% 
  mutate(id.iso3.age = group_indices(., age, id.iso3)) %>% 
  mutate(denom = 1) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))





data_prep_cfsw <- cfsw_dat %>%
  left_join(moz.utils::region()) %>%
  pivot_wider(
    names_from = hiv,
    values_from = n,
    names_prefix = "hiv_",
    values_fill = 0  # fill missing with 0 if a group is missing hiv==1 or hiv==0
  ) %>% 
  rename(n = hiv_1,
         n_neg = hiv_0) %>% 
  mutate(denom = n + n_neg) %>% 
  # kitchen.sink::single_year_to_five_year(age) %>% 
  # age %in% 15:49) %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  left_join(pred_cfsw %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age, id.iso3.age)) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))


cfsw_inla_dat <- pred_cfsw %>%
  bind_rows(data_prep_cfsw) %>% 
  mutate(id.year = multi.utils::to_int(year),
         # id.age = multi.utils::to_int(age),
         # id.year.age = group_indices(., age, year),
         id.year2 = id.year+1) %>% 
  mutate(kp_prev = n/denom,
         kp_odds = kp_prev/(1-kp_prev),
         genpop_odds = tot_prev/(1-tot_prev),
         or_obs = kp_odds/genpop_odds) %>% 
  mutate(#id.age2 = id.age,
    id.survey_id = multi.utils::to_int(survey_id),
    id.age2 = id.age,
    id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2),
    id.iso3.year = group_indices(., iso3, year),
    id.iso3.year = ifelse(is.na(iso3), NA_integer_, id.iso3.year)) %>% 
  #id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2)) %>% 
  # kitchen.sink::single_year_to_five_year(age) %>% 
  mutate(id.age = multi.utils::to_int(age),
         id.year.age = group_indices(., age, year),
         id.age2 =  ifelse(model == "regions", NA_integer_, id.age),
         mean_age = age - 34) %>% 
  filter(!is.na(age)) %>% 
  filter(!sex == "female")



cfsw_formulae <- list(mod1 = n ~ 1 + mean_age + f(id.iso3, model = "besag", graph = national_adj()),
                      mod2 = n ~ 1 + mean_age + f(id.survey_id, model = "iid"),
                      mod3 = n ~ 1 + f(mean_age, model = "ar1") +  f(id.survey_id, model = "iid"),
                      mod4 = n ~ 1 + mean_age,
                      mod5 = n ~ 1 + f(mean_age, model = "ar1"))

