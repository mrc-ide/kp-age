library(tidyverse)
library(moz.utils)
library(INLA)


dat2 <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_1305.rds") 

spectrum_vls_dat_male <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop,
         tot_treat = artpop/hivpop) %>% 
  filter(year %in% c(1990:2024)) %>% filter(sex == "male")


msm_dat2 <- dat2 %>% 
  filter(kp %in% c("MSM", "MSMTG", "TG", "TGW")) %>% 
  mutate(kp2 = case_when(kp == "MSMTG" & gender %in% c("female", "non-binary", "tgw") ~ "TGW",
                         kp == "MSMTG" & gender %in% c("male") ~ "MSM",
                         kp == "MSM" & gender %in% c("female", "TGW", "other", "3", "5") ~ "TGW",
                         kp == "MSM" & gender %in% c("male", "0", "1") ~ "MSM",
                         kp == "TG" & gender %in% c(0, 4) ~ "TGM",
                         kp == "TG" & gender == c(1, 3) ~ "TGW", 
                         kp == "TG" & sex == 1 ~ "TGM", 
                         kp == "TG" & sex == 0 ~ "TGW",
                         TRUE ~ kp)) %>% 
  filter(kp2 == "MSM") %>% 
  mutate(duration_calc = age - age_fs_man,
         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
         duration_calc2 = age - age_fs_man_anal,
         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc))


msm_vls_dat <- msm_dat2 %>% 
  mutate(vl_detect = ifelse(vl %in% c("<20", "INDETECTABLE"), 1, 0),
         vl = extract_numeric(vl),
         vl = ifelse(is.na(vl) & vl_detect == 1, 0, vl),
         vl_result_suppressed2 = case_when(vl<1000 ~ 1,
                                           vl >= 1000 & vl <100000000000 ~ 0,
                                           is.na(vl) ~ NA_integer_),
         vl_result_suppressed2 = ifelse(is.na(vl_result_suppressed2), vl_result_suppressed, vl_result_suppressed2),
         vl_10 = 10^vl_result_log10,
         vl_result_suppressed2 = case_when(is.na(vl_result_suppressed2) & vl_10 < 1000 ~ 1,
                                           is.na(vl_result_suppressed2) & vl_10 >= 1000 ~ 0,
                                           TRUE ~ vl_result_suppressed2),
         vl_result_suppressed2 = ifelse(is.na(vl_result_suppressed2) & vl_result_detectable == "undetectable", 1, vl_result_suppressed2 )) %>% 
  select(survey_id, year, kp, hiv, vl_result_suppressed = vl_result_suppressed2,  age, duration_estimate) %>% 
  filter(!is.na(vl_result_suppressed)) 

msm_vls_age_dat <- msm_vls_dat %>% 
  filter(!is.na(age)) %>% 
  group_by(survey_id, year, age, vl_result_suppressed) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, year, age) %>% 
  mutate(denom = sum(n)) %>% 
  ungroup() %>% 
  filter(vl_result_suppressed == 1) %>% 
  separate_survey_id() %>% 
  left_join(spectrum_vls_dat)

msm_vls_age_dat %>% 
  mutate(vls_prev = n/denom) %>% 
  filter(!is.na(age),
         !is.na(survey_id)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = vls_prev), color = "blue") + 
  geom_line(aes(x = age, y = tot_treat)) + 
  facet_wrap(~survey_id, nrow = 3) + 
  moz.utils::standard_theme() + 
  theme(aspect.ratio = 1)
