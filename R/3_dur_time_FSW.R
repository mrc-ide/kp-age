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

fsw_duration_dat <- dat %>% 
  filter(kp == "FSW", !survey_id %in% c("ETH2020ACA_FSW")) %>% 
  filter(sex == 1 | is.na(sex)) %>% 
  filter(!survey_id == "KEN1993ACA_FSW") %>% 
  bind_rows(KEN1993) %>% 
  mutate(survey_id = ifelse(str_detect(survey_id, "KEN1993"), "KEN1993ACA_FSW", survey_id)) %>% 
  filter(!(age <10 | age > 70)) %>% 
  select(survey_id, year, age, duration_yr, duration_mnth, age_fs_paid, age_fs_gift, age_fs_paidorgift, age_startsw) %>% 
  mutate(duration_yr = ifelse(is.na(duration_yr), duration_mnth/12, duration_yr),
         duration_calc = age - age_fs_paid,
         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
         duration_calc2 = age - age_fs_gift,
         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
         duration_calc3 = age - age_fs_paidorgift,
         duration_calc3 = ifelse(duration_calc3 < 0 , NA_integer_, duration_calc3),
         duration_calc4 = age - age_startsw,
         duration_calc4 = ifelse(duration_calc4 < 0 , NA_integer_, duration_calc4),
         duration_yr = ifelse(duration_yr > age, NA_integer_, duration_yr),
         duration_estimate = ifelse(is.na(duration_yr), duration_calc, duration_yr),
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc4, duration_estimate),
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc2, duration_estimate),
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate)) %>% 
  group_by(survey_id, year) %>% 
  mutate(denom = n()) %>% 
  ungroup() %>% 
  filter(!is.na(duration_estimate)) %>% 
  mutate(duration_estimate = round(duration_estimate, 0)) %>% 
  # mutate(duration_estimate = plyr::round_any(duration_estimate, 0.5),
  #        duration_estimate = ifelse(duration_estimate > 0.5, round(duration_estimate, 0), duration_estimate)) %>% 
  filter(!duration_estimate < 0)
  

fsw_inla_durationdat <- crossing(year = 1993:2023,
                                 denom = 1) %>% 
  bind_rows(fsw_duration_dat %>% filter(!duration_estimate > 5)) %>% 
  mutate(id.year = multi.utils::to_int(year)-1, 
         duration_estimate = ifelse(duration_estimate > 0.5 , duration_estimate + 0.5, duration_estimate),
         duration_estimate2 = duration_estimate,
         zero_dur = ifelse(duration_estimate < 1, 1, 0),
         i.zero_dur = ifelse(duration_estimate < 1, survey_id, 0),
         mean_year = year - 2012)


duration_formula <- duration_estimate ~ 1 + f(duration_estimate2, model = "ar1")  + mean_year #+ f(survey_id, model = "iid") #+ zero_dur + f(i.zero_dur, model = "iid", constr = T)

duration_mod <- INLA::inla(formula = duration_formula,
                           family = "exponential",
                           data = fsw_inla_durationdat,
                           control.compute=list(config = TRUE, waic = T, dic = T))      
