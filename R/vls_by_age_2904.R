library(tidyverse)
library(moz.utils)
library(INLA)

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_2904.rds")


spectrum_vls_dat <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop,
         tot_treat = artpop/totpop) %>% 
  filter(year %in% c(1990:2024)) %>% filter(sex == "female")


sup_survs <- dat %>% filter(!is.na(vl_result_suppressed)) %>% distinct(survey_id) %>% pull(survey_id)

vls_dat <- dat %>% filter(!(is.na(vl) | survey_id %in% sup_survs), kp == "FSW") %>% 
  mutate(duration_calc = age - age_fs_paid,
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
  select(survey_id, year, age, duration_estimate, vl) %>% 
  mutate(vl_result_suppressed = ifelse(vl < 1000, 1, 0)) %>% 
  bind_rows(
    dat %>% 
      filter(!is.na(vl_result_suppressed), kp == "FSW") %>%
      mutate(duration_calc = age - age_fs_paid,
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
             duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate))
  ) %>% 
  group_by(survey_id, year, age, vl_result_suppressed) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, year, age) %>% 
  mutate(denom = sum(n)) %>% 
  ungroup() %>% 
  filter(vl_result_suppressed == 1) %>% 
  separate_survey_id() %>% 
  left_join(spectrum_vls_dat)


inla_vlsdat <- data.frame(age = 15:60,
                          denom = 1) %>% 
  bind_rows(vls_dat)

formula = n ~ 1 + f(age, model = "rw2") + f(survey_id, model = "iid") 

fsw_vls_mod <- INLA::inla(formula, 
                           family = "binomial",
                           Ntrials = denom,
                           # E = tot_prev,
                           offset = qlogis(tot_treat),
                           data = inla_vlsdat,
                           control.compute=list(config = TRUE))
summary(fsw_vls_mod)

fsw_vlsamples <- moz.utils::sample_model(fsw_vls_mod, inla_vlsdat, "n")

fsw_vlsamples %>% 
  mutate(plogis_lower = plogis(lower),
         plogis_mean = plogis(mean),
         plogis_upper = plogis(upper)) %>% 
  ggplot() +
  geom_line(aes(x = age, y = plogis_mean)) + 
  geom_ribbon(aes(x = age, ymin = plogis_lower, ymax = plogis_upper), alpha = 0.5) +
  geom_point(data = inla_vlsdat, aes(x = age, y = (n/denom)/tot_treat, color = survey_id), show.legend = F) + 
  moz.utils::standard_theme() + 
  labs(x = "Age", y = "% with VLS") + 
 scale_y_continuous(trans = "log")
  


 

vls_dur_dat <- dat %>% filter(!(is.na(vl) | survey_id %in% sup_survs), kp == "FSW") %>% 
  mutate(duration_calc = age - age_fs_paid,
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
  select(survey_id, year, age, duration_estimate, vl) %>% 
  mutate(vl_result_suppressed = ifelse(vl < 1000, 1, 0)) %>% 
  bind_rows(
    dat %>% 
      filter(!is.na(vl_result_suppressed), kp == "FSW") %>%
      mutate(duration_calc = age - age_fs_paid,
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
             duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate))
  ) %>% 
  group_by(survey_id, year, age, duration_estimate, vl_result_suppressed) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, year, age, duration_estimate) %>% 
  mutate(denom = sum(n)) %>% 
  ungroup() %>% 
  filter(vl_result_suppressed == 1)



vls_dur_dat_plot <- dat %>% filter(!(is.na(vl) | survey_id %in% sup_survs), kp == "FSW") %>% 
  mutate(duration_calc = age - age_fs_paid,
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
  select(survey_id, year, age, duration_estimate, vl) %>% 
  mutate(vl_result_suppressed = ifelse(vl < 1000, 1, 0)) %>% 
  bind_rows(
    dat %>% 
      filter(!is.na(vl_result_suppressed), kp == "FSW") %>%
      mutate(duration_calc = age - age_fs_paid,
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
             duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate))
  ) %>% 
  group_by(survey_id, year, duration_estimate, vl_result_suppressed) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, year, duration_estimate) %>% 
  mutate(denom = sum(n)) %>% 
  ungroup() %>% 
  filter(vl_result_suppressed == 1)


quantile(vls_dur_dat$age, na.rm = T)
inla_vls_dur_dat <- data.frame(duration_estimate = 0:30,
                               age = 34,
                          denom = 1) %>% 
  bind_rows(vls_dur_dat)

vls_dur_dat %>% filter(!is.na(age)) %>% mean(age)

formula = n ~ 1 + f(duration_estimate, model = "rw2") + age + f(survey_id, model = "iid") 

formula = n ~ 1 + duration_estimate + f(survey_id, model = "iid") 

fsw_vls_dur_mod <- INLA::inla(formula, 
                          family = "binomial",
                          Ntrials = denom,
                          # E = tot_prev,
                          # offset = qlogis(tot_prev),
                          data = inla_vls_dur_dat,
                          control.compute=list(config = TRUE))
summary(fsw_vls_dur_mod)

fsw_dur_vlsamples <- moz.utils::sample_model(fsw_vls_dur_mod, inla_vls_dur_dat, "n")

fsw_dur_vlsamples %>% 
  mutate(plogis_lower = plogis(lower),
         plogis_mean = plogis(mean),
         plogis_upper = plogis(upper)) %>% 
  ggplot() +
  geom_line(aes(x = duration_estimate, y = plogis_mean)) + 
  geom_ribbon(aes(x = duration_estimate, ymin = plogis_lower, ymax = plogis_upper), alpha = 0.3) +
  geom_point(data = vls_dur_dat_plot, aes(x = duration_estimate, y = n/denom, color = survey_id), show.legend = F) + 
  moz.utils::standard_theme() + 
  labs(x = "Duration of SW (years)", y = "% with VLS") +
  lims(x = c(0, 30)) + 
  moz.utils::scale_manual(F, n = 5)

cor(inla_vls_dur_dat$age, inla_vls_dur_dat$duration_estimate, use = "complete.obs")


