library(tidyverse)
library(moz.utils)
library(INLA)

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_2904.rds")

dat2 <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_1305.rds") 



fsw_dat2 <- dat2 %>% filter(kp == "FSW") %>% 
  mutate(duration_calc = age - age_fs_paid,
         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
         duration_calc2 = age - age_fs_gift,
         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
         duration_calc3 = age - age_fs_paidorgift,
         duration_calc3 = ifelse(duration_calc3 < 0 , NA_integer_, duration_calc3),
         duration_calc4 = age - age_startsw,
         duration_calc4 = ifelse(duration_calc4 < 0 , NA_integer_, duration_calc4),
         duration_yr = ifelse(duration_yr > age, NA_integer_, duration_yr),
         duration_yr = ifelse(duration_yr %in% c(0, NA_integer_) & duration_mnth >= 0, duration_mnth/12, duration_yr),
         duration_estimate = ifelse(is.na(duration_yr), duration_calc, duration_yr),
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc4, duration_estimate),
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc2, duration_estimate),
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate)) 


spectrum_vls_dat <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop,
         tot_treat = artpop/hivpop) %>% 
  filter(year %in% c(1990:2024)) %>% filter(sex == "female")


# sup_survs <- dat %>% filter(!is.na(vl_result_suppressed)) %>% distinct(survey_id) %>% pull(survey_id)

fsw_vls_dat <- fsw_dat2 %>% 
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


fsw_vls_age_dat <- fsw_vls_dat %>% 
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

# For no year
inla_fsw_vlsdat_age <- data.frame(age = 15:60,
                          denom = 1) %>% 
  bind_rows(fsw_vls_age_dat)

# For no year, certain genpop level
inla_fsw_vlsdat_age <- crossing(age = 15:60,
                                tot_treat = c(0.3, 0.75, 0.95),
                                  denom = 1) %>% 
  bind_rows(fsw_vls_age_dat)

# With year
inla_fsw_vlsdat_age <- crossing(age = 15:60, year = 2000:2023) %>% 
  mutate(denom = 1) %>% 
  bind_rows(fsw_vls_age_dat) %>% 
  mutate(age_centre = age - 14,
         year_centre = year - 1999) %>% 
  filter(!is.na(age))



formula = n ~ 1 + age + f(survey_id, model = "iid") 

formula = n ~ 1 + f(age, model = "rw2") + f(survey_id, model = "iid") 
formula = n ~ 1 + age_centre*year_centre + f(survey_id, model = "iid") 
formula = n ~ 1 +
  f(year_centre, model = "rw2", scale.model = F, group = age_centre, control.group = list(model = "rw2", scale.model = F)) + 
  f(survey_id, model = "iid", hyper = multi.utils::tau_fixed(0.000001)) 

fsw_vls_mod <- INLA::inla(formula, 
                           family = "binomial",
                           Ntrials = denom,
                           # E = tot_prev,
                           offset = qlogis(tot_treat),
                           data = inla_fsw_vlsdat_age,
                           control.compute=list(config = TRUE))
summary(fsw_vls_mod)

fsw_vlsamples <- moz.utils::sample_model(fsw_vls_mod, inla_fsw_vlsdat_age, "n")

# without year to plot OR
fsw_vlsamples %>% 
  mutate(plogis_lower = plogis(lower),
         plogis_mean = plogis(mean),
         plogis_upper = plogis(upper),
         exp_lower = exp(lower),
         exp_mean = exp(mean),
         exp_upper = exp(upper)) %>% 
  ggplot() +
  geom_line(aes(x = age, y = exp_mean)) + 
  geom_ribbon(aes(x = age, ymin = exp_lower, ymax = exp_upper), alpha = 0.5) +
  geom_point(data = fsw_vls_age_dat, aes(x = age, y = ((n/denom)/(1-(n/denom)))/((tot_treat)/(1-tot_treat)), color = survey_id), show.legend = F) + 
  moz.utils::standard_theme() + 
  labs(x = "Age", y = "OR for VLS in KP relative to genpop") + 
  scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.1)) + 
  theme(aspect.ratio = 1) + 
  guides(color = FALSE) +
  lims(x = c(15,50)) +
  geom_hline(yintercept = 1, color = "darkred", linetype = 2)

# without year to plot at certain levels of genpop covg.
fsw_vlsamples %>% 
  mutate(plogis_lower = plogis(lower),
         plogis_mean = plogis(mean),
         plogis_upper = plogis(upper)
         
         
# with year
fsw_vlsamples %>% 
  mutate(plogis_lower = plogis(lower),
         plogis_mean = plogis(mean),
         plogis_upper = plogis(upper),
         exp_lower = exp(lower),
         exp_mean = exp(mean),
         exp_upper = exp(upper)) %>% 
  filter(year %in%  c(2012, 2017, 2023)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot() +
  geom_line(aes(x = age, y = exp_mean, color = year)) + 
  geom_ribbon(aes(x = age, ymin = exp_lower, ymax = exp_upper, fill = year), alpha = 0.5) +
  geom_point(data = inla_vlsdat, aes(x = age, y = ((n/denom)/(1-(n/denom)))/((tot_treat)/(1-tot_treat)), color = survey_id), show.legend = F) + 
  moz.utils::standard_theme() + 
  labs(x = "Age", y = "OR for VLS in KP relative to genpop") + 
 scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.1)) + 
  theme(aspect.ratio = 1) + 
  guides(color = FALSE)


fsw_vls_mod_no_offset <- INLA::inla(formula, 
                          family = "binomial",
                          Ntrials = denom,
                          # E = tot_prev,
                          # offset = qlogis(tot_treat),
                          data = inla_vlsdat,
                          control.compute=list(config = TRUE))
summary(fsw_vls_mod_no_offset)

fsw_vlsamples_no_offset <- moz.utils::sample_model(fsw_vls_mod_no_offset, inla_vlsdat, "n")

fsw_vlsamples_no_offset %>% 
  mutate(plogis_lower = plogis(lower),
         plogis_mean = plogis(mean),
         plogis_upper = plogis(upper),
         exp_lower = exp(lower),
         exp_mean = exp(mean),
         exp_upper = exp(upper)) %>% 
  filter(year %in%  c(2012, 2017, 2023)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot() +
  geom_line(aes(x = age, y = plogis_mean, color = year)) + 
  geom_ribbon(aes(x = age, ymin = plogis_lower, ymax = plogis_upper, fill = year), alpha = 0.5) +
  geom_point(data = inla_vlsdat, aes(x = age, y = (n/denom), color = survey_id), show.legend = F) + 
  moz.utils::standard_theme() + 
  labs(x = "Age", y = "% KP with VLS")  + 
  guides(color = FALSE)
  # scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.1))


inla_vlsdat %>% 
  mutate(vls_prev = n/denom) %>% 
  filter(!is.na(age),
         !is.na(survey_id)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = vls_prev), color = "blue") + 
  geom_line(aes(x = age, y = tot_treat)) + 
  facet_wrap(~survey_id, nrow = 1)



  


 

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


