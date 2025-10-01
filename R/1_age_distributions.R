library(tidyverse)
library(metamedian)



single_year_to_five_year <- function (df, fifteen_to_49 = TRUE)  {
  df <- df %>% dplyr::mutate(age_group_label = cut(age, c(0, 
                                                          seq(5, 85, 5) - 1), c(paste0(seq(0, 79, 5), "-", seq(5, 
                                                                                                               80, 5) - 1), "80+"), include.lowest = TRUE)) %>% dplyr::left_join(naomi::get_age_groups() %>% 
                                                                                                                                                                                   select(age_group, age_group_label)) %>% dplyr::select(-age_group_label)
  if (fifteen_to_49) {
    df %>% dplyr::filter(age %in% 15:49) 
  }
  else {
    df 
  }
}

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agehiv_data_extract_240925.rds") 

spectrum_dat <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop)

KEN1993 <- dat %>% 
  filter(survey_id == "KEN1993ACA_FSW") %>% 
  filter(visit == 0) %>% 
  separate(survey_day, into = c(NA, NA, "survey_year"), sep = "/") %>% 
  mutate(year = paste0(19, survey_year),
         survey_id = paste0(survey_id, "_", survey_year)) %>% 
  select(survey_year, everything()) %>% 
  type.convert(as.is = T) %>% 
  mutate(year = ifelse(year < 1993, year + 100, year))

KEN1993 %>% 
  ggplot() + 
  geom_density(aes(x = age, color = factor(year))) +
  theme_minimal()

#FSW : 74 surveys 
fsw_dat <- dat %>% 
  filter(kp == "FSW", !survey_id %in% c("ETH2020ACA_FSW")) %>% 
  filter(sex == 1 | is.na(sex)) %>% 
  filter(!survey_id == "KEN1993ACA_FSW") %>% 
  bind_rows(KEN1993) %>% 
  filter(!(age <10 | age > 70)) %>% 
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

fsw_dat_medians <- fsw_dat %>% 
  summarise(median_age = median(age, na.rm = TRUE),
            mean_age = mean(age, na.rm = T),
            age_iqr_low = quantile(age, 0.25, na.rm = TRUE),
            age_iqr_high = quantile(age, 0.75, na.rm = TRUE),
            median_duration = median(duration_estimate, na.rm = TRUE),
            mean_duration = mean(duration_estimate, na.rm = T),
            duration_iqr_low = quantile(duration_estimate, 0.25, na.rm = TRUE),
            duration_iqr_high = quantile(duration_estimate, 0.75, na.rm = TRUE),
            max_dens_age = max(density(age, na.rm = TRUE)$y, na.rm = TRUE),
            max_dens_dur = max(density(duration_estimate, na.rm = T)$y, na.rm = T)) %>%
  mutate(kp = "FSW") 

fsw_dat_medians

fsw_dat %>% 
  ggplot() +
  geom_density(aes(x = age, color = factor(survey_id)), show.legend = F) + 
  theme_minimal()

KEN1993 %>% 
  ggplot() + 
  geom_density(aes(x = duration_yr, color = factor(year))) +
  theme_minimal()

fsw_dat %>% 
  filter(iso3 == "ZAF") %>% 
  ggplot() +
  geom_density(aes(x = age, color = factor(survey_id))) + 
  theme_minimal()

# 49 surveys
msm_dat <- dat %>%
  filter(kp %in% c("MSM", "MSMTG", "TG", "TGW", "FSW")) %>% 
  filter(!(kp == "FSW" & (sex == 1 | (gender == "male" | gender == 1)))) %>% 
  filter(!survey_id %in% c("MOZ2021BBS_FSW", "COD2022PSE_FSW")) %>% 
  mutate(kp2 = case_when(kp == "MSMTG" & gender %in% c("female", "non-binary", "tgw") ~ "TGW",
                         kp == "MSMTG" & gender %in% c("male") ~ "MSM",
                         kp == "MSM" & gender %in% c("female", "TGW", "other", "3", "5") ~ "TGW",
                         kp == "MSM" & gender %in% c("male", "0", "1") ~ "MSM",
                         kp == "TG" & gender %in% c(0, 4) ~ "TGM",
                         kp == "TG" & gender == c(1, 3) ~ "TGW", 
                         kp == "TG" & sex == 1 ~ "TGM", 
                         kp == "TG" & sex == 0 ~ "TGW",
                         kp == "FSW" ~ "TGW",
                         TRUE ~ kp)) %>% 
  filter(kp2 == "MSM") %>% 
  mutate(kp = kp2) %>% 
  filter(!(age < 10 | age > 60)) %>% 
  mutate(duration_calc = age - age_fs_man,
         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
         duration_calc2 = age - age_fs_man_anal,
         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc))


msm_dat_medians <- msm_dat %>% 
  summarise(median_age = median(age, na.rm = TRUE),
            mean_age = mean(age, na.rm = T),
            age_iqr_low = quantile(age, 0.25, na.rm = TRUE),
            age_iqr_high = quantile(age, 0.75, na.rm = TRUE),
            median_duration = median(duration_estimate, na.rm = TRUE),
            mean_duration = mean(duration_estimate, na.rm = T),
            duration_iqr_low = quantile(duration_estimate, 0.25, na.rm = TRUE),
            duration_iqr_high = quantile(duration_estimate, 0.75, na.rm = TRUE),
            max_dens = max(density(age, na.rm = TRUE)$y, na.rm = TRUE),
            max_dens_age = max(density(age, na.rm = TRUE)$y, na.rm = TRUE),
            max_dens_dur = max(density(duration_estimate, na.rm = T)$y, na.rm = T)) 
  
msm_dat %>% 
  ggplot() +
  geom_density(aes(x = age, color = factor(survey_id)), show.legend = F) + 
  theme_minimal()

# 28** surveys
tgw_dat <- dat %>%
  filter(kp %in% c("MSM", "MSMTG", "TG", "TGW", "FSW")) %>% 
  filter(!(kp == "FSW" & (sex == 1 | (gender == "male" | gender == 1)))) %>% 
  filter(!survey_id %in% c("MOZ2021BBS_FSW", "COD2022PSE_FSW")) %>% 
  mutate(kp2 = case_when(kp == "MSMTG" & gender %in% c("female", "non-binary", "tgw") ~ "TGW",
                         kp == "MSMTG" & gender %in% c("male") ~ "MSM",
                         kp == "MSM" & gender %in% c("female", "TGW", "other", "3", "5") ~ "TGW",
                         kp == "MSM" & gender %in% c("male", "0", "1") ~ "MSM",
                         kp == "TG" & gender %in% c(0, 4) ~ "TGM",
                         kp == "TG" & gender == c(1, 3) ~ "TGW", 
                         kp == "TG" & sex == 1 ~ "TGM", 
                         kp == "TG" & sex == 0 ~ "TGW",
                         kp == "FSW" ~ "TGW",
                         TRUE ~ kp)) %>% 
  filter(kp2 %in% c("TG", "TGM", "TGW")) %>% 
  mutate(kp = ifelse((kp == "TGW" | (kp == "TG" & (sex ==0 | is.na(sex)))), "TGW", "TGM_other")) %>%  
  filter(!(age < 10 | age > 60)) %>% 
  mutate(duration_estimate = 0)


tgw_dat_medians <- tgw_dat %>% 
  group_by(kp) %>% 
  summarise(median_age = median(age, na.rm = TRUE),
            mean_age = mean(age, na.rm = T),
            age_iqr_low = quantile(age, 0.25, na.rm = TRUE),
            age_iqr_high = quantile(age, 0.75, na.rm = TRUE),
            max_dens_age = max(density(age, na.rm = TRUE)$y, na.rm = TRUE)
           ) %>% 
  ungroup()

tgw_dat %>% 
  ggplot() + 
  geom_density(aes(x = age, color = survey_id), show.legend = F) + 
  theme_minimal()



cfsw_dat <- dat %>% 
  filter(kp == "CFSW") %>% 
  select(survey_id, iso3, year, kp, sex, age, age_fs_paidfor, age_fs_paidfor_anal, age_fs_paidfor_vag, duration_client) %>% #duration_cfsw
  mutate(duration_cfsw = duration_client,
    # duration_csfw = ifelse(is.na(duration_cfsw), duration_client, duration_cfsw),
         dur_est1 = age-age_fs_paidfor,
         dur_est1 = ifelse(dur_est1 < 0, NA_integer_, dur_est1),
         dur_est2 = age-age_fs_paidfor_vag,
         dur_est2 = ifelse(dur_est2 < 0, NA_integer_, dur_est2),
         dur_est3 = age-age_fs_paidfor_anal,
         dur_est3 = ifelse(dur_est3 < 0, NA_integer_, dur_est2),
         duration_estimate = ifelse(is.na(duration_cfsw), dur_est1, duration_cfsw),
         duration_estimate = ifelse(is.na(duration_estimate), dur_est2, duration_estimate),
         duration_estimate = ifelse(is.na(duration_estimate), dur_est3, duration_estimate)) 

cfsw_dat_medians <- cfsw_dat %>% 
  summarise(median_age = median(age, na.rm = TRUE),
            mean_age = mean(age, na.rm = T),
            age_iqr_low = quantile(age, 0.25, na.rm = TRUE),
            age_iqr_high = quantile(age, 0.75, na.rm = TRUE),
            median_duration = median(duration_estimate, na.rm = TRUE),
            mean_duration = mean(duration_estimate, na.rm = T),
            duration_iqr_low = quantile(duration_estimate, 0.25, na.rm = TRUE),
            duration_iqr_high = quantile(duration_estimate, 0.75, na.rm = TRUE),
            max_dens_age = max(density(age, na.rm = TRUE)$y, na.rm = TRUE),
            max_dens_dur = max(density(duration_estimate, na.rm = T)$y, na.rm = T)) 
  
  
cfsw_dat %>% 
  ggplot() + 
  geom_density(aes(x = age, color = survey_id))


pwid_dat <- dat %>% 
  filter(kp == "PWID") %>% 
  select(survey_id, iso3, year, kp, age, inject_dur, age_inject) %>% 
  mutate(duration_estimate = age - age_inject,
         duration_estimate = ifelse(duration_estimate < 0, NA_integer_, duration_estimate))


pwid_dat_medians <- pwid_dat %>% 
  summarise(median_age = median(age, na.rm = TRUE),
            mean_age = mean(age, na.rm = T),
            age_iqr_low = quantile(age, 0.25, na.rm = TRUE),
            age_iqr_high = quantile(age, 0.75, na.rm = TRUE),
            median_duration = median(duration_estimate, na.rm = TRUE),
            mean_duration = mean(duration_estimate, na.rm = T),
            duration_iqr_low = quantile(duration_estimate, 0.25, na.rm = TRUE),
            duration_iqr_high = quantile(duration_estimate, 0.75, na.rm = TRUE),
            max_dens_age = max(density(age, na.rm = TRUE)$y, na.rm = TRUE),
            max_dens_dur = max(density(duration_estimate, na.rm = T)$y, na.rm = T)) 



#### Meta-median

dat %>%
  filter(kp == "FSW") %>%
  group_by(survey_id) %>%
  summarise(all_na = all(is.na(age)), .groups = "drop") %>%
  filter(all_na) %>%
  pull(survey_id)

median_dat_fsw <- fsw_dat %>%  
  select(survey_id, age) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(age, na.rm = TRUE), 
    q1.g1 = quantile(age, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(age, 0.75, na.rm = TRUE),
    sd.g1 = sd(age, na.rm = T),
    mean.g1 = mean(age, na.rm = T),
    min.g1 = min(age, na.rm = T),
    max.g1 = max(age, na.rm = T)
  ) %>% ungroup()


fsw_median <- metamedian(median_dat_fsw)

metamedian(median_dat_fsw, median_method = "wm")


msm_mediandat <- dat %>%
  filter(kp %in% c("MSM", "MSMTG", "TG", "TGW", "FSW")) %>% 
  filter(!(kp == "FSW" & (sex == 1 | (gender == "male" | gender == 1)))) %>% 
  filter(!survey_id %in% c("MOZ2021BBS_FSW", "COD2022PSE_FSW")) %>% 
  mutate(kp2 = case_when(kp == "MSMTG" & gender %in% c("female", "non-binary", "tgw") ~ "TGW",
                         kp == "MSMTG" & gender %in% c("male") ~ "MSM",
                         kp == "MSM" & gender %in% c("female", "TGW", "other", "3", "5") ~ "TGW",
                         kp == "MSM" & gender %in% c("male", "0", "1") ~ "MSM",
                         kp == "TG" & gender %in% c(0, 4) ~ "TGM",
                         kp == "TG" & gender == c(1, 3) ~ "TGW", 
                         kp == "TG" & sex == 1 ~ "TGM", 
                         kp == "TG" & sex == 0 ~ "TGW",
                         kp == "FSW" ~ "TGW",
                         TRUE ~ kp)) %>% 
  filter(kp2 == "MSM", !survey_id %in% c("CIV2020BBS_MSM", "CIV2012ACA_MSM")) %>% 
  select(survey_id, age) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(age, na.rm = TRUE), 
    q1.g1 = quantile(age, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(age, 0.75, na.rm = TRUE),
    sd.g1 = sd(age, na.rm = T),
    mean.g1 = mean(age, na.rm = T),
    min.g1 = min(age, na.rm = T),
    max.g1 = max(age, na.rm = T)
  ) %>% ungroup()

# msm_median <- metamedian(msm_mediandat)


tg_median <- dat %>%
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
  filter(kp2 %in% c("TG", "TGW"), !survey_id == "MLI2014BBS_MSM") %>% 
  select(survey_id, age) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(age, na.rm = TRUE), 
    q1.g1 = quantile(age, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(age, 0.75, na.rm = TRUE),
    sd.g1 = sd(age, na.rm = T),
    mean.g1 = mean(age, na.rm = T),
    min.g1 = min(age, na.rm = T),
    max.g1 = max(age, na.rm = T)
  ) %>% ungroup()

tgw_median <- metamedian(tg_median)





pwid_mediandat <- dat %>%
  filter(kp == "PWID") %>% 
  select(survey_id, age) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(age, na.rm = TRUE), 
    q1.g1 = quantile(age, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(age, 0.75, na.rm = TRUE),
    sd.g1 = sd(age, na.rm = T),
    mean.g1 = mean(age, na.rm = T),
    min.g1 = min(age, na.rm = T),
    max.g1 = max(age, na.rm = T)
  ) %>% ungroup()

pwid_median <- metamedian(pwid_mediandat)


cfsw_mediandat <- dat %>%
  filter(kp == "CFSW", !survey_id == "CIV1999ACA_CFSW") %>% 
  select(survey_id, age) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(age, na.rm = TRUE), 
    q1.g1 = quantile(age, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(age, 0.75, na.rm = TRUE),
    sd.g1 = sd(age, na.rm = T),
    mean.g1 = mean(age, na.rm = T),
    min.g1 = min(age, na.rm = T),
    max.g1 = max(age, na.rm = T)
  ) %>% ungroup()

cfsw_median <- metamedian(cfsw_mediandat)

# 
# # FSW Age distributions
# 
# (fswage_dist <- (dat %>% 
#                   filter(kp == "FSW", !iso3 %in% c("ETH")) %>% 
#                   ggplot() + 
#                   geom_density(aes(x = age, color = survey_id), show.legend = F) + 
#                   # geom_point(aes(x = fsw_median$beta, y = 0), color = "black", size = 2, show.legend = F) + 
#                   # geom_linerange(aes(xmin = fsw_median$ci.lb, xmax = fsw_median$ci.ub, y = 0), 
#                   #               color = "black", size = 1) + 
#                   theme_minimal() + 
#                   theme(axis.title = element_text(size = 16),
#                         axis.text = element_text(size = 14)) + 
#                   geom_boxplot(aes(x = age, y = 0.18), width = 0.03) +
#                   labs(x = "", y = "")) + 
#   theme(aspect.ratio = 1))
# 


#Age + Duration distributions

dist_dat <- fsw_dat %>% 
  bind_rows(msm_dat) %>%
  bind_rows(pwid_dat) %>% 
  bind_rows(cfsw_dat) %>% 
  bind_rows(tgw_dat) %>% 
  select(survey_id, iso3, year, kp, Age = age, Duration = duration_estimate) %>% 
  pivot_longer(cols = c(Age, Duration), names_to = "Distribution", values_to = "Value") %>% 
  moz.utils::name_kp() %>% 
  mutate(kp = ifelse(kp == "TGM_other", "Transgender \nMen/Other",kp)) %>% 
  mutate(kp = factor(kp, levels = c("Female sex\nworkers", "Men who have\nsex with men", "People who\ninject drugs", "Clients of female\nsex workers", "Transgender\nwomen" , "Transgender \nMen/Other"))) 


dist_medians <- fsw_dat_medians %>% mutate(kp = "FSW") %>% 
  bind_rows(msm_dat_medians %>% mutate(kp = "MSM")) %>% 
  bind_rows(pwid_dat_medians %>% mutate(kp = "PWID")) %>% 
  bind_rows(tgw_dat_medians) %>% 
  bind_rows(cfsw_dat_medians %>% mutate(kp = "CFSW")) %>% 
  moz.utils::name_kp() %>% 
  mutate(kp = ifelse(kp == "TGM_other", "Transgender \nMen/Other",kp)) %>% 
  mutate(kp = factor(kp, levels = c("Female sex\nworkers", "Men who have\nsex with men", "People who\ninject drugs", "Clients of female\nsex workers", "Transgender\nwomen" , "Transgender \nMen/Other"))) 

install.packages("ggforce")  
library(ggforce)
(ages <- dist_dat %>% 
  left_join(dist_medians) %>% 
  filter(!Value < 0) %>% 
  mutate(mean = ifelse(Distribution == "Age", round(mean_age, 1), round(mean_duration, 1)),
         median = ifelse(Distribution == "Age", round(median_age,1), round(median_duration, 1)),
         iqr_low = ifelse(Distribution == "Age", round(age_iqr_low, 0), round(duration_iqr_low, 0)),
         iqr_high = ifelse(Distribution == "Age", round(age_iqr_high, 0), round(duration_iqr_high,0)),
         max_dens = ifelse(Distribution == "Age", max_dens_age, max_dens_dur)) %>% 
  select(survey_id, iso3, year, kp, Distribution, Value, mean, median, iqr_low, iqr_high, max_dens) %>% 
  mutate(label = paste0("Mean:", mean, " Med:", median, "\nIQR:", iqr_low, "-", iqr_high)) %>% 
  filter(Distribution == "Age") %>% 
  ggplot() + 
  geom_density(aes(x = Value, color = survey_id), show.legend = F) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~kp,  scales = "free_y", ncol = 1, strip.position = "right") +
  labs(x = "", y = "") +
  moz.utils::standard_theme() +
  theme(strip.text.y = element_blank()) +
  theme(aspect.ratio = 1) + 
    ggtitle("Age") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
  geom_text(data = . %>% group_by(kp) %>% slice(1) %>% ungroup(), aes(label = label, x = 44, y = 1.5*max_dens), size = 3.3, color = "darkred", fontface = "bold") +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))
  )

(durations <- dist_dat %>% 
  left_join(dist_medians) %>% 
  filter(!Value < 0) %>% 
  mutate(mean = ifelse(Distribution == "Age", round(mean_age, 1), round(mean_duration, 1)),
         median = ifelse(Distribution == "Age", round(median_age,1), round(median_duration, 1)),
         iqr_low = ifelse(Distribution == "Age", round(age_iqr_low, 0), round(duration_iqr_low, 0)),
         iqr_high = ifelse(Distribution == "Age", round(age_iqr_high, 0), round(duration_iqr_high,0)),
         max_dens = ifelse(Distribution == "Age", max_dens_age, max_dens_dur),
         custom_dens = case_when(kp %in% c("Transgender\nwomen", "Transgender \nMen/Other") ~ 0.15,
                                 kp == "Female sex\nworkers" ~ 0.7,
                                 kp == "Men who have\nsex with men" ~ 0.22,
                                 kp == "People who\ninject drugs" ~ 0.13,
                                 kp == "Clients of female\nsex workers" ~ 0.13)) %>% 
  select(survey_id, iso3, year, kp, Distribution, Value, mean, median, iqr_low, iqr_high, max_dens, custom_dens) %>% 
  mutate(label = paste0("Mean:", mean, " Med:", median, "\nIQR:", iqr_low, "-", iqr_high),
         label = ifelse(kp %in% c("Transgender\nwomen", "Transgender \nMen/Other"), "Not applicable", label)) %>% 
  filter(Distribution == "Duration") %>% 
  mutate(Value = ifelse(kp %in% c("Transgender\nwomen", "Transgender \nMen/Other"), NA_integer_, Value)) %>% 
  ggplot() + 
  geom_density(aes(x = Value, color = survey_id), show.legend = F) +
  scale_color_grey(start = 0.8, end = 0.2) +
  moz.utils::scale_percent() +
  lims(x = c(0, 30)) +
  facet_wrap(~kp,  scales = "free_y", ncol = 1, strip.position = "right") +
  labs(x = "", y = "") +
  moz.utils::standard_theme() +
  theme(aspect.ratio = 1) + 
  geom_text(data = . %>% group_by(kp) %>% slice(1) %>% ungroup(), aes(label = label, x = 17, y = custom_dens), size = 3.3, color = "darkred", fontface = "bold") +
    ggtitle("Duration") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))
)

ggpubr::ggarrange(ages, durations) +theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

 and 

            