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

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_1703.rds")

spectrum_dat <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop)


median_dat <- dat %>% filter(kp == "FSW", !survey_id %in% c("ETH2020ACA_FSW", "MOZ2021BBS_FSW", "ZAF2019ACA_FSW", "SEN2019BBS_FSW")) %>% 
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


fsw_median <- metamedian(median_dat)

metamedian(median_dat, median_method = "wm")


msm_mediandat <- dat %>%
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

msm_median <- metamedian(msm_mediandat)


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


# FSW Age distributions

fswage_dist <- (dat %>% 
    filter(kp == "FSW", !iso3 %in% c("ETH")) %>% 
    ggplot() + 
    geom_density(aes(x = age, color = survey_id), show.legend = F) + 
    # geom_point(aes(x = fsw_median$beta, y = 0), color = "black", size = 2, show.legend = F) + 
    # geom_linerange(aes(xmin = fsw_median$ci.lb, xmax = fsw_median$ci.ub, y = 0), 
    #               color = "black", size = 1) + 
    theme_minimal() + 
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)) + 
    geom_boxplot(aes(x = age, y = 0.18), width = 0.03) +
    labs(x = "", y = "")) + 
  theme(aspect.ratio = 1)

# MSM Age distributions

msm_agedist <- (dat %>%
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
  ggplot() + 
  geom_density(aes(x = age, color = survey_id), show.legend = F) + 
    # geom_point(aes(x = msm_median$beta, y = 0), color = "black", size = 2) + 
    # geom_linerange(aes(xmin = msm_median$ci.lb, xmax = msm_median$ci.ub, y = 0), 
    #                color = "black", size = 1) + 
    theme_minimal() + 
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)) + 
    geom_boxplot(aes(x = age, y = 0.18), width = 0.03) +
    labs(x = "", y = "")) + 
  theme(aspect.ratio = 1)

# TGW Age distributions
tgw_agedist <- (dat %>%
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
    filter(kp2 %in% c("TG", "TGW")) %>% 
    filter(!survey_id == "ZAF2019BBS_TGW") %>% 
    ggplot() + 
    geom_density(aes(x = age, color = survey_id), show.legend = F) + 
    # geom_point(aes(x = tgw_median$beta, y = 0), color = "black", size = 2) + 
    # geom_linerange(aes(xmin = tgw_median$ci.lb, xmax = tgw_median$ci.ub, y = 0), 
    #                color = "black", size = 1) + 
    theme_minimal() + 
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)) + 
    geom_boxplot(aes(x = age, y = 0.19), width = 0.03) +
    labs(x = "", y = "")) + 
  theme(aspect.ratio = 1)


# CFSW Age distributions
cfsw_agedist <- (dat %>%
    filter(kp == "CFSW") %>% 
    ggplot() + 
    geom_density(aes(x = age, color = survey_id), show.legend = F) + 
    # geom_point(aes(x = cfsw_median$beta, y = 0), color = "black", size = 2) + 
    # geom_linerange(aes(xmin = cfsw_median$ci.lb, xmax = cfsw_median$ci.ub, y = 0), 
    #                color = "black", size = 1) + 
    theme_minimal() + 
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)) + 
    geom_boxplot(aes(x = age, y = 0.18), width = 0.03) +
    labs(x = "", y = "")) + 
  theme(aspect.ratio = 1)

# PWID Age distributions
pwid_agedist <- (dat %>%
    filter(kp == "PWID") %>% 
    ggplot() + 
    geom_density(aes(x = age, color = survey_id), show.legend = F) + 
    # geom_point(aes(x = pwid_median$beta, y = 0), color = "black", size = 2) + 
    # geom_linerange(aes(xmin = pwid_median$ci.lb, xmax = pwid_median$ci.ub, y = 0), 
    #                color = "black", size = 1) +
    theme_minimal() + 
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)) + 
    geom_boxplot(aes(x = age, y = 0.18), width = 0.03) +
    labs(x = "Age", y = "Density")) + 
  theme(aspect.ratio = 1)


# 

  
  




# Duration distributions

mediandur_dat <- dat %>% 
  filter(kp == "FSW", !survey_id == "ETH2020ACA_FSW") %>% 
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
  filter(kp == "FSW", !survey_id %in% c("ETH2020ACA_FSW", "MOZ2021BBS_FSW", "ZAF2019ACA_FSW", "SEN2019BBS_FSW", "BEN2002ACA_FSW", "BEN1993ACA_FSW", "BEN1998ACA_FSW")) %>% 
  select(survey_id, duration_estimate) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(duration_estimate, na.rm = TRUE), 
    q1.g1 = quantile(duration_estimate, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(duration_estimate, 0.75, na.rm = TRUE),
    sd.g1 = sd(duration_estimate, na.rm = T),
    mean.g1 = mean(duration_estimate, na.rm = T),
    min.g1 = min(duration_estimate, na.rm = T),
    max.g1 = max(duration_estimate, na.rm = T)
  ) %>% ungroup() %>% 
  filter(!is.na(med.g1))


fsw_mediandur <- metamedian(mediandur_dat)




msmdur_dat <- dat %>%
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
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
  select(survey_id, duration_estimate) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(duration_estimate, na.rm = TRUE), 
    q1.g1 = quantile(duration_estimate, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(duration_estimate, 0.75, na.rm = TRUE),
    sd.g1 = sd(duration_estimate, na.rm = T),
    mean.g1 = mean(duration_estimate, na.rm = T),
    min.g1 = min(duration_estimate, na.rm = T),
    max.g1 = max(duration_estimate, na.rm = T)
  ) %>% ungroup() %>% 
  filter(!is.na(med.g1))

# BEN2002ACA_FSW 
msm_mediandur <- metamedian(msmdur_dat)



tgdur_dat <- dat %>%
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
  filter(kp2 %in% c("TG", "TGW"),
         !survey_id %in%  c("MLI2014BBS_MSM", "NAM2019BBS_MSM")) %>% 
  mutate(duration_calc = age - age_fs_man,
         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
         duration_calc2 = age - age_fs_man_anal,
         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
  select(survey_id, duration_estimate) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(duration_estimate, na.rm = TRUE), 
    q1.g1 = quantile(duration_estimate, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(duration_estimate, 0.75, na.rm = TRUE),
    sd.g1 = sd(duration_estimate, na.rm = T),
    mean.g1 = mean(duration_estimate, na.rm = T),
    min.g1 = min(duration_estimate, na.rm = T),
    max.g1 = max(duration_estimate, na.rm = T)
  ) %>% ungroup() %>% 
  filter(!is.na(med.g1) )


tgw_mediandur <- metamedian(tgdur_dat)



pwiddur_dat <- dat %>%
  filter(kp == "PWID") %>% 
  mutate(duration_calc = age - age_inject,
         duration_calc = ifelse(duration_calc < 0, NA_integer_, duration_calc),
         duration_estimate = ifelse(is.na(duration_calc), inject_dur, duration_calc)) %>% 
  select(survey_id, duration_estimate) %>% 
  group_by(survey_id) %>%
  summarise(
    n.g1 = n(),  # Sample size
    med.g1 = median(duration_estimate, na.rm = TRUE), 
    q1.g1 = quantile(duration_estimate, 0.25, na.rm = TRUE),  
    q3.g1 = quantile(duration_estimate, 0.75, na.rm = TRUE),
    sd.g1 = sd(duration_estimate, na.rm = T),
    mean.g1 = mean(duration_estimate, na.rm = T),
    min.g1 = min(duration_estimate, na.rm = T),
    max.g1 = max(duration_estimate, na.rm = T)
  ) %>% ungroup() %>% 
  filter(!is.na(med.g1))


pwid_mediandur <- metamedian(pwiddur_dat)





# Duration plots

fsw_duration <- dat %>% 
  filter(kp == "FSW", !survey_id == "ETH2020ACA_FSW") %>% 
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
  ggplot() + 
  geom_density(aes(x = duration_estimate, color = survey_id), show.legend = F) + 
  lims(x = c(0, 30))  + 
  # geom_point(aes(x = fsw_mediandur$beta, y = 0), color = "black", size = 2) + 
  # geom_linerange(aes(xmin = fsw_mediandur$ci.lb, xmax = fsw_mediandur$ci.ub, y = 0), 
  #                color = "black", size = 1) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) + 
  geom_boxplot(aes(x = duration_estimate, y = 0.4), width = 0.03) +
  labs(x = "", y = "") + 
  theme(aspect.ratio = 1)


msm_duration <- dat %>%
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
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
  ggplot() + 
  geom_density(aes(x = duration_estimate, color = survey_id), show.legend = F) + 
  lims(x = c(0, 35))  +
  # geom_point(aes(x = msm_mediandur$beta, y = 0), color = "black", size = 2) + 
  # geom_linerange(aes(xmin = msm_mediandur$ci.lb, xmax = msm_mediandur$ci.ub, y = 0), 
  #                color = "black", size = 1) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) + 
  geom_boxplot(aes(x = duration_estimate, y = 0.4), width = 0.03) +
  labs(x = "", y = "") +
  theme(aspect.ratio = 1)


blank_duration <- dat %>%
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
  filter(kp2 %in% c("TG", "TGW"), !iso3 == "NAM") %>% 
  mutate(duration_calc = age - age_fs_man,
         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
         duration_calc2 = age - age_fs_man_anal,
         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
  mutate(duration_estimate = 0) %>% 
  ggplot() + 
  # geom_density(aes(x = duration_estimate, color = survey_id), show.legend = F) + 
  lims(x = c(0, 40))  +
  # geom_point(aes(x = tgw_mediandur$beta, y = 0), color = "black", size = 2) + 
  # geom_linerange(aes(xmin = tgw_mediandur$ci.lb, xmax = tgw_mediandur$ci.ub, y = 0), 
  #                color = "black", size = 1) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) + 
  geom_boxplot(aes(x = duration_estimate, y = 0.4), width = 0.03) +
  labs(x = "", y = "") + 
  theme(aspect.ratio = 1)


pwid_duration <- dat %>%
  filter(kp == "PWID") %>% 
  mutate(duration_calc = age - age_inject,
         duration_calc = ifelse(duration_calc < 0, NA_integer_, duration_calc),
         duration_estimate = ifelse(is.na(duration_calc), inject_dur, duration_calc)) %>%
  ggplot() + 
  geom_density(aes(x = duration_estimate, color = survey_id), show.legend = F) + 
  lims(x = c(0, 40))  +
  # geom_point(aes(x = pwid_mediandur$beta, y = 0), color = "black", size = 2) + 
  # geom_linerange(aes(xmin = pwid_mediandur$ci.lb, xmax = pwid_mediandur$ci.ub, y = 0), 
  #                color = "black", size = 1) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) + 
  geom_boxplot(aes(x = duration_estimate, y = 0.4), width = 0.03) +
  labs(x = "Duration", y = "Density") + 
  theme(aspect.ratio = 1)



# 


distribution_plot <- dat %>%
  filter(kp == "PWID") %>% 
  bind_rows(dat %>%
              filter(kp == "CFSW") ) %>% 
  bind_rows(dat %>%
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
              filter(kp2 %in% c("TG", "TGW")) %>% 
              mutate(kp = "TGW")) %>% 
  bind_rows(dat %>%
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
              mutate(kp = "MSM")) %>% 
  bind_rows(dat %>% 
              filter(kp == "FSW", !iso3 %in% c("ETH"))) %>% 
  mutate(distribution = "Age") %>%
  rename(variable = age) %>% 
  bind_rows(
    dat %>%
              filter(kp == "PWID") %>% 
              mutate(duration_calc = age - age_inject,
                     duration_calc = ifelse(duration_calc < 0, NA_integer_, duration_calc),
                     duration_estimate = ifelse(is.na(duration_calc), inject_dur, duration_calc)) %>%
      bind_rows(dat %>%
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
                  filter(kp2 %in% c("TG", "TGW"), !iso3 == "NAM") %>% 
                  mutate(duration_calc = age - age_fs_man,
                         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
                         duration_calc2 = age - age_fs_man_anal,
                         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
                         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
                  mutate(duration_estimate = NA_integer_,
                         kp = "TGW")) %>% 
      bind_rows(dat %>%
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
                  filter(kp2 %in% c("TG", "TGW"), !iso3 == "NAM") %>% 
                  mutate(duration_calc = age - age_fs_man,
                         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
                         duration_calc2 = age - age_fs_man_anal,
                         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
                         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
                  mutate(duration_estimate = NA_integer_,
                         kp = "CFSW")) %>% 
      bind_rows(dat %>% 
                  filter(kp == "FSW", !survey_id == "ETH2020ACA_FSW") %>% 
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
                         duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate))) %>% 
      bind_rows(dat %>%
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
                         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
                  mutate(kp = "MSM")) %>% 
      mutate(distribution = "Duration") %>% 
      rename(variable = duration_estimate) 
    ) %>% 
  select(survey_id, kp, iso3, year, variable, distribution)  %>% 
  mutate(kp = factor(kp, levels = c("FSW", "MSM", "PWID", "TGW", "CFSW"))) %>% 
  mutate(x_min = ifelse(distribution == "Age", 15, 0),
         x_max = ifelse(distribution == "Age", 60, 40))

max_density <- distribution_plot %>%
  left_join(moz.utils::region()) %>% 
  group_by(kp, distribution, region) %>%
  filter(!is.na(variable)) %>% 
  summarise( median_val = median(variable, na.rm = TRUE),
             iqr_low = quantile(variable, 0.25, na.rm = TRUE),
             iqr_high = quantile(variable, 0.75, na.rm = TRUE),
             max_dens = max(density(variable, na.rm = TRUE)$y, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(boxplot_y = max_dens + 0.05) %>% 
  mutate(boxplot_y = ifelse(distribution == "Duration" & kp == "FSW", 0.4, boxplot_y),
         boxplot_y = ifelse(distribution == "Age" & kp == "FSW", 0.125, boxplot_y),
         boxplot_y = ifelse(distribution == "Age" & kp == "PWID", 0.125, boxplot_y),
         boxplot_y = ifelse(distribution == "Age" & kp == "CFSW", 0.125, boxplot_y),
         boxplot_y = ifelse(distribution == "Age" & kp == "TGW", 0.2, boxplot_y),
         boxplot_y = ifelse(distribution == "Duration" & kp == "MSM", 0.3, boxplot_y),
         boxplot_y = ifelse(distribution == "Age" & kp == "MSM", 0.2, boxplot_y))


# Region values

region_dist_iqrs <- distribution_plot %>%
  left_join(moz.utils::region()) %>% 
  group_by(kp, distribution, region) %>%
  filter(!is.na(variable)) %>% 
  summarise( median_val = median(variable, na.rm = TRUE),
             iqr_low = quantile(variable, 0.25, na.rm = TRUE),
             iqr_high = quantile(variable, 0.75, na.rm = TRUE),
             max_dens = max(density(variable, na.rm = TRUE)$y, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`Median, IQR` = paste0(median_val, "(", iqr_low, "-", iqr_high, ")")) %>% 
  select(distribution, kp, region, `Median, IQR`) %>% 
  arrange(distribution, kp) %>% 
  pivot_wider(names_from = "region", values_from = `Median, IQR`)

write_csv(region_dist_iqrs, "~/Downloads/region_dist_iqrs.csv", na = "")
library(ggnewscale)

#Age dist 
age_plots <- distribution_plot %>% 
    left_join(max_density) %>%
    filter(distribution == "Age") %>% 
  moz.utils::name_kp() %>% 
  ggplot() + 
    geom_density(aes(x = variable, color = survey_id, alpha = 0.3), linewidth = 0.2, show.legend = FALSE) +
    scale_color_grey() +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) + 
    new_scale_color() +
    # geom_pointrange(aes(x = median_val, xmin = iqr_low, xmax = iqr_high, y = boxplot_y, color = kp), size = 0.25, show.legend = F) +
    # geom_text(aes(x = median_val, y = boxplot_y, label = round(median_val, 1), color = kp), 
    #           vjust = -0.5, size = 4, fontface = "bold", show.legend = F) +
  geom_pointrange(aes(x = median_val, xmin = iqr_low, xmax = iqr_high, y = 0.15), color = "#4D251D", size = 0.25, show.legend = F) +
  geom_text(aes(x = median_val, y = 0.15, label = round(median_val, 1)), color = "#4D251D",
            vjust = -0.5, size = 5, fontface = "plain", show.legend = F) +
    # scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738",
    #                               "TGW" = "#195972", "CFSW" = "#4D251D")) +
  labs(x = "Years", y = "")  +
    lims(x = c(15, 60)) +
    coord_cartesian(ylim = c(0, 0.20)) + 
  # facet_grid(kp ~ distribution, scales = c("free")) +
    facet_wrap(~kp, ncol = 1, scales = "free_y") +
    scale_y_continuous(limits = c(0, 0.25), labels = scales::percent) +
    moz.utils::standard_theme() +
  theme(aspect.ratio = 1) + 
  ggtitle("Age") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,0.2,0,1), 'lines'))
  
  
  duration_plots <- distribution_plot %>% 
    left_join(max_density) %>%
    filter(distribution == "Duration") %>%
    moz.utils::name_kp() %>% 
    ggplot() + 
    geom_density(aes(x = variable, color = survey_id), alpha = 0.5, linewidth = 0.2, show.legend = FALSE) +
    scale_color_grey() +
    theme_minimal() + 
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)
          ) + 
    new_scale_color() +
    # geom_pointrange(aes(x = median_val, xmin = iqr_low, xmax = iqr_high, y = boxplot_y, color = kp), size = 0.25, show.legend = F) +
    # geom_text(aes(x = median_val, y = boxplot_y, label = round(median_val, 1), color = kp), 
    #           vjust = -0.5, size = 4, fontface = "bold", show.legend = F) +
    # geom_pointrange(aes(x = median_val, xmin = iqr_low, xmax = iqr_high, y = 0.15, color = kp), size = 0.25, show.legend = F) +
    # geom_text(aes(x = median_val, y = 0.15, label = round(median_val, 1), color = kp), 
    #           vjust = -0.5, size = 5, fontface = "plain", show.legend = F) +
    # geom_pointrange(aes(x = median_val, xmin = iqr_low, xmax = iqr_high, y = 0.15, color = kp), size = 0.25, show.legend = F) +
    geom_pointrange(aes(x = median_val, xmin = iqr_low, xmax = iqr_high, y = 0.15), color = "#4D251D", size = 0.25, show.legend = F) +
    geom_text(aes(x = median_val, y = 0.15, label = round(median_val, 1)), color = "#4D251D", vjust = -0.5, size = 5, fontface = "plain", show.legend = F) +
    # scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738",
    #                               "TGW" = "#195972", "CFSW" = "#4D251D")) +
    labs(x = "Years", y = "")  +
    lims(x = c(0, 30)) +
    coord_cartesian(ylim = c(0,0.20)) + 
    facet_wrap(~kp, ncol = 1, scales = "free_y", strip.position = "right") +
    scale_y_continuous(limits = c(0, 0.5), labels = scales::percent) +
    moz.utils::standard_theme() +
    theme(aspect.ratio = 1,
          panel.grid = element_blank()) + 
    geom_text(data = distribution_plot %>% 
                moz.utils::name_kp() %>% 
                filter(kp %in% c("Clients of female\nsex workers"), distribution == "Duration") %>%
                distinct(kp) %>% 
                mutate(x = 15, y = 0.10, label = "No duration \ninformation \navailable"),
              aes(x = x, y = y, label = label), color = "#4D251D",
              size = 5, fontface = "bold", show.legend = F) + 
    geom_text(data = distribution_plot %>% 
                moz.utils::name_kp() %>% 
                filter(kp %in% c("Transgender\nwomen"), distribution == "Duration") %>%
                distinct(kp) %>% 
                mutate(x = 15, y = 0.10, label = "NA"),
              aes(x = x, y = y, label = label), color = "#4D251D",
              size = 5, fontface = "bold", show.legend = F) + 
    ggtitle("Duration") + 
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.margin = unit(c(0,0.2,0,1), 'lines'),
          axis.text.y = element_blank(),
          strip.text = element_text(size = 10, face = "bold"))
    # theme(strip.text = element_text(size = 14, face = "bold",
    #                                 color =kp))


  # geom_boxplot(aes(x = variable, y = boxplot_y, color = kp), width = 0.03, show.legend = F) +

age_dur_dist <- ggpubr::ggarrange(age_plots, duration_plots, ncol = 2, legend = "none")

png("figs/age_dur_dist_2703.png", width = 450, height = 800, units = "px")
age_dur_dist
dev.off()

library(patchwork)

# Create labels for population groups as vertical bars
fsw_label <- wrap_elements(full = ggplot() + 
                             annotate("text", x = 1, y = 0.5, label = "FSW", angle = 90, size = 4, fontface = "bold") + 
                             theme_void())

msm_label <- wrap_elements(full = ggplot() + 
                             annotate("text", x = 1, y = 0.5, label = "MSM", angle = 90, size = 4, fontface = "bold") + 
                             theme_void())
tgw_label <- wrap_elements(full = ggplot() + 
                             annotate("text", x = 1, y = 0.5, label = "TGW", angle = 90, size = 4, fontface = "bold") + 
                             theme_void())

pwid_label <- wrap_elements(full = ggplot() + 
                              annotate("text", x = 1, y = 0.5, label = "PWID", angle = 90, size = 4, fontface = "bold") + 
                              theme_void())

cfsw_label <- wrap_elements(full = ggplot() + 
                              annotate("text", x = 1, y = 0.5, label = "CFSW", angle = 90, size = 4, fontface = "bold") + 
                              theme_void())

label_plot <- function(label) {
  wrap_elements(full = ggplot() + 
                  annotate("text", x = 1, y = 0.5, label = label, angle = 90, size = 6, fontface = "bold") + 
                  theme_void())
}

fsw_label <- label_plot("FSW")
msm_label <- label_plot("MSM")
tgw_label <- label_plot("TGW")
cfsw_label <- label_plot("CFSW")
pwid_label <- label_plot("PWID")

# Arrange plots into a 2-column layout
 (fsw_label | fswage_dist | fsw_duration) / (msm_label | msm_agedist | msm_duration) / (tgw_label | tgw_agedist | blank_duration) / (cfsw_label | cfsw_agedist | blank_duration) / (pwid_label | pwid_agedist | pwid_duration) 



(fswage_dist | fsw_duration) / (msm_agedist | msm_duration) / (tgw_agedist | blank_duration) / (cfsw_agedist | blank_duration) / (pwid_agedist | pwid_duration) 


# Prevalence by age 


dat %>% 
  filter(kp == "FSW") %>% 
  select(survey_id, age, hiv) %>% 
  single_year_to_five_year() %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id) %>% 
  mutate(prev = mean(hiv)) %>% 
  ungroup() %>% 
  group_by(survey_id, prev, age_group) %>% 
  summarise(age_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(ratio = age_prev/prev) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(moz.utils::region()) %>% 
  ggplot() + 
  geom_boxplot(aes(x = age_group, y = ratio)) + 
  geom_point(aes(x = age_group, y = ratio, color = region)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) + 
  geom_hline(aes(yintercept = 1), color = "darkred") + 
  labs(x = "Age Group", y = "Prevalence ratio (age group prev /total study prev)")



dat %>%
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
  select(survey_id, age, hiv) %>% 
  single_year_to_five_year() %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, prev, denom, age_group) %>% 
  summarise(age_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(ratio = age_prev/prev) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(moz.utils::region()) %>% 
  ggplot() + 
  geom_boxplot(aes(x = age_group, y = ratio)) + 
  geom_point(aes(x = age_group, y = ratio, color = region, size = denom/2)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) + 
  geom_hline(aes(yintercept = 1), color = "darkred") + 
  labs(x = "Age Group", y = "Prevalence ratio (age group prev /total study prev)")


dat %>%
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
  filter(kp2 %in% c( "TG", "TGW"), !iso3 == "NAM") %>% 
  select(survey_id, age, hiv) %>% 
  single_year_to_five_year() %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, prev, denom, age_group) %>% 
  summarise(age_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(ratio = age_prev/prev) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(moz.utils::region()) %>% 
  ggplot() + 
  geom_boxplot(aes(x = age_group, y = ratio)) + 
  geom_point(aes(x = age_group, y = ratio, color = region, size = denom/2)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) + 
  geom_hline(aes(yintercept = 1), color = "darkred") + 
  labs(x = "Age Group", y = "Prevalence ratio (age group prev /total study prev)")


dat %>% 
  filter(kp == "PWID") %>% 
  select(survey_id, age, hiv) %>% 
  single_year_to_five_year() %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, prev, denom, age_group) %>% 
  summarise(age_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(ratio = age_prev/prev) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(moz.utils::region()) %>% 
  ggplot() + 
  geom_boxplot(aes(x = age_group, y = ratio)) + 
  geom_point(aes(x = age_group, y = ratio, color = region, size = denom/2)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) + 
  geom_hline(aes(yintercept = 1), color = "darkred") + 
  labs(x = "Age Group", y = "Prevalence ratio (age group prev /total study prev)")

dat %>% 
  filter(kp == "CFSW") %>% 
  select(survey_id, age, hiv) %>% 
  single_year_to_five_year() %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, prev, denom, age_group) %>% 
  summarise(age_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(ratio = age_prev/prev) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(moz.utils::region()) %>% 
  ggplot() + 
  geom_boxplot(aes(x = age_group, y = ratio)) + 
  geom_point(aes(x = age_group, y = ratio, color = region, size = denom/2)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) + 
  geom_hline(aes(yintercept = 1), color = "darkred") + 
  labs(x = "Age Group", y = "Prevalence ratio (age group prev /total study prev)") + 
  lims(y = c(0,8))


# Prev by duration

dat %>% 
  filter(kp == "FSW", !survey_id == "ETH2020ACA_FSW") %>% 
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
  mutate(duration_cat = case_when(duration_estimate <= 1 ~ "<=1 year",
                                  duration_estimate == 2 ~ "2 years",
                                  duration_estimate == 3 ~ "3 years",
                                  duration_estimate == 4 ~ "4 years",
                                  duration_estimate == 5 ~ "5 years",
                                  duration_estimate > 5 & duration_estimate < 10 ~ "6-9 years",
                                  duration_estimate >= 10 ~ "Over 10 years")) %>% 
  filter(!is.na(duration_cat)) %>% 
  group_by(survey_id) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, prev, denom, duration_cat) %>% 
  summarise(dur_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(ratio = dur_prev/prev) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(moz.utils::region()) %>% 
  ggplot() + 
  geom_boxplot(aes(x = duration_cat, y = ratio)) + 
  geom_point(aes(x = duration_cat, y = ratio, color = region, size = denom/2)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) + 
  geom_hline(aes(yintercept = 1), color = "darkred") + 
  labs(x = "Duration Cat", y = "Prevalence ratio (duration category prev /total study prev)")


dat %>%
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
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
mutate(duration_cat = case_when(duration_estimate <= 1 ~ "<=1 year",
                                duration_estimate == 2 ~ "2 years",
                                duration_estimate == 3 ~ "3 years",
                                duration_estimate == 4 ~ "4 years",
                                duration_estimate == 5 ~ "5 years",
                                duration_estimate > 5 & duration_estimate < 10 ~ "6-9 years",
                                duration_estimate >= 10 ~ "Over 10 years")) %>% 
  filter(!is.na(duration_cat)) %>% 
  group_by(survey_id) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, prev, denom, duration_cat) %>% 
  summarise(dur_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(ratio = dur_prev/prev) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(moz.utils::region()) %>% 
  ggplot() + 
  geom_boxplot(aes(x = duration_cat, y = ratio)) + 
  geom_point(aes(x = duration_cat, y = ratio, color = region, size = denom/2)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) + 
  geom_hline(aes(yintercept = 1), color = "darkred") + 
  labs(x = "Duration Cat", y = "Prevalence ratio (duration category prev /total study prev)")


dat %>%
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
  filter(kp2  %in% c("TG", "TGW")) %>% 
  mutate(duration_calc = age - age_fs_man,
         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
         duration_calc2 = age - age_fs_man_anal,
         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc)) %>% 
  mutate(duration_cat = case_when(duration_estimate <= 1 ~ "<=1 year",
                                  duration_estimate == 2 ~ "2 years",
                                  duration_estimate == 3 ~ "3 years",
                                  duration_estimate == 4 ~ "4 years",
                                  duration_estimate == 5 ~ "5 years",
                                  duration_estimate > 5 & duration_estimate < 10 ~ "6-9 years",
                                  duration_estimate >= 10 ~ "Over 10 years")) %>% 
  filter(!is.na(duration_cat)) %>% 
  group_by(survey_id) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, prev, denom, duration_cat) %>% 
  summarise(dur_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(ratio = dur_prev/prev) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(moz.utils::region()) %>% 
  ggplot() + 
  geom_boxplot(aes(x = duration_cat, y = ratio)) + 
  geom_point(aes(x = duration_cat, y = ratio, color = region, size = denom/2)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) + 
  geom_hline(aes(yintercept = 1), color = "darkred") + 
  labs(x = "Duration Cat", y = "Prevalence ratio (duration category prev /total study prev)")



###### Age over time

age_dat <- dat %>%
  filter(kp == "PWID") %>% 
  bind_rows(dat %>%
              filter(kp == "CFSW") ) %>% 
  bind_rows(dat %>%
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
              filter(kp2 %in% c("TG", "TGW")) %>% 
              mutate(kp = "TGW")) %>% 
  bind_rows(dat %>%
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
              mutate(kp = "MSM")) %>% 
  bind_rows(dat %>% 
              filter(kp == "FSW", !iso3 %in% c("ETH"))) %>% 
  select(survey_id, kp, kp2, iso3, year, sex, age) %>% 
  mutate(kp2 = ifelse(is.na(kp2), kp, kp2)) %>% 
  mutate(sex = case_when(kp2 %in% c("FSW", "TGW") ~ 1,
                         kp2 %in% c("MSM", "CFSW", "TG") ~ 0,
                         TRUE ~ sex)) %>% 
  filter(!is.na(age),
         !is.na(sex)) %>% 
  group_by(survey_id) %>% 
  mutate(denom = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, denom, kp, year, iso3, sex) %>% 
  summarise(median_age = median(age)) %>% 
  ungroup() %>% 
  mutate(sex = ifelse(sex == 0, "male", "female"))

spectrum_dat %>% 
  group_by(iso3, year, )


genpop_median_ages <- spectrum_dat %>%
  filter(year %in% dat$year,
         iso3 %in% dat$iso3) %>% 
  select(iso3, year, age, sex, totpop) %>% 
  group_by(iso3, year, sex) %>%
  summarise(median_age = median(rep(age, totpop), na.rm = TRUE)) %>% 
  ungroup()

age_diff_plot <-  age_dat %>% rename(median_kp_age = median_age) %>% left_join(genpop_median_ages) %>% mutate(age_diff = median_kp_age - median_age) %>% mutate(sex = ifelse(kp == "TGW", "female", sex)) %>% mutate(kp = factor(kp, levels = c("FSW", "MSM", "PWID", "TGW", "CFSW"))) %>% 
  mutate(sex = ifelse(kp != "PWID", 0, sex)) %>% 
  moz.utils::name_kp() %>% 
  ggplot() + 
  geom_point(aes(x = year, y = age_diff, color = sex, size = denom), show.legend = T, alpha = 0.2, shape = 16) + 
  # moz.utils::scale_manual(type = F, n = 2) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = "")) +
  facet_wrap(~kp, ncol = 1, strip.position = "right") + 
  # new_scale_color() +
  # geom_smooth(aes(x = year, y = age_diff, group = interaction(kp, sex), weight = denom, color = sex), method = "lm") +
  geom_smooth(aes(x = year, y = age_diff, group = interaction(kp, sex), weight = denom, color = sex), method = "lm", show.legend = F) +
   scale_color_manual(values = c("black","violetred", "royalblue4")) +
  moz.utils::standard_theme() + 
  geom_hline(yintercept = 0, color = "darkred", linetype = 3) +
  labs(x = "Year", y = "Age difference (years)", size = "Survey Denominator", color = "Sex") + 
  theme(aspect.ratio = 1) +
  # guides(color = "none") +
  ggtitle(label = "Difference between median KP age and \nmedian total population age")  +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        strip.text = element_text(size = 10.5),
        axis.title.y = element_text(size = 10.5),
        axis.title.x = element_text(size = 10.5))


median_age_plot <- age_dat %>% 
  rename(median_kp_age = median_age) %>% 
  left_join(genpop_median_ages) %>% 
  mutate(age_diff = median_kp_age - median_age) %>% 
  mutate(sex = ifelse(kp == "TGW", "female", sex)) %>% 
  mutate(kp = factor(kp, levels = c("FSW", "MSM", "PWID", "TGW", "CFSW"))) %>% 
  mutate(sex = ifelse(kp != "PWID", 0, sex)) %>% 
  moz.utils::name_kp() %>%
  ggplot() + 
  geom_point(aes(x = year, y = median_kp_age, size = denom, color = sex), alpha = 0.2, shape = 16) + 
  geom_smooth(aes(x = year, y = median_kp_age, weight = denom, color = sex, group = interaction(kp, sex)), method = "lm") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = "")) +
  facet_wrap(~kp, ncol = 1) +
  moz.utils::standard_theme() + 
  scale_color_manual(values = c( "black","violetred", "royalblue4")) +
  # moz.utils::scale_manual(type = F, n = 5) + 
  # guides(color = "none") +
  theme(aspect.ratio = 1,
        strip.text = element_blank()) +
  labs(x= "Year", y = "Age (Years)", size = "Survey Denominator", color = "Sex") +
  ggtitle(label = "Median \nAge") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10.5) ,
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        axis.title.y = element_text(size = 10.5),
        axis.title.x = element_text(size = 10.5))

ggpubr::ggarrange(median_age_plot, age_diff_plot, common.legend = T, legend = "bottom")
