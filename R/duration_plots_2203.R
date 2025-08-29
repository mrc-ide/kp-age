library(tidyverse)
library(ggpubr)
library(moz.utils)
library(ggnewscale)

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


duration_dat <- dat %>% 
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
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate),
         kp2 = "FSW") %>%
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
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc))) %>% 
  # bind_rows(
  #   dat %>%
  #     filter(kp %in% c("MSM", "MSMTG", "TG", "TGW")) %>% 
  #     mutate(kp2 = case_when(kp == "MSMTG" & gender %in% c("female", "non-binary", "tgw") ~ "TGW",
  #                            kp == "MSMTG" & gender %in% c("male") ~ "MSM",
  #                            kp == "MSM" & gender %in% c("female", "TGW", "other", "3", "5") ~ "TGW",
  #                            kp == "MSM" & gender %in% c("male", "0", "1") ~ "MSM",
  #                            kp == "TG" & gender %in% c(0, 4) ~ "TGM",
  #                            kp == "TG" & gender == c(1, 3) ~ "TGW", 
  #                            kp == "TG" & sex == 1 ~ "TGM", 
  #                            kp == "TG" & sex == 0 ~ "TGW",
  #                            TRUE ~ kp)) %>% 
  #     filter(kp2 %in% c("TG", "TGW"),
  #            !survey_id %in%  c("MLI2014BBS_MSM", "NAM2019BBS_MSM")) %>% 
  #     mutate(duration_calc = age - age_fs_man,
  #            duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
  #            duration_calc2 = age - age_fs_man_anal,
  #            duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
  #            duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc),
  #            duration_estimate = NA_integer_,
  #            kp2 = "TGW")) %>% 
  bind_rows(dat %>%
              filter(kp == "PWID") %>% 
              mutate(duration_calc = age - age_inject,
                     duration_calc = ifelse(duration_calc < 0, NA_integer_, duration_calc),
                     duration_estimate = ifelse(is.na(duration_calc), inject_dur, duration_calc),
                     kp2 = "PWID"
                     )
               ) %>% 
  select(survey_id, kp2, iso3, year, age, duration_estimate, hiv) %>% 
  left_join(moz.utils::region()) %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id) %>% 
  mutate(survey_prev = mean(hiv))
  


pwiddur_dat <- dat %>%
  filter(kp == "PWID") %>% 
  mutate(duration_calc = age - age_inject,
         duration_calc = ifelse(duration_calc < 0, NA_integer_, duration_calc),
         duration_estimate = ifelse(is.na(duration_calc), inject_dur, duration_calc),
         kp2 = "PWID") %>% 
  select(age, duration_estimate)

pwid_cortest <- data.frame(kp2 = "PWID" , cortest = cor.test(pwiddur_dat$age, pwiddur_dat$duration_estimate)$estimate)

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
  select(age, duration_estimate)

msm_cortest <- data.frame(kp2 = "MSM" , cortest = cor.test(msmdur_dat$age, msmdur_dat$duration_estimate)$estimate)


fswdur_dat <- dat %>% 
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
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate),
         kp2 = "FSW") %>% 
  select(age, duration_estimate)

fsw_cortest <- data.frame(kp2 = "FSW" , cortest = cor.test(fswdur_dat$age, fswdur_dat$duration_estimate)$estimate)


duration_dat %>% 
  left_join(pwid_cortest %>% bind_rows(msm_cortest) %>% bind_rows(fsw_cortest)) %>% 
  ggplot() +
  geom_point(aes(x = age, y = duration_estimate, color = region), alpha = 0.7) + 
  geom_smooth(aes(x = age, y = duration_estimate, group = kp2), method = "lm", color = "#F98F5B") +
  geom_text(aes(x = 25, y = 50, label = paste0("PMCC: ", round(cortest, 2))), size = 4, show.legend = F) +
  facet_wrap(~kp2) +
  scale_color_manual(values = c("#11304F", "#989B6C")) +
  moz.utils::standard_theme() +
  theme(aspect.ratio = 1) + 
  labs(x = "Age", y = "Reported Duration")


# stratified by years
duration_dat %>% 
  filter(!is.na(hiv)) %>% 
  mutate(yearcat = case_when(year < 2001 ~ "1991-2000",
                             year > 2000 & year < 2011 ~ "2001-2010",
                             year > 2010 ~ "2011-present")) %>% 
  group_by(survey_id, kp2, iso3, year, yearcat, region, duration_estimate) %>% 
  summarise(prev = mean(hiv), denom = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(x = duration_estimate, y = prev, color = kp2, size = denom), show.legend = F) + 
  # geom_smooth(aes(x = duration_estimate, y = prev, group = interaction(kp2, yearcat)), method = "lm", color = "#F98F5B") +
  facet_grid(region+yearcat~kp2) +
  scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738")) +
  moz.utils::standard_theme() +
  theme(aspect.ratio = 1) + 
  labs(x = "Reported Duration (Years)", y = "HIV Prevalence")



quantiles_hivduration_region <- duration_dat %>%
  mutate(duration_estimate = round(duration_estimate, 0)) %>% 
  # mutate(duration_estimate = ifelse(duration_estimate < 0.5, 0, duration_estimate),
  #        duration_estimate = ifelse(duration_estimate >= 0.5 & duration_estimate <= 1, 1, duration_estimate)) %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id, kp2, iso3, year, duration_estimate, region) %>% 
  mutate(prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(hivratio = prev/survey_prev) %>% 
  group_by(kp2, duration_estimate, region) %>%
  filter(!is.na(hivratio)) %>% 
  summarise( median_val = median(hivratio, na.rm = TRUE),
             iqr_low = quantile(hivratio, 0.25, na.rm = TRUE),
             iqr_high = quantile(hivratio, 0.75, na.rm = TRUE)) 


duration_dat %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id, kp2, iso3, year, duration_estimate, region) %>% 
  mutate(prev = mean(hiv), denom = n()) %>% 
  ungroup() %>% 
  mutate(hivratio = prev/survey_prev) %>% 
  left_join(quantiles_hivduration_region) %>% 
  ggplot() + 
  geom_pointrange(aes(x = duration_estimate, y = median_val, ymin = iqr_low, ymax = iqr_high, color = kp2), show.legend = F) +
  facet_grid(region~kp2) + 
  coord_cartesian(ylim = c(0,10)) +
  moz.utils::standard_theme() +
  scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738")) +
  geom_hline(yintercept = 1, color = "#F98F5B") +
  labs(x = "Reported Duration", y = "Prevalence ratio \n (prev @ duration/study prev)")


quantiles_hivduration_region_cat <- duration_dat %>%
  ungroup() %>% 
  filter(!is.na(duration_estimate)) %>% 
  mutate(duration_estimate = round(duration_estimate, 0),
         duration_cat = factor(case_when(duration_estimate < 1 ~ "<1 year",
                                  duration_estimate == 1 ~ "1 year",
                                  duration_estimate > 1.5 & duration_estimate <= 2 ~ "2 years",
                                  duration_estimate >2.5 & duration_estimate <= 3 ~ "3 years",
                                  duration_estimate > 3 & duration_estimate < 4.5 ~ "4 years",
                                  duration_estimate >4.5 & duration_estimate <5.5 ~ "5 years",
                                  duration_estimate >5.5 & duration_estimate <6.5 ~ "6 years",
                                  duration_estimate >6.5 & duration_estimate <7.5 ~ "7 years",
                                  duration_estimate >7.5 & duration_estimate <8.5 ~ "8 years",
                                  duration_estimate >8.5 & duration_estimate <9.5 ~ "9 years",
                                  duration_estimate >9.5 & duration_estimate <10.5 ~ "10 years",
                                  duration_estimate >10.5 & duration_estimate <15.5 ~ "10-15 years",
                                  duration_estimate >=15.5  ~ ">15 years",
                                  TRUE ~ "Missing")),
         duration_cat = factor(duration_cat, levels = c("<1 year", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years", "10-15 years", ">15 years", "Missing"))) %>% 
  filter(!is.na(hiv)) %>% 
  # group_by(survey_id, kp2, iso3, year, duration_cat, region) %>% 
  group_by(survey_id, kp2, iso3, year, duration_cat) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  mutate(hivratio = prev/survey_prev) %>% 
  # group_by(kp2, duration_cat, region) %>%
  group_by(kp2, duration_cat) %>%
  filter(!is.na(hivratio)) %>% 
  summarise( median_val = median(hivratio, na.rm = TRUE),
             iqr_low = quantile(hivratio, 0.25, na.rm = TRUE),
             iqr_high = quantile(hivratio, 0.75, na.rm = TRUE)) 


duration_dat_plot <- duration_dat %>% 
  filter(!is.na(hiv)) %>% 
  filter(!is.na(duration_estimate)) %>% 
  mutate(duration_estimate = round(duration_estimate, 0),
         duration_cat = factor(case_when(duration_estimate < 1 ~ "<1 year",
                                         duration_estimate > 0.99 & duration_estimate <= 1.5 ~ "1 year",
                                         duration_estimate > 1.5 & duration_estimate <= 2 ~ "2 years",
                                         duration_estimate >2.5 & duration_estimate <= 3 ~ "3 years",
                                         duration_estimate > 3 & duration_estimate < 4.5 ~ "4 years",
                                         duration_estimate >4.5 & duration_estimate <5.5 ~ "5 years",
                                         duration_estimate >5.5 & duration_estimate <6.5 ~ "6 years",
                                         duration_estimate >6.5 & duration_estimate <7.5 ~ "7 years",
                                         duration_estimate >7.5 & duration_estimate <8.5 ~ "8 years",
                                         duration_estimate >8.5 & duration_estimate <9.5 ~ "9 years",
                                         duration_estimate >9.5 & duration_estimate <10.5 ~ "10 years",
                                         duration_estimate >10.5 & duration_estimate <15.5 ~ "10-15 years",
                                         duration_estimate >15.5  ~ ">15 years",
                                         TRUE ~ "Missing")),
         duration_cat = factor(duration_cat, levels = c("<1 year", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years", "10-15 years", ">15 years", "Missing"))) %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id, kp2, iso3, year, duration_cat) %>% 
  mutate(prev = mean(hiv),
         denom = n()) %>% 
  ungroup() %>% 
  mutate(hivratio = prev/survey_prev,
         hivratio = ifelse(hivratio == 0, 0.001, hivratio)) %>% 
  left_join(quantiles_hivduration_region_cat)
  

  
duration_dat_plot %>%   
  filter(!hivratio < 0.03) %>% 
  ggplot() + 
  geom_point(aes(x = hivratio, y = duration_cat, size = denom), color = "grey", alpha = 0.1, show.legend = F) +
  geom_vline(xintercept = 1, color = "darkred", linewidth = 0.75, linetype = "dotted") +
  geom_pointrange(aes(y = duration_cat, x = median_val, xmin = iqr_low, xmax = iqr_high, color = kp2), show.legend = F) +
  facet_wrap(~kp2) +
  # facet_grid(region~kp2) + 
  # coord_cartesian(xlim = c(0,3)) +
  scale_x_continuous(transform = "log", breaks = c(0, 0.3, 1, 3, 8)) +
  # coord_trans(x = "log") +
  moz.utils::standard_theme() +
  # scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738")) +
  moz.utils::scale_manual(type = F, n = 3) +
  labs(y = "Reported Duration", x = "Prevalence ratio, log scale \n (prev @ duration/study prev)") + 
  theme(aspect.ratio = 1,
        axis.text.x = element_text(size = 11)) 
  # scale_x_log10()

duration_dat_plot %>% 
  filter(kp2 == "PWID") %>% 
  ggplot() + 
  geom_point(aes(x = year, y = hivratio, color = survey_id), show.legend = T) +
  # geom_pointrange(aes(y = duration_cat, x = median_val, xmin = iqr_low, xmax = iqr_high, color = kp2), show.legend = F) +
  facet_wrap(~duration_cat) +
  # facet_grid(region~kp2) + 
  # coord_cartesian(xlim = c(0,3)) +
  # scale_x_continuous(transform = "log", breaks = c(0, 0.3, 1, 5, 10)) +
  # coord_trans(x = "log2") +
  moz.utils::standard_theme() +
  # scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738")) +
  # moz.utils::scale_manual(type = F, n = 3) +
  # geom_vline(xintercept = 1, color = "darkred", linewidth = 0.75) +
  labs(y = "Reported Duration", x = "Prevalence ratio \n (prev @ duration/study prev)") + 
  theme(aspect.ratio = 1,
        axis.text.x = element_text(size = 9)) 


duration_dat_plot %>%
  group_by(survey_id, kp2, year) %>% 
  mutate(median_duration = median(duration_estimate)) %>% 
  ungroup() %>% 
  group_by(survey_id, kp2, year, median_duration, duration_cat, denom, duration_estimate) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id) %>% 
  mutate(dur_prev = n/sum(n))  %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(x = year, y = dur_prev, color = duration_cat), show.legend = T) +
  # geom_pointrange(aes(y = duration_cat, x = median_val, xmin = iqr_low, xmax = iqr_high, color = kp2), show.legend = F) +
  facet_grid(kp2 ~ duration_cat) +
  # facet_grid(region~kp2) + 
  # coord_cartesian(xlim = c(0,3)) +
  # scale_x_continuous(transform = "log", breaks = c(0, 0.3, 1, 5, 10)) +
  # coord_trans(x = "log2") +
  moz.utils::standard_theme() +
  # scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738")) +
  # moz.utils::scale_manual(type = F, n = 3) +
  # geom_vline(xintercept = 1, color = "darkred", linewidth = 0.75) +
  labs(y = "Prevalence of duration category", x = "Year") + 
  theme(aspect.ratio = 1,
        axis.text.x = element_text(size = 9)) 



duration_model <- duration_dat_plot %>%
  group_by(survey_id, iso3, kp2, year) %>% 
  mutate(median_duration = median(duration_estimate),
         lower_duration = quantile(duration_estimate, 0.25),
         upper_duration = quantile(duration_estimate, 0.75),
         mean_duration = mean(duration_estimate),
         median_log_duration = median(log(duration_estimate))) %>% 
  ungroup() %>% 
  group_by(survey_id, iso3, kp2, year, mean_duration, median_duration, median_log_duration, lower_duration, upper_duration, duration_cat, denom, duration_estimate) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id) %>% 
  mutate(dur_prev = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(i.survey_id = multi.utils::to_int(survey_id))


fsw_inla_durationdat <- crossing(year = 1993:2023,
         denom = 1) %>% 
  bind_rows(duration_dat_plot %>% filter(kp2 == "FSW") %>% 
              filter(!duration_estimate == 0)) %>% 
  mutate(id.year = multi.utils::to_int(year)-1) 
  # mutate(duration_estimate = ifelse(duration_estimate == 0, 0.001, duration_estimate))

# log_x <- log(fsw_inla_durationdat$duration_estimate)
# qqnorm(log_x, main = "Q-Q Plot of Log-Transformed Duration Data, FSW")
# qqline(log_x, col = "red")

msm_inla_durationdat <- crossing(year = 2005:2023,
                                 denom = 1) %>% 
  bind_rows(duration_dat_plot %>% filter(kp2 == "MSM") %>% 
              filter(!duration_estimate == 0)) %>% 
  mutate(id.year = multi.utils::to_int(year)-1) 
  # mutate(duration_estimate = ifelse(duration_estimate == 0, 0.001, duration_estimate))

pwid_inla_durationdat <- crossing(year = 2005:2023,
                                 denom = 1) %>% 
  bind_rows(duration_dat_plot %>% filter(kp2 == "PWID") %>% 
              filter(!duration_estimate == 0))  %>% 
  mutate(id.year = multi.utils::to_int(year)-1) 
  # mutate(duration_estimate = ifelse(duration_estimate == 0, 0.001, duration_estimate))


duration_datlist <- list(fsw_inla_durationdat, msm_inla_durationdat,
                         pwid_inla_durationdat)

dur_model_summaries <- list()
predictions <- list()

for(i in seq_along(duration_datlist)){

  df <- duration_datlist[[i]]
  
  kp <- df %>% filter(!is.na(kp2)) %>% pull(kp2) %>% unique()
  
duration_formula <- duration_estimate ~ 1 + id.year + f(survey_id, model = "iid")

duration_mod <- INLA::inla(formula = duration_formula,
                           # E = denom,
                           family = "exponential",
                           # weights = denom,
                           # control.family = list(weights = denom),
                           data = df,
                           control.compute=list(config = TRUE, waic = T))      

dur_model_summaries[[i]] <- summary(duration_mod)


filt_df <- df %>%
  filter(!is.na(survey_id))

samples <- inla.posterior.sample(1000, duration_mod)

contents = duration_mod$misc$configs$contents

effect = "Predictor"
id.effect = which(contents$tag==effect)
ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])

ind.effect <- 1:(nrow(df) - nrow(filt_df))

samples.effect = lapply(samples, function(x) x$latent[ind.effect])

incidence_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)

ident <- df[ind.effect, ]

qtls <- apply(incidence_samples, 1, quantile, c(0.025, 0.5, 0.975))
mean_pred <- rowMeans(incidence_samples)

predicted_dat <- ident %>%
  ungroup() %>%
  mutate(
    lower = qtls[1,],
    median = qtls[2,],
    mean = mean_pred,
    upper = qtls[3,]
  ) %>% 
  mutate(exp_lower = exp(lower),
         exp_median = exp(median),
         exp_mean = exp(mean),
         exp_upper = exp(upper),
         kp = kp)

incidence_samples <- data.frame(incidence_samples) %>% rowid_to_column() #remove when reverting

means <- rowMeans(incidence_samples)
test <- data.frame(year = 1993:2023) %>%
  mutate(mean = exp(means)) %>%
  rowwise() %>%
  mutate(distn = list(diff(pexp(0:20, mean)))) %>%
                        unnest(distn)
                      
ident <- ident %>% rowid_to_column()  #remove when reverting
ident <- ident %>% left_join(incidence_samples)


predictions[[i]] <- predicted_dat




}


test %>% filter(year %in% c(2002, 2015, 2017)) %>% 
  rowid_to_column() %>%  
  mutate(rowid = ifelse(year == 2015, rowid - 20, rowid),
         rowid = ifelse(year == 2017, rowid - 40, rowid)) %>% 
  ggplot() +
  geom_density(data = (fswdur_dat %>% filter(year %in% c(2002, 2015, 2017))), aes(x = duration_estimate, color = survey_id)) +
  geom_line(aes(x = rowid, y = distn)) + 
  facet_wrap(~year) + 
  theme_minimal()

predictions %>% bind_rows() %>% dplyr::select(year, median, mean, lower, upper, exp_median, exp_mean, exp_lower, exp_upper, kp) %>% moz.utils::name_kp() %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = 1/exp_lower, ymax = 1/exp_upper), alpha = 0.3) +
  geom_line(aes(x = year, y = 1/exp_median)) +
  geom_line(aes(x = year, y = 1/exp_mean), color = "darkred") +
  # geom_pointrange(data = duration_model  %>% select(survey_id, iso3, kp = kp2, year, median_duration, lower_duration, upper_duration, denom) %>% distinct() %>% moz.utils::name_kp(), aes(y = median_duration, ymin = lower_duration, ymax = upper_duration , x = year, size = denom, color = iso3)) +
  geom_point(data = duration_model  %>% dplyr::select(survey_id, iso3, kp = kp2, year, mean_duration, median_log_duration, median_duration, lower_duration, upper_duration, denom) %>% distinct() %>% moz.utils::name_kp(), aes(y = mean_duration, x = year, size = denom, color = iso3)) +
  moz.utils::standard_theme() +
  facet_wrap(~kp) +
  # coord_cartesian(ylim = c(0, 12)) +
  scale_size_continuous(range = c(0.5, 3)) + 
  labs(y = "Duration, years", x = "Year", size = "Survey Sample Size") +
  guides(color = "none") + 
  theme(aspect.ratio = 1)

                        


# HIV Status by Duration

fsw_hivduration_df <- crossing(year = 1993:2023,
                               duration_estimate = 0:15,
                               denom = 1) %>% 
  bind_rows(duration_dat %>% group_by(survey_id, duration_estimate) %>% mutate(denom = n(), age_pos = sum(hiv)) %>% ungroup() %>% filter(kp2 == "FSW")) 

hivdur_datlist <- list(fsw_hivduration_df)
hivdur_model_summaries <- list()
hivdur_predictions <- list()

for(i in seq_along(hivdur_datlist)){
  
  df <- hivdur_datlist[[i]]
  
  kp <- df %>% filter(!is.na(kp2)) %>% pull(kp2) %>% unique()
  
hivdur_formula <- age_pos ~ 1 + duration_estimate*year + f(survey_id, model = "iid")

hivduration_mod <- INLA::inla(formula = hivdur_formula,
                           family = "binomial",
                           Ntrials = denom,
                           # offset = qlogis(survey_prev),
                           data = df,
                           control.compute=list(config = TRUE))          




hivdur_model_summaries[[i]] <- summary(hivduration_mod)


filt_df <- df %>%
  filter(!is.na(survey_id))

samples <- inla.posterior.sample(1000, hivduration_mod)

contents = hivduration_mod$misc$configs$contents

effect = "Predictor"
id.effect = which(contents$tag==effect)
ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])

ind.effect <- 1:(nrow(df) - nrow(filt_df))

samples.effect = lapply(samples, function(x) x$latent[ind.effect])

incidence_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)

ident <- df[ind.effect, ]

qtls <- apply(incidence_samples, 1, quantile, c(0.025, 0.5, 0.975))

predicted_dat <- ident %>%
  ungroup() %>%
  mutate(
    lower = qtls[1,],
    median = qtls[2,],
    upper = qtls[3,]
  ) %>% 
  mutate(exp_lower = qlogis(lower),
         exp_median = qlogis(median),
         exp_upper = qlogis(upper),
         kp = kp)

hivdur_predictions[[i]] <- predicted_dat
}

hivdur_predictions[[1]] %>% select(year, duration_estimate, median, lower, upper) %>%
  left_join(fsw_hivduration_df %>% filter(!is.na(survey_id)) %>% mutate(survey_prev_dur = age_pos/denom) %>% ungroup() %>% distinct()) %>% 
  mutate(exp_lower = plogis(lower),
         exp_upper = plogis(upper),
         exp_median = plogis(median)) %>% 
  ggplot() +
  geom_line(aes(x = duration_estimate, y = exp_median, color = factor(year))) +
  geom_point(aes(x = duration_estimate, y = survey_prev_dur, color = factor(year))) +
  facet_wrap(~year)

fsw_hivduration_df %>% filter(!is.na(survey_id)) %>% 
  mutate(dur_prev = age_pos/denom,
         ratio = dur_prev/survey_prev) %>% 
  ggplot() + 
  geom_point(aes(x = duration_estimate, y = ratio, color = factor(year))) +
  facet_wrap(~year)

duration_dat %>% group_by(survey_id, duration_estimate) %>% mutate(denom = n(), age_pos = sum(hiv)) %>% ungroup() %>%  
  left_join(duration_dat %>% group_by(survey_id) %>%  mutate(survey_denom = n(), median_dur = median(duration_estimate, na.rm = T), median_dur = round(median_dur, 0)) %>% ungroup() %>%  select(survey_id, median_dur , survey_denom) %>% distinct()) %>% 
  mutate(dur_prev = age_pos/denom,
         ratio = dur_prev/survey_prev) %>% 
  filter(duration_estimate == median_dur) %>% 
  select(-age, -hiv) %>% 
  distinct() %>% 
  rename(kp = kp2) %>% 
  moz.utils::name_kp() %>% 
  ggplot() + 
  geom_point(aes(x = year, y = ratio, size = denom )) + 
  geom_smooth(aes(x = year, y = ratio, weight = survey_denom, group = kp), method = "lm") +
  facet_wrap(~kp, scales = "free_x") + 
  coord_cartesian(ylim = c(0,2)) +
  moz.utils::standard_theme() +
  geom_hline(yintercept = 1, color = "darkred", linewidth = 0.5) + 
  theme(aspect.ratio = 1) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = "")) +
  labs(x = "Year", y = "Prevalence ratio", size = "Survey Sample Size")


duration_dat %>% group_by(survey_id, duration_estimate) %>% mutate(denom = n(), age_pos = sum(hiv)) %>% ungroup() %>%  
  left_join(duration_dat %>% group_by(survey_id) %>%  mutate(survey_denom = n(), median_dur = median(duration_estimate, na.rm = T), median_dur = round(median_dur, 0)) %>% ungroup() %>%  select(survey_id, median_dur , survey_denom) %>% distinct()) %>% 
  mutate(dur_prev = age_pos/denom,
         ratio = dur_prev/survey_prev) %>% 
  filter(kp2 == "FSW") %>% 
  ggplot() +
  geom_point(aes(x = duration_estimate, y = ratio, color = factor(year))) +
  geom_vline(aes(xintercept = median_dur)) +
  facet_wrap(~year)
            