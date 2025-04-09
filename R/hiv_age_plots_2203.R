library(tidyverse)
library(ggnewscale)

spectrum_dat <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop)

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_1703.rds")


hivdat <-  dat %>% 
  mutate(kp2 = case_when(kp == "MSMTG" & gender %in% c("female", "non-binary", "tgw") ~ "TGW",
                         kp == "MSMTG" & gender %in% c("male") ~ "MSM",
                         kp == "MSM" & gender %in% c("female", "TGW", "other", "3", "5") ~ "TGW",
                         kp == "MSM" & gender %in% c("male", "0", "1") ~ "MSM",
                         kp == "TG" & gender %in% c(0, 4) ~ "TGM",
                         kp == "TG" & gender == c(1, 3) ~ "TGW", 
                         kp == "TG" & sex == 1 ~ "TGM", 
                         kp == "TG" & sex == 0 ~ "TGW",
                         TRUE ~ kp)) %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id, kp, kp2, sex, year, age) %>% 
  summarise(denom = n(),
            kp_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(sex = case_when(sex == 0 ~ "male",
                         sex == 1 ~ "female",
                         TRUE ~ NA),
         sex = ifelse(is.na(sex) & kp2 %in% c("CFSW", "MSM", "PWID", "PWUD"), "male", sex),
         sex = ifelse(is.na(sex) & kp2 %in% c("FSW", "TGW"), "female", sex)) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(spectrum_dat) %>% 
  filter(!age<15) %>% 
  mutate(hivratio = kp_prev/tot_prev) %>% 
  mutate(kp = factor(kp, levels = c("FSW", "MSM", "PWID", "TGW", "CFSW")),
         kp2 = factor(kp2, levels = c("FSW", "MSM", "PWID", "TGW", "CFSW")))


hivdat_agegroup <-  dat %>% 
  mutate(kp2 = case_when(kp == "MSMTG" & gender %in% c("female", "non-binary", "tgw") ~ "TGW",
                         kp == "MSMTG" & gender %in% c("male") ~ "MSM",
                         kp == "MSM" & gender %in% c("female", "TGW", "other", "3", "5") ~ "TGW",
                         kp == "MSM" & gender %in% c("male", "0", "1") ~ "MSM",
                         kp == "TG" & gender %in% c(0, 4) ~ "TGM",
                         kp == "TG" & gender == c(1, 3) ~ "TGW", 
                         kp == "TG" & sex == 1 ~ "TGM", 
                         kp == "TG" & sex == 0 ~ "TGW",
                         TRUE ~ kp)) %>% 
  filter(!is.na(hiv)) %>%
  single_year_to_five_year() %>% 
  group_by(survey_id, kp, kp2, sex, year, age_group) %>% 
  summarise(denom = n(),
            kp_prev = mean(hiv)) %>% 
  ungroup() %>% 
  mutate(sex = case_when(sex == 0 ~ "male",
                         sex == 1 ~ "female",
                         TRUE ~ NA),
         sex = ifelse(is.na(sex) & kp2 %in% c("CFSW", "MSM", "PWID", "PWUD"), "male", sex),
         sex = ifelse(is.na(sex) & kp2 %in% c("FSW", "TGW"), "female", sex)) %>% 
  moz.utils::separate_survey_id() %>% 
  left_join(spectrum_dat %>% single_year_to_five_year() %>% group_by(iso3, year, age_group, sex) %>%  summarise(hivpop = sum(hivpop), totpop = sum(totpop)) %>% mutate(tot_prev = hivpop/totpop)) %>% 
  # filter(!age<15) %>% 
  mutate(hivratio = kp_prev/tot_prev) %>% 
  mutate(kp = factor(kp, levels = c("FSW", "MSM", "PWID", "TGW", "CFSW")),
         kp2 = factor(kp2, levels = c("FSW", "MSM", "PWID", "TGW", "CFSW")))

hivdat %>% 
  filter(kp2 %in% c("FSW", "MSM", "PWID", "TGW", "CFSW")) %>% 
  left_join(moz.utils::region()) %>% 
  filter(age < 50, !hivratio == 0 ) %>% 
  mutate(kp = kp2) %>% 
  moz.utils::name_kp() %>% 
  moz.utils::name_region() %>% 
  group_by(survey_id) %>% 
  mutate(survey_denom = sum(denom)) %>% 
  # mutate(age = factor(age)) %>% 
  ggplot() + 
  geom_point(aes(x = age, y = hivratio, color = region, size = survey_denom), shape = 16, alpha = 0.13) +
  # coord_trans(y = "log2") +
  # geom_boxplot(aes(x = age, y = hivratio, color = kp2), show.legend = F) +
  # lims(y = c(0,40)) + 
  scale_y_continuous(trans = "log", breaks = c(1, 4, 20, 80, 400)) +
  geom_hline(yintercept = 1, color = "darkred", linetype = 3) +
  # coord_cartesian(ylim = c(0,30)) +
  moz.utils::standard_theme() + 
  theme(aspect.ratio = 1) + 
  moz.utils::scale_manual(F, n = 2) +
  geom_smooth(aes(x = age, y = hivratio, colour = region, group = interaction(kp, region), weight = survey_denom), method = "lm")+
  # scale_color_manual(moz.utils::scale_manual(F, n = 2), values = 2) +
  # scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738",
  #                               "TGW" = "#195972", "CFSW" = "#4D251D")) +
  # scale_color_manual(values = c("#4D251D", "#989B6C" )) +
  labs(x = "Age", y = "HIV Prevalence ratio \n(kp/genpop) [Log Scale]", size = "Study sample \nsize", color = "Region") + 
  facet_wrap(~kp, nrow = 2)
  # theme(axis.title = element_text(size = 13),
  #       axis.text = element_text(size = 5))




# Individual ages, faceted by KP + region
hivdat %>% 
  filter(kp2 %in% c("FSW", "MSM", "PWID", "TGW", "CFSW")) %>% 
  left_join(moz.utils::region()) %>% 
  filter(age < 50 ) %>% 
  mutate(age = factor(age)) %>% 
  ggplot() + 
  # geom_point(aes(x = age, y = hivratio, color = region, size = denom), alpha = 0.3) +
  geom_boxplot(aes(x = age, y = hivratio, color = kp2), show.legend = F) +
  # lims(y = c(0,40)) + 
  coord_cartesian(ylim = c(0,30)) +
  moz.utils::standard_theme() + 
  theme(aspect.ratio = 1) + 
  scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738",
                                  "TGW" = "#195972", "CFSW" = "#4D251D")) +
  # scale_color_manual(values = c("#4D251D", "#989B6C" )) +
  labs(x = "Age", y = "HIV Prevalence ratio (kp/genpop)") + 
  facet_grid(region~kp2) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 5))


# not by region 
quantiles_hivratio <- hivdat %>%
  filter(age < 50) %>% 
  group_by(kp2, age) %>%
  filter(!is.na(hivratio)) %>% 
  summarise( median_val = median(hivratio, na.rm = TRUE),
             iqr_low = quantile(hivratio, 0.25, na.rm = TRUE),
             iqr_high = quantile(hivratio, 0.75, na.rm = TRUE)) 
hivdat %>% 
  filter(kp2 %in% c("FSW", "MSM", "PWID", "TGW", "CFSW")) %>% 
  left_join(moz.utils::region()) %>% 
  left_join(quantiles_hivratio) %>% 
  filter(age < 50 ) %>% 
  # mutate(age = factor(age)) %>% 
  ggplot() +
  geom_pointrange(aes(x = age, y = median_val, ymin = iqr_low, ymax = iqr_high, color = kp2), size = 0.4, show.legend = F) +
  # geom_point(aes(x = age, y = hivratio, color = region, size = denom), alpha = 0.3) +
  # geom_boxplot(aes(x = age, y = hivratio, color = kp2), show.legend = F) +
  # lims(y = c(0,40)) + 
  coord_cartesian(xlim = c(15, 50), ylim = c(0,30)) +
  moz.utils::standard_theme() + 
  theme(aspect.ratio = 1) + 
  scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738",
                                "TGW" = "#195972", "CFSW" = "#4D251D")) +
  # scale_color_manual(values = c("#4D251D", "#989B6C" )) +
  labs(x = "Age", y = "HIV Prevalence ratio (kp/genpop)") + 
  facet_wrap(~kp2, nrow = 1) +
  # facet_grid(region~kp2) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))



# yes by region

quantiles_hivratio_region <- hivdat %>%
  filter(age < 50) %>% 
  left_join(moz.utils::region()) %>% 
  group_by(kp2, age, region) %>%
  filter(!is.na(hivratio)) %>% 
  summarise( median_val = median(hivratio, na.rm = TRUE),
             iqr_low = quantile(hivratio, 0.25, na.rm = TRUE),
             iqr_high = quantile(hivratio, 0.75, na.rm = TRUE)) 
hivdat %>% 
  filter(kp2 %in% c("FSW", "MSM", "PWID", "TGW", "CFSW")) %>% 
  single_year_to_five_year() %>% 
  left_join(moz.utils::region()) %>% 
  left_join(quantiles_hivratio_region) %>% 
  filter(age < 50 ) %>% 
  # mutate(age = factor(age)) %>% 
  ggplot() +
  geom_pointrange(aes(x = age, y = median_val, ymin = iqr_low, ymax = iqr_high, color = kp2), size = 0.2, show.legend = F) +
  # geom_point(aes(x = age, y = hivratio, color = region, size = denom), alpha = 0.3) +
  # geom_boxplot(aes(x = age, y = hivratio, color = kp2), show.legend = F) +
  # lims(y = c(0,40)) + 
  coord_cartesian(xlim = c(15, 50), ylim = c(0,40)) +
  moz.utils::standard_theme() + 
  theme(aspect.ratio = 1) + 
  scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738",
                                "TGW" = "#195972", "CFSW" = "#4D251D")) +
  # scale_color_manual(values = c("#4D251D", "#989B6C" )) +
  labs(x = "Age", y = "HIV Prevalence ratio (kp/genpop)") + 
  # facet_wrap(~kp2, nrow = 1) +
  facet_grid(region~kp2) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))



# BY 5 year age group
quantiles_hivratio_region_agegp <- hivdat_agegroup%>%
  left_join(moz.utils::region()) %>% 
  group_by(kp2, age_group, region) %>%
  filter(!is.na(hivratio)) %>% 
  summarise( median_val = median(hivratio, na.rm = TRUE),
             iqr_low = quantile(hivratio, 0.25, na.rm = TRUE),
             iqr_high = quantile(hivratio, 0.75, na.rm = TRUE)) 


hivdat_agegroup %>% 
  filter(kp2 %in% c("FSW", "MSM", "PWID", "TGW", "CFSW")) %>% 
  left_join(moz.utils::region()) %>% 
  left_join(quantiles_hivratio_region_agegp) %>% 
  # mutate(age = factor(age)) %>% 
  ggplot() +
  geom_point(aes(y = age_group, x = hivratio, size = denom), color = "#806655", alpha = 0.3) + 
  geom_pointrange(aes(y = age_group, x = median_val, xmin = iqr_low, xmax = iqr_high, color = kp2), size = 0.2, show.legend = F) +
  # geom_boxplot(aes(x = age, y = hivratio, color = kp2), show.legend = F) +
  # lims(y = c(0,40)) + 
  coord_cartesian(xlim = c(0,40)) +
  moz.utils::standard_theme() + 
  theme(aspect.ratio = 1) + 
  scale_color_manual(values = c("FSW" = "#11304F", "MSM" = "#989B6C", "PWID" = "#3F4738",
                                "TGW" = "#195972", "CFSW" = "#4D251D")) +

  labs(y = "Age Group", x = "HIV Prevalence ratio (kp/genpop)") + 
  # facet_wrap(~kp2, nrow = 1) +
  facet_grid(region~kp2) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))



# Over time
quantiles_hivratio_region_agegp <- hivdat_agegroup%>%
  mutate(yearcat = case_when(year < 2001 ~ "1991-2000",
                             year > 2000 & year < 2011 ~ "2001-2010",
                             year > 2010 ~ "2011-present")) %>% 
  left_join(moz.utils::region()) %>% 
  group_by(kp2, age_group, region, yearcat) %>%
  filter(!is.na(hivratio)) %>% 
  summarise( median_val = median(hivratio, na.rm = TRUE),
             iqr_low = quantile(hivratio, 0.25, na.rm = TRUE),
             iqr_high = quantile(hivratio, 0.75, na.rm = TRUE)) 


hivdat_agegroup %>% 
  filter(kp2 %in% c("FSW", "MSM", "PWID", "TGW", "CFSW")) %>% 
  left_join(moz.utils::region()) %>% 
  mutate(yearcat = case_when(year < 2001 ~ "1991-2000",
                             year > 2000 & year < 2011 ~ "2001-2010",
                             year > 2010 ~ "2011-present")) %>% 
  left_join(quantiles_hivratio_region_agegp) %>% 
  # mutate(age = factor(age)) %>% 
  ggplot() +
  geom_point(aes(y = age_group, x = hivratio, color = year), alpha = 0.4) +
  scale_color_distiller(type = "seq",
                        direction = -1,
                        palette = "Greys") +
  new_scale_color() +
  geom_pointrange(aes(y = age_group, x = median_val, xmin = iqr_low, xmax = iqr_high, color = yearcat), size = 0.2, show.legend = F) +
  # geom_boxplot(aes(x = age, y = hivratio, color = kp2), show.legend = F) +
  # lims(y = c(0,40)) + 
  coord_cartesian(xlim = c(0,40)) +
  moz.utils::standard_theme() + 
  theme(aspect.ratio = 1) + 
  scale_color_manual(values = c( "#F98F5B", "#F17983", "#525832")) +
  labs(y = "Age Group", x = "HIV Prevalence ratio (kp/genpop)") + 
  # facet_wrap(~kp2, nrow = 1) +
  facet_grid(region+yearcat~kp2) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))


# INLA Model

formula = n ~ 1 + age + region  + f(iso3, model = "iid") + f(survey_id, model = "iid")
pwid_formula = n ~ 1 + age + region  + sex + f(iso3, model = "iid") + f(survey_id, model = "iid")

inla_hivdat <- crossing(iso3 = moz.utils::ssa_iso3(),
                      year = 1993:2024,
                      age = 15:60,
                      kp = c("FSW", "MSM", "PWID", "PWID2", "TGW", "CFSW")) %>% 
  left_join(moz.utils::region()) %>% 
  mutate(denom = 1) %>% 
  select(-four_region) %>% 
  mutate(sex = ifelse(kp %in% c("FSW", "PWID2", "TGW"), "female", "male"),
         kp2 = ifelse(kp == "PWID2", "PWID", kp)) %>% 
   select(-kp) %>% 
  left_join(spectrum_dat %>% select(iso3, year, age, sex, tot_prev)) %>% 
   bind_rows(hivdat %>% 
               mutate(n = denom*kp_prev,
                      n = round(n, 0))) %>% 
   filter(!is.na(kp2)) 


fsw_inla_hivdat <- inla_hivdat %>% 
  filter(kp2 == "FSW" & sex == "female", age %in% 15:60) %>% 
  mutate(id.age = multi.utils::to_int(age),
         id.year = multi.utils::to_int(year))

msm_inla_hivdat <- inla_hivdat %>% 
  filter(kp2 == "MSM" & sex == "male")

pwid_inla_hivdat <- inla_hivdat %>% 
  filter(kp2 == "PWID" ) %>% 
  mutate(sex = factor(sex))

tg_inla_hivdat <- inla_hivdat %>% 
  filter(kp2 == "TGW" & sex == "female")

cfsw_inla_hivdat <- inla_hivdat %>% 
  filter(kp2 == "CFSW" & sex == "male")



formula = n ~ 1  + qlogis(tot_prev) + f(age, model = "ar", order = 2) + f(year, model = "rw2") +f(id.year, model = "rw2", scale.model = T, group = id.age, control.group = list(model = "rw2"))

formula = n ~ 1  + qlogis(tot_prev) + f(age, model = "ar", order = 2) + f(year, model = "ar", order = 2) + f(id.year, model = "rw2", group = id.age, control.group = list(model = "rw2"))

# + f(year, model = "rw1") + f(age, model = "rw1")
# FE for the 2 + group interaction between the 2 - 2nd order 
fsw_inla_mod <- INLA::inla(formula, 
                       family = "binomial",
                       Ntrials = denom,
                       # E = tot_prev,
                       # offset = qlogis(tot_prev),
                       data = fsw_inla_hivdat,
                       control.compute=list(config = TRUE))
summary(fsw_inla_mod)
fsw_inla_mod$misc$configs$constr

# fsw_samples <- moz.utils::sample_model(fsw_inla_mod, fsw_inla_hivdat, n) 

filt_df <- fsw_inla_hivdat %>%
  filter(!is.na(n))

samples <- inla.posterior.sample(1000, fsw_inla_mod)

contents = fsw_inla_mod$misc$configs$contents

effect = "Predictor"
id.effect = which(contents$tag==effect)
ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])

ind.effect <- 1:(nrow(fsw_inla_hivdat) - nrow(filt_df))

samples.effect = lapply(samples, function(x) x$latent[ind.effect])

incidence_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)

ident <- fsw_inla_hivdat[ind.effect, ]

qtls <- apply(incidence_samples, 1, quantile, c(0.025, 0.5, 0.975))

predicted_dat <- ident %>%
  ungroup() %>%
  mutate(
    lower = qtls[1,],
    median = qtls[2,],
    upper = qtls[3,]
  ) %>% 
  mutate(exp_lower = plogis(lower),
         exp_median = plogis(median),
         exp_upper = plogis(upper))

ben_years <- fsw_inla_hivdat %>% filter(iso3 == "BEN", !is.na(hivratio)) %>% mutate(year = factor(year)) %>% pull(year) %>% unique()
# predicted_dat %>% 
predicted_dat %>% select(iso3:tot_prev, exp_median, exp_upper, exp_lower) %>% mutate(model_ratio = exp_median/tot_prev, model_lower = exp_lower/tot_prev, model_upper = exp_upper/tot_prev) %>% 
  filter(iso3 == "BEN",
         year %in% ben_years) %>%
  mutate(year = factor(year)) %>%
  ggplot() +
  geom_ribbon(aes(x = age, ymin = model_lower, ymax = model_upper, fill = year), alpha = 0.1) +
  geom_line(aes(x = age, y = model_ratio, color = year)) + 
  # geom_line(aes(x = year, y = exp_median, color = age)) + 
  geom_point(data = fsw_inla_hivdat %>% filter(iso3 == "BEN", !is.na(hivratio)) %>% mutate(year = factor(year)), aes(x = age, y = hivratio, color = year, size = denom), alpha = 0.2, shape = 16) +
  coord_cartesian(xlim = c(15, 60), ylim = c(0,150)) +
  guides(color=guide_legend(nrow=4), size = "none") +
  moz.utils::standard_theme() + 
  theme(aspect.ratio = 1) + 
  labs(x = "Age", y = "HIV Prevalence Ratio \n(kp/genpop)")
  # scale_y_continuous(trans = "log")
  # geom_point(data = duration_model, aes(x = year, y = dur_prev))


predicted_dat %>% 
  filter(iso3 == "BEN", year == 2017) %>%
  # mutate(age = factor(age)) %>% 
  ggplot() +
  geom_line(aes(x = age, y = exp_median)) + 
  facet_wrap(~kp2)
  # geom_point(data = fsw_inla_hivdat, aes(x = age, y = hivratio))


fsw_samples %>% 
  mutate(exp_mean = exp(mean),
         exp_lower = exp(lower),
         exp_upper = exp(upper)) %>% 
  filter(iso3 == "BEN",
         is.na(kp_prev)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = exp_mean, color = year))
  # geom_point(data = fsw_samples, aes(x = age, y = hivratio))


msm_inla_mod <- INLA::inla(formula, 
                           family = "binomial",
                           Ntrials = denom,
                           offset = log(tot_prev),
                           data = msm_inla_hivdat,
                           control.compute=list(config = TRUE))
summary(msm_inla_mod)

msm_samples <- moz.utils::sample_model(msm_inla_mod, msm_inla_hivdat, everything())

msm_samples %>% 
  mutate(exp_mean = exp(mean),
         exp_lower = exp(lower),
         exp_upper = exp(upper)) %>% 
  filter(iso3 == "BEN",
         is.na(kp_prev)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = exp_mean, color = year))
  # geom_point(data = msm_samples, aes(x = age, y = hivratio))


####### HIV Prevalence over age

inla_hivdat_agemod <- crossing(region = c("WCA", "ESA"),
                        # iso3 = moz.utils::ssa_iso3(),
                        year = 1993:2023,
                        age = 15:60,
                        kp = c("FSW", "MSM", "PWID", "PWID2", "TGW", "CFSW")) %>% 
  # left_join(moz.utils::region()) %>% 
  mutate(denom = 1) %>% 
  # select(-four_region) %>% 
  mutate(sex = ifelse(kp %in% c("FSW", "PWID2", "TGW"), "female", "male"),
         kp2 = ifelse(kp == "PWID2", "PWID", kp)) %>% 
  select(-kp) %>% 
  left_join(spectrum_dat %>% select(iso3, year, age, sex, hivpop, totpop) %>% left_join(moz.utils::region()) %>% group_by(region, year, age, sex) %>%  summarise(tot_prev = sum(hivpop)/sum(totpop))) %>% 
  bind_rows(hivdat %>% 
              left_join(moz.utils::region()) %>% 
              mutate(n = denom*kp_prev,
                     n = round(n, 0))) %>% 
  filter(!is.na(kp2)) 


fsw_inla_hivdat_agemod <- inla_hivdat_agemod %>% 
  filter(kp2 == "FSW" & sex == "female", age %in% 15:60) %>% 
  mutate(id.age = multi.utils::to_int(age),
         id.year = multi.utils::to_int(year)) %>% 
  mutate(tot_prev = ifelse(is.na(survey_id), NA_integer_, tot_prev),
         n = ifelse(n == denom, n-0.05, n),
         n = ifelse(n == 0, 0.05, n),
         id.year = year - min(year),
         id.year2 = id.year) 


iso3_list <- data.frame(iso3 = moz.utils::ssa_iso3()) %>% rownames_to_column() %>% rename(id.iso3 = rowname)
fsw_inla_hivdat_agemod <- fsw_inla_hivdat_agemod %>% left_join(iso3_list) %>% mutate(id.iso3 = multi.utils::to_int(id.iso3))
dat <- fsw_inla_hivdat_agemod %>%
  mutate(id.year.age = group_indices(., year, age))
n_ages <- length(unique(dat$age))
n_years <- length(unique(dat$year))
n_interactions_year <- n_years * n_ages

# Sum-to-zero constraint within each year for age effects
A_sum_age_year <- matrix(0, nrow = n_years, ncol = n_interactions_year)

# Loop over each year to impose sum-to-zero constraint across the 5 age groups
for (year in 1:n_years) {
  A_sum_age_year[year, ((year - 1) * n_ages + 1):(year * n_ages)] <- 1
} # This makes the matrix so row 1 is a 1 for every age for the first year

## Same in reverse to have sum-to-zero constraints within each age over year
A_sum_year_age <- matrix(0, nrow = n_ages, ncol = n_interactions_year)

for (age in 1:n_ages) {
  A_sum_year_age[age, ((age - 1) * n_years + 1):(age * n_years)] <- 1
} # This makes the matrix so row 1 is a 1 for ever year for the first age

A_combined2 <- rbind(A_sum_year_age, A_sum_age_year) # Combining our matrices for the interaction
e2 <- matrix(0, nrow(A_combined2), nrow = 1) # This is saying what we want our constraints to sum to

R_year <- dfertility::make_rw_structure_matrix(n_years, 2)
R_age <- dfertility::make_rw_structure_matrix(n_ages, 2)
R_age <- as(diag(1, n_ages), "sparseMatrix")
Q2 <- kronecker(R_year, R_age)

fsw_agehiv_mod <- inla(n ~ 1
                       + region
                       + f(survey_id, model = "iid")
                       + id.year
                       + f(id.year.age, model = "generic0", Cmatrix = Q2, extraconstr = list(A = A_combined2, e = e2))
                       # +f(id.year2, model = "rw2", scale.model = T, group = id.age, control.group = list(model = "rw2"))
                + f(id.age, model = "ar", order = 2), 
                # + f(id.year, model = "ar", order = 2)
                # + f(id.iso3, model = "besag", graph = moz.utils::national_adj(), scale.model = T),
                Ntrials = denom,
                offset = qlogis(tot_prev),
                data = dat,
                family = "xbinomial",
               control.family = list(link = "logit"),
                control.compute=list(config = TRUE)
)

summary(fsw_agehiv_mod)

clean_inla(fsw_agehiv_mod)$random %>% filter(var == "id.year.age")

fsw_agehiv_mod$misc$configs$constr$n

count(fsw_inla_hivdat_agemod, year)

fsw_inla_hivdat_agemod[c(2107, 1932 ,2004, 1972),]

predicted_dat <- moz.utils::sample_model(fsw_agehiv_mod, fsw_inla_hivdat_agemod, "n") %>% 
  mutate(exp_lower = exp(lower),
         exp_mean = exp(mean),
         exp_upper = exp(upper))

# filt_df <- fsw_inla_hivdat_agemod %>%
#   filter(!is.na(n))
# 
# samples <- inla.posterior.sample(1000, fsw_agehiv_mod)
# 
# contents = fsw_agehiv_mod$misc$configs$contents
# 
# effect = "Predictor"
# id.effect = which(contents$tag==effect)
# ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
# 
# ind.effect <- 1:(nrow(fsw_inla_hivdat_agemod) - nrow(filt_df))
# 
# samples.effect = lapply(samples, function(x) x$latent[ind.effect])
# 
# incidence_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)
# 
# ident <- fsw_inla_hivdat_agemod[ind.effect, ]
# 
# qtls <- apply(incidence_samples, 1, quantile, c(0.025, 0.5, 0.975))
# 
# predicted_dat <- ident %>%
#   ungroup() %>%
#   mutate(
#     lower = qtls[1,],
#     median = qtls[2,],
#     upper = qtls[3,]
#   ) %>% 


  # mutate(exp_lower = plogis(lower),
  #        exp_median = plogis(median),
  #        exp_upper = plogis(upper))

# predicted_dat %>% 
#   # select(iso3:tot_prev, exp_median, exp_upper, exp_lower) %>% mutate(model_ratio = exp_median/tot_prev, model_lower = exp_lower/tot_prev, model_upper = exp_upper/tot_prev) %>% 
#   filter(iso3 == "BEN",
#          year %in% ben_years) %>%
#   mutate(year = factor(year)) %>%
#   ggplot() +
#   geom_ribbon(aes(x = age, ymin = exp_lower, ymax = exp_upper, fill = year), alpha = 0.1) +
#   geom_line(aes(x = age, y = exp_mean, color = year)) + 
#   # geom_line(aes(x = year, y = exp_median, color = age)) + 
#   geom_point(data = fsw_inla_hivdat %>% filter(iso3 == "BEN", !is.na(hivratio)) %>% mutate(year = factor(year)), aes(x = age, y = hivratio, color = year, size = denom), alpha = 0.2, shape = 16) +
#   coord_cartesian(xlim = c(15, 60), ylim = c(0,150)) +
#   guides(color=guide_legend(nrow=4), size = "none") +
#   moz.utils::standard_theme() + 
#   theme(aspect.ratio = 1) + 
#   labs(x = "Age", y = "HIV Prevalence Ratio \n(kp/genpop)")


predicted_dat %>% 
  # select(region:prev, exp_median, exp_upper, exp_lower) %>% mutate(model_ratio = exp_median/prev, model_lower = exp_lower/prev, model_upper = exp_upper/prev) %>% 
  # filter(!model_ratio < 0.5) %>% 
  filter(year %in% c(2000, 2005, 2010, 2015, 2020)) %>%
  ggplot() +
  # geom_point(data = inla_hivdat %>% 
  #              # mutate(odds_ratio = ((kp_prev/(1-kp_prev))/(tot_prev/(1-tot_prev)))) %>% 
  #              filter(!is.na(hivratio), kp2 == "FSW", !hivratio < 0.005) %>% 
  #              select(-region) %>% 
  #              mutate(year = factor(year)) %>% 
  #              moz.utils::separate_survey_id() %>% 
  #              left_join(moz.utils::region()) %>% 
  #              mutate(region = factor(region),
  #                     year = ifelse(year == 2017, 1, 0)), 
  #            aes(x = age, y = hivratio, color = survey_id, size = denom), 
  #            alpha = 0.5, shape = 16, show.legend = F) +
  geom_line(aes(x = age, y = exp_mean, color = factor(year))) +
  # geom_ribbon(aes(x =age, ymin = exp_lower, ymax = exp_upper, fill = year), alpha = 0.5) +
  facet_wrap(~region) + 
  moz.utils::standard_theme()
  # scale_y_continuous(trans = "log") +
  # coord_cartesian(ylim = c(0.5 , 400))

moz.utils::clean_inla(fsw_agehiv_mod)$random
  