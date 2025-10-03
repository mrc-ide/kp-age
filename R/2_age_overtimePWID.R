library(tidyverse)
library(INLA)
library(matrixStats)

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agehiv_data_extract_240925.rds") 

spectrum_dat <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop)


genpop_median_ages <- spectrum_dat %>%
  group_by(iso3, year, sex) %>%
  summarise(
    median_age = weightedMedian(age, w = totpop, na.rm = TRUE),
    mean_age   = weighted.mean(age, w = totpop, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(year %in% 1993:2023) %>% ungroup() %>% 
  group_by(year, sex) %>%
  mutate(megamedian = median(median_age),
         megamean = mean(mean_age)) %>% 
  ungroup()

genpop_median_ages %>% 
  select(year, sex, Median = megamedian, Mean = megamean) %>% 
  distinct() %>% 
  pivot_longer(cols = c(Median, Mean), names_to = "Measure", values_to = "Value") %>% 
  ggplot() +
  geom_line(aes(x = year, y = Value, color = sex), linewidth = 0.75) + 
  facet_wrap(~Measure) + 
  moz.utils::standard_theme()


# genpop_median_ages2 <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/genpop_median_ages.rds")
# # 
# genpop_median_ages2 %>%
#   select(year, sex, megamedian, megamean) %>%
#   distinct() %>%
#   ggplot() +
#   geom_line(aes(x = year, y = megamean, color = sex))


# KEN1993 %>% 
#   ggplot() + 
#   geom_density(aes(x = age, color = factor(year))) +
#   theme_minimal()

#FSW : 108 surveys 
pwid_age_dat <-  dat %>% 
  filter(kp == "PWID") %>%  
  mutate(gender = case_when(gender %in% c(0, "male") ~ 0,
                            gender %in% c(1, "female") ~1,
                            gender %in% c("other", "TGW", "transgender", "TGM") ~ 2),
         sex = case_when(sex %in% c(0, "male") ~ 0,
                         sex %in% c(1, "female") ~1,
                         TRUE ~ NA_integer_)) %>% 
  mutate(sex = as.character(ifelse(is.na(sex), gender, sex))) %>% 
  mutate(sex = case_when(sex == 0 ~ "male",
                      sex == 1 ~ "female",
                      TRUE ~ sex)) %>% 
  filter(!(age < 10 | age > 60)) %>%  
  filter(!is.na(age)) %>% 
  group_by(survey_id, year, sex) %>% 
  mutate(mean_kp_age = mean(age),
         denom = n()) %>%
  ungroup() %>% 
  left_join(genpop_median_ages) %>% 
  select(kp, year, survey_id, iso3, sex, age, mean_kp_age, genpop_mean = mean_age, denom) %>% 
  filter(sex %in% c("male", "female"))



inla_pwid_age <- crossing(year = 2014:2023) %>%
              left_join(genpop_median_ages) %>% 
              select(year, sex, genpop_median = megamedian, genpop_mean = megamean) %>% 
              distinct() %>% 
              mutate(model = "ssa_genpop") %>% 
  bind_rows(crossing(year = 2014:2023) %>%
              left_join(genpop_median_ages) %>% 
              select(year, sex, iso3, genpop_median = median_age, genpop_mean = mean_age) %>% 
              distinct() %>% 
              mutate(model = "country_slopes"))



inla_pwid_age <- inla_pwid_age %>% 
  select(year, sex, model, iso3, genpop_median, genpop_mean) %>%
  distinct() %>% 
  bind_rows(pwid_age_dat) %>% 
  left_join(genpop_median_ages) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-2017) %>% 
  mutate(year_min = year - min(year),
         iso3_id = iso3,
         mean_year = (year - 2019)/5)


pwid_age_formula1 <- age ~ 1 + mean_year*sex + f(iso3, model = "iid") + f(iso3_id, mean_year, model = "iid") + offset(log(genpop_mean))

pwid_agemod_gamma_offset <- INLA::inla(formula = pwid_age_formula1,
                                       family = "gamma",
                                       control.compute=list(config = TRUE, waic = T),
                                       data = inla_pwid_age)

summary(pwid_agemod_gamma_offset)

pwid_agemod_gamma_offset_samples <- moz.utils::sample_model(pwid_agemod_gamma_offset, inla_pwid_age, col = "survey_id")


(pwid_agecount_gamma_offset <- pwid_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean)), linewidth = 0.75) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_line(data = genpop_median_ages %>% filter(year >2013) %>% select(year, sex, megamean) %>% distinct(), aes(x = year, y = megamean), color = "darkred", linetype = "dashed", linewidth = 0.75) +
    geom_point(data = pwid_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    facet_wrap(~sex) +
    lims(y = c(15,40)) +
    labs(x = "Year", y = "PWID age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA") )


(pwid_ageratio_gamma_offset <- pwid_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = (exp(mean))-genpop_mean)) +
    geom_ribbon(aes(x = year, ymin = (exp(lower))- genpop_mean, ymax = (exp(upper))-genpop_mean), alpha = 0.3) +
    geom_point(data = pwid_age_dat , aes(x = year, y = mean_kp_age-genpop_mean, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-7,5)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 3.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 4.7, label = "PWID older than total \npopulation", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -6.3, label = "PWID younger than total \npopulation", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend") +
    facet_wrap(~sex)) 

(pwid_ageratio_gamma_offset_countries <- pwid_agemod_gamma_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% pwid_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    # filter(iso3 %in% c("KEN", "BEN", "ZAF", "COD")) %>%
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_mean, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_mean, ymax = exp(upper)-genpop_mean, fill = iso3), alpha = 0.05) +
    geom_point(data = pwid_age_dat, aes(x = year, y = (mean_kp_age)-genpop_mean, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    # coord_cartesian(ylim = c(-7, 10)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 6.7, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -6.3, label = "PWID younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 4), color = guide_legend(title = "Country", nrow = 4)) +
    theme(legend.position = "bottom") +
    facet_wrap(~sex))
