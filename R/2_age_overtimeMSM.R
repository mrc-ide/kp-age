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
msm_age_dat <- dat %>%
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
  filter(!is.na(age)) %>% 
  group_by(survey_id, year) %>% 
  mutate(mean_kp_age = mean(age),
         denom = n()) %>%
  ungroup() %>% 
  left_join(genpop_median_ages %>% filter(sex == "male") %>% select(-sex)) %>% 
  select(kp, year, survey_id, iso3, age, mean_kp_age, genpop_mean = mean_age, denom)



inla_msm_age <- crossing(year = 2005:2023,
                         iso3 = unique(msm_age_dat$iso3),
                         model = "full_sample") %>% 
  bind_rows(crossing(year = 2005:2023) %>%
              left_join(genpop_median_ages %>% filter(sex == "male") %>% select(-sex)) %>% 
              select(year, genpop_median = megamedian, genpop_mean = megamean) %>% 
              distinct() %>% 
              mutate(model = "ssa_genpop")) %>% 
  bind_rows(crossing(year = 2005:2023) %>%
              left_join(genpop_median_ages  %>% filter(sex == "male") %>% select(-sex)) %>% 
              select(year, iso3, genpop_median = median_age, genpop_mean = mean_age) %>% 
              distinct() %>% 
              mutate(model = "country_slopes"))



inla_msm_age <- inla_msm_age %>% 
  select(year, model, iso3, genpop_median, genpop_mean) %>%
  distinct() %>% 
  bind_rows(msm_age_dat) %>% 
  left_join(genpop_median_ages %>% filter(sex == "male") %>% select(-sex)) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-2017) %>% 
  mutate(year_min = year - min(year),
         iso3_id = iso3,
         mean_year = (year - 2017)/5)


msm_age_formula1 <- age ~ 1 + mean_year +f(iso3, model = "iid") + f(survey_id, model = "iid") + f(iso3_id, mean_year, model = "iid") + offset(log(genpop_mean))

msm_agemod_gamma_offset <- INLA::inla(formula = msm_age_formula1,
                                      family = "gamma",
                                      control.compute=list(config = TRUE, waic = T),
                                      data = inla_msm_age)

summary(msm_agemod_gamma_offset)

msm_agemod_gamma_offset_samples <- moz.utils::sample_model(msm_agemod_gamma_offset, inla_msm_age, col = "survey_id")


(msm_agecount_gamma_offset <- msm_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean)), linewidth = 0.75) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_line(data = genpop_median_ages %>% filter(year >2004, sex == "male") %>% select(year, megamean) %>% distinct(), aes(x = year, y = megamean), color = "darkred", linetype = "dashed", linewidth = 0.75) +
    geom_point(data = msm_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "Year", y = "MSM age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(msm_ageratio_gamma_offset <- msm_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = (exp(mean))-genpop_mean)) +
    geom_ribbon(aes(x = year, ymin = (exp(lower))- genpop_mean, ymax = (exp(upper))-genpop_mean), alpha = 0.3) +
    geom_point(data = msm_age_dat , aes(x = year, y = mean_kp_age-genpop_mean, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2005, xend = 2005, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2005, y = 6.7, label = "MSM older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2005, xend = 2005, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2005, y = -6.3, label = "MSM younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 

(msm_ageratio_gamma_offset_countries <- msm_agemod_gamma_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% msm_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    filter(iso3 %in% c("KEN", "BEN", "ZAF", "COD")) %>%
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_mean, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_mean, ymax = exp(upper)-genpop_mean, fill = iso3), alpha = 0.05) +
    geom_point(data = msm_age_dat %>% filter(iso3 %in% c("KEN", "BEN", "ZAF", "COD")), aes(x = year, y = (mean_kp_age)-genpop_mean, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    # coord_cartesian(ylim = c(-7, 10)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2005, xend = 2005, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2005, y = 6.7, label = "MSM older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2005, xend = 2005, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2005, y = -6.3, label = "MSM younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 4), color = guide_legend(title = "Country", nrow = 4)) +
    theme(legend.position = "right"))
