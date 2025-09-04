library(tidyverse)
library(INLA)

# msm_age_dat <- msm_age_dat %>% select(-median_age, -megamedian, -genpop_median) %>%  left_join(genpop_median_ages %>% filter(sex == "male") %>% select(-sex)) %>% rename(genpop_mean = mean_age ,  genpop_median = median_age)
# 
# saveRDS(msm_age_dat, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/msm_age_dat.rds")

# fsw_age_dat <- fsw_age_dat %>% select(-megamedian, -genpop_median) %>%  left_join(genpop_median_ages %>% filter(sex == "female") %>% select(-sex)) %>% rename(genpop_mean = mean_age ,  genpop_median = median_age)
# 
# saveRDS(fsw_age_dat, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fsw_age_dat2105.rds")

genpop_median_ages <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/genpop_median_ages.rds")
msm_age_dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/msm_age_dat.rds")
fsw_age_dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fsw_age_dat2105.rds")


inla_fsw_age <- crossing(year = 1993:2023,
                         iso3 = unique(fsw_age_dat$iso3),
                         model = "full_sample") %>% 
  bind_rows(crossing(year = 1993:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "female")%>%
              select(year, sex, genpop_median = megamedian, genpop_mean = megamean) %>% 
              distinct() %>% 
              mutate(model = "ssa_genpop") %>% 
              filter(sex == "female")) %>% 
  bind_rows(crossing(year = 1993:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "female") %>%
              select(year, iso3, sex, genpop_median = median_age, genpop_mean = mean_age) %>% 
              distinct() %>% 
              mutate(model = "country_slopes") )

inla_fsw_age <- inla_fsw_age %>% 
  select(year, model, iso3, genpop_median, genpop_mean) %>%
  distinct() %>% 
  bind_rows(fsw_age_dat) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-2000) %>% 
  mutate(year_min = year - min(year),
         iso3_id = iso3,
         mean_year = year - 2017)

fsw_age_formula1 <- age ~ 1 + mean_year + f(iso3, model = "iid") + f(iso3_id, mean_year, model = "iid") + offset(log(genpop_median))

fsw_agemod_gamma_offset <- INLA::inla(formula = fsw_age_formula1,
                                      family = "gamma",
                                      control.compute=list(config = TRUE),
                                      data = inla_fsw_age)

summary(fsw_agemod_gamma_offset)

fsw_agemod_gamma_offset_samples <- moz.utils::sample_model(fsw_agemod_gamma_offset, inla_fsw_age, col = "survey_id")

(fsw_ageratio_gamma_offset <- fsw_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



inla_msm_age <- crossing(year = 2010:2023,
                         iso3 = unique(msm_age_dat$iso3),
                         model = "full_sample") %>% 
  bind_rows(crossing(year = 2010:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "male")%>%
              select(year, sex, genpop_median = megamedian, genpop_mean = megamean) %>% 
              distinct() %>% 
              mutate(model = "ssa_genpop") %>% 
              filter(sex == "male")) %>% 
  bind_rows(crossing(year = 2010:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "male") %>%
              select(year, iso3, sex, genpop_median = median_age, genpop_mean = mean_age) %>% 
              distinct() %>% 
              mutate(model = "country_slopes") )


inla_msm_age <- inla_msm_age %>% 
  dplyr::select(year, model, iso3, genpop_median, genpop_mean) %>%
  distinct() %>% 
  bind_rows(msm_age_dat) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-2010) %>% 
  mutate(year_min = year - min(year),
         mean_year = year - 2018,
         iso3_id = iso3)


msm_age_formula1 <- age ~ 1 + mean_year + f(iso3, model = "iid") + f(iso3_id, mean_year, model = "iid") + offset(log(genpop_median))

msm_agemod_gamma_offset <- INLA::inla(formula = msm_age_formula1,
                                      family = "gamma",
                                      control.compute=list(config = TRUE),
                                      data = inla_msm_age)

summary(msm_agemod_gamma_offset)

msm_agemod_gamma_offset_samples <- moz.utils::sample_model(msm_agemod_gamma_offset, inla_msm_age, col = "survey_id")

(msm_ageratio_gamma_offset <- msm_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = msm_age_dat , aes(x = year, y = mean_kp_age-megamedian, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-12, 3)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.7, label = "MSM older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -8,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -8.7, label = "MSM younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


# 
# (fsw_ageratio_gamma_offset_countries <- fsw_agemod_gamma_offset_samples %>% 
#     filter(model == "country_slopes",
#            iso3 %in% fsw_age_dat$iso3) %>% 
#     mutate(exp_mean = exp(mean),
#            exp_lower = exp(lower),
#            exp_upper = exp(upper)) %>% 
#     ggplot() + 
#     geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
#     geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
#     geom_point(data = fsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
#     geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
#     moz.utils::standard_theme() + 
#     labs(x = "Year", y = "Number of years older \nthan the total population") +
#     coord_cartesian(ylim = c(-7, 10)) +
#     theme(aspect.ratio = 1) + 
#     annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
#              arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
#     annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
#     annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
#              arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
#     annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
#     ggtitle("Country-level trends") +
#     guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))
# 
# 
# 
# (fsw_agecount_gamma_offset <- fsw_agemod_gamma_offset_samples %>%
#     filter(model == "ssa_genpop") %>%
#     mutate(exp_mean = exp(mean),
#            exp_lower = exp(lower),
#            exp_upper = exp(upper)) %>%
#     ggplot() +
#     geom_line(aes(x = year , y = exp(mean))) +
#     geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
#     geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
#     moz.utils::standard_theme() +
#     lims(y = c(15,40)) +
#     labs(x = "Year", y = "FSW age") +
#     theme(aspect.ratio = 1) + 
#     ggtitle("SSA"))



## Generate a means 

test <- inla_fsw_age %>% filter(!is.na(survey_id)) %>% 
  filter(!is.na(age)) %>% 
  group_by(survey_id) %>% 
  mutate(mean_kp_age = mean(age),
         sd_kp_age = sd(age),
         denom = n()) %>% 
  ungroup() %>% 
  select(survey_id, year, median_kp_age, mean_kp_age, sd_kp_age, denom, genpop_mean) %>% 
  distinct() %>% 
  # group_by(survey_id, age) %>% 
  # mutate(age_denom = n()) %>% 
  # ungroup() %>% 
  mutate(se = sd_kp_age/sqrt(denom),
         variance = se^2,
         weights = 1/variance,
         diff = mean_kp_age - genpop_mean,
         mean_year = year - 2017)  


weighted_model <- lm(diff ~ mean_year, 
                     data = test, 
                     weights = weights)

summary(weighted_model)

pred_data <- data.frame(year = 1993:2023) %>% 
  mutate(mean_year = year - 2017)
predictions <- predict(weighted_model, newdata = pred_data, 
                       interval = "confidence", level = 0.95)
pred_df <- data.frame(
  year = 1993:2023,
  estimate = predictions[,"fit"],
  lower = predictions[,"lwr"],
  upper = predictions[,"upr"]
)

test %>% 
ggplot() +
  geom_point(aes(x = year, y = diff, size = weights), alpha = 0.8) +
  geom_ribbon(data = pred_df, aes(ymin = lower, ymax = upper, x = year), 
              alpha = 0.3, fill = "blue", inherit.aes = FALSE) +
  geom_line(data = pred_df, aes(y = estimate, x = year ), color = "blue", size = 1, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  labs(x = "Year", y = "Age Difference (KP - GenPop)") +
  moz.utils::standard_theme()



#### MSM

test_msm <- inla_msm_age %>% filter(!is.na(survey_id)) %>% 
  filter(!is.na(age)) %>% 
  group_by(survey_id) %>% 
  mutate(mean_kp_age = mean(age),
         sd_kp_age = sd(age),
         denom = n()) %>% 
  ungroup() %>% 
  select(survey_id, year, genpop_mean, mean_kp_age, sd_kp_age, denom) %>% 
  distinct() %>% 
  # group_by(survey_id, age) %>% 
  # mutate(age_denom = n()) %>% 
  # ungroup() %>% 
  mutate(se = sd_kp_age/sqrt(denom),
         variance = se^2,
         weights = 1/variance,
         diff = mean_kp_age - genpop_mean,
         mean_year = year - 2018)  


weighted_model_msm <- lm(diff ~ mean_year, 
                     data = test_msm, 
                     weights = weights)

summary(weighted_model_msm)

pred_data_msm <- data.frame(year = 2010:2023) %>% 
  mutate(mean_year = year - 2018)

predictions_msm <- predict(weighted_model_msm, newdata = pred_data_msm, 
                       interval = "confidence", level = 0.95)
pred_df_msm <- data.frame(
  year = 2010:2023,
  estimate = predictions_msm[,"fit"],
  lower = predictions_msm[,"lwr"],
  upper = predictions_msm[,"upr"]
)

# Create the plot
test_msm %>% 
  ggplot() +
  geom_point(aes(x = year, y = diff, size = denom), alpha = 0.8) +
  geom_ribbon(data = pred_df_msm, aes(ymin = lower, ymax = upper, x = year), 
              alpha = 0.3, fill = "blue", inherit.aes = FALSE) +
  geom_line(data = pred_df_msm, aes(y = estimate, x = year ), color = "blue", size = 1, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  labs(x = "Year", y = "Age Difference (KP - GenPop)") +
  moz.utils::standard_theme()
