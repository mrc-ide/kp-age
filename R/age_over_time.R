# age_dat <- single_year_ages %>% 
#   group_by(survey_id, denom, kp, year, iso3, sex) %>% 
#   summarise(median_age = median(age)) %>% 
#   ungroup() %>% 
#   mutate(sex = ifelse(sex == 0, "male", "female"))


# genpop_median_ages <- spectrum_dat %>%
#   filter(year %in% dat$year,
#          iso3 %in% dat$iso3) %>% 
#   select(iso3, year, age, sex, totpop) %>% 
#   group_by(iso3, year, sex) %>%
#   summarise(median_age = median(rep(age, totpop), na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   group_by(year) %>% 
#   mutate(megamedian = median(median_age)) %>% 
#   ungroup()

genpop_median_ages <- spectrum_dat %>%
  group_by(iso3, year, sex) %>%
  arrange(iso3, year, sex, age) %>%
  mutate(cum_totpop = cumsum(totpop),
         total_totpop = sum(totpop),
         half_totpop = total_totpop / 2) %>%
  filter(cum_totpop >= half_totpop) %>%
  slice(1) %>%
  ungroup() %>% 
  select(iso3, year, sex, median_age = age) %>% 
  group_by(year) %>% 
  mutate(megamedian = median(median_age)) %>% 
  ungroup()

fsw_age_dat <- distribution_plot %>% 
  filter(kp == "FSW",
         distribution == "Age") %>% 
         # !survey_id == "ETH2020ACA_FSW") %>%
  rename(age = variable) %>% 
  select(survey_id, kp, iso3, year, age) %>% 
  group_by(survey_id) %>% 
  mutate(median_kp_age = median(age, na.rm = T),
         mean_kp_age = mean(age, na.rm = T),
         denom = n()) %>% 
  ungroup() %>% 
  left_join(genpop_median_ages %>% filter(sex == "female") %>% rename(genpop_median = median_age))

# saveRDS(fsw_age_dat, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fsw_age_dat2105.rds")
# 
# saveRDS(genpop_median_ages, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/genpop_median_ages2105.rds")

inla_fsw_age <- crossing(age = 15:50,
                         # year = 2000:2023,
                         model = "full_sample") 
  # bind_rows(crossing(age = 15:50,
  #           year = 2000:2023) %>%
  #             left_join(genpop_median_ages) %>%
  #             filter(iso3 == "BEN" & sex == "female") %>%
  #             rename(genpop_median = median_age) %>%
  #             mutate(model = "case_study"))

inla_fsw_age <- inla_fsw_age %>% 
  select(age,   model) %>% 
  distinct() %>% 
  bind_rows(fsw_age_dat %>% mutate(model = "data")) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-2015) 
# %>% 
  # mutate(log_genpop_median = log(genpop_median))

age_formula <- log(age_centre) ~ 1 + year_centre + f(survey_id, model = "iid") + offset(log_genpop_median)

age_formula <- age ~ 1 + f(survey_id, model = "iid") #+ offset(log_genpop_median)

fsw_agemod <- INLA::inla(formula = age_formula,
                         # family = "gaussian",
                         family = "lognormal",
                        # E = genpop_median,
                         control.compute=list(config = TRUE),
                         data = inla_fsw_age)

summary(fsw_agemod)

fsw_agemod_samples <- moz.utils::sample_model(fsw_agemod, inla_fsw_age, col = "survey_id")


# fsw_medianages_plot <- 
  fsw_agemod_samples %>% 
    filter(model == "case_study") %>% 
  mutate(exp_mean = exp(mean),
         exp_lower = exp(lower),
         exp_upper = exp(upper)) %>% 
  ggplot() + 
  geom_line(aes(x = year , y = (exp_mean + 11))) +
  geom_ribbon(aes(x = year, ymin = (exp_lower+11), ymax = (exp_upper+11)), alpha = 0.3) +
  geom_point(data = fsw_age_dat %>% filter(iso3 == "BEN") , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) + 
  # geom_line(aes(x = year, y = genpop_median), color = "blue") +
  moz.utils::standard_theme() + 
  labs(x = "Year", y = "Mean Age") +
    theme(aspect.ratio = 1) + 
    ggtitle("FSW")
  
  
  fsw_agemod_samples %>% 
    filter(model == "full_sample") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = (mean))) +
    geom_ribbon(aes(x = year, ymin = (lower), ymax = (upper)), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = log(mean_kp_age), size = denom, color = iso3), show.legend = F) + 
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Mean Age") +
    theme(aspect.ratio = 1) + 
    ggtitle("FSW")
    # scale_y_continuous(trans = "log")
    # facet_wrap(year~iso3) + 
    # lims(x = c(y )
  # geom_line(data = genpop_medians_plot_fem %>% group_by(year) %>% mutate(median = median(genpop_median)), aes(x = year, y = median), color = "darkred")
  
  age_formula <- (age_centre) ~ 1 + year_centre + f(survey_id, model = "iid")
  
  fsw_agemod_gamma <- INLA::inla(formula = age_formula,
                           family = "gamma",
                           offset = log(genpop_median),
                           control.compute=list(config = TRUE),
                           data = inla_fsw_age)
  
  summary(fsw_agemod_gamma)
  
  fsw_agemod_gamma_samples <- moz.utils::sample_model(fsw_agemod_gamma, inla_fsw_age, col = "survey_id")
  
  fsw_agemod_gamma_samples %>% 
    filter(model == "full_sample") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year, y = exp_mean+11)) +
    geom_ribbon(aes(x = year, ymin = exp_lower+11, ymax = exp_upper+11), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) + 
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Median Age") +
    theme(aspect.ratio = 1)
  
  
  age_formula <- (age_centre) ~ 1 + year_centre + f(survey_id, model = "iid")
  
  fsw_agemod_weibull <- INLA::inla(formula = age_formula,
                                 family = "weibull",
                                 # offset = log(genpop_median),
                                 control.compute=list(config = TRUE),
                                 data = inla_fsw_age)
  
  summary(fsw_agemod_weibull)
  
  fsw_agemod_weibull_samples <- moz.utils::sample_model(fsw_agemod_weibull, inla_fsw_age, col = "survey_id")
  
  fsw_agemod_weibull_samples %>% 
    filter(model == "full_sample") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year, y = exp_mean+11)) +
    geom_ribbon(aes(x = year, ymin = exp_lower+11, ymax = exp_upper+11), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) + 
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Median Age") +
    theme(aspect.ratio = 1)
  
  
  