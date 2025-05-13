age_dat <- single_year_ages %>% 
  group_by(survey_id, denom, kp, year, iso3, sex) %>% 
  summarise(median_age = median(age)) %>% 
  ungroup() %>% 
  mutate(sex = ifelse(sex == 0, "male", "female"))


genpop_median_ages <- spectrum_dat %>%
  filter(year %in% dat$year,
         iso3 %in% dat$iso3) %>% 
  select(iso3, year, age, sex, totpop) %>% 
  group_by(iso3, year, sex) %>%
  summarise(median_age = median(rep(age, totpop), na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(megamedian = median(median_age)) %>% 
  ungroup()


fsw_age_dat <- distribution_plot %>% 
  filter(kp == "FSW",
         distribution == "Age") %>% 
  rename(age = variable) %>% 
  group_by(survey_id) %>% 
  mutate(median_kp_age = median(age, na.rm = T),
         mean_kp_age = mean(age, na.rm = T),
         denom = n()) %>% 
  ungroup() %>% 
  select(-x_min, -x_max, -distribution) %>% 
  left_join(genpop_median_ages %>% filter(sex == "female") %>% rename(genpop_median = median_age))


inla_fsw_age <- crossing(age = 15:50,
                         genpop_median
                         year = 2000:2023) 

inla_fsw_age <- fsw_age_dat %>% 
  select(iso3, year, genpop_median, age) %>% 
  distinct() %>% 
  bind_rows(fsw_age_dat)

age_formula <- log(age) ~ 1 + year + f(survey_id, model = "iid")

fsw_agemod <- INLA::inla(formula = age_formula,
                         family = "gaussian",
                         offset = genpop_median,
                         control.compute=list(config = TRUE),
                         data = inla_fsw_age)

summary(fsw_agemod)

fsw_agemod_samples <- moz.utils::sample_model(fsw_agemod, inla_fsw_age, col = "survey_id")


# fsw_medianages_plot <- 
  fsw_agemod_samples %>% 
  mutate(exp_mean = exp(mean),
         exp_lower = exp(lower),
         exp_upper = exp(upper)) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = exp_mean)) +
  geom_ribbon(aes(x = year, ymin = exp_lower, ymax = exp_upper), alpha = 0.3) +
  geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) + 
  moz.utils::standard_theme() + 
  labs(x = "Year", y = "Median Age") +
    facet_wrap(year~iso3) + 
    lims(x = c(y ))
  # geom_line(data = genpop_medians_plot_fem %>% group_by(year) %>% mutate(median = median(genpop_median)), aes(x = year, y = median), color = "darkred")
  