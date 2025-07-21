library(tidyverse)
library(INLA)

fsw_age_dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fsw_age_dat2105.rds")
genpop_median_ages <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/genpop_median_ages2105.rds")

inla_fsw_age <- data.frame() %>%
  bind_rows(fsw_age_dat %>% select(year, age, survey_id)) %>%
  mutate(year_min = year - min(year))

age_formula <- log(age) ~ 1

fsw_agemod <- INLA::inla(formula = age_formula,
                         # family = "gaussian",
                         family = "normal",
                         # E = genpop_median,
                         control.compute=list(config = TRUE),
                         data = inla_fsw_age)

summary(fsw_agemod)

fsw_age_dat %>%
  ggplot() +
  geom_density(aes(x=log(age), color = survey_id), show.legend = F) +
  geom_vline(aes(xintercept = fsw_agemod$summary.fixed$mean))

#######

inla_fsw_age <- data.frame(year = 2000:2020) %>%
  bind_rows(fsw_age_dat %>% select(year, age, survey_id)) %>%
  mutate(year_min = year - min(year))

age_formula <- log(age) ~ 1 + year_min

fsw_agemod <- INLA::inla(formula = age_formula,
                         # family = "gaussian",
                         family = "normal",
                         # E = genpop_median,
                         control.compute=list(config = TRUE),
                         data = inla_fsw_age)

summary(fsw_agemod)

fsw_agemod_samples <- moz.utils::sample_model(fsw_agemod, inla_fsw_age, col = "age")

inla_fsw_age %>%
  ggplot() +
  geom_density(aes(x=log(age), color = survey_id), show.legend = F) +
  geom_vline(data = fsw_agemod_samples %>% distinct(year, mean), aes(xintercept = mean)) +
  facet_wrap(~year)

########

inla_df <- fsw_age_dat %>% 
  select(iso3, year) %>% distinct()

# inla_fsw_age <- crossing(year = 2000:2020, 
#                          iso3 = moz.utils::ssa_iso3()) %>%
#   bind_rows(fsw_age_dat %>% select(year, iso3, age, survey_id)) %>%
#   mutate(year_min = year - min(year)) %>% 
#   # separate_survey_id() %>% 
#   mutate(iso3_id = iso3)

inla_fsw_age <- inla_df %>%
  bind_rows(fsw_age_dat %>% select(year, iso3, age, survey_id)) %>%
  mutate(year_min = year - min(year)) %>% 
  mutate(iso3_id = iso3)

age_formula <- age ~ 1 + year_min + f(iso3) + f(iso3_id, year_min)

fsw_agemod <- INLA::inla(formula = age_formula,
                         # family = "gaussian",
                         family = "lognormal",
                         # E = genpop_median,
                         control.compute=list(config = TRUE),
                         data = inla_fsw_age)

summary(fsw_agemod)

fsw_agemod_samples <- moz.utils::sample_model(fsw_agemod, inla_fsw_age, col = "age")

inla_fsw_age %>%
  ggplot() +
  geom_density(aes(x=log(age), color = survey_id), show.legend = F) +
  geom_vline(data = fsw_agemod_samples %>% filter(is.na(survey_id)) %>% distinct(year, mean), aes(xintercept = mean)) +
  facet_wrap(~year)

#########

fsw_agemod_samples %>% 
  filter(is.na(survey_id)) %>%
  ggplot() + 
  geom_line(aes(x = year , y = (mean), color = iso3)) +
  # geom_ribbon(aes(x = year, ymin = (lower), ymax = (upper), fill = iso3), alpha = 0.3) +
  geom_point(data = fsw_age_dat , aes(x = year, y = log(mean_kp_age), size = denom, color = iso3), show.legend = F) + 
  moz.utils::standard_theme() + 
  labs(x = "Year", y = "Log mean Age") +
  theme(aspect.ratio = 1) + 
  ggtitle("FSW")


#### Add offset

inla_fsw_age <- data.frame(year = 2000:2020) %>%
  bind_rows(fsw_age_dat %>% select(iso3, year, age, survey_id) %>%
              left_join(genpop_median_ages %>% filter(sex == "female") %>% select(iso3, year, genpop_median_age = median_age))) %>%
  mutate(year_min = year - min(year))


age_formula <- log(age) ~ 1 + year_min + f(survey_id) + offset(log(genpop_median_age))

fsw_agemod <- INLA::inla(formula = age_formula,
                         # family = "gaussian",
                         family = "normal",
                         # E = genpop_median,
                         control.compute=list(config = TRUE),
                         data = inla_fsw_age)

summary(fsw_agemod)

fsw_agemod_samples <- moz.utils::sample_model(fsw_agemod, inla_fsw_age, col = "age")

fsw_agemod_samples %>%
  filter(is.na(survey_id)) %>%
  ggplot() +
  geom_line(aes(x=year, y=exp(mean))) +
  moz.utils::standard_theme() + 
  labs(x = "Year", y = "FSW:genpop age ratio") +
  theme(aspect.ratio = 1) + 
  ggtitle("FSW")
# geom_density(aes(x=log(age), color = survey_id), show.legend = F) +
# geom_vline(data = fsw_agemod_samples %>% filter(is.na(survey_id)) %>% distinct(year, mean), aes(xintercept = mean)) +
# facet_wrap(~year)