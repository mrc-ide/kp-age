library(tidyverse)
library(INLA)
library(moz.utils)


spectrum_dat <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop)

hivdat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/hivbyage_inla_model_dat_0904.rds")


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
  dplyr::select(-kp) %>% 
  left_join(spectrum_dat %>% dplyr::select(iso3, year, age, sex, hivpop, totpop) %>% left_join(moz.utils::region()) %>% group_by(region, year, age, sex) %>%  summarise(tot_prev = sum(hivpop)/sum(totpop))) %>% 
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

saveRDS(fsw_inla_hivdat_agemod, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/inla_model_test_2904.rds")
dat <- fsw_inla_hivdat_agemod %>%
  mutate(id.year.age = group_indices(., age, year))
  # mutate(id.year.age = group_indices(., age, year))
n_ages <- length(unique(dat$age))
n_years <- length(unique(dat$year))
n_interactions_year <- n_years * n_ages


# Sum-to-zero constraint within each year for age effects
A_sum_age_year <- matrix(0, nrow = n_years, ncol = n_interactions_year)

 # Put this back in when done replicating INLA defauly
# Loop over each year to impose sum-to-zero constraint across the 5 age groups
for (year in 1:n_years) {
  A_sum_age_year[year, ((year - 1) * n_ages + 1):(year * n_ages)] <- 1
} # This makes the matrix so row 1 is a 1 for every age for the first year

## Same in reverse to have sum-to-zero constraints within each age over year
A_sum_year_age <- matrix(0, nrow = n_ages, ncol = n_interactions_year)

for (age in 1:n_ages) {
  A_sum_year_age[age, ((age - 1) * n_years + 1):(age * n_years)] <- 1
} # This makes the matrix so row 1 is a 1 for ever year for the first age

A_combined2 <- rbind(A_sum_year_age) 
# A_combined2 <- rbind(A_sum_year_age, A_sum_age_year) # Combining our matrices for the interaction ## Add back in when not matching INLA defaults
e2 <- matrix(0, nrow(A_combined2), nrow = 1) # This is saying what we want our constraints to sum to

R_year <- dfertility::make_rw_structure_matrix(n_years, 1)
R_age <- dfertility::make_rw_structure_matrix(n_ages, 1)
R_age <- as(diag(1, n_ages), "sparseMatrix")
Q2 <- kronecker(R_age, R_year)

fsw_agehiv_mod <- inla(n ~ 1
                       + f(id.year.age, model = "generic0", Cmatrix = Q2, extraconstr = list(A = A_combined2, e = e2)),
                       Ntrials = denom,
                       offset = qlogis(tot_prev),
                       data = dat,
                       family = "xbinomial",
                       control.family = list(link = "logit"),
                       control.compute=list(config = TRUE)
)

fsw_agehiv_mod$misc$configs$constr

# fsw_agehiv_mod <- inla(n ~ 1
#                        + region
#                        + f(survey_id, model = "iid")
#                        + id.year
#                        + f(id.year.age, model = "generic0", Cmatrix = Q2, extraconstr = list(A = A_combined2, e = e2))
#                        # +f(id.year2, model = "rw2", scale.model = T, group = id.age, control.group = list(model = "rw2"))
#                        + f(id.age, model = "ar", order = 2), 
#                        # + f(id.year, model = "ar", order = 2)
#                        # + f(id.iso3, model = "besag", graph = moz.utils::national_adj(), scale.model = T),
#                        Ntrials = denom,
#                        offset = qlogis(tot_prev),
#                        data = dat,
#                        family = "xbinomial",
#                        control.family = list(link = "logit"),
#                        control.compute=list(config = TRUE)
# )

# fsw_agehiv_mod <- inla(n ~ 1
#                        + region
#                        + f(survey_id, model = "iid")
#                        + id.year
#                        + f(id.year.age, model = "generic0", Cmatrix = Q2, extraconstr = list(A = A_combined2, e = e2))
#                        # +f(id.year2, model = "rw2", scale.model = T, group = id.age, control.group = list(model = "rw2"))
#                        + f(id.age, model = "ar", order = 2), 
#                        # + f(id.year, model = "ar", order = 2)
#                        # + f(id.iso3, model = "besag", graph = moz.utils::national_adj(), scale.model = T),
#                        Ntrials = denom,
#                        offset = qlogis(tot_prev),
#                        data = dat,
#                        family = "xbinomial",
#                        control.family = list(link = "logit"),
#                        control.compute=list(config = TRUE)
# )

summary(fsw_agehiv_mod)

moz.utils::clean_inla(fsw_agehiv_mod)$random %>% filter(var == "id.year.age")

fsw_agehiv_mod$misc$configs$constr$n

count(fsw_inla_hivdat_agemod, year)

fsw_inla_hivdat_agemod[c(2107, 1932 ,2004, 1972),]

predicted_dat <- moz.utils::sample_model(fsw_agehiv_mod, fsw_inla_hivdat_agemod, "n") %>% 
  mutate(exp_lower = exp(lower),
         exp_mean = exp(mean),
         exp_upper = exp(upper))

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
