library(tidyverse)
library(INLA)
library(dfertility)
library(moz.utils)

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_2105.rds")

# Custom Matrices
spec_hiv <- spectrum_dat %>% 
  group_by(iso3, year, age, sex) %>% 
  summarise(hivpop = sum(hivpop), 
            totpop = sum(totpop)) %>% 
  ungroup() %>% 
  mutate(tot_prev = hivpop/totpop) %>% 
  bind_rows(spectrum_dat %>% 
              group_by(iso3, year, age) %>% 
              summarise(hivpop = sum(hivpop), 
                        totpop = sum(totpop)) %>% 
              ungroup() %>% 
              mutate(tot_prev = hivpop/totpop,
                     sex = "everyone"))


saveRDS(spec_hiv, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/spec_hiv.rds")
spec_hiv <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/spec_hiv.rds")

dat <- dat %>%
  mutate(sex = case_when(kp == "PWID" & is.na(sex) & gender %in%  c("female", "transgender") ~ 1,
                         kp == "PWID" & is.na(sex) & gender %in%  c("male", "TGM") ~ 0,
                         TRUE ~ sex),
         kp2 = case_when(kp == "MSM" & gender %in% c("0", "male", NA) ~ "MSM",
                         kp == "MSM" & gender %in% c("1", "3", "female", "TGW") ~ "TGW",
                         kp == "MSM" & gender %in% c("5", "other") ~ "TGM/Other",
                         kp == "MSMTG" & gender == "male" ~ "MSM",
                         kp == "MSMTG" & gender %in% c("female", "tgw") ~ "TGW",
                         kp == "MSMTG" & gender == "non-binary" ~ "TGM/Other",
                         kp == "TG" & gender %in% c("3", "1") & sex == 0 ~ "TGW",
                         kp == "TG" & gender %in% c("0", "4") & sex == 1 ~ "TGM/Other",
                         kp == "TGM" ~ "TGM/Other",
                         TRUE ~ kp)) %>% 
  filter(!kp %in% c("PWUD", "TG")) %>% 
  mutate(kp = kp2) 



hivdat_allkp <- dat %>% 
  filter(!is.na(hiv)) %>% 
         # !(kp == "FSW" & sex == 0),
         # !(kp == "MSM" & sex == 1)) %>%
  group_by(survey_id, iso3, sex, year, kp, age, hiv) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, iso3, sex, year, kp, age) %>% 
  mutate(denom = sum(n)) %>% 
  ungroup() %>% 
  mutate(sex = case_when(sex == 1 ~ "female",
                         sex == 0 ~ "male",
                         is.na(sex) & kp == "PWID" ~ "everyone",
                         kp == "CFSW" ~ "male",
                         kp == "TGW" ~ "female",
                         kp == "FSW" ~ "female",
                         is.na(sex) & kp == "MSM" ~ "male",
                         kp == "TGM/Other" ~ "male")) %>% 
  left_join(spec_hiv) %>% 
  filter(!age < 15)
  

crossing(hiv = c(1, 0),
         age = c(15:49)) %>% 
  left_join(hivdat_allkp)


fsw_dat <- hivdat_allkp %>% filter(kp == "FSW")

fsw_dat_iso3 <- unique(fsw_dat$iso3)


fsw_hivtime_inla <- spec_hiv %>% 
  filter(iso3 %in% fsw_dat_iso3,
         sex == "female") %>% 
  left_join(moz.utils::region()) %>% 
  group_by(year, age, sex, region) %>% 
  summarise(hivpop = sum(hivpop),
            totpop = sum(totpop),
            tot_prev = hivpop/totpop) %>% 
  ungroup() %>% 
  mutate(model = "regions") %>% 
  bind_rows(
    spec_hiv %>% 
      filter(iso3 %in% fsw_dat_iso3,
             sex == "female") %>% 
      left_join(moz.utils::region()) %>% 
      group_by(year, age, sex) %>% 
      summarise(hivpop = sum(hivpop),
                totpop = sum(totpop),
                tot_prev = hivpop/totpop) %>% 
      ungroup() %>% 
      mutate(model = "SSA")
  ) %>% 
  bind_rows(spec_hiv %>% 
                    filter(iso3 %in% fsw_dat_iso3,
                           sex == "female") %>% 
                    mutate(model = "countries") ) %>% 
  select(region, iso3, year, age, tot_prev, model) %>% 
  filter(year %in% 1993:2023) %>% 
  mutate(denom = 1) %>% 
  bind_rows(fsw_dat %>% left_join(moz.utils::region()) %>% filter(hiv == 1)) %>% 
  mutate(id.year = multi.utils::to_int(year),
         id.age = multi.utils::to_int(age),
         id.year.age = group_indices(., age, year),
         id.year2 = id.year+1) %>% 
  mutate(kp_prev = n/denom,
         kp_odds = kp_prev/(1-kp_prev),
         genpop_odds = tot_prev/(1-tot_prev),
         or_obs = kp_odds/genpop_odds) %>% 
  filter(age %in% c(15:49)) %>% 
  select(region, iso3, year, age, tot_prev, model, n, denom, or_obs, genpop_odds, kp_odds, survey_id, sex, kp, id.year, id.age, id.year.age, id.year2) %>% 
  mutate(id.age2 = id.age,
         id.survey_id = multi.utils::to_int(survey_id),
         id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2)) 




n_ages <- length(unique(fsw_hivtime_inla$age))
n_years <- length(unique(fsw_hivtime_inla$year))
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

A_combined2 <- rbind(A_sum_year_age, A_sum_age_year) # Combining our matrices for the interaction ## Add back in when not matching INLA defaults
e2 <- matrix(0, nrow(A_combined2), nrow = 1) 

## JIE: changed adjust_diagonal = FALSE
R_year <- dfertility::make_rw_structure_matrix(n_years, 2, FALSE)
R_age <- dfertility::make_rw_structure_matrix(n_ages, 2, FALSE)
# R_age <- as(diag(1, n_ages), "sparseMatrix")
Q2 <- kronecker(R_age, R_year)

# custom_mod <- inla(n ~ 1
#                    + f(id.year.age, model = "generic0", Cmatrix = Q2, extraconstr = list(A = A_sum_year_age, e = e2)),
#                    Ntrials = denom,
#                    offset = qlogis(tot_prev),
#                    data = dat,
#                    family = "xbinomial",
#                    control.family = list(link = "logit"),
#                    control.compute=list(config = TRUE)
# )

jeff_formula <- n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") + f(iso3, model = "iid") + f(id.year.age, model = "generic0",
    Cmatrix = Q2,
    extraconstr = list(A = A_combined2, e = e2),
    rankdef = n_years + n_ages - 1L)

geog_graph = read_sf(moz.utils::national_areas()) %>% mutate(iso3 = area_id) %>% rename(id.iso3_2 = id.iso3)

jeff_formula_survey <- n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") + f(iso3, model = "iid") + f(id.year.age, model = "generic0",
      Cmatrix = Q2,
      extraconstr = list(A = A_combined2, e = e2),
      rankdef = n_years + n_ages - 1L) + 
  f(id.age2, model = "ar1", group = id.iso3, control.group = list(model = "besag", graph = ))
  # f(id.age2, model = "ar1", group = id.survey_id, control.group = list(model = "iid"))




jeff_formula_noiso3 <- n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") + f(id.year.age, model = "generic0",
                                                                                                Cmatrix = Q2,
                                                                                                          extraconstr = list(A = A_combined2, e = e2),
                                                                                                          rankdef = n_years + n_ages - 1L)

jeff_formula_less <- n ~ 1 + f(id.year, model = "rw2") + f(iso3, model = "iid") + f(id.year.age, model = "generic0",  Cmatrix = Q2, extraconstr = list(A = A_combined2, e = e2), rankdef = n_years + n_ages - 1L)
                                                                                                          
jeff_formula_even_less <- n ~ 1 + f(iso3, model = "iid") + f(id.year.age, model = "generic0",  Cmatrix = Q2, extraconstr = list(A = A_combined2, e = e2), rankdef = n_years + n_ages - 1L)

basic_formula <- n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1")

#Basic 
fsw_hivtime_mod <- inla(formula = basic_formula,
                        Ntrials = denom,
                        # E = tot_prev,
                        offset = logit(tot_prev),
                        data = fsw_hivtime_inla,
                        family = "xbinomial",
                        control.family = list(link = "logit"),
                        control.compute=list(config = TRUE)
)

fsw_hivtime_samples <- moz.utils::sample_model(fsw_hivtime_mod, fsw_hivtime_inla, col ="survey_id")

# Plotting the OR
(basic_form_plot <- fsw_hivtime_samples %>% 
    mutate(logit_totprev = logit(tot_prev),
           log_or = mean - logit_totprev,
           or = exp(log_or)) %>% 
  filter(model == "countries") %>% 
  # select(year, iso3, age, tot_prev, mean, lower, upper, or) %>%
  # mutate(nat_mean = exp(mean),
  #        nat_lower = exp(lower),
  #        nat_upper = exp(upper)) %>% 
  filter(year %in% c(1995, 2000, 2010, 2015, 2020)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot() + 
    geom_point(data = fsw_hivtime_inla, aes(x = age, y = or_obs, color = factor(year)), alpha = 0.5) +
    scale_y_log10() +
    geom_line(aes(x = age, y = or, color = year)) +
    facet_wrap(~iso3)) +
  theme_minimal()


## JIE: accounted for rank deficiency of RW1 x RW1 Qmatrix
fsw_hivtime_mod_jf <- inla(formula = jeff_formula_survey,
                        Ntrials = denom,
                        # E = tot_prev,
                        offset = qlogis(tot_prev),
                        data = fsw_hivtime_inla,
                        family = "xbinomial",
                        control.inla = list(int.strategy = "eb"),
                        control.family = list(link = "logit"),
                        control.compute=list(config = TRUE)
)

plot(fsw_hivtime_mod_jf$summary.random$id.year$mean)

summary(fsw_hivtime_mod_jf)

fsw_hivtime_samples_jf <- moz.utils::sample_model(fsw_hivtime_mod_jf, fsw_hivtime_inla, col ="survey_id")

# Plotting the OR
(jeff_form_plot <- fsw_hivtime_samples_jf %>% 
    mutate(logit_totprev = qlogis(tot_prev),
           log_or = mean - logit_totprev,
           or = exp(log_or)) %>% 
  filter(model == "countries") %>% 
  # select(year, iso3, age, tot_prev, mean, lower, upper) %>%
  # mutate(nat_mean = exp(mean),
  #        nat_lower = exp(lower),
  #        nat_upper = exp(upper)) %>% 
  filter(year %in% c(1995, 2000, 2010, 2015, 2020)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot() + 
    geom_point(data = fsw_hivtime_inla, aes(x = age, y = or_obs, color = factor(year)), shape = 1, show.legend = F, alpha = 0.5) +
  geom_line(aes(x = age, y = or, color = year)) + 
    scale_y_log10() +
  facet_wrap(~iso3)) + 
  theme_minimal()


# Plotting the Oprev
(jeff_form_plot <- fsw_hivtime_samples_jf %>% 
    mutate(logit_totprev = qlogis(tot_prev),
           log_or = mean - logit_totprev,
           or = exp(log_or),
           prev = plogis(mean)) %>% 
    filter(model == "countries") %>% 
    select(iso3, age, year, prev) %>% 
    right_join(fsw_hivtime_inla) %>% 
    # select(year, iso3, age, tot_prev, mean, lower, upper) %>%
    # mutate(nat_mean = exp(mean),
    #        nat_lower = exp(lower),
    #        nat_upper = exp(upper)) %>% 
    # filter(year %in% c(1995, 2000, 2010, 2015, 2020)) %>% 
    mutate(year = factor(year)) %>% 
    ggplot() + 
    geom_point(data = fsw_hivtime_inla, aes(x = age, y = n/denom, color = factor(year)),  show.legend = F, alpha = 0.5) +
    geom_line(aes(x = age, y = prev, color = year)) + 
    # scale_y_log10() +
    facet_wrap(~survey_id)) + 
  theme_minimal()


# saveRDS(fsw_hivtime_ben, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/Age dat/BEN_snippet_1206.rds")

# Benin
fsw_hivtime_ben <- fsw_hivtime_inla %>% 
  filter(iso3 == "BEN") 

saveRDS(fsw_hivtime_ben, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/Age dat/BEN_snippet_1206.rds")

# data check
fsw_hivtime_inla %>% 
  filter(iso3 == "BEN") %>% ggplot() + geom_line(aes(x = age, y = qlogis(tot_prev))) + facet_wrap(~year)
# + geom_line(aes(x = age, y = n/denom , color = survey_id)) 


fsw_hivtime_inla %>% 
  filter(iso3 == "BEN") %>% 
  mutate(kp_prev = n/denom,
         kp_odds = kp_prev/(1-kp_prev),
         genpop_odds = tot_prev/(1-tot_prev),
         or = kp_odds/genpop_odds) %>% 
  ggplot() + geom_line(aes(x = age, y = or , color = survey_id)) + facet_wrap(~year) + scale_y_log10()


basic_formula <- n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") + logit(tot_prev)

fsw_hivtime_mod_ben <- inla(formula = basic_formula,
                               Ntrials = denom,
                               # E = tot_prev,
                               # offset = logit(tot_prev),
                               data = fsw_hivtime_ben,
                               family = "xbinomial",
                               # control.family = list(link = "logit"),
                               control.compute=list(config = TRUE)
)


summary(fsw_hivtime_mod_ben)

fsw_hivtime_samples_ben <- moz.utils::sample_model(fsw_hivtime_mod_ben, fsw_hivtime_ben, col ="survey_id")

# Plotting the OR
(ben_form_plot <- fsw_hivtime_samples_ben %>% 
    filter(model == "countries") %>% 
    select(year, iso3, age, tot_prev, mean, lower, upper) %>%
    mutate(nat_mean = exp(mean),
           nat_lower = exp(lower),
           nat_upper = exp(upper)) %>% 
    filter(year %in% c(1995, 2000, 2010, 2015, 2020)) %>% 
    mutate(year = factor(year)) %>% 
    ggplot() + 
    geom_line(aes(x = age, y = nat_upper, color = year)) + 
    facet_wrap(~year))

plotdat <- fsw_hivtime_ben %>% filter(!is.na(survey_id)) %>% mutate(prev = n/denom)
# Plotting the OR
(ben_form_plot <- fsw_hivtime_samples_ben %>% 
    filter(model == "countries") %>% 
    select(year, iso3, age, tot_prev, mean, lower, upper) %>%
    mutate(nat_mean = plogis(mean),
           nat_lower = plogis(lower),
           nat_upper = plogis(upper)) %>% 
    filter(year %in% plotdat$year) %>% 
    # mutate(year = factor(year)) %>% 
    ggplot() + 
    geom_line(aes(x = age, y = nat_upper, color = year)) +
    geom_point(data = plotdat , aes(x = age, y = prev)) +
    facet_wrap(~year))

# 




fsw_hivtime_mod_jf_ben <- inla(formula = jeff_formula,
                           Ntrials = denom,
                           # E = tot_prev,
                           offset = logit(tot_prev),
                           data = fsw_hivtime_ben,
                           family = "xbinomial",
                           control.family = list(link = "logit"),
                           control.compute=list(config = TRUE)
)


summary(fsw_hivtime_mod)

fsw_hivtime_samples_jf_ben <- moz.utils::sample_model(fsw_hivtime_mod_jf_ben, fsw_hivtime_ben, col ="survey_id")

# Plotting the OR
(jeff_ben_form_plot <- fsw_hivtime_samples_jf_ben %>% 
    filter(model == "countries") %>% 
    select(year, iso3, age, tot_prev, mean, lower, upper) %>%
    mutate(nat_mean = exp(mean),
           nat_lower = exp(lower),
           nat_upper = exp(upper)) %>% 
    filter(year %in% c(1995, 2000, 2010, 2015, 2020)) %>% 
    mutate(year = factor(year)) %>% 
    ggplot() + 
    geom_line(aes(x = age, y = nat_upper, color = year)) + 
    facet_wrap(~year, scales = "free"))


fsw_hivtime_mod_jf_ben <- inla(formula = jeff_formula,
                               Ntrials = denom,
                               # E = tot_prev,
                               offset = logit(tot_prev),
                               data = fsw_hivtime_ben,
                               family = "xbinomial",
                               control.family = list(link = "logit"),
                               control.compute=list(config = TRUE)
)


summary(fsw_hivtime_mod)

fsw_hivtime_samples_jf_ben <- moz.utils::sample_model(fsw_hivtime_mod_jf_ben, fsw_hivtime_ben, col ="survey_id")

# Plotting the OR
(jeff_ben_form_plot <- fsw_hivtime_samples_jf_ben %>% 
    filter(model == "countries") %>% 
    select(year, iso3, age, tot_prev, mean, lower, upper) %>%
    mutate(nat_mean = exp(mean),
           nat_lower = exp(lower),
           nat_upper = exp(upper)) %>% 
    filter(year %in% c(1995, 2000, 2010, 2015, 2020)) %>% 
    mutate(year = factor(year)) %>% 
    ggplot() + 
    geom_line(aes(x = age, y = nat_upper, color = year)) + 
    facet_wrap(~year, scales = "free"))




# # Tests
# default_mod$misc$configs$constr$e #46 sum to zero
# custom_mod$misc$configs$constr$e #46 sum to zero
# 
# default_mod$misc$configs$constr$e #46 sum to zero
# custom_mod$misc$configs$constr$e #46 sum to zero
# 
# all.equal(default_mod$misc$configs$constr$A, custom_mod$misc$configs$constr$A ) #Identical in both models.
# 
# # These are different
# default_mod$misc$configs$config[[1]]$Q
# custom_mod$misc$configs$config[[1]]$Q
# 
# default_mod$misc$configs$config[[1]]$Qprior
# custom_mod$misc$configs$config[[1]]$Qprior
# 
# # Both 1427 x 1427
# dim(default_mod$misc$configs$config[[1]]$Q)
# dim(custom_mod$misc$configs$config[[1]]$Q)
# 
# 
# summary(default_mod) #Huge CrI on the intercept, big precision on the hyperparameters.
# summary(custom_mod)
# summary(correct_mod)

# ## JIE: this is useful
# grep("rank", default_mod$logfile, value = TRUE)
# grep("rank", custom_mod$logfile, value = TRUE)
# grep("rank", correct_mod$logfile, value = TRUE)


fsw_hivtime_samples <- moz.utils::sample_model(fsw_hivtime_mod, fsw_hivtime_inla, col ="survey_id")


fsw_hivtime_samples %>% 
  filter(model == "countries") %>% 
  select(year, iso3, age, tot_prev, mean, lower, upper) %>%
  mutate(nat_mean = invlogit(mean),
         nat_lower = invlogit(lower),
         nat_upper = invlogit(upper)) %>% 
  filter(year %in% c(1995, 2000, 2010, 2015, 2020)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = nat_upper, color = year)) +
  facet_wrap(~iso3)
  # geom_point(dat = fsw_hivtime_inla %>% filter(is.na(model), aes(x = (n/denom)/totprev)))

fsw_hivtime_samples %>% 
  filter(model == "countries") %>% 
  select(year, iso3, age, tot_prev, mean, lower, upper) %>%
  mutate(nat_mean = exp(mean),
         nat_lower = exp(lower),
         nat_upper = exp(upper)) %>% 
  filter(year %in% c(1995, 2000, 2010, 2015, 2020)) %>% 
  mutate(year = factor(year)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = nat_upper, color = year)) + 
  facet_wrap(~iso3)

fsw_dat %>% 
  mutate(prev = n/denom) %>% 
  filter(year %in% c(1998:2005),
         hiv == 1) %>% 
  left_join(spec_hiv %>% filter(sex == "female")) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = prev, color = survey_id)) + 
  geom_line(aes(x = age, y = tot_prev), color = "red") + 
  facet_wrap(~survey_id)


fsw_dat %>% mutate(year_cat = plyr::round_any(year, 3)) %>% 
  filter(age %in% c(15:49),
         hiv == 1) %>%
  ggplot() +
  geom_point(aes(x = age, y = n/denom, color = survey_id)) +
  facet_wrap(~year_cat)


