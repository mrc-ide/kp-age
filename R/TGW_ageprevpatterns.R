library(purrr)
library(INLA)
library(tidyverse)
library(sf)
library(moz.utils)

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_2105.rds")
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
  kitchen.sink::single_year_to_five_year() %>% 
  group_by(survey_id, iso3, sex, year, kp, age_group, hiv) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, iso3, sex, year, kp, age_group) %>% 
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
  left_join(spec_hiv %>% 
              kitchen.sink::single_year_to_five_year()  %>% 
              group_by(iso3, year, sex, age_group) %>% 
              summarise(hivpop = sum(hivpop),
                        totpop = sum(totpop),
                        tot_prev = hivpop/totpop)) 
# filter(!age < 15)


tgw_dat <- hivdat_allkp %>% filter(kp == "TGW")

tgw_dat_iso3 <- unique(tgw_dat$iso3)

pred_tgw <- spec_hiv %>%
  kitchen.sink::single_year_to_five_year() %>% 
  filter( #iso3 %in% tgw_dat_iso3,
    sex == "male",
    # age %in% 15:49,
    year %in% 2010:2025) %>%
  left_join(moz.utils::region()) %>%
  bind_rows(mutate(., region = "SSA")) %>%
  group_by(year, age_group, region) %>%
  summarise(hivpop = sum(hivpop),
            totpop = sum(totpop),
            tot_prev = hivpop/totpop) %>%
  ungroup() %>%
  mutate(model = "regions") %>%
  bind_rows(
    spec_hiv %>% 
      kitchen.sink::single_year_to_five_year() %>% 
      group_by(iso3, year, sex, age_group) %>% 
      summarise(hivpop = sum(hivpop),
                totpop = sum(totpop),
                tot_prev = hivpop/totpop) %>% 
      ungroup() %>% 
      filter( #iso3 %in% tgw_dat_iso3,
        sex == "female",
        # age %in% 15:49,
        year %in% 2010:2025) %>%
      mutate(model = "countries") %>%
      left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>% 
      mutate(id.iso3.age = group_indices(., age_group, id.iso3))
  ) %>%
  mutate(denom = 1) %>% 
  mutate(id.age_group = factor(multi.utils::to_int(age_group))
  )

data_prep_tgw <- tgw_dat %>%
  left_join(moz.utils::region()) %>%
  pivot_wider(
    names_from = hiv,
    values_from = n,
    names_prefix = "hiv_",
    values_fill = 0  # fill missing with 0 if a group is missing hiv==1 or hiv==0
  ) %>% 
  rename(n = hiv_1,
         n_neg = hiv_0) %>% 
  mutate(denom = n + n_neg) %>% 
  # kitchen.sink::single_year_to_five_year(age) %>% 
  # age %in% 15:49) %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  left_join(pred_tgw %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age_group, id.iso3.age)) %>% 
  mutate(id.age_group = factor(multi.utils::to_int(age_group)))


tgw_inla_dat <- pred_tgw %>%
  bind_rows(data_prep_tgw) %>% 
  mutate(id.year = multi.utils::to_int(year),
         id.year2 = id.year+1) %>% 
  mutate(kp_prev = n/denom,
         kp_odds = kp_prev/(1-kp_prev),
         genpop_odds = tot_prev/(1-tot_prev),
         or_obs = kp_odds/genpop_odds) %>% 
  mutate(id.survey_id = multi.utils::to_int(survey_id),
         id.age_group = multi.utils::to_int(id.age_group),
         id.age_group2 = ifelse(is.na(survey_id), NA_integer_, id.age_group),
         id.iso3.year = group_indices(., iso3, year),
         id.iso3.year = ifelse(is.na(iso3), NA_integer_, id.iso3.year)) %>% 
  #id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2)) %>% 
  # kitchen.sink::single_year_to_five_year(age) %>% 
  mutate(id.age_group = factor(multi.utils::to_int(age_group)),
         id.year.age = group_indices(., age_group, year)
  ) 

## Generic 0 set-up
n_ages <- length(unique(pred_tgw$age))
n_ages <- length(unique(pred_tgw$age_group))
n_years_tgw <- length(unique(pred_tgw$year))

n_interactions_year_tgw <- n_years_tgw * n_ages

# Sum-to-zero constraint within each year for age effects
A_sum_age_year_tgw <- matrix(0, nrow = n_years_tgw, ncol = n_interactions_year_tgw)

for (year in 1:n_years_tgw) {
  A_sum_age_year_tgw[year, ((year - 1) * n_ages + 1):(year * n_ages)] <- 1
} 

## Same in reverse to have sum-to-zero constraints within each age over year
A_sum_year_age_tgw <- matrix(0, nrow = n_ages, ncol = n_interactions_year_tgw)

for (age in 1:n_ages) {
  A_sum_year_age_tgw[age, ((age - 1) * n_years_tgw + 1):(age * n_years_tgw)] <- 1
}

A_combined2_tgw <- rbind(A_sum_year_age_tgw, A_sum_age_year_tgw) 
e2_tgw <- matrix(0, nrow(A_combined2_tgw), nrow = 1) 

R_age <- dfertility::make_rw_structure_matrix(n_ages, 2, FALSE) 
R_year_tgw <- dfertility::make_rw_structure_matrix(n_years_tgw, 2, FALSE) 

Q2_tgw <- kronecker(R_age, R_year_tgw)


### 

tgw_formulas_simple <- list(
  mod0 = n ~ 1 + f(id.year, model = "rw2"),
  
  mod1 = n ~ 1 + f(id.age_group, model = "ar1"),
  
  mod2 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1")
  # 
  # mod3 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") +
  #   f(id.year.age, model = "generic0",
  #     Cmatrix = Q2_tgw,
  #     extraconstr = list(A = A_combined2_tgw, e = e2_tgw),
  #     rankdef = n_years_tgw + n_ages - 1L)
)

tgw_formulas_countrysurv1 <- list(
  
  mod3 = n ~ 1 + f(survey_id, model = "iid") + f(id.age_group, model = "ar1") +
    f(id.age_group2, model = "rw2",  group = id.survey_id, control.group = list(model = "iid")),
  
  mod4 = n ~ 1 + f(id.iso3, model = "besag", graph = national_adj()) + f(id.age_group, model = "ar1") +
    f(id.age_group2, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj())),
  
  mod5 = n ~ 1 + f(id.age_group, model = "ar1") + f(survey_id, model = "iid"),
  
  mod6 = n ~ 1 + f(id.age_group, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj())
)


formula_labs = data.frame(
  mod_name = c("mod0","mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8", "mod9", "mod10", "mod11", "mod12"),
  formula = c("Year RW2",
              "Age AR1",
              "Year RW2 + Age AR1",
              "Age AR1 + Survey IID + Survey X Age", 
              "Age AR1 + Country ICAR + Country x Age",
              "Age AR1 + Survey IID",
              "Age AR1 + Country ICAR",
              "Year RW2 + Age AR1 + Country ICAR",
              "Year RW2 + Age AR1 + Year X Age + Country ICAR", 
              "Year RW2 + Age AR1 + Country ICAR + Age X Country",
              "Year RW2 + Age AR1 + Year X Age + Country ICAR + Age X Country",
              "Year RW2 + Age AR1 + Year X Age\n+ ISO3 ICAR + Survey IID", 
              "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age + Survey IID")
)





run_inla_model_tgw <- function(formula, data, denom, tot_prev) {
  print(formula)
  
  # data <- data %>% filter(!model == "regions")
  
  mod <- inla(
    formula = formula,
    Ntrials = denom,
    offset = qlogis(tot_prev),
    data = data,
    family = "binomial",
    control.inla = list(int.strategy = "eb"),
    control.family = list(link = "logit"),
    control.compute=list(config = TRUE),
    verbose = F,
    keep = F
  )
  
  model_summary <- summary(mod)
  
  model_samples <- moz.utils::sample_model(mod, data, col = "survey_id")
  # 
  
  list(
    model = mod,
    summary = model_summary,
    samples = model_samples
  )
}

# debugonce(run_inla_model_tgw)
tgw_results_simple_formulas <- lapply(tgw_formulas_simple, run_inla_model_tgw, data = tgw_inla_dat)

all_samples_tgw_simple <- imap_dfr(tgw_results_simple_formulas, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(formula_labs)

tgw_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age_group, kp_odds) %>%
  left_join(
    all_samples_tgw_simple %>% 
      select(iso3, year, age_group, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries")) %>%
  ggplot() +
  geom_point(aes(x = age_group, y = kp_prev)) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = prev, color = formula)) +
  geom_ribbon(aes(x = multi.utils::to_int(age_group), ymin = prev_lower, ymax = prev_upper, fill = formula), alpha = 0.3) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = tot_prev), color = "black", linetype = "dashed") +
  facet_wrap(year~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "TGW HIV Prevalence", x = "Age Group")




tgw_inla_dat  %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age_group, kp_odds) %>%
  left_join(
    all_samples_tgw_simple %>% 
      select(iso3, year, sex, age_group, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean),
             log_or_lower = lower - logit_totprev,
             log_or_upper = upper - logit_totprev) %>%
      filter(model == "countries")) %>%
  droplevels() %>% 
  ggplot() +
  geom_point(data = tgw_inla_dat %>% filter(!is.na(survey_id) | is.na(sex), !denom == 0) %>% mutate(sex = factor(sex))  %>% droplevels()  , aes(x = age_group, y = (kp_prev/(1-kp_prev))/(tot_prev/(1-tot_prev)), size = denom)) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = exp(log_or), color = formula)) +
  geom_ribbon(aes(x = multi.utils::to_int(age_group), ymin = exp(log_or_lower), ymax = exp(log_or_upper), fill = formula), alpha = 0.3) +
  # geom_line(aes(x = multi.utils::to_int(age_group), y = tot_prev), color = "black", linetype = "dashed") +
  geom_hline(aes(yintercept = 1), color = "darkred", linetype = "dashed") +
  facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, size = 6),
        axis.text.y = element_text( size = 6),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        strip.text.x = element_text(size = 6)) +
  scale_y_log10() +
  labs(y = "TGW HIV Prevalence", x = "Age Group")




# COuntrysurvs1
tgw_results_countrysurv1_formulas <- lapply(tgw_formulas_countrysurv1, run_inla_model_tgw, data = tgw_inla_dat)


tgw_results_countrysurv1_formulas$mod5$summary
all_samples_tgw_countrysurv1 <- imap_dfr(tgw_results_countrysurv1_formulas, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(formula_labs)



tgw_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age_group, kp_odds) %>%
  left_join(
    all_samples_tgw_countrysurv1 %>% 
      select(iso3, year, age_group, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries")) %>%
  ggplot() +
  geom_point(aes(x = age_group, y = kp_prev)) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = prev, color = formula)) +
  geom_ribbon(aes(x = multi.utils::to_int(age_group), ymin = prev_lower, ymax = prev_upper, fill = formula), alpha = 0.3) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = tot_prev), color = "black", linetype = "dashed") +
  facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "TGW HIV Prevalence", x = "Age Group")


tgw_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age_group, kp_odds, or_obs) %>%
  left_join(
    all_samples_tgw_countrysurv1 %>% 
      select(iso3, year, age_group, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             log_loweror = lower - logit_totprev,
             log_upperor = upper - logit_totprev,
             or = exp(log_or),
             or_lower = exp(log_loweror),
             or_upper = exp(log_upperor),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries")) %>%
  ggplot() +
  geom_point(aes(x = age_group, y = or_obs)) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = or, color = formula)) +
  geom_ribbon(aes(x = multi.utils::to_int(age_group), ymin = or_lower, ymax = or_upper, fill = formula), alpha = 0.3) +
  geom_hline(yintercept = 1, color = "darkred", linetype = "dashed") +
  facet_wrap(~survey_id) +
  scale_y_log10() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "TGW:Genpop OR", x = "Age Group")
