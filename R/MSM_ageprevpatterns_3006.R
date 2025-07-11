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


msm_dat <- hivdat_allkp %>% filter(kp == "MSM")

msm_dat_iso3 <- unique(msm_dat$iso3)

pred_msm <- spec_hiv %>%
  kitchen.sink::single_year_to_five_year() %>% 
  filter( #iso3 %in% msm_dat_iso3,
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
      filter( #iso3 %in% msm_dat_iso3,
        sex == "male",
        # age %in% 15:49,
        year %in% 2010:2025) %>%
      mutate(model = "countries") %>%
      left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>% 
      mutate(id.iso3.age = group_indices(., age_group, id.iso3))
  ) %>%
  mutate(denom = 1) %>% 
  mutate(id.age_group = factor(multi.utils::to_int(age_group))
  )

data_prep_msm <- msm_dat %>%
  left_join(moz.utils::region()) %>%
  # kitchen.sink::single_year_to_five_year(age) %>% 
  filter(hiv == 1) %>% 
  # age %in% 15:49) %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  left_join(pred_msm %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age_group, id.iso3.age)) %>% 
  mutate(id.age_group = factor(multi.utils::to_int(age_group)))


msm_inla_dat <- pred_msm %>%
  bind_rows(data_prep_msm) %>% 
  mutate(id.year = multi.utils::to_int(year),
         # id.age = multi.utils::to_int(age),
         # id.year.age = group_indices(., age, year),
         id.year2 = id.year+1) %>% 
  mutate(kp_prev = n/denom,
         kp_odds = kp_prev/(1-kp_prev),
         genpop_odds = tot_prev/(1-tot_prev),
         or_obs = kp_odds/genpop_odds) %>% 
  mutate(#id.age2 = id.age,
    id.survey_id = multi.utils::to_int(survey_id),
    id.age_group2 = id.age_group,
    id.age_group2 = ifelse(is.na(survey_id), NA_integer_, id.age_group2),
    id.iso3.year = group_indices(., iso3, year),
    id.iso3.year = ifelse(is.na(iso3), NA_integer_, id.iso3.year)) %>% 
  #id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2)) %>% 
  # kitchen.sink::single_year_to_five_year(age) %>% 
  mutate(id.age_group = factor(multi.utils::to_int(age_group)),
         id.year.age = group_indices(., age_group, year),
         id.age_group2 =  ifelse(model == "regions", NA_integer_, id.age_group)) 





## Generic 0 set-up
n_ages <- length(unique(pred_msm$age))
n_ages <- length(unique(pred_msm$age_group))
n_years_msm <- length(unique(pred_msm$year))

n_interactions_year_msm <- n_years_msm * n_ages

# Sum-to-zero constraint within each year for age effects
A_sum_age_year_msm <- matrix(0, nrow = n_years_msm, ncol = n_interactions_year_msm)

for (year in 1:n_years_msm) {
  A_sum_age_year_msm[year, ((year - 1) * n_ages + 1):(year * n_ages)] <- 1
} 

## Same in reverse to have sum-to-zero constraints within each age over year
A_sum_year_age_msm <- matrix(0, nrow = n_ages, ncol = n_interactions_year_msm)

for (age in 1:n_ages) {
  A_sum_year_age_msm[age, ((age - 1) * n_years_msm + 1):(age * n_years_msm)] <- 1
}

A_combined2_msm <- rbind(A_sum_year_age_msm, A_sum_age_year_msm) 
e2_msm <- matrix(0, nrow(A_combined2_msm), nrow = 1) 

R_age <- dfertility::make_rw_structure_matrix(n_ages, 2, FALSE) 
R_year_msm <- dfertility::make_rw_structure_matrix(n_years_msm, 2, FALSE) 

Q2_msm <- kronecker(R_age, R_year_msm)


### 

msm_formulas_simple <- list(
  mod0 = n ~ 1 + f(id.year, model = "rw2"),
  
  mod1 = n ~ 1 + f(id.age_group, model = "ar1"),
  
  mod2 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1"),
  
  mod3 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2_msm,
      extraconstr = list(A = A_combined2_msm, e = e2_msm),
      rankdef = n_years_msm + n_ages - 1L)
)

msm_formulas_countrysurv1 <- list(
  mod2 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1"),
  
  mod3 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2_msm,
      extraconstr = list(A = A_combined2_msm, e = e2_msm),
      rankdef = n_years_msm + n_ages - 1L),
  
  mod4 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") + f(survey_id, model = "iid"),
  
  mod5 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj())
)


msm_formulas_countrysurv2 <- list(
  
  mod3 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2_msm,
      extraconstr = list(A = A_combined2_msm, e = e2_msm),
      rankdef = n_years_msm + n_ages - 1L),
  
  mod5 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()),
  
  mod6 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2_msm,
      extraconstr = list(A = A_combined2_msm, e = e2_msm),
      rankdef = n_years_msm + n_ages - 1L) + f(id.iso3, model = "besag", graph = national_adj()),
  
  mod7 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") + 
    f(id.iso3, model = "besag", graph = national_adj()) +
    f(id.age_group2, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj())),
  
  mod8 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2_msm,
      extraconstr = list(A = A_combined2_msm, e = e2_msm),
      rankdef = n_years_msm + n_ages - 1L) + f(id.iso3, model = "besag", graph = national_adj()) +
    f(id.age_group2, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj()))
)

# msm_formulas <- list(
#   mod1 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") +
#     f(id.year.age, model = "generic0",
#       Cmatrix = Q2_msm,
#       extraconstr = list(A = A_combined2_msm, e = e2_msm),
#       rankdef = n_years_msm + n_ages - 1L),
# 
#   # mod2 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) +
#   #   f(id.year.age, model = "generic0",
#   #     Cmatrix = Q2_msm,
#   #     extraconstr = list(A = A_combined2_msm, e = e2_msm),
#   #     rankdef = n_years_msm + n_ages - 1L),
#   # 
#   # 
#   #   mod3 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) + f(id.survey_id, model = "iid")+
#   #     f(id.year.age, model = "generic0",
#   #       Cmatrix = Q2_msm,
#   #       extraconstr = list(A = A_combined2_msm, e = e2_msm),
#   #       rankdef = n_years_msm + n_ages - 1L) ,
#   # 
#   mod4 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) +
#     f(id.age_group2, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj())) +
#     f(id.year.age, model = "generic0",
#       Cmatrix = Q2_msm,
#       extraconstr = list(A = A_combined2_msm, e = e2_msm),
#       rankdef = n_years_msm + n_ages - 1L) 
#   # 
#   # mod5 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group2, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) + f(id.survey_id, model = "iid") +
#   #   f(id.age_group, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj())) +
#   #   f(id.year.age, model = "generic0",
#   #     Cmatrix = Q2_msm,
#   #     extraconstr = list(A = A_combined2_msm, e = e2_msm),
#   #     rankdef = n_years_msm + n_ages - 1L)
#   # 
#   
# )

formula_labs = data.frame(
  mod_name = c("mod0","mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8", "mod9", "mod10"),
  formula = c("Year RW2",
              "Age AR1",
              "Year RW2 + Age AR1",
              "Year RW2 + Age AR1 + Year X Age", 
              "Year RW2 + Age AR1 + Survey IID",
              "Year RW2 + Age AR1 + Country ICAR",
              "Year RW2 + Age AR1 + Year X Age + Country ICAR", 
              "Year RW2 + Age AR1 + Country ICAR + Age X Country",
              "Year RW2 + Age AR1 + Year X Age + Country ICAR + Age X Country",
              "Year RW2 + Age AR1 + Year X Age\n+ ISO3 ICAR + Survey IID", 
              "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age + Survey IID")
)





run_inla_model_msm <- function(formula, data, denom, tot_prev) {
  print(formula)
  
  # data <- data %>% filter(!model == "regions")
  
  mod <- inla(
    formula = formula,
    Ntrials = denom,
    # offset = qlogis(tot_prev),
    data = data,
    family = "xbinomial",
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

# debugonce(run_inla_model_msm)
msm_results_simple_formulas <- lapply(msm_formulas_simple, run_inla_model_msm, data = msm_inla_dat)

all_samples_msm_simple <- imap_dfr(msm_results_simple_formulas, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(formula_labs)

msm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age_group, kp_odds) %>%
  left_join(
    all_samples_msm_simple %>% 
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
  labs(y = "MSM HIV Prevalence", x = "Age Group")


msm_results_countrysurv1_formulas <- lapply(msm_formulas_countrysurv1, run_inla_model_msm, data = msm_inla_dat)

all_samples_msm_countrysurv1 <- imap_dfr(msm_results_countrysurv1_formulas, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(formula_labs)



msm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age_group, kp_odds) %>%
  left_join(
    all_samples_msm_countrysurv1 %>% 
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
  labs(y = "MSM HIV Prevalence", x = "Age Group")



msm_results_countrysurv2_formulas <- lapply(msm_formulas_countrysurv2, run_inla_model_msm, data = msm_inla_dat)

all_samples_msm_countrysurv2 <- imap_dfr(msm_results_countrysurv2_formulas, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(formula_labs)

msm_results_countrysurv2_formulas$mod6$summary
msm_results_countrysurv2_formulas$mod5$summary


msm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age_group, kp_odds) %>%
  left_join(
    all_samples_msm_countrysurv2 %>% 
      select(iso3, year, age_group, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries",
             !mod_name %in% c("mod7", "mod8"))) %>%
  ggplot() +
  geom_point(aes(x = age_group, y = kp_prev)) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = prev, color = formula)) +
  geom_ribbon(aes(x = multi.utils::to_int(age_group), ymin = prev_lower, ymax = prev_upper, fill = formula), alpha = 0.3) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = tot_prev), color = "black", linetype = "dashed") +
  facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "MSM HIV Prevalence", x = "Age Group")


msm_inla_dat %>%
  filter(!is.na(id.year.age)) %>%
  distinct(year, age_group, id.year.age) %>%
  arrange(id.year.age) %>%
  left_join(msm_results_countrysurv2_formulas$mod6$model$summary.random$id.year.age %>% select(id.year.age = ID, mean)) %>% ggplot() + geom_point(aes(x = year, y = mean, color = factor(age_group)), size = 3, shape = 20) + theme_minimal()


msm_inla_dat %>%
  filter(!is.na(age_group)) %>%
  distinct(age_group, id.age_group) %>%
  arrange(id.age_group) %>%
  left_join(msm_results_countrysurv2_formulas$mod6$model$summary.random$id.age_group %>% select(id.age_group = ID, mean)) %>% ggplot() + geom_point(aes(x = age_group, y = mean), size = 3, shape = 20) + theme_minimal()


msm_inla_dat %>%
  filter(!is.na(age_group)) %>%
  distinct(age_group, id.age_group) %>%
  arrange(id.age_group) %>%
  left_join(msm_results_countrysurv2_formulas$mod5$model$summary.random$id.age_group %>% select(id.age_group = ID, mean)) %>% ggplot() + geom_point(aes(x = age_group, y = mean), size = 3, shape = 20) + theme_minimal()

msm_inla_dat %>% filter(!is.na(survey_id)) %>% 
  group_by(age_group) %>% 
  summarise(denom = sum(denom),
            n = sum(n)) %>% 
  ggplot() + geom_point(aes(x = age_group, y = n))

msm_inla_dat %>% filter(!is.na(survey_id)) %>% 
  group_by(survey_id, age_group) %>% 
  summarise(denom = sum(denom),
            n = sum(n),
            prev = n/denom) %>% 
  ungroup() %>% 
  group_by(survey_id) %>% 
  mutate(survey_prev = sum(n)/sum(denom)) %>% 
  ungroup() %>% 
  mutate(ratio = prev/survey_prev) %>% 
  mutate(id.age_group = multi.utils::to_int(age_group)) %>% 
  ggplot() + geom_boxplot(aes(x = age_group, y = ratio)) + geom_hline(yintercept = 1, color = "darkred") + theme_minimal()
  

msm_inla_dat %>% filter(!is.na(survey_id)) %>% 
  group_by(survey_id, age_group, tot_prev) %>% 
  summarise(denom = sum(denom),
            n = sum(n),
            prev = n/denom,
            pr = prev/tot_prev) %>% 
  ungroup() %>% 
  ggplot() + geom_boxplot(aes(x = age_group, y = pr)) + geom_hline(yintercept = 1, color = "darkred") +
  scale_y_log10() +
theme_minimal()

## trialling the formula

# 
# mod1 = n ~ 1 + f(id.year, model = "rw2") + f(id.age_group, model = "ar1") +
#   f(id.year.age, model = "generic0",
#     Cmatrix = Q2_msm,
#     extraconstr = list(A = A_combined2_msm, e = e2_msm),
#     rankdef = n_years_msm + n_ages - 1L)
# 
# mod1 <- n ~ 1 + f(id.year, model = "rw2") + f(id.age_group2, model = "ar1") + 
#   f(id.iso3, model = "besag", graph = national_adj()) + 
#   f(id.age_group, model = "rw2", group = id.iso3, control.group = list(model = "besag",graph = national_adj())) +
#  f(id.year.age, model = "generic0", 
#                                                                                                                                             Cmatrix = Q2_msm, extraconstr = list(A = A_combined2_msm, 
#                                                                                                                                                                                  e = e2_msm), rankdef = n_years_msm + n_ages - 1L)
# 
# 
# mod <- inla(
#   formula = mod1,
#   Ntrials = denom,
#   offset = qlogis(tot_prev),
#   data = msm_inla_dat,
#   family = "xbinomial",
#   control.inla = list(int.strategy = "eb"),
#   control.family = list(link = "logit"),
#   control.compute=list(config = TRUE),
#   verbose = F
# )
# 
# model_samples <- moz.utils::sample_model(mod, msm_inla_dat, col = "survey_id")
# 
# 
# 
# 
# 
# 
# 
# 
# 
