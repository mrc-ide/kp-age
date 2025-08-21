library(purrr)
library(INLA)
library(tidyverse)
library(sf)
library(moz.utils)
library(splines)

###### Data set up #####

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
  # kitchen.sink::single_year_to_five_year() %>% 
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
  left_join(spec_hiv %>% 
              kitchen.sink::single_year_to_five_year()  %>% 
              group_by(iso3, year, sex, age) %>% 
              summarise(hivpop = sum(hivpop),
                        totpop = sum(totpop),
                        tot_prev = hivpop/totpop)) 
# filter(!age < 15)


msm_dat <- hivdat_allkp %>% filter(kp == "MSM") %>% filter(!sex == "female")

msm_dat_iso3 <- unique(msm_dat$iso3)

pred_msm <- spec_hiv %>%
  kitchen.sink::single_year_to_five_year() %>% 
  filter( #iso3 %in% msm_dat_iso3,
    sex == "male",
    # age %in% 15:49,
    year %in% 2010:2025) %>%
  left_join(moz.utils::region()) %>%
  bind_rows(mutate(., region = "SSA")) %>%
  group_by(year, age, region) %>%
  summarise(tot_prev = median(tot_prev)) %>% 
            #hivpop = sum(hivpop),
            #totpop = sum(totpop),
            #tot_prev = hivpop/totpop) %>%
  ungroup() %>%
  mutate(model = "regions") %>%
  bind_rows(
    spec_hiv %>% 
      # kitchen.sink::single_year_to_five_year() %>% 
      group_by(iso3, year, sex, age) %>% 
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
      mutate(id.iso3.age = group_indices(., age, id.iso3))
  ) %>%
  # bind_rows(
  #   spec_hiv %>%
  #     filter(iso3 %in% msm_dat$iso3,
  #            sex == "male",
  #            year %in% 2010:2025) %>%
  #     group_by(iso3, year, sex, age) %>%
  #     summarise(hivpop = sum(hivpop),
  #               totpop = sum(totpop),
  #               tot_prev = hivpop/totpop) %>%
  #     ungroup() %>%
  #     left_join(moz.utils::region()) %>%
  #     group_by(region, age, year, sex) %>%
  #     summarise(tot_prev = median(tot_prev)) %>%
  #     ungroup() %>%
  #     mutate(model = "survey-specific regions")
  # ) %>%
  mutate(denom = 1) %>% 
  mutate(id.age = factor(multi.utils::to_int(age))
  )

data_prep_msm <- msm_dat %>%
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
  filter(age %in% 15:49) %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  left_join(pred_msm %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age, id.iso3.age)) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))


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
    id.age2 = id.age,
    id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2),
    id.iso3.year = group_indices(., iso3, year),
    id.iso3.year = ifelse(is.na(iso3), NA_integer_, id.iso3.year)) %>% 
  #id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2)) %>% 
  # kitchen.sink::single_year_to_five_year(age) %>% 
  mutate(id.age = (multi.utils::to_int(age)),
         id.year.age = group_indices(., age, year),
         id.age2 =  ifelse(model == "regions", NA_integer_, id.age)) %>% 
  filter(!is.na(age)) 




## Generic 0 set-up
n_ages <- length(unique(pred_msm$age))
n_ages <- length(unique(pred_msm$age))
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

#### Model Functions #####

run_inla_model_msm <- function(formula, data, denom, tot_prev) {
  print(formula)
  
  # data <- data %>% filter(!model == "regions")
  
  mod <- inla(
    formula = formula,
    Ntrials = denom,
    # offset = qlogis(tot_prev),
    data = data,
    family = "binomial",
    # control.inla = list(int.strategy = "eb"),
    control.family = list(link = "logit"),
    control.compute=list(config = TRUE, dic = TRUE, waic = T),
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



### Basic mods

focused_mods <- list(
  mod5 = n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()),
  mod5a = n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") + f(id.iso3, model = "iid")
)

focused_mods_labs = data.frame(
  mod_name = c("mod5", "mod5a"),
  formula = c("Year RW2 + Age AR1 + Country ICAR", "Year RW2 + Age AR1 + Country IID") 
)

msm_results_focused_mods <- lapply(focused_mods, run_inla_model_msm, data = msm_inla_dat)

all_samples_msm_focused_mods <- imap_dfr(msm_results_focused_mods, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(focused_mods_labs)

msm_results_focused_mods$mod5a$summary
msm_results_focused_mods$mod5$summary


msm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age, kp_odds, denom) %>%
  left_join(
    all_samples_msm_focused_mods%>% 
      select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries",
             !mod_name %in% c("mod7", "mod8"))) %>%
  ggplot() +
  geom_point(aes(x = age, y = kp_prev, size = denom)) +
  geom_line(aes(x = age, y = prev, color = formula)) +
  geom_ribbon(aes(x = age, ymin = prev_lower, ymax = prev_upper, fill = formula), alpha = 0.3) +
  geom_line(aes(x = age, y = tot_prev), color = "black", linetype = "dashed") +
  facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "MSM HIV Prevalence", x = "Age Group")




##### Time interaction mods ######


msm_mods <- list(
  # mod0 = n ~ 1 + id.year * id.age ,
  # mod1 = n ~ 1 + id.year * id.age + f(id.iso3, model = "iid"),
  mod5 = n ~ 1 + id.year * id.age + f(id.iso3, model = "besag", graph = national_adj()),
  mod50 = n ~ 1 + id.year + f(id.age, model = "ar1") + id.year:id.age + f(id.iso3, model = "besag", graph = national_adj()),
  mod500 = n ~ 1 + id.year + f(id.age, model = "ar1") + id.year:id.age + f(id.iso3, model = "iid"))
  # mod500 = n ~ 1 + 
  # mod5000 = n ~ 1 + f(id.year, model = "rw2") + bs(age, df = 5)  +
  #       f(id.year.age, model = "generic0",
  #         Cmatrix = Q2_msm,
  #         extraconstr = list(A = A_combined2_msm, e = e2_msm),
  #         rankdef = n_years_msm + n_ages - 1L) + f(id.iso3, model = "besag", graph = national_adj())


msm_mods_labs = data.frame(
  mod_name = c("mod0", "mod1","mod5", "mod50", "mod500"),
  formula = c("Year X Age", "Year X Age + Country IID", "Year X Age + Country ICAR", "Year + Age AR1 + Year:Age + Country ICAR", "Year + Age AR1 + Year:Age + Country IID") 
)

msm_results__mods <- lapply(msm_mods, run_inla_model_msm, data = msm_inla_dat)

all_samples_msm_mods <- imap_dfr(msm_results__mods, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(msm_mods_labs)

msm_results__mods$mod0$summary
msm_results__mods$mod1$summary
msm_results__mods$mod5$summary
msm_results__mods$mod50$summary
msm_results__mods$mod500$summary


msm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age, kp_odds, denom) %>%
  left_join(
    all_samples_msm_mods %>% 
      select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries")) %>%
  ggplot() +
  geom_point(aes(x = age, y = kp_prev, size = denom)) +
  geom_line(aes(x = age, y = prev, color = formula)) +
  geom_ribbon(aes(x = age, ymin = prev_lower, ymax = prev_upper, fill = formula), alpha = 0.3) +
  geom_line(aes(x = age, y = tot_prev), color = "black", linetype = "dashed") +
  facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "MSM HIV Prevalence", x = "Age Group")




msm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age, kp_odds, or_obs) %>%
  left_join(
    all_samples_msm_mods %>% 
      select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
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
  geom_point(aes(x = age, y = or_obs)) +
  geom_line(aes(x = age, y = or, color = formula)) +
  geom_ribbon(aes(x = age, ymin = or_lower, ymax = or_upper, fill = formula), alpha = 0.3) +
  geom_hline(yintercept = 1, color = "darkred", linetype = "dashed") +
  facet_wrap(~survey_id) +
  scale_y_log10() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  # coord_cartesian(ylim = c(0, 1000)) +
  labs(y = "MSM:Genpop OR", x = "Age Group")




##
saveRDS(msm_inla_dat, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/msm_inla_dat_aug25.rds")

formula <- n ~ 1 + id.age * id.year + f(id.iso3, model = "besag", graph = national_adj())

inla_mod <- inla(
  formula = formula,
  Ntrials = denom,
  offset = qlogis(tot_prev),
  data = msm_inla_dat,
  family = "binomial",
  # control.inla = list(int.strategy = "eb"),
  control.family = list(link = "logit"),
  control.compute=list(config = TRUE, dic = TRUE),
  verbose = F,
  keep = F
)


df_test <- msm_inla_dat %>% filter(!is.na(survey_id))
samples <- inla.posterior.sample(1000, inla_mod)
contents = inla_mod$misc$configs$contents
effect = "Predictor"
id.effect = which(contents$tag == effect)
ind.effect = contents$start[id.effect] - 1 + (1:contents$length[id.effect])
ind.effect <- 1:(nrow(msm_inla_dat) - nrow(df_test))
samples.effect = lapply(samples, function(x) x$latent[ind.effect])
samples <- matrix(sapply(samples.effect, cbind), ncol = 1000)

samples <- samples %>% data.frame() %>% rowid_to_column() %>% 
  left_join(msm_inla_dat %>% rowid_to_column()) %>% 
  filter(model == "countries") %>% 
  select(rowid:X1000, year, age, iso3) %>% 
  left_join(msm_inla_dat %>% filter(!is.na(survey_id))) %>% 
  filter(!is.na(survey_id)) 

samples2 <- samples %>%
  group_by(age) %>%
  summarise(
    across(starts_with("X"), ~ weighted.mean(.x, w = denom, na.rm = TRUE)),
    .groups = "drop"
  ) %>% select(-age)

mean <- rowMeans(samples2)
qtls <- apply(samples2, 1, quantile, c(0.025, 0.5, 0.975))
ident <- data.frame(age = 15:49)
samples <- ident %>% cbind(samples2)
art <- ident %>% mutate(mean = mean, lower = qtls[1, ], upper = qtls[3, 
])

# msm_inla_dat %>% filter(!(is.na(survey_id))) %>%
#   select(survey_id, iso3, year, kp_prev, age, kp_odds, denom) %>%
#   left_join(
#     all_samples_msm_time_interaction_mods %>% 
#       select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%

msm_inla_dat %>% filter(!is.na(survey_id)) %>% 
  group_by(age) %>% 
  summarise(tot_prev = weighted.mean(tot_prev, w = denom)) %>% 
  ungroup() %>% 
  left_join(art) %>% 
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>% 
  ggplot() +
  geom_point(data = msm_inla_dat, aes(x = age, y = kp_prev, size = denom)) +
  geom_line(aes(x = age, y = prev), color = "red3") + 
  geom_ribbon(aes(x = age, ymin = prev_lower, ymax = prev_upper), fill = "red3", alpha = 0.3) +
  geom_line(aes(x = age, y = tot_prev), color = "black", linetype = "dashed") +
  # facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "MSM HIV Prevalence", x = "Age Group")


### Not post-stratified

model_samples <- moz.utils::sample_model(inla_mod, msm_inla_dat, col = "survey_id")

model_samples %>% 
  filter(model == "regions", year == 2019) %>% 
  mutate(prev = plogis(mean),
         prev_lower = plogis(lower),
         prev_upper = plogis(upper)) %>% 
  ggplot() + 
  geom_point(data = msm_inla_dat, aes(x = age, y = kp_prev, size = denom, color = region)) +
  geom_line(aes(x = age, y = prev, color = region)) + 
  geom_ribbon(aes(x = age, ymin = prev_lower, ymax = prev_upper, fill = region), alpha = 0.3) +
  geom_line(aes(x = age, y = tot_prev, color = region), linetype = "dashed") +
  # facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "MSM HIV Prevalence", x = "Age Group")



function (inla_mod, inla_df, col) 
{
  df <- inla_df %>% filter(across(all_of(col), ~!is.na(.x)))
  samples <- inla.posterior.sample(1000, msm_results_time_interaction_mods$mod5$model)
  contents = inla_mod$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag == effect)
  ind.effect = contents$start[id.effect] - 1 + (1:contents$length[id.effect])
  ind.effect <- 1:(nrow(inla_df) - nrow(df))
  samples.effect = lapply(samples, function(x) x$latent[ind.effect])
  samples <- matrix(sapply(samples.effect, cbind), ncol = 1000)
  mean <- rowMeans(samples)
  qtls <- apply(samples, 1, quantile, c(0.025, 0.5, 0.975))
  ident <- inla_df[ind.effect, ]
  samples <- ident %>% cbind(samples)
  art <- ident %>% mutate(mean = mean, lower = qtls[1, ], upper = qtls[3, 
  ])
}

msm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age, kp_odds, denom) %>%
  left_join(
    all_samples_msm_time_interaction_mods %>% 
      select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries",
             !mod_name %in% c("mod500"))) %>%
  group_by(age, mod_name) %>% 
  mutate(central_estimate = sum((prev*denom))/sum(denom),
         central_lower = sum((prev_lower*denom))/sum(denom),
         central_upper = sum((prev_upper * denom))/sum(denom),
         central_tot_prev = sum((tot_prev*denom))/sum(denom)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = age, y = kp_prev, size = denom)) +
  geom_line(aes(x = age, y = central_estimate, color = formula)) +
  geom_ribbon(aes(x = age, ymin = central_lower, ymax = central_upper, fill = formula), alpha = 0.3) +
  geom_line(aes(x = age, y = central_tot_prev), color = "black", linetype = "dashed") +
  # facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "MSM HIV Prevalence", x = "Age Group")

############

# Regional plot

all_samples_msm_time_interaction_mods %>% 
  # select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula, region) %>%
  mutate(logit_totprev = qlogis(tot_prev),
         log_or = mean - logit_totprev,
         or = exp(log_or),
         prev_lower = plogis(lower),
         prev_upper = plogis(upper),
         prev = plogis(mean)) %>%
  filter(model == "regions",
         formula == "Year RW2 + Age AR1 + Country ICAR",
         region %in% c("ESA", "WCA"),
         year %in%  c(2016,2019,2021)) %>%
  ggplot() +
  geom_point(data = msm_inla_dat %>% filter(!is.na(survey_id)) , aes(x = age, y = kp_prev, color = factor(year))) +
  geom_line(aes(x = age, y = prev, color =  factor(year))) +
  geom_ribbon(aes(x = age, ymin = prev_lower, ymax = prev_upper, fill = factor(year)), alpha = 0.3) +
  geom_line(aes(x = age, y = tot_prev, color = factor(year)), linetype = "dashed") +
  facet_wrap(~region) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "MSM HIV Prevalence", x = "Age Group")
