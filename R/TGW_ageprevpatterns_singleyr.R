library(purrr)
library(INLA)
library(tidyverse)
library(sf)
library(moz.utils)

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
              # kitchen.sink::single_year_to_five_year()  %>% 
              group_by(iso3, year, sex, age) %>% 
              summarise(hivpop = sum(hivpop),
                        totpop = sum(totpop),
                        tot_prev = hivpop/totpop)) 
# filter(!age < 15)


tgw_dat <- hivdat_allkp %>% filter(kp == "TGW") %>% mutate(sex = "female")

tgw_dat_iso3 <- unique(tgw_dat$iso3)

pred_tgw <- spec_hiv %>% 
  # kitchen.sink::single_year_to_five_year() %>% 
  group_by(iso3, year, sex, age) %>% 
  summarise(hivpop = sum(hivpop),
            totpop = sum(totpop),
            tot_prev = hivpop/totpop) %>% 
  ungroup() %>% 
  filter( iso3 %in% tgw_dat_iso3,
          sex == "female",
          # age %in% 15:49,
          year %in% 2002:2025) %>%
  mutate(model = "countries") %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>% 
  mutate(id.iso3.age = group_indices(., age, id.iso3)) %>% 
  mutate(denom = 1) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))



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
  left_join(pred_tgw %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age, id.iso3.age)) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))


tgw_inla_dat <- pred_tgw %>%
  bind_rows(data_prep_tgw) %>% 
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
  mutate(id.age = multi.utils::to_int(age),
         id.year.age = group_indices(., age, year),
         id.age2 =  ifelse(model == "regions", NA_integer_, id.age)) %>% 
  filter(!is.na(age)) %>% 
  filter(!sex == "male")



tgw_formulae <- list(mod1 = n ~ 1 + id.age + f(id.iso3, model = "besag", graph = national_adj()),
                      mod2 = n ~ 1 + id.age + f(id.survey_id, model = "iid"),
                      mod3 = n ~ 1 + f(id.age, model = "ar1") +  f(id.survey_id, model = "iid"))

tgw_mods_labs = data.frame(
  mod_name = c("mod1", "mod2", "mod3"),
  formula = c("Linear Age + Country ICAR", "Linear Age + Survey IID", "Age AR1 + Survey IID") 
)

tgw_mods <- lapply(tgw_formulae, run_inla_model_msm, data = tgw_inla_dat)

all_samples_tgw_mods <- imap_dfr(tgw_mods, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(tgw_mods_labs)

tgw_mods$mod1$summary
tgw_mods$mod2$summary
tgw_mods$mod3$summary

tgw_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age, kp_odds, denom) %>%
  left_join(
    all_samples_tgw_mods%>% 
      select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries",
             !mod_name %in% c("mod7", "mod8"))) %>%
  filter(age %in% 15:49) %>% 
  ggplot() +
  geom_point(aes(x = age, y = kp_prev, size = denom)) +
  geom_line(aes(x = age, y = prev, color = formula)) +
  geom_ribbon(aes(x = age, ymin = prev_lower, ymax = prev_upper, fill = formula), alpha = 0.3) +
  geom_line(aes(x = age, y = tot_prev), color = "black", linetype = "dashed") +
  facet_wrap(~survey_id) +
  # scale_y_log10() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "TGW HIV Prevalence", x = "Age Group")




tgw_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age, or_obs, denom) %>%
  left_join(
    all_samples_tgw_mods%>% 
      select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             log_or_lower = lower - logit_totprev,
             log_or_upper = upper - logit_totprev,
             or_lower = exp(log_or_lower),
             or_upper = exp(log_or_upper),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries",
             !mod_name %in% c("mod7", "mod8"))) %>%
  filter(age %in% 15:49) %>% 
  ggplot() +
  geom_point(aes(x = age, y = or_obs, size = denom)) +
  geom_line(aes(x = age, y = or, color = formula)) +
  geom_ribbon(aes(x = age, ymin = or_lower, ymax = or_upper, fill = formula), alpha = 0.3) +
  # geom_line(aes(x = multi.utils::to_int(age), y = tot_prev), color = "black", linetype = "dashed") +
  facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_y_log10() +
  geom_hline(yintercept = 1, color = "darkred", linetype = "dashed") +
  # lims(y = c(0,50)) +
  labs(y = "TGW/Genpop OR", x = "Age Group")


## If running wihtout offset
tgw_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(survey_id, iso3, year, kp_prev, age, or_obs, denom) %>%
  left_join(
    all_samples_tgw_mods%>% 
      select(iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             log_or_lower = lower - logit_totprev,
             log_or_upper = upper - logit_totprev,
             or_lower = exp(log_or_lower),
             or_upper = exp(log_or_upper),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "countries",
             !mod_name %in% c("mod7", "mod8"))) %>%
  ggplot() +
  geom_point(aes(x = age, y = or_obs, size = denom)) +
  geom_line(aes(x = age, y = or, color = formula)) +
  geom_ribbon(aes(x = age, ymin = or_lower, ymax = or_upper, fill = formula), alpha = 0.3) +
  # geom_line(aes(x = multi.utils::to_int(age), y = tot_prev), color = "black", linetype = "dashed") +
  facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_y_log10() +
  geom_hline(yintercept = 1, color = "darkred", linetype = "dashed") +
  # lims(y = c(0,50)) +
  labs(y = "TGW/Genpop OR", x = "Age Group")