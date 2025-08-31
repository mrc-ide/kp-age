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
    offset = qlogis(tot_prev),
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
                         kp == "MSM" & sex == "female" ~ "TGW",
                         TRUE ~ kp)) %>% 
  filter(!kp %in% c("PWUD", "TG")) %>% 
  mutate(kp = kp2) 



hivdat_allkp <- dat %>% 
  mutate(hiv = ifelse(survey_id == "SLE2021BBS_TGW" & is.na(hiv), 0, hiv)) %>% 
  filter(!survey_id %in% c("MWI2016ACA_MSM", "ZAF2016ACA_MSM", "KEN2016ACA_MSM")) %>% 
  filter(!is.na(hiv)) %>% 
  # !(kp == "FSW" & sex == 0),
  # !(kp == "MSM" & sex == 1)) %>%
  # kitchen.sink::single_year_to_five_year() %>% 
  mutate(sex = case_when(sex == 1 ~ "female",
                         sex == 0 ~ "male",
                         is.na(sex) & kp == "PWID" ~ "everyone",
                         kp == "CFSW" ~ "male",
                         kp == "TGW" ~ "female",
                         kp == "FSW" ~ "female",
                         is.na(sex) & kp == "MSM" ~ "male",
                         kp == "TGM/Other" ~ "male")) %>%
  group_by(survey_id, iso3, sex, year, kp, age, hiv) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, iso3, sex, year, kp, age) %>% 
  mutate(denom = sum(n)) %>% 
  ungroup() %>% 
  select(-sex) %>% 
  left_join(spec_hiv %>% 
              filter(sex == "everyone") %>% 
              # kitchen.sink::single_year_to_five_year()  %>% 
              group_by(iso3, year, sex, age) %>% 
              summarise(hivpop = sum(hivpop),
                        totpop = sum(totpop),
                        tot_prev = hivpop/totpop)) 
# filter(!age < 15)


tgw_dat <- hivdat_allkp %>% filter(kp == "TGW")

tgw_dat_iso3 <- unique(tgw_dat$iso3)

pred_msmtgw <- spec_hiv %>% 
  filter(sex == "everyone") %>% 
  # kitchen.sink::single_year_to_five_year() %>% 
  group_by(iso3, year, sex, age) %>% 
  summarise(hivpop = sum(hivpop),
            totpop = sum(totpop),
            tot_prev = hivpop/totpop) %>% 
  ungroup() %>% 
  filter( iso3 %in% c(unique(tgw_dat_iso3), unique(msm_dat_iso3)),
          # sex == "female",
          # age %in% 15:49,
          year %in% 2010:2023) %>%
  mutate(model = "countries") %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>% 
  mutate(id.iso3.age = group_indices(., age, id.iso3)) %>% 
  mutate(denom = 1) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))

data_prep_tgw <- tgw_dat %>%
  left_join(moz.utils::region()) %>%
  distinct() %>% 
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
  left_join(pred_msmtgw %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age, id.iso3.age)) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))


##### MSM Data #####
msm_dat <- hivdat_allkp %>% filter(kp == "MSM") %>% filter(!sex == "female") %>% select(-sex)

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
  filter(age %in% 15:49) %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  left_join(pred_msm %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age, id.iso3.age)) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))


tgwmsm_inla_dat <- pred_msmtgw %>% mutate(kp = "TGW") %>% bind_rows(pred_msmtgw %>% mutate(kp = "MSM")) %>% 
  bind_rows(data_prep_tgw) %>% 
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
  mutate(id.age = multi.utils::to_int(age),
         id.year.age = group_indices(., age, year),
         id.age2 =  ifelse(model == "regions", NA_integer_, id.age)) %>% 
  filter(!is.na(age)) %>% 
  mutate(id.kp = ifelse(kp == "MSM", 0, 1),
         id.kp2 = id.kp,
         mean_year = year - 2017,
         mean_age = age - 24)



tgw_msm_formulae <- list(mod1 = n ~ 1 + id.kp + mean_age + mean_age:id.kp + mean_year + age:mean_year + f(id.iso3, model = "besag", graph = national_adj()),
                     mod2 = n ~ 1 + id.kp + f(mean_age, model = "ar1", replicate = id.kp2) + mean_year + age:mean_year + f(id.iso3, model = "besag", graph = national_adj()))

tgw_msm_mods_labs = data.frame(
  mod_name = c("mod1", "mod2"),
  formula = c("Age X KP + Age X Year + Country ICAR", "Age AR1 + KP + Year + Age x Year + Country ICAR") 
)

tgw_msm_mods <- lapply(tgw_msm_formulae, run_inla_model_msm, data = tgwmsm_inla_dat)

all_samples_tgw_msm_mods <- imap_dfr(tgw_msm_mods, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(tgw_msm_mods_labs)

tgw_msm_mods$mod1$summary
tgw_msm_mods$mod2$summary
tgw_msm_mods$mod3$summary

tgwmsm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(kp, survey_id, iso3, year, kp_prev, age, kp_odds, denom) %>%
  left_join(
    all_samples_tgw_msm_mods %>% 
      select(kp, iso3, year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      # filter(!sex == "everyone") %>% 
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
  geom_point(aes(x = age, y = kp_prev, size = denom, color = kp), alpha = 0.5) +
  geom_line(aes(x = age, y = prev, color = interaction(kp, formula))) +
  geom_ribbon(aes(x = age, ymin = prev_lower, ymax = prev_upper, fill = interaction(formula, kp)), alpha = 0.3) +
  scale_color_manual(values = c("orange", "palegreen4", "red", "blue", "darkred", "blue", "blue", "darkred")) +
  scale_fill_manual(values = c("orange", "red", "palegreen4", "cornflowerblue")) +
  geom_line(aes(x = age, y = tot_prev), linetype = "dashed") +
  facet_wrap(~survey_id) +
  # scale_y_log10() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "TGW/MSM HIV Prevalence", x = "Age Group")


tgwmsm_inla_dat %>% filter(!(is.na(survey_id))) %>%
  select(kp, survey_id, iso3, year, kp_prev, age, or_obs, denom) %>%
  left_join(
    all_samples_tgw_msm_mods %>% 
      select(iso3, kp,  year, age, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
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
  geom_point(aes(x = age, y = or_obs, size = denom, color = kp), alpha = 0.5) +
  geom_line(aes(x = age, y = or, color = interaction(formula, kp))) +
  geom_ribbon(aes(x = age, ymin = or_lower, ymax = or_upper, fill = interaction(formula, kp)), alpha = 0.3) +
  scale_color_manual(values = c("red", "blue", "orange", "palegreen4", "darkred", "blue", "blue", "darkred")) +
  scale_fill_manual(values = c( "red", "cornflowerblue",  "orange",  "palegreen4")) +
  facet_wrap(~survey_id) +
  scale_y_log10() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "OR", x = "Age Group")

