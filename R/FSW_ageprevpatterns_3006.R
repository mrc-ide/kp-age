library(purrr)
library(INLA)
library(tidyverse)
library(sf)

run_inla_model <- function(formula, data, denom, tot_prev) {
  print(formula)
  
  mod <- inla(
    formula = formula,
    Ntrials = denom,
    offset = qlogis(tot_prev),
    data = inla_dat %>% filter(!is.na(iso3)),
    family = "xbinomial",
    control.inla = list(int.strategy = "eb"),
    control.family = list(link = "logit"),
    control.compute=list(config = TRUE),
    verbose = F
  )
  
  model_summary <- summary(mod)
  
  model_samples <- moz.utils::sample_model(mod, data, col = "survey_id")
  
  
  list(
    model = mod,
    summary = model_summary,
    samples = model_samples
  )
}


dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agehiv_data_extract_240925.rds")
spec_hiv <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/spec_hiv.rds")

KEN1993 <- dat %>% 
  filter(survey_id == "KEN1993ACA_FSW") %>% 
  filter(visit == 0) %>% 
  separate(survey_day, into = c(NA, NA, "survey_year"), sep = "/") %>% 
  mutate(year = paste0(19, survey_year),
         survey_id = paste0(survey_id, "_", survey_year)) %>% 
  select(survey_year, everything()) %>% 
  type.convert(as.is = T) %>% 
  mutate(year = ifelse(year < 1993, year + 100, year)) %>% mutate(survey_id = "KEN1993ACA_FSW") %>% 
  filter(!year <2001)

dat2 <- dat %>%
  filter(kp == "FSW", !survey_id %in% c("ETH2020ACA_FSW", "KEN2021ACA_FSW", "NGA2014BBS_FSW", "KEN1993ACA_FSW")) %>% 
  bind_rows(KEN1993) %>% 
  filter(sex == 1 | is.na(sex)) %>% 
  mutate(age = round(age, 0))
  # mutate(sex = case_when(kp == "PWID" & is.na(sex) & gender %in%  c("female", "transgender") ~ 1,
  #                        kp == "PWID" & is.na(sex) & gender %in%  c("male", "TGM") ~ 0,
  #                        TRUE ~ sex),
  #        kp2 = case_when(kp == "MSM" & gender %in% c("0", "male", NA) ~ "MSM",
  #                        kp == "MSM" & gender %in% c("1", "3", "female", "TGW") ~ "TGW",
  #                        kp == "MSM" & gender %in% c("5", "other") ~ "TGM/Other",
  #                        kp == "MSMTG" & gender == "male" ~ "MSM",
  #                        kp == "MSMTG" & gender %in% c("female", "tgw") ~ "TGW",
  #                        kp == "MSMTG" & gender == "non-binary" ~ "TGM/Other",
  #                        kp == "TG" & gender %in% c("3", "1") & sex == 0 ~ "TGW",
  #                        kp == "TG" & gender %in% c("0", "4") & sex == 1 ~ "TGM/Other",
  #                        kp == "TGM" ~ "TGM/Other",
  #                        TRUE ~ kp)) %>% 
  # filter(!kp %in% c("PWUD", "TG")) %>% 
  # mutate(kp = kp2) 



fsw_dat <- dat2 %>% 
  filter(!is.na(hiv), !(hiv > 0 & hiv < 1)) %>%
  # !(kp == "FSW" & sex == 0),
  # !(kp == "MSM" & sex == 1)) %>%
  # kitchen.sink::single_year_to_five_year() %>% 
  group_by(survey_id, iso3, year, kp, age, hiv) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, iso3, year, kp, age) %>% 
  mutate(denom = sum(n)) %>% 
  ungroup() %>% 
  left_join(spec_hiv %>% 
              filter(sex == "female") %>% 
              # kitchen.sink::single_year_to_five_year()  %>% 
              group_by(iso3, year, age) %>% 
              summarise(hivpop = sum(hivpop),
                        totpop = sum(totpop),
                        tot_prev = hivpop/totpop)) %>% 
  pivot_wider(
    names_from = hiv,
    values_from = n,
    names_prefix = "hiv_",
    values_fill = 0  # fill missing with 0 if a group is missing hiv==1 or hiv==0
  ) %>% 
  rename(n = hiv_1,
         n_neg = hiv_0) %>% 
  mutate(denom = n + n_neg) %>% 
  filter(age %in% 15:49)
# filter(!age < 15)


fsw_dat %>% filter(age %in% 15:49) %>% ggplot() +geom_line(aes(x = age, y = n/denom), show.legend = F) + facet_wrap(year~survey_id) + theme_minimal() 



###########
### You need to make n and n_neg 
###########

fsw_dat_iso3 <- unique(fsw_dat$iso3)

pred <- spec_hiv %>%
  # kitchen.sink::single_year_to_five_year() %>% 
  filter( #iso3 %in% fsw_dat_iso3,
    sex == "female",
    age %in% 15:49,
    year %in% 1993:2025) %>%
  left_join(moz.utils::region()) %>%
  bind_rows(mutate(., region = "SSA")) %>%
  group_by(year, age, region) %>%
  summarise(hivpop = sum(hivpop),
            totpop = sum(totpop),
            tot_prev = hivpop/totpop) %>%
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
      filter( #iso3 %in% fsw_dat_iso3,
        sex == "female",
        age %in% 15:49,
        year %in% 1993:2025) %>%
      mutate(model = "countries") %>%
      left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>% 
      mutate(id.iso3.age = group_indices(., age, id.iso3))
  ) %>%
  mutate(denom = 1) %>% 
  mutate(id.age = factor(multi.utils::to_int(age))
  )

data_prep <- fsw_dat %>%
  left_join(moz.utils::region()) %>%
  # kitchen.sink::single_year_to_five_year(age) %>% 
  filter(age %in% 15:49) %>%
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  left_join(pred %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age, id.iso3.age)) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))


inla_dat <- pred %>%
  bind_rows(data_prep) %>% 
  mutate(id.year = multi.utils::to_int(year),
         id.age = multi.utils::to_int(age),
         # id.year.age = group_indices(., age, year),
         id.year2 = id.year+1) %>% 
  mutate(kp_prev = n/denom,
         kp_odds = kp_prev/(1-kp_prev),
         genpop_odds = tot_prev/(1-tot_prev),
         or_obs = kp_odds/genpop_odds) %>% 
  mutate(#id.age2 = id.age,
    id.survey_id = multi.utils::to_int(survey_id),
    id.age2 = id.age,
    id.age3 = ifelse(is.na(iso3), NA_integer_, id.age),
    id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2),
    id.iso3.year = group_indices(., iso3, year),
    id.iso3.year = ifelse(is.na(iso3), NA_integer_, id.iso3.year)) %>% 
  #id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2)) %>% 
  # kitchen.sink::single_year_to_five_year(age) %>% 
  mutate(id.year.age = group_indices(., id.age, year)) 





## Generic 0 set-up
n_ages <- length(unique(pred$age))
# n_ages <- length(unique(pred$age))
n_years <- length(unique(pred$year))

n_interactions_year <- n_years * n_ages

# Sum-to-zero constraint within each year for age effects
A_sum_age_year <- matrix(0, nrow = n_years, ncol = n_interactions_year)

for (year in 1:n_years) {
  A_sum_age_year[year, ((year - 1) * n_ages + 1):(year * n_ages)] <- 1
} 

## Same in reverse to have sum-to-zero constraints within each age over year
A_sum_year_age <- matrix(0, nrow = n_ages, ncol = n_interactions_year)

for (age in 1:n_ages) {
  A_sum_year_age[age, ((age - 1) * n_years + 1):(age * n_years)] <- 1
}

A_combined2 <- rbind(A_sum_year_age, A_sum_age_year) 
e2 <- matrix(0, nrow(A_combined2), nrow = 1) 

R_age <- dfertility::make_rw_structure_matrix(n_ages, 2, FALSE) 
R_year <- dfertility::make_rw_structure_matrix(n_years, 2, FALSE) 

Q2 <- kronecker(R_age, R_year)


## Spatial stuff

national_areas <- read_sf(moz.utils::national_areas()) %>% mutate(iso3 = area_id) %>% st_make_valid()

nb <- national_areas %>%
  spdep::poly2nb()
names(nb) <- national_areas$area_id
nb <- lapply(nb, as.integer)
class(nb) <- "nb"

adj <- spdep::nb2mat(nb, zero.policy=TRUE, style="B")
R_iso3 <- INLA::inla.scale.model(
  diag(rowSums(adj)) - 0.99*adj,
  constr = list(A = matrix(1, 1, nrow(adj)), e = 0)
) 


### 

formulas <- list(
  mod1 = n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2,
      extraconstr = list(A = A_combined2, e = e2),
      rankdef = n_years + n_ages - 1L),
  #
  mod2 = n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2,
      extraconstr = list(A = A_combined2, e = e2),
      rankdef = n_years + n_ages - 1L),


  mod3 = n ~ 1 + f(id.year, model = "rw2") + f(id.age, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) + f(id.survey_id, model = "iid")+
    f(id.year.age, model = "generic0",
      Cmatrix = Q2,
      extraconstr = list(A = A_combined2, e = e2),
      rankdef = n_years + n_ages - 1L) ,

  mod4 = n ~ 1 + f(id.year, model = "rw2") + f(id.age2, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) +
    f(id.age3, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj())) +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2,
      extraconstr = list(A = A_combined2, e = e2),
      rankdef = n_years + n_ages - 1L),

  mod5 = n ~ 1 + f(id.year, model = "rw2") + f(id.age2, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) + f(id.survey_id, model = "iid") +
    f(id.age3, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj())) +
    f(id.year.age, model = "generic0",
      Cmatrix = Q2,
      extraconstr = list(A = A_combined2, e = e2),
      rankdef = n_years + n_ages - 1L),
  
  mod6 = n ~ 1 + id.age:year + f(id.year, model = "rw2") + f(id.age2, model = "ar1") + f(id.iso3, model = "besag", graph = national_adj()) + f(id.survey_id, model = "iid") +
    f(id.age3, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj()))

  
)

formula_labs = data.frame(
  mod_name = c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6"),
  formula = c("Year RW2 + Age AR1 + Year X Age", "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR", "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + Survey IID", "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age", "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age + Survey IID", "Year RW2 + Age AR1 + Linear Year:Linear Age + ISO3 ICAR + ISO3 X Age + Survey IID")
)

formula_lab_list = list(
  mod1_lab = "Year RW2 + Age AR1 + Year X Age",
  mod2_lab = "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR",
  mod3_lab = "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + Survey IID",
  mod4_lab = "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age",
  mod5_lab = "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age + Survey IID",
  mod6_lab = "Year RW2 + Age AR1 + Linear Year:Linear Age + ISO3 ICAR + ISO3 X Age + Survey IID"
)

results <- lapply(formulas, run_inla_model, data = inla_dat)

beepr

all_samples <- imap_dfr(results, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(formula_labs)


inla_dat %>% filter(!(is.na(survey_id) |survey_id =="NGA2014BBS_FSW")) %>%
  select(survey_id, iso3, year, kp_prev, age, kp_odds, denom) %>%
  left_join(
    all_samples %>% 
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
  facet_wrap(year~survey_id, nrow = 6) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "FSW HIV Prevalence", x = "Age Group") + 
  theme(legend.position = "right")




inla_dat %>% filter(!(is.na(survey_id) | survey_id == "NGA2014BBS_FSW"), !sex == "male") %>%
  select(survey_id, iso3, year, kp_prev, age_group, kp_odds,  or_obs ) %>%
  left_join(
    all_samples %>% 
      select(iso3, year, age_group, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean),
             or_lower = exp((lower - logit_totprev)),
             or_upper = exp((upper - logit_totprev))) %>%
      filter(model == "countries")) %>%
  ggplot() +
  geom_point(aes(x = age_group, y = or_obs)) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = or, color = formula)) +
  geom_ribbon(aes(x = multi.utils::to_int(age_group), ymin = or_lower, ymax = or_upper, fill = formula), alpha = 0.3) +
  facet_wrap(~survey_id) +
  theme_minimal() + 
  scale_y_continuous(trans = "log10", labels = scales::label_number()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Age Group", y = "FSW:Genpop OR", fill = "Formula", color = "Formula")


### Region spec
# inla_dat %>% filter(!(is.na(survey_id) | survey_id == "NGA2014BBS_FSW")) %>%
#   select(survey_id, iso3, year, kp_prev, age_group, kp_odds) %>%
#   left_join(
    all_samples %>% 
      select(iso3, region, year, age_group, mean, lower, upper, tot_prev, mod_name, model, formula) %>%
      mutate(logit_totprev = qlogis(tot_prev),
             log_or = mean - logit_totprev,
             or = exp(log_or),
             prev_lower = plogis(lower),
             prev_upper = plogis(upper),
             prev = plogis(mean)) %>%
      filter(model == "regions", year %in% c(1995, 2000, 2005, 2010, 2015, 2020, 2025), !is.na(region)) %>%
  ggplot() +
  geom_point(data = inla_dat %>% mutate(year = plyr::round_any(year, 5)) %>% filter(!is.na(region)), aes(x = age_group, y = kp_prev)) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = prev, color = interaction(formula, region))) +
  geom_ribbon(aes(x = multi.utils::to_int(age_group), ymin = prev_lower, ymax = prev_upper, fill = interaction(formula, region)), alpha = 0.3) +
  geom_line(aes(x = multi.utils::to_int(age_group), y = tot_prev), color = "black", linetype = "dashed") +
      facet_wrap(region~year, ncol = 7) +
  # facet_wrap(~survey_id) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(y = "FSW HIV Prevalence", x = "Age Group") +
      scale_color_manual(values = region_shades) +
      scale_fill_manual(values = region_shades)
    
    
region_colors <- c(
      "WCA" = "#1f77b4",  # base blue
      "ESA" = "#ff7f0e" ,
      "SSA" = "#c61a09" # base orange
    )


# Ensure formula is a factor (ordered)
all_samples_colors <- all_samples %>%
  mutate(formula = factor(formula)) %>% filter(!is.na(region))

# Get all combos
combos <- all_samples_colors %>%
  distinct(region, formula) %>%
  arrange(region, formula)

# For each region, get lightness levels
region_shades_df <- combos %>%
  group_by(region) %>%
  mutate(
    lightness = seq(0.05, 0.5, length.out = n()),
    shade = mapply(lighten, region_colors[region], lightness)
  ) %>%
  ungroup() %>%
  mutate(group = interaction(formula, region)) %>%
  select(group, shade)

# Named vector for ggplot
region_shades <- setNames(region_shades_df$shade, region_shades_df$group)
