library(tidyverse)
library(INLA)
library(sf)
library(moz.utils)


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
  mutate(year = ifelse(year < 1993, year + 100, year))

fsw_hiv_age_dat <- dat %>% 
  filter(kp == "FSW", !survey_id %in% c("ETH2020ACA_FSW", "KEN2021ACA_FSW")) %>% 
  filter(sex == 1 | is.na(sex)) %>% 
  filter(!survey_id == "KEN1993ACA_FSW") %>% 
  bind_rows(KEN1993) %>% 
  mutate(survey_id = ifelse(str_detect(survey_id, "KEN1993"), "KEN1993ACA_FSW", survey_id)) %>% 
  filter(!(age <10 | age > 70)) %>% 
  mutate(age = round(age, 0)) %>% 
  select(survey_id, iso3, year, age, hiv) %>% 
  group_by(survey_id, iso3, year, age, hiv) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(!is.na(hiv),
         !is.na(age)) %>% 
  left_join(moz.utils::region() %>% select(-four_region)) %>%
  pivot_wider(
    names_from = hiv,
    values_from = n,
    names_prefix = "hiv_",
    values_fill = 0  # fill missing with 0 if a group is missing hiv==1 or hiv==0
  ) %>% 
  rename(n = hiv_1,
         n_neg = hiv_0) %>% 
  mutate(denom = n + n_neg) %>% 
  left_join(sf::read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% sf::st_drop_geometry()) %>%
  mutate(id.age = factor(multi.utils::to_int(age)))

fsw_hiv_age_dat %>% ggplot() +
  geom_line(aes(x = age, y = n/denom)) +
  facet_wrap(~survey_id) +
  theme_minimal()

pred_fsw <- spec_hiv %>% 
  # kitchen.sink::single_year_to_five_year() %>% 
  group_by(iso3, year, sex, age) %>% 
  summarise(hivpop = sum(hivpop),
            totpop = sum(totpop),
            tot_prev = hivpop/totpop) %>% 
  ungroup() %>% 
  filter(iso3 %in% fsw_hiv_age_dat$iso3,
          sex == "female",
          # age %in% 15:49,
          year %in% 1993:2023) %>%
  mutate(model = "countries") %>%
  left_join(sf::read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% sf::st_drop_geometry()) %>% 
  mutate(id.iso3.age = group_indices(., age, id.iso3)) %>% 
  mutate(denom = 1) %>% 
  mutate(id.age = factor(multi.utils::to_int(age)))


# data_prep_cfsw <- cfsw_dat %>%
#   left_join(moz.utils::region()) %>%
#   pivot_wider(
#     names_from = hiv,
#     values_from = n,
#     names_prefix = "hiv_",
#     values_fill = 0  # fill missing with 0 if a group is missing hiv==1 or hiv==0
#   ) %>% 
#   rename(n = hiv_1,
#          n_neg = hiv_0) %>% 
#   mutate(denom = n + n_neg) %>% 
#   # kitchen.sink::single_year_to_five_year(age) %>% 
#   # age %in% 15:49) %>%
#   left_join(read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
#   left_join(pred_cfsw %>% filter(!is.na(id.iso3.age)) %>% distinct(iso3, age, id.iso3.age)) %>% 
#   mutate(id.age = factor(multi.utils::to_int(age)))


fsw_agehiv_inla_dat <- pred_fsw %>%
  bind_rows(fsw_hiv_age_dat %>%
              left_join(pred_fsw %>% 
                          filter(!is.na(id.iso3.age)) %>% 
                          distinct(iso3, age, id.iso3.age))) %>% 
  mutate(id.year = multi.utils::to_int(year),
         id.year2 = id.year+1) %>% 
  mutate(kp_prev = n/denom,
         kp_odds = kp_prev/(1-kp_prev),
         genpop_odds = tot_prev/(1-tot_prev),
         or_obs = kp_odds/genpop_odds) %>% 
  mutate(id.age = multi.utils::to_int(id.age),
    id.survey_id = multi.utils::to_int(survey_id),
    id.age2 = id.age,
    # id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2),
    id.iso3.year = group_indices(., iso3, year),
    id.iso3.year = ifelse(is.na(iso3), NA_integer_, id.iso3.year)) %>% 
  #id.age2 = ifelse(is.na(survey_id), NA_integer_, id.age2)) %>% 
  # kitchen.sink::single_year_to_five_year(age) %>% 
  mutate(id.year.age = group_indices(., id.age, id.year),
         mean_age = age - 28,
         mean_year = year - 2011) %>% 
  filter(!is.na(age)) %>% 
  filter(!sex == "male")


## Generic 0 set-up
n_ages <- length(unique(pred_fsw$age))
n_years <- length(unique(pred_fsw$year))

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



run_inla_model <- function(formula, data, denom, tot_prev) {
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


fsw_formulae <- list( #mod 1 = n ~ 1 + f(id.year, model = "rw2") + f(id.age2, model = "ar1") 
                       
                     mod1 = n ~ 1 + f(id.year, model = "rw2") + f(id.age2, model = "ar1") +
                       f(id.iso3, model = "besag", graph = national_adj()) +
                       f(id.age, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = adj)) +
                       f(id.year.age, model = "generic0",
                         Cmatrix = Q2,
                         extraconstr = list(A = A_combined2, e = e2),
                         rankdef = n_years + n_ages - 1L) 
                     # 
                     # mod2 = n ~ 1 + f(id.year, model = "rw2") + f(id.age2, model = "ar1") + f(id.iso3, model = "besag", graph = adj) + f(id.survey_id, model = "iid") +
                     #   f(id.age, model = "rw2",  group = id.iso3, control.group = list(model = "besag", graph = national_adj())) +
                     #   f(id.year.age, model = "generic0",
                     #     Cmatrix = Q2,
                     #     extraconstr = list(A = A_combined2, e = e2),
                     #     rankdef = n_years + n_ages - 1L)
                     )


formula_labs = data.frame(
  mod_name = c("mod1",
               "mod2"
               ),
  formula = c("Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age", 
              "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age + Survey IID")
)

formula_lab_list = list(
  mod1_lab = "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age",
  mod2_lab = "Year RW2 + Age AR1 + Year X Age + ISO3 ICAR + ISO3 X Age + Survey IID"
)

fsw_results <- lapply(fsw_formulae, run_inla_model, data = fsw_agehiv_inla_dat)

all_samples <- imap_dfr(results, ~ {
  .x$samples %>%
    mutate(mod_name = .y)
}) %>% 
  left_join(formula_labs)


#Error hit when just running mod1 

# 2: In parallel::mclapply(1:cs$nconfig, (function(k) { :
#     all scheduled cores encountered errors in user code

#Errors hit when running mod2

# Warning in inla.core(formula = formula, family = family, contrasts = contrasts,  :
#               f(id.age, ...) :  There is no indices where group[]=1, this is *usually* a misspecification
# Assertion failed: (ged->n == n * ngroup), function inla_make_group_graph, file inla-graph.c, line 302.
#               sh: line 1: 39560 Abort trap: 6           '/Library/Frameworks/R.framework/Versions/4.4-x86_6....

