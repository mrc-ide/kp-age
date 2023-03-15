library(RDS)
library(tidyverse)
library(rdhs)
library(INLA)
library(sf)
library(spdep)
library(countrycode)
library(stringdist)
library(splines)
library(multi.utils)

# source("C:/Users/rla121/OneDrive - Imperial College London/Documents/GitHub/survey-extraction/src/kp_recoding_functions_21_02.R")
# setwd("C:/Users/rla121/OneDrive - Imperial College London/Documents/GitHub/survey-extraction")
# source("src/kp_recoding_functions_21_02.R")
# ssa_iso3 <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")


age_dat2 <- read_csv("~/Downloads/age_dat.csv")

ssd_boundary <- read_sf("~/Downloads/ssd_adm_imwg_nbs_20220121/ssd_admbnda_adm0_imwg_nbs_20210924.shp")
ssd_boundary_simple <- rmapshaper::ms_simplify(ssd_boundary, 0.05)

geographies <- read_sf("~/Downloads/Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp") %>%
  bind_rows(
    ssd_boundary_simple %>%
      transmute(CNTRY_NAME = "South Sudan")
  ) %>%
  mutate(iso3 = countrycode(CNTRY_NAME, "country.name", "iso3c"),
         area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  filter(iso3 %in% ssa_iso3)

geographies <- geographies %>%
  arrange(iso3) %>%
  mutate(id.iso3 = as.numeric(factor(iso3))) %>%
  select(iso3, area_name, id.iso3) %>%
  st_make_valid() 



pred2 <- crossing(iso3 = ssa_iso3,
                  age = 15:49) %>%
  # mutate(age_group = naomi::cut_naomi_age_group(age)) %>%
  # distinct(iso3, age_group) %>%
  bind_rows(age_dat2 %>%
              filter(sex == "female", !is.na(ageratio)) %>%
              # filter(age %in% 15:49) %>%
              mutate(id.ref = multi.utils::to_int(survey_id),
                     id.ref2 = id.ref,
                     age_group = naomi::cut_naomi_age_group(age)) %>%
              # group_by(iso3, survey_id, id.ref, id.ref2, age_group) %>%
              # summarise(n = sum(n)) %>%
              group_by(survey_id) %>%
              mutate(n_eff_kish = sum(n)) %>%
              select(x_eff = n, n_eff_kish, any_of(c("age_group", "age")), survey_id, iso3, id.ref, id.ref2, genpop_estimate, ageratio, totpop)
  ) %>%
  left_join(geographies %>% st_drop_geometry() %>% select(iso3, id.iso3)) %>%
  mutate(id.obs = multi.utils::to_int(row_number()),
         id.age = multi.utils::to_int(age),
         # id.age = multi.utils::to_int(age_group),
         id.age2 = id.age,
         estimate = x_eff/n_eff_kish) %>%
  type.convert(as.is = TRUE) %>%
  filter(!iso3 == "HTI",
         !iso3 == "ERI") %>%
  ungroup()




# formula <- n ~ totpop + f(age, model = "ar1") + f(id.ref) + f(id.iso3, model="besag", graph=geog.adj, scale.model = TRUE)
# formula <- n ~ genpop_estimate + f(age, model = "ar1") + f(id.ref) + f(id.iso3, model="besag", graph=geog.adj, scale.model = TRUE)
# formula <- n ~ genpop_estimate + f(age, model = "ar1") + f(id.ref)
# formula2 <- n ~ f(id.idx, model = "iid", hyper = multi.utils::tau_fixed(0.000001)) + f(id.age, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u =2.5, alpha = 0.01)) + f(age, model = "ar1")

ar1_group_prior <- list(
  rho = list(rho = "pc.cor1", param = c(0, 0.75)),
  prec = list(prec = "pc.prec", param = c(2.5, 0.01), initial = log(0.001))
)

formula <- x_eff ~ -1 + 
  f(id.ref, model = "iid" , hyper = multi.utils::tau_fixed(1E-6)) +
  bs(id.age, df = 5)  +
  # f(id.age, model = "ar1") +
  f(id.iso3, model="besag", graph="geog.adj", scale.model = TRUE, group = id.age, control.group = list(model = "rw2"), constr = TRUE) +
  f(id.ref2, model="iid", group = id.age, control.group = list(model = "rw2"), constr = TRUE)

# df <- pred2 %>%
#   filter(!is.na(x_eff)) 

#debugonce(multinomial_model)
# int <- multinomial_model(formula, "test", 1000)

fit <- inla(formula, data = pred2, family = 'xPoisson',
            control.predictor = list(link = 1),
            control.compute = list(dic = TRUE, waic = TRUE,
                                   cpo = TRUE, config = TRUE),
            inla.mode = "experimental",
            verbose = TRUE)

df <- pred2 %>%
  filter(across(all_of("x_eff"), ~!is.na(.x)))

samples <- inla.posterior.sample(1000, fit)
ind.effect <- 1:(nrow(pred2) - nrow(df))
samples.effect = lapply(samples, function(x) x$latent[ind.effect])
prev_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)

ident <- pred2[ind.effect, ]

qtls <- apply(prev_samples, 1, quantile, c(0.025, 0.5, 0.975))

prev <- ident %>%
  ungroup() %>%
  mutate(
    lower = qtls[1,],
    median = qtls[2,],
    upper = qtls[3,]
  )

prev %>%
  group_by(iso3) %>%
  mutate(rate = exp(median)/sum(exp(median))) %>%
  ggplot(aes(x=age, group = survey_id)) +
  geom_line(data = pred2, aes(y=estimate, color=survey_id), show.legend = FALSE) +
  geom_line(aes(y=rate)) +
  # geom_ribbon(aes(ymin = (prob_lower), ymax = (prob_upper)), alpha = 0.4) +
  facet_wrap(~iso3) +
  standard_theme()


