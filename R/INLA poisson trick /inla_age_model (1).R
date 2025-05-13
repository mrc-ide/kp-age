library(tidyverse)
library(TMB)
library(INLA)
library(moz.utils)


survs <-  read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agemoddat_20_06.csv", show_col_types = F) %>%
  moz.utils::separate_survey_id() %>%
  filter(kp == "FSW") %>%
  select(iso3, survey_id, year, age, n) %>%
  single_year_to_five_year() %>%
  group_by(iso3, survey_id, year, age_group) %>%
  summarise(Freq = sum(n)) %>%
  ungroup() 


# pop <- readRDS("../fertility_orderly/global/pop.rds")
pop <- readRDS("~/Downloads/pop 2.rds")
f24 <- filter(age_groups, age_group_sort_order %in% 18:24)$age_group
pop$COD <- distinct(pop$COD)
pop$COD$iso3 <- "COD"
pop$COG$iso3 <- "COG"
pop$MOZ$iso3 <- "MOZ"

pop <- crossing(year = 1990:2024,
                age_group = f24,
                iso3 = ssa_iso3()) %>%
  left_join(bind_rows(pop) %>%
              filter(area_id == iso3,
                     sex == "female",
                     age_group %in% f24)) %>%
  arrange(iso3, age_group, year) %>%
  group_by(iso3, age_group) %>%
  fill(population, .direction = "updown") %>%
  select(iso3, year, age_group, population)

genpop_offset <- pop %>%
  group_by(iso3, year) %>%
  mutate(population_prop = population/sum(population))

mf_model <- crossing(year = 1990:2022,
                     age_group = unique(survs$age_group)) %>%
  mutate(idx = factor(row_number()),
         id.age = as.numeric(factor(age_group)),
         id.year = year - min(year) +1) %>%
  mutate(
    id.age.year = factor(group_indices(., id.year, id.age))
  )

dat <- survs %>%
  left_join(mf_model %>% select(year, age_group, id.year, id.age, idx)) %>%
  group_by(survey_id) %>%
  mutate(N = sum(Freq)) %>%
  ungroup() %>%
  mutate(obs_iid = row_number()) %>%
  left_join(genpop_offset)


pred_dat <- mf_model %>% 
  distinct(year, age_group) %>%
  left_join(genpop_offset %>% ungroup() %>% filter(iso3 == "MWI") %>% select(year, age_group, population_prop)) %>%
  mutate(N = 1) %>%
  bind_rows(dat %>% select(survey_id, year, age_group, Freq, N, population_prop) %>% group_by(survey_id) %>% mutate(obs_iid = cur_group_id())) %>%
  left_join(mf_model %>% select(year, age_group, id.year, id.age))


####
# No offset
mod_ar1_int <- inla(Freq ~ -1 
                    + f(age_group, model = "rw1") 
                    + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1"), constr = T)
                    # + f(id.age, id.year, model = "iid", constr = T)
                    + f(obs_iid, hyper = multi.utils::tau_fixed(0.000001)),
                    family = "poisson",
                    # E = N,
                    # offset = log(population_prop),
                    dat = pred_dat,
                    control.compute = list(config = T))

summary(mod_ar1_int)

mod_ar1_int$summary.random$id.age

res <- sample_model(mod_ar1_int, pred_dat, "Freq") %>%
  mutate(across(mean:upper, exp)) %>%
  group_by(year) %>%
  mutate(m_norm = mean/sum(mean),
         u_norm = upper/sum(upper),
         l_norm = lower/sum(lower))


# Offset model

mod_ar1_int_offset <- inla(Freq ~ -1 
                           + f(age_group, model = "rw1") 
                           + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1"), constr = T)
                           + f(obs_iid, hyper = multi.utils::tau_fixed(0.000001)),
                           family = "poisson",
                           E = N,
                           offset = log(population_prop),
                           dat = pred_dat,
                           control.compute = list(config = T))

summary(mod_ar1_int_offset)

res_offset <- sample_model(mod_ar1_int_offset, pred_dat, "Freq") %>%
  mutate(across(mean:upper, exp)) %>%
  group_by(year) %>%
  mutate(m_norm = mean/sum(mean),
         u_norm = upper/sum(upper),
         l_norm = lower/sum(lower))

res <- bind_rows(res %>% mutate(source = "no offset"),
                 res_offset %>% mutate(source = "offset")) %>% 
  filter(year %in% unique(dat$year))

dat %>%
  ggplot(aes(x=age_group, y=Freq/N)) +
  geom_point(aes(size = Freq), show.legend = F) +
  geom_line(aes(group = survey_id), show.legend = F) +
  geom_line(data = res2, aes(y=m_norm, group = source, color = source), linewidth = 1.5) +
  # geom_line(data = res, aes(y=mean, group = year), color = "black", linewidth = 1.5) +
  # geom_ribbon(data = res_offset, aes(ymin = l_norm, ymax = u_norm, group = year), alpha = 0.3) +
  facet_wrap(~year)
