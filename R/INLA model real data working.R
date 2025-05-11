library(tidyverse)
library(TMB)
library(INLA)
library(moz.utils)

single_year_to_five_year <- function (df, fifteen_to_49 = TRUE) {
  df <- df %>% dplyr::mutate(age_group_label = cut(age, c(0, 
                                                          seq(5, 85, 5) - 1), c(paste0(seq(0, 79, 5), "-", seq(5, 
                                                                                                               80, 5) - 1), "80+"), include.lowest = TRUE)) %>% dplyr::left_join(naomi::get_age_groups() %>% 
                                                                                                                                                                                   select(age_group, age_group_label)) %>% dplyr::select(-age_group_label)
  if (fifteen_to_49) {
    df %>% dplyr::filter(age %in% 15:49) %>% dplyr::select(-age)
  }
  else {
    df %>% dplyr::select(-age)
  }
}

survs <-  read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agemoddat_20_06.csv", show_col_types = F) %>%
  separate_survey_id() %>%
  filter(kp == "FSW") %>%
  select(iso3, survey_id, year, age, n) %>%
  single_year_to_five_year() %>%
  group_by(iso3, survey_id, year, age_group) %>%
  summarise(Freq = sum(n)) %>%
  ungroup() 

age_groups <- naomi::get_age_groups()

pop <- readRDS("~/Downloads/pop 3.rds")
f24 <- filter(age_groups, age_group_sort_order %in% 18:24)$age_group
pop$COD <- distinct(pop$COD)
pop$COD$iso3 <- "COD"
pop$COG$iso3 <- "COG"
pop$MOZ$iso3 <- "MOZ"

pop <- crossing(year = 1990:2024,
                age_group = f24,
                iso3 = moz.utils::ssa_iso3()) %>%
  left_join(bind_rows(pop) %>%
              filter(area_id == iso3,
                     sex == "female",
                     age_group %in% f24)) %>%
  arrange(iso3, age_group, year) %>%
  group_by(iso3, age_group) %>%
  fill(population, .direction = "updown") %>%
  select(iso3, year, age_group, population) %>% 
  ungroup()

pop %>%
  filter(iso3 == "MWI", age_group == "Y015_019") %>%
  pull(population) %>%
  plot

genpop_offset <- pop %>%
  group_by(iso3, year) %>%
  mutate(population_prop = population/sum(population)) %>% 
  ungroup()

mf_model <- crossing(year = 1990:2023,
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
  # mutate(obs_iid = row_number()) %>%
  mutate(obs_iid = multi.utils::to_int(survey_id)) %>% 
  left_join(genpop_offset)

pred_dat <- mf_model %>% distinct(year, age_group) %>% 
  mutate(N = 1) %>%
  left_join(genpop_offset %>% filter(iso3 == "MWI") %>% select(year, age_group, population, population_prop)) %>%
  bind_rows(dat %>% select(year, age_group, Freq, N, population, population_prop, obs_iid)) %>%
  left_join(mf_model %>% select(year, age_group, id.year, id.age)) 

mod_ar1_int <- inla(Freq ~ -1 
                    + f(age_group, model = "rw1")
                    + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1"), constr = T)
                    + f(obs_iid, hyper = multi.utils::tau_fixed(0.000001)),
                    family = "poisson",
                    E = N,
                    offset = log(population_prop),
                    dat = pred_dat,
                    control.compute = list(config = T))

summary(mod_ar1_int)

res_offset <- sample_model(mod_ar1_int, pred_dat, "Freq") %>%
  mutate(across(mean:upper, exp))


mod_ar1_int_no_offset <- inla(Freq ~ -1 
                    + f(age_group, model = "rw1")
                    + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1"), constr = T)
                    + f(obs_iid, hyper = multi.utils::tau_fixed(0.000001)),
                    family = "poisson",
                    # E = N,
                    # offset = log(population_prop),
                    dat = pred_dat,
                    control.compute = list(config = T))

summary(mod_ar1_int_no_offset)

res_offset <- sample_model(mod_ar1_int, pred_dat, "Freq") %>%
  mutate(across(mean:upper, exp))

res_no_offset <- sample_model(mod_ar1_int, pred_dat, "Freq") %>%
  mutate(across(mean:upper, exp))

mod_ar1_int$summary.random


plot_dat <- dat %>%
  left_join(res %>% select(year, age_group, mean, lower, upper)) %>% 
  group_by(survey_id) %>%
  mutate(mean_prop = mean/sum(mean),
         lower_prop = lower/sum(lower),
         upper_prop = upper/sum(upper))

plot_dat_offset <- dat %>%
  left_join(res_offset %>% select(year, age_group, mean, lower, upper)) %>% 
  group_by(survey_id) %>%
  mutate(mean_prop = mean/sum(mean),
         lower_prop = lower/sum(lower),
         upper_prop = upper/sum(upper))

res_no_offset <- res_no_offset %>% 
  group_by(year) %>% 
  mutate(mean_prop = mean/sum(mean),
         lower_prop = lower/sum(lower),
         upper_prop = upper/sum(upper)) %>% 
  ungroup()

res_offset <- res_offset %>% 
  group_by(year) %>% 
  mutate(mean_prop = mean/sum(mean),
         lower_prop = lower/sum(lower),
         upper_prop = upper/sum(upper)) %>% 
  ungroup()

res_offset2 <- res_offset %>% 
  filter(year %in% dat$year)

dat %>% 
  # filter(year == 2014) %>% 
  ggplot(aes(x=age_group, y=Freq/N)) +
  geom_point(aes(size = Freq, color = survey_id), show.legend = F) +
  geom_line(aes(color = survey_id, group = survey_id), show.legend = F) +
  geom_line(data = res_offset2, aes(y=m_norm, group = year), color = "black", linewidth = 1) +
  # geom_line(data = res_no_offset, aes(y=mean_prop, group = year), color = "black", linewidth = 1) +
  # geom_line(data = res_offset, aes(y=mean_prop, group = year), color = "red", linewidth = 1) +
  geom_ribbon(data = res_offset2 , aes(ymin = l_norm, ymax = u_norm, group = year), alpha = 0.3) +
  facet_wrap(~year)



pred_dat <- mf_model %>%   distinct(year, age_group) %>%  left_join(genpop_offset %>% ungroup() %>% filter(iso3 == "MWI") %>% select(year, age_group, population_prop)) %>%  mutate(N = 1) %>%  bind_rows(dat %>% select(survey_id, year, age_group, Freq, N, population_prop) %>% group_by(survey_id) %>% mutate(obs_iid = cur_group_id())) %>%  left_join(mf_model %>% select(year, age_group, id.year, id.age))

mod_ar1_int_no_offset <- inla(Freq ~ -1 
                    + f(age_group, model = "rw1") 
                    + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1"), constr = T)
                    # + f(id.age, id.year, model = "iid", constr = T)
                    + f(obs_iid, hyper = multi.utils::tau_fixed(0.000001)),
                    family = "poisson",
                    # E = N,
                    # offset = log(population_prop),
                    dat = pred_dat,
                    control.compute = list(config = T))

summary(mod_ar1_int_no_offset)

mod_ar1_int$summary.random$id.age

res <- sample_model(mod_ar1_int_no_offset, pred_dat, "Freq") %>%
  mutate(across(mean:upper, exp)) %>%
  group_by(year) %>%
  mutate(m_norm = mean/sum(mean),
         u_norm = upper/sum(upper),
         l_norm = lower/sum(lower))


mod_ar1_int <- inla(Freq ~ -1 
                    + f(age_group, model = "rw1") 
                    + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1"), constr = T)
                    # + f(id.age, id.year, model = "iid", constr = T)
                    + f(obs_iid, hyper = multi.utils::tau_fixed(0.000001)),
                    family = "poisson",
                    E = N,
                    offset = log(population_prop),
                    dat = pred_dat,
                    control.compute = list(config = T))

summary(mod_ar1_int)

res_offset <- sample_model(mod_ar1_int, pred_dat, "Freq") %>%
  mutate(across(mean:upper, exp)) %>%
  group_by(year) %>%
  mutate(m_norm = mean/sum(mean),
         u_norm = upper/sum(upper),
         l_norm = lower/sum(lower))

res2 <- bind_rows(res %>% mutate(source = "no offset"),
                 res_offset %>% mutate(source = "offset")) %>% 
  filter(year %in% unique(dat$year))


## But predictions look fine?
dat %>%
  ggplot(aes(x=age_group, y=Freq/N)) +
  geom_point(aes(size = Freq), show.legend = F) +
  geom_line(aes(group = survey_id), show.legend = F) +
  geom_line(data = res2, aes(y=m_norm, group = source, color = source), linewidth = 1.5) +
  # geom_line(data = res, aes(y=mean, group = year), color = "black", linewidth = 1.5) +
  # geom_ribbon(data = res, aes(ymin = l_norm, ymax = u_norm, group = year), alpha = 0.3) +
  facet_wrap(~year)


