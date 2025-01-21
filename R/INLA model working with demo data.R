library(tidyverse)
library(TMB)
library(INLA)

# Example age dsisribution
hist(rgamma(1000, 2, 1))
## Probability in each group
prob <- diff(pgamma(seq(0, 7, 1), 2, 1))
# Normalise cos upper age group represents an open interval
prob <- prob/sum(prob)
## Here are some surveys for a given year
rmultinom(5, 1000, prob)

age_groups <- naomi::get_age_groups()
age_groups <- read_csv("../naomi/inst/metadata/meta_age_group.csv", show_col_types = F)

## Now loop over e.g. 10 years, increasing the mean age at each year by a little bit (in this case increasing the gamma mean by a tenth)
survs <- lapply(seq(2, 3, length.out = length(1990:2020)), function(x) {
  prob <- diff(pgamma(seq(0, 7, 1), x, 1))
  prob <- prob/sum(prob)
  survs <- rmultinom(5, round(runif(1, 100, 1000)), prob)
  dimnames(survs) <- list("age_group" = age_groups %>% filter(age_group_sort_order %in% 18:24) %>% pull(age_group),
                          "survey_id" = paste0("survey", 1:5))
  as.data.frame.table(survs)
})

names(survs) <- 1990:2020

survs <- bind_rows(survs, .id = "year") %>%
  type.convert(as.is = T)

survs %>%
  # filter(survey_id == "survey1") %>%
  group_by(survey_id, year) %>% 
  summarise(prop = Freq/sum(Freq),
            age_group = age_group) %>% 
  ungroup() %>% 
  ggplot(aes(x=age_group, y=prop, group = interaction(year, survey_id), color = factor(year))) +
  geom_line()


mf_model <- crossing(year = 1990:2020,
                     age_group = unique(survs$age_group)) %>%
  mutate(idx = factor(row_number()),
         id.age = as.numeric(factor(age_group)),
         id.year = year - min(year) +1) %>%
  mutate(
    id.age.year = factor(group_indices(., id.year, id.age))
  )

dat <- survs %>%
  left_join(mf_model %>% select(year, age_group, id.year, id.age, idx)) %>%
  group_by(survey_id, year) %>%
  mutate(N = sum(Freq)) %>%
  ungroup() %>%
  mutate(obs_iid = row_number())

mod_fe <- inla(Freq ~ -1 + age_group,
               family = "poisson",
               E = dat$N,
               dat = dat)

clean_inla(mod_fe)$fixed

mod_iid <- inla(Freq ~ -1 + age_group + f(obs_iid),
                family = "poisson",
                E = dat$N,
                dat = dat)

clean_inla(mod_iid)$fixed

mod_ar1 <- inla(Freq ~ -1 + f(age_group, model = "ar1") + f(obs_iid),
                family = "poisson",
                E = dat$N,
                dat = dat)

summary(mod_ar1)

clean_inla(mod_ar1)$random %>%
  filter(var == "age_group")

mod_ar1_int <- inla(Freq ~ -1 
                    + f(age_group, model = "ar1") 
                    + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1")) 
                    + f(obs_iid),
                    family = "poisson",
                    E = dat$N,
                    dat = dat,
                    control.compute = list(config = T))

## Horrendous uncertainty:
summary(mod_ar1_int)

clean_inla(mod_ar1_int)$random %>%
  filter(var == "age_group") %>%
  mutate(source = "AR1 and interaction") %>%
  bind_rows(
    clean_inla(mod_ar1)$random %>%
      filter(var == "age_group") %>%
      mutate(source = "AR1 only")
  ) %>%
  ggplot(aes(x=ID, y=mean, color = source)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge2(.2))

## No constraints on the interaction - can't identify main effect and interaction
mod_ar1_int$misc$configs$constr

mod_ar1_int_constr <- inla(Freq ~ -1 
                           + f(age_group, model = "ar1") 
                           + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1"), constr = T) 
                           + f(obs_iid),
                           family = "poisson",
                           E = dat$N,
                           dat = dat,
                           control.compute = list(config = T))

summary(mod_ar1_int_constr)

## 31 constraints
mod_ar1_int_constr$misc$configs$constr$nc
## .. that sum to:
mod_ar1_int_constr$misc$configs$constr$e
## So it's applying a sum to zero constraint across age in each year (which is what we want)

## Better but not great:
clean_inla(mod_ar1_int)$random %>%
  filter(var == "age_group") %>%
  mutate(source = "AR1 and interaction") %>%
  bind_rows(
    clean_inla(mod_ar1)$random %>%
      filter(var == "age_group") %>%
      mutate(source = "AR1 only"),
    clean_inla(mod_ar1_int_constr)$random %>%
      filter(var == "age_group") %>%
      mutate(source = "AR1 and interaction + constraint")
  ) %>%
  ggplot(aes(x=ID, y=mean, color = source)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge2(.2))

## Interaction is doing the right thing, just quite uncertain.
clean_inla(mod_ar1_int_constr)$random %>%
  filter(var == "id.age") %>%
  mutate(year = rep(1990:2020, each = 7)) %>%
  ggplot(aes(x=year, y=mean)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap(~ID)

pred_dat <- dat %>% distinct(year, age_group) %>%
  mutate(N = 1) %>%
  bind_rows(dat %>% select(year, age_group, Freq, N) %>% mutate(obs_iid = row_number())) %>%
  left_join(mf_model %>% select(year, age_group, id.year, id.age))

mod_ar1_int_constr <- inla(Freq ~ -1 
                           + f(age_group, model = "ar1") 
                           + f(id.age, model = "ar1", group = id.year, control.group = list(model = "ar1"), constr = T) 
                           + f(obs_iid),
                           family = "poisson",
                           E = pred_dat$N,
                           dat = pred_dat,
                           control.compute = list(config = T))

res <- sample_model(mod_ar1_int_constr, pred_dat, "Freq") %>%
  mutate(across(mean:upper, exp))

## But predictions look fine?
dat %>%
  ggplot(aes(x=age_group, y=Freq/N)) +
  geom_point(aes(color = survey_id, size = Freq)) +
  geom_line(aes(color = survey_id, group = survey_id)) +
  geom_line(data = res, aes(y=mean, group = year), color = "black", linewidth = 1.5) +
  geom_ribbon(data = res, aes(ymin = lower, ymax = upper, group = year), alpha = 0.3) +
  facet_wrap(~year)


sample_model <- function(inla_mod, inla_df, col) {
  df <- inla_df %>%
    filter(across(all_of(col), ~!is.na(.x)))
  samples <- inla.posterior.sample(1000, inla_mod)
  contents = inla_mod$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag==effect)
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
  ind.effect <- 1:(nrow(inla_df) - nrow(df))
  samples.effect = lapply(samples, function(x) x$latent[ind.effect])
  samples <- matrix(sapply(samples.effect, cbind), ncol=1000)
  mean <- rowMeans(samples)
  qtls <- apply(samples, 1, quantile, c(0.025, 0.5, 0.975))
  ident <- inla_df[ind.effect, ]
  samples <- ident %>% cbind(samples)
  art <- ident %>%
    mutate(
      mean = mean,
      lower = qtls[1,],
      upper = qtls[3,]
    )
}
