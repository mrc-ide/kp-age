msm_inla_dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/msm_inla_dat_aug25.rds")

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