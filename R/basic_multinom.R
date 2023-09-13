# install.packages("nnet")
library(nnet)
library(tidyverse)

nga <- readRDS("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/NGA/NGA2020BBS_FSW/NGA2020BBS_FSW.rds")
nga <- nga %>% 
  mutate(age2 = age,
  age = as.factor(age))

model <- multinom(age ~ 1, data = nga)

plot(model$fitted.values[1,])

age_values <- as.factor(seq(from = min(nga$age2), to = max(nga$age2), by = 1))
predicted_probs <- predict(model, newdata = data.frame(age = age_values), type = "probs")
total_counts <- 4974 
expected_counts <- data.frame(predicted_probs * total_counts)

expected_counts <- expected_counts[1,] %>% 
  pivot_longer(cols = everything(), names_to = "age", values_to = "count") %>% 
  mutate(id.age = row_number())


nga2 <- nga %>% 
  group_by(age2) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  select(age2, count) %>% 
  distinct() %>% 
  arrange(age2) %>% 
  mutate(age.id = row_number()) %>% 
  select(age.id, count)

ggplot(expected_counts, aes(x = id.age, y = count)) +
  geom_line() +
  labs(x = "Age", y = "Number of People", color = "Age Group") +
  scale_color_discrete(name = "Age Group") +
  theme_minimal() + 
  geom_line(aes(x = nga2$age.id, y = nga2$count, color = "orange"))



# mnl_pred_ova(model = model,
#              data = ages_to_predict,
#              x = "age",
#              by = 1,
#              seed = "random", # default
#              nsim = 100, # faster
#              probs = c(0.025, 0.975))
# # Print the results
# print(results)


model <-VGAM::vglm(age ~ 1, family = multinomial, data = nga)
plot(model@fitted.values[1,])


# predict(model, newdata = data.frame(age = age_values), type = "probs")
# VGAM::predict.vglm(model, newdata = data.frame(age = agevalues), type = "response", se.fit = T)

prediction <- predictvglm(object = model, newdata = data.frame(age = age_values), 
             type =  "link", se.fit = T)

plot(prediction[1,])

full_dat
a <-  data.frame(prediction[["fitted.values"]][1,],  prediction[["se.fit"]][1,]) %>% 
  mutate(age.id = row_number()) %>% 
  rename(logestimate = prediction...fitted.values....1..., se = prediction...se.fit....1...) %>% 
  mutate(loglower = logestimate - 1.96*se,
         logupper = logestimate + 1.96*se, 
         lower = exp(loglower),
         upper = exp(logupper),
         estimate = exp(logestimate)) 

nga3 <- nga2 %>% 
  filter(!age.id == 48)

full_data %>% 
  ggplot() + 
  geom_line(aes(x = age.id, y = estimate)) +
  geom_ribbon(aes(x = age.id, ymin = lower, ymax = upper), alpha = 0.2) + 
  moz.utils::standard_theme() +
  geom_point(aes(x = nga3$age.id, y = nga3$count)) + 
  coord_cartesian(ylim = c(0, 1000))



basic_age <- readxl::read_xlsx("C:/Users/rla121/Downloads/basic_age_mod.xlsx") %>% 
  filter(!age_group == 6) %>% 
  mutate(iso = factor(multi.utils::to_int(country))) %>% 
  mutate(odds = prop/(1- prop)) %>% 
  group_by(country) %>% 
  mutate(odds_ratio = odds/odds[age_group == 1]) %>% 
  ungroup()

basic_age2 <- basic_age


new_agedata <- data.frame(
  age_group = rep(basic_age$age_group, basic_age$count),
  iso = rep(basic_age$iso, basic_age$count)
) %>% 
  left_join((basic_age %>% select(country, iso, year, old_age, age_group))) %>% 
  mutate(age_group = as.factor(age_group),
         iso = as.factor(iso))

model <-VGAM::vglm(age_group ~ 1, family = "multinomial", data = new_agedata)
plot(model@fitted.values[1,])

data.frame(model@fitted.values[1,]) %>% 
  rownames_to_column() %>% 
  mutate(age_group = factor(rowname),
         value = as.numeric(`model.fitted.values.1...`)) %>% 
  ggplot() + 
  geom_point(aes(x = age_group, y = value), shape = 4, size = 2) + 
  geom_point(aes(x = basic_age$age_group, y = basic_age$prop, color = basic_age$iso))

basic_age <- basic_age %>% 
  mutate(age_group = factor(age_group))

ggplot(
  geom_point(aes(model, y = model@fitted.values[1,]))
)

mf_model <- crossing(
  iso = c(1,2,3),
  age_group = c(1,2,3,4,5)
) %>% 
  mutate(iso = factor(iso),
         age_group = factor(age_group),
         idx = factor(row_number()))

new_agedata <- new_agedata %>% 
  left_join(mf_model)

basic_age <- basic_age %>% 
  mutate(idx = row_number())

M_obs <- sparse.model.matrix(~0 + idx, basic_age)
Z_age <- sparse.model.matrix(~0 + age_group, basic_age)
Z_survey <- sparse.model.matrix(~0 + iso, basic_age)


tmb_int <- list()

observed_x <- matrix(basic_age$count, nrow = length(unique(basic_age$iso)), byrow = TRUE)

tmb_int$data <- list(
  M_obs = M_obs,
  observed_x = observed_x,
  
  # Z_age = Z_age,
  
  Z_survey = Z_survey,
  R_survey = as(diag(1, nrow = length(unique(dat$survey_id))), "dgCMatrix")
  )

tmb_int$par <- list(
  beta_0 = 0,
  # u_age = rep(0, ncol(Z_age)),
  
  u_survey = rep(0, ncol(Z_survey)),
  log_prec_survey = 0
)

tmb_int$random <- c(                          # PUt everything here except hyperparamters
  "beta_0",
  "u_survey"
)

TMB::compile("src/tmb_sample.cpp", flags = "-w")
dyn.load(dynlib("src/tmb"))