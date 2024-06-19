library(nnet)
library(tidyverse)
library(Matrix)
library(TMB)

basic_age <- read_csv("~Downloads/basic_age_mod.xlsx") %>% 
  filter(!age_group == 6) %>% 
  group_by(survey_id) %>% 
  mutate(prop = count/sum(count),
         odds = prop/prop[age_group == 1],
         odds_ratio = odds/odds[age_group == 1])


addis <- basic_age %>% 
  filter(area == "Addis Ababa")


#### TMB Model ####

counts <- addis %>% 
  select(age_group, survey_id, age_group, count, year) %>% 
  pivot_wider(names_from = age_group, values_from = count, names_prefix = "age") 

counts 

model5 <- multinom(cbind(age1, age2, age3, age4, age5) ~ 1 , data = counts)
summary(model5)
nnet_coef <- data.frame(exp(coef(model5)),
                        age_cat = c(2,3,4,5)) %>% 
  rename(odds_ratio = X.Intercept.) %>% 
  left_join(
data.frame(confint(model5)) %>% 
  pivot_longer(cols = everything(), names_to = "age", values_to = "CI") %>% 
  separate(col = age, into = c("interval", "age_cat"), sep = "age", remove = T) %>% 
  type.convert(as.is = T) %>% 
  pivot_wider(names_from = "interval", values_from = "CI") %>% 
  rename(lowerCI = "X2.5...", upperCI = "X97.5...")
) %>% 
  select(age_cat, everything()) %>% 
  rename(age_group = age_cat) %>% 
  mutate(source = "nnet") %>% 
  mutate(lowerCI = exp(lowerCI),
         upperCI = exp(upperCI))

### TMB model ####

addis <- addis %>% 
  mutate(idx = factor(row_number()),
         age_group = factor(age_group),
         year = factor(year))

M_obs <- sparse.model.matrix(~0 + idx, addis)

X_stand_in <- sparse.model.matrix(~0 + age_group, addis)

X_stand_in[,1] <- 0

# X_stand_in <- X_stand_in[,c(2:5)] #use to remove first age_group completely

tmb_int <- list()

observed_x <- matrix(addis$count, nrow = length(unique(addis$survey_id)), byrow = TRUE)

tmb_int$data <- list(
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in
  
)

tmb_int$par <- list(
  beta_0 = rep(0, ncol(X_stand_in))
)

tmb_int$random <- c(                          
)



TMB::compile("~Downloads/tmb_sample.cpp", flags = "-w")
dyn.load(dynlib("~Downloads/tmb_sample"))

f <- TMB::MakeADFun(data = tmb_int$data,
                    parameters = tmb_int$par,
                    DLL = "tmb_sample",
                    silent=0,
                    checkParameterOrder=FALSE)



if(is.null((f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}




obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "tmb_sample",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par


fit <- c(f, obj = list(obj))

fit$sdreport <- sdreport(fit$obj, fit$par)
sd_report <- fit$sdreport
sd_report <- summary(sd_report, "all")
sd_report

# c(0.528093, -0.3171204, -1.740898, -1.870755) %>% exp
# c(0.528093, -0.3171204, -1.740898, -1.870755) %>% plogis


tmb_coefs <- data.frame(sd_report,
           age_group = seq(2,5, 1)) %>% 
  mutate(lowerCI = plogis(`Estimate` - 1.96 * `Std..Error`),
         upperCI = plogis(`Estimate` + 1.96 * `Std..Error`),
         odds = plogis(Estimate),
         odds_ratio = odds/odds[age_group == 1]) %>% 
  select(age_group, odds, odds_ratio, lowerCI, upperCI) %>% 
  mutate(source = "TMB")

### Comparison 
png("multinom plots/one study.png", width = 600, height = 400)
addis %>% 
  select(age_group, odds, odds_ratio) %>% 
  mutate(source = "HandCalc") %>% 
  type.convert(as.is = T) %>% 
  bind_rows(tmb_coefs) %>%
  bind_rows(nnet_coef) %>% 
  ggplot(aes(x = age_group)) +
  geom_point(aes(y = odds_ratio, color = source), position = position_jitter(w = 0.3, h = 0)) + 
  # geom_pointrange(aes(y = odds_ratio, ymin = lowerCI, ymax = upperCI, color = source), linewidth =0.4, alpha = 0.8, position = position_jitter(w = 0.4, h = 0)) + 
  moz.utils::standard_theme() +
  labs(x = "Age Group", y = "OR")
dev.off()

# They match quite well where we have area set to Addis Ababa, however weird things happen if you filter basic_age down to area ==  "Adama"
