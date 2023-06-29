library(RTMB)
library(tidyverse)

newmultinom2 <- function (x, size = NULL, prob, log = FALSE)
{
  print(x)
  K <- length(prob)
  if (length(x) != K)
    stop("x[] and prob[] must be equal length vectors.")
  s <- sum(prob)
  prob <- prob/s
  x <- as.integer(x + 0.5)
  if (any(x < 0))
    stop("'x' must be non-negative")
  N <-55                                                       ## Have just forced this to be the number of surveys (was formerly sum(x))
  if (is.null(size))
    size <- N
  else if (size != N)
    stop("size != n, i.e. one is wrong")
  r <- lgamma(size + 1) + sum(x * log(prob) - lgamma(x + 1))
  if (log)
    r
  else exp(r)
}


agedat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agemoddat_20_06.csv") %>% 
  separate(survey_id, into = c(NA, "kp"), remove = FALSE, sep = "_") %>% 
  filter(kp == "FSW") %>% 
  filter(!is.na(age),
         !age<15,
         !age>49) %>% 
  mutate(age = as.numeric(age),
         age_5yr = as.factor(naomi::cut_naomi_age_group(age = age))) %>% 
  group_by(survey_id, age_5yr) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  select(survey_id, age_5yr, x = total) %>% 
  mutate(id.survey = multi.utils::to_int(survey_id),
         id.age = multi.utils::to_int(age_5yr)) %>% 
  select(-survey_id, -age_5yr) %>% 
  distinct() %>% 
  group_by(id.survey) %>% 
  mutate(n = sum(x)) %>% 
  ungroup()



fullage_dat <- crossing(id.survey = seq(1,max(agedat$id.survey), 1), 
                        id.age = seq(1,max(agedat$id.age),1)) %>% 
  left_join(agedat) %>% 
  mutate(x = ifelse(is.na(x), 0, x)) %>% 
  group_by(id.survey) %>% 
  mutate(n = sum(x))

parameters <- list(
  p = rep(0.1428571, 7)
)



f <- function(parms) {
  getAll(parms, fullage_dat, warn=FALSE)                  
  
  
  nll <- 0
  
  
  for(i_curr in 1:55) {
    idx <- seq(1,385, 7)
    x_curr <- x[idx[i_curr]:c(idx[i_curr]+6)]
    nll <- nll - sum(newmultinom2(x =x_curr, size = 55, prob = p, log=TRUE))     ##again here I think n is the problem - just not 100% sure what it should be
  }
  
  ADREPORT(p)                                    
  ## Return
  nll
}


debugonce(f)
f(parameters)


obj <- MakeADFun(f, parameters)  

opt <- nlminb(obj$par, obj$fn, obj$gr)    

sdr <- sdreport(obj)
sdr

preddat1 <- data.frame(estimate = sdr["value"]) %>% 
  mutate(sum = sum(value),
         modprop = value/sum) %>% 
  rownames_to_column() %>% 
  select(id.age = rowname, everything()) %>% 
  mutate(id.age = as.factor(id.age)) %>% 
  arrange(id.age)

fullage_dat %>% 
  mutate(prop = x/n) %>% 
  mutate(idx = as.factor(idx),
         id.age = as.factor(id.age)
  ) %>% 
  ggplot() +
  geom_point(aes(x = id.age, y = prop, colour = idx), alpha = 0.5, size = 0.7) +
  geom_point(data = preddat1 , aes(x = id.age , y = modprop))





##### Ref random effects


agedat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agemoddat_20_06.csv") %>% 
  separate(survey_id, into = c(NA, "kp"), remove = FALSE, sep = "_") %>% 
  filter(kp == "FSW") %>% 
  filter(!is.na(age),
         !age<15,
         !age>49) %>% 
  mutate(age = as.numeric(age),
         age_5yr = as.factor(naomi::cut_naomi_age_group(age = age))) %>% 
  group_by(survey_id, age_5yr) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  select(survey_id, age_5yr, x = total) %>% 
  mutate(id.survey = multi.utils::to_int(survey_id),
         id.age = multi.utils::to_int(age_5yr)) %>% 
  select(-survey_id, -age_5yr) %>% 
  distinct() %>% 
  group_by(id.survey) %>% 
  mutate(n = sum(x)) %>% 
  ungroup()



fullage_dat <- crossing(id.survey = seq(1,max(agedat$id.survey), 1), 
                        id.age = seq(1,max(agedat$id.age),1)) %>% 
  left_join(agedat) %>% 
  mutate(x = ifelse(is.na(x), 0, x)) %>% 
  group_by(id.survey) %>% 
  mutate(n = sum(x))

parameters <- list(
  # p = rep(0.1428571, max(agedat$id.age)),
  
  ## Intercept
  b0 = 0,
  # mu_b0 = 0,
  
  ## Fixed effect for age
  age = runif(max(agedat$id.age), min = 0, max = 0.001),
  
  ## Survey iid
  u_survey = runif(max(agedat$id.survey), min = 0, max = 0.001),
  sd_survey = 1

)


f <- function(parms) {
  getAll(parms, fullage_dat, warn=FALSE)                  
  
  
  nll <- 0
  
  # Intercept prior [diffuse normal prior]
  nll <- nll - dnorm(b0, mean = 0, sd = 5, log = TRUE)
  
  # Fixed effect age prior [diffuse normal prior]
  nll <- nll - sum(dnorm(age, mean = 0, sd = 5, log = TRUE))
  
  # Prior for survey iid effects
  nll <- nll - sum(dnorm(u_survey, mean=0, sd=sd_survey, log=TRUE))
  
  #' x ~ Multinom(n, p)
  #' p = b0 + u_survey
  #' 

  logit_p <- b0 + age[id.age] + u_survey[id.survey]
  
  p <- plogis(logit_p)
  
  for(i_curr in 1:55) {
    idx <- seq(1,385, 7)
    x_curr <- x[idx[i_curr]:c(idx[i_curr]+6)]
    p_curr <- p[idx[i_curr]:c(idx[i_curr]+6)]
    nll <- nll - sum(newmultinom2(x =x_curr, prob = p_curr, log=TRUE))
  }
  
  ADREPORT(p)
  # ADREPORT(u_survey)
  ## Return
  nll
}

debugonce(f)
f(parameters)

# debug(newmultinom)
obj <- MakeADFun(f, parameters, random = c("b0", "age", "u_survey"))

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
fit$sdreport <- sdreport(fit$obj, fit$par)

sd_report <- fit$sdreport
sd_report <- summary(sd_report, "all")
sd_report

# class(fit) <- "naomi_fit"  # this is hacky...
# fit <- naomi::sample_tmb(fit, random_only=TRUE)

fullage_dat %>%
  cbind(estimate = data.frame(sd_report) %>%
          rownames_to_column() %>%
          filter(str_detect(rowname, "p")) %>%
          pull(Estimate)) %>%
  group_by(id.survey) %>%
  mutate(estimate = estimate/sum(estimate),
         norm_x = x/sum(x)) %>%
  ggplot(aes(x=id.age)) +
    geom_point(aes(y=norm_x)) +
    geom_line(aes(y=estimate)) +
    facet_wrap(~id.survey)

preddat <- data.frame(estimate = sdr["value"]) %>% 
  mutate(sum = sum(value),
         modprop = value/sum) %>% 
  rownames_to_column() %>% 
  select(id.age = rowname, everything()) %>% 
  mutate(id.age = as.factor(id.age)) %>% 
  arrange(id.age)

fullage_dat %>% 
  mutate(prop = x/n) %>% 
  mutate(idx = as.factor(idx),
         id.age = as.factor(id.age)
  ) %>% 
  ggplot() +
  geom_point(aes(x = id.age, y = prop, colour = idx), alpha = 0.5, size = 0.7) +
  geom_point(data = preddat , aes(x = id.age , y = modprop))
