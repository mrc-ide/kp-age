library(tidyverse)
library(sf)
library(dfertility)
library(multi.utils)
library(moz.utils)
library(countrycode)
library(Matrix)
library(TMB)

single_year_to_five_year <- function (df, fifteen_to_49 = TRUE)
{
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


pop4 <- readRDS("~/Downloads/pop 3.rds") %>% 
  bind_rows() %>% 
  filter(
    # iso3 %in% c("MWI", "ZAF"),
    year %in% c(1995:2020)) 

pop4 <- pop4 %>% 
  filter(sex == "female",
         age_group %in% c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039", "Y040_044", "Y045_049")) %>% 
  group_by(iso3, area_id, year) %>% 
  mutate(local_tpa = population/sum(population)) %>% 
  ungroup()

kpdatiso3 <- unique(kpdat$iso3)

pop5 <- pop4 %>% 
  bind_rows(pop4 %>% filter(year == 1995) %>% mutate(year = 1993)) %>%
  bind_rows(pop4 %>% filter(year == 1995) %>% mutate(year = 1994)) %>%
  bind_rows(pop4 %>% filter(year == 2020) %>% mutate(year = 2021)) %>% 
  bind_rows(pop4 %>% filter(year == 2020) %>% mutate(year = 2022)) %>% 
  bind_rows(pop4 %>% filter(year == 2020) %>% mutate(year = 2023)) %>% 
  mutate(iso3 = ifelse(is.na(iso3), area_id , iso3)) %>% 
  filter(iso3 %in% kpdatiso3) %>% 
  distinct()





restricted_kpdat <- data.frame(rmultinom(n = 23, size = 1000, prob = c(0.12, 0.2, 0.25, 0.17, 0.12, 0.08, 0.06))) %>% 
  rownames_to_column() %>% 
  mutate(age_group = case_when(rowname == 1 ~ "Y015_019",
                               rowname == 2 ~ "Y020_024",
                               rowname == 3 ~ "Y025_029",
                               rowname == 4 ~ "Y030_034",
                               rowname == 5 ~ "Y035_039",
                               rowname == 6 ~ "Y040_044",
                               rowname == 7 ~ "Y045_049")) %>% 
  pivot_longer(cols = c(2:24), names_to = "survey_id", values_to = "fswcount") %>% arrange(survey_id, age_group) %>% 
  mutate(area_id = case_when(survey_id %in% c("X2", "X1", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10") ~ "ZAF",
                             TRUE ~ "BEN")) %>% 
  mutate(year = case_when(survey_id %in% c("X2", "X1", "X10") ~ 2000,
                          survey_id %in% c("X3", "X4") ~ 2003, 
                          survey_id %in% c("X5") ~ 2009,
                          survey_id %in% c("X6", "X11") ~ 2014,
                          survey_id %in% c("X7", "X12") ~ 2019,
                          survey_id %in% c("X8", "X9") ~2021,
                          survey_id %in% c("X13", "X14") ~2022,
                          survey_id %in% c("X15") ~ 1999,
                          survey_id %in% c("X16", "X17") ~1996,
                          survey_id %in% c("X18", "X19") ~2017,
                          survey_id %in% c("X20") ~2005,
                          survey_id %in% c("X21", "X22") ~2012,
                          TRUE ~ 1995)) %>% # not my finest I know
  mutate(area_name = area_id,
         iso3 = area_id)



dat <- crossing(age_group = unique(restricted_kpdat$age_group),
                select(restricted_kpdat, iso3, survey_id, area_name, area_id, year)) %>%
  arrange(survey_id) %>%
  left_join(restricted_kpdat)




mf_model <- crossing(distinct(dat, iso3, area_id),
                     year = 1993:2023,
                     age_group = unique(pop5$age_group)
) %>%
  left_join(distinct(dat, age_group)) %>% 
  left_join(pop5) %>%
  left_join(
    pop5 %>%
      filter(area_id == iso3) %>% 
      rename(nat_tpa = local_tpa) %>%
      select(iso3, year, age_group, nat_tpa)
  ) %>%
  mutate(id.age =  factor(to_int(age_group))) %>% 
  ungroup() %>% 
  mutate(idx = factor(row_number()),
         local_tpa = ifelse(is.na(local_tpa), nat_tpa, local_tpa),
         id.year = factor(year)
  )




dat2 <- dat %>%
  type.convert(as.is = T) %>%
  left_join(mf_model) %>% 
  rename(n = fswcount) %>%
  arrange(survey_id, area_name) %>% 
  mutate(area_name_matrix = paste0(survey_id, "_", area_name)) %>%
  ungroup()


dat2$n[is.na(dat2$n)] <- 0



# 
M_obs <- sparse.model.matrix(~0 + idx, dat2) 

X_stand_in <- sparse.model.matrix(~0 + id.age, mf_model)
X_stand_in <- X_stand_in[,c(1,2, 4:7)]

observed_x <- matrix(dat2$n, nrow = length(unique(dat2$area_name_matrix)), byrow = TRUE)


observed_totpop <- matrix(dat2$local_tpa, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
newoffset2 <- log(observed_totpop) - log(observed_totpop) #For running with no offset

Z_age <- sparse.model.matrix(~0 + id.age, mf_model)
Z_period <- sparse.model.matrix(~0 + id.year, mf_model)

Z_periodage <-  mgcv::tensor.prod.model.matrix(list(Z_period, Z_age))
dim(Z_periodage)
dim(X_stand_in)





tmb_int <- list()

tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  
  R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))-1), "dgTMatrix"), #when setting a base age gp
  # R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))), "dgTMatrix"),
  
  Z_periodage = Z_periodage,
  
  # Z_yeariso = Z_yeariso,
  # logit_totpop = newoffset2
  logit_totpop = as.vector(t(newoffset2)) ##use this one
)

tmb_int$par <- list( 
  beta_0 = rep(0, ncol(X_stand_in)),
  
  ## Intercept 
  lag_logit_phi_beta = 0, #--> for AR1
  log_sigma_rw_beta = 0,
  # interaction = rep(0, ncol(Z_yeariso))
  
  ### Adding time
  # logit_eta2_phi_age = 0,
  lag_logit_eta2_phi_age = 0,
  eta2 = array(0, c(ncol(Z_period), ncol(Z_age))),
  log_sigma_eta2 = 0,
  # logit_eta2_phi_period = 0
  lag_logit_eta2_phi_period = 0
  
  
)

tmb_int$random <- c(                        
  "beta_0",
  "eta2"
  
)






tmb_unload("flib_time_AR1")

lapply(list.files("src/", pattern = "\\.o|\\.so", full.names = T), file.remove)


TMB::compile("src/flib_time_AR1.cpp", flags = "-w")

dyn.load(dynlib("src/flib_time_AR1"))



f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "flib_time_AR1",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})


if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}



obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "flib_time_AR1",
                       # random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj)) 

fit$sdreport <- sdreport(fit$obj, fit$par) 
sd_report <- fit$sdreport
sd_report <- summary(sd_report, "all")
sd_report

estimates <- fit$obj$report()$p_norm ### point estimates --> sampling gives you uncertainty




tmb_int <- list()

tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  
  R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))-1), "dgTMatrix"), #when setting a base age gp
  # R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))), "dgTMatrix"),
  
  Z_periodage = Z_periodage,
  
  # Z_yeariso = Z_yeariso,
  # logit_totpop = newoffset2
  logit_totpop = as.vector(t(newoffset2)) ##use this one
)

tmb_int$par <- list( 
  beta_0 = rep(0, ncol(X_stand_in)),
  
  ## Intercept 
  lag_logit_phi_beta = 0, #--> for AR1
  log_sigma_rw_beta = 0,
  # interaction = rep(0, ncol(Z_yeariso))
  
  ### Adding time
  logit_eta2_phi_age = 0,
  # lag_logit_eta2_phi_age = 0,
  eta2 = array(0, c(ncol(Z_period), ncol(Z_age))),
  log_sigma_eta2 = 0,
  logit_eta2_phi_period = 0
  # lag_logit_eta2_phi_period = 0
  
  
)

tmb_int$random <- c(                        
  "beta_0",
  "eta2"
  
)