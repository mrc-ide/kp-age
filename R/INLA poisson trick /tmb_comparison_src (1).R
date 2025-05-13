library(tidyverse)
library(TMB)

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

survs <-  read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agemoddat_20_06.csv") %>% 
  moz.utils::separate_survey_id() %>% 
  select(survey_id, year, age, n) %>% 
  single_year_to_five_year() %>% 
  group_by(survey_id, year, age_group) %>% 
  summarise(n = sum(n))

mf <- crossing(survey_id = survs$survey_id,
                       age_group = unique(survs$age_group)) %>% 
  moz.utils::separate_survey_id()

# Data for offsets //at the moment just using Malawi for everything as no country-specific effect in the model.
age_groups <- naomi::get_age_groups()
pop <- readRDS("~/Downloads/pop 2.rds")$MWI
f24 <- filter(age_groups, age_group_sort_order %in% 18:24)$age_group

pop <- crossing(year = 1990:2024,
                age_group = f24) %>%
  left_join(pop %>% filter(sex == "female",
         age_group %in% f24,
         area_id == "MWI")) %>%
  arrange(iso3, age_group, year) %>%
  group_by(age_group) %>%
  fill(population, .direction = "updown") %>%
  ungroup() %>% 
  select(year, age_group, population)

genpop_offset <- pop %>%
  group_by(year) %>%
  mutate(population_prop = population/sum(population))


survs <- mf %>% 
  left_join(survs) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  rename(Freq = n) %>% 
  left_join(genpop_offset)

survs %>%
  ggplot(aes(x=age_group, group = survey_id, y=Freq, color = factor(year))) +
    geom_line()


mf_model <- crossing(year = 1990:2023,
                     age_group = unique(survs$age_group)) %>%
  mutate(idx = factor(row_number()),
         id.age = factor(as.numeric(factor(age_group))),
         id.year = factor(year - min(year) +1)) %>%
  mutate(
         id.age.year = factor(group_indices(., id.year, id.age))
         )

dat <- survs %>%
  left_join(mf_model %>% select(year, age_group, idx))

M_obs <- sparse.model.matrix(~0 + idx, dat) 
dim(M_obs)

X_stand_in <- sparse.model.matrix(~0 + id.age, mf_model)
## To use 15-19 as reference category:
# X_stand_in <- X_stand_in[,c(2:7)]

observed_x <- matrix(dat$Freq, ncol = 7, byrow = TRUE)

observed_totpop <- matrix(dat$population_prop, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
newoffset2 <- log(observed_totpop)

Z_age <- sparse.model.matrix(~0 + id.age, mf_model)
Z_period <- sparse.model.matrix(~0 + id.year, mf_model)

## If time splines wanted
# x <- 0:30
# k <- seq(-15, 50, by = 5)
# spline_mat <- splines::splineDesign(k, x, ord = 4)
# spline_mat <- as(spline_mat, "sparseMatrix")
# Z_period <- Z_period %*% spline_mat

Z_periodage <-  mgcv::tensor.prod.model.matrix(list(Z_period, Z_age))
# Z_periodage <-  sparse.model.matrix(~0 + id.age.year, mf_model)
dim(Z_periodage)
dim(X_stand_in)

tmb_int <- list()

tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  
  # R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))-1), "dgTMatrix"), #when setting a base age gp
  # R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))), "dgTMatrix"),
  R_beta = dfertility::make_rw_structure_matrix(ncol(X_stand_in), 1, adjust_diagonal = T),
  
  Z_periodage = Z_periodage,
  
  R_age = as(diag(1, nrow = ncol(Z_age)), "sparseMatrix"),
  R_period = as(diag(1, nrow = ncol(Z_period)), "sparseMatrix"),
  
  # Z_yeariso = Z_yeariso,
  # logit_totpop = newoffset2
  logit_totpop = as.vector(t(newoffset2)) ##use this one
)

tmb_int$par <- list( 
  beta_0 = rep(0, ncol(X_stand_in)),
  
  ## Intercept 
  lag_logit_phi_beta = 0, #--> for AR1
  log_sigma_rw_beta = 0,
  
  ### Adding time
  eta2 = array(0, c(ncol(Z_age), ncol(Z_period))),
  log_sigma_eta2 = 0,
  lag_logit_eta2_phi_age = 0,
  lag_logit_eta2_phi_period = 0

  
)

tmb_int$random <- c(                        
  "beta_0",
  "eta2"
  
)

tmb_unload <- function(name) {
  ldll <- getLoadedDLLs()
  idx  <- grep(name, names(ldll))
  for (i in seq_along(idx))
    dyn.unload(unlist(ldll[[idx[i]]])$path)
  cat('Unload ', length(idx), "loaded versions.\n")
}

tmb_unload("tmb_comparison")
lapply(list.files("~/Downloads", pattern = "\\.o|\\.so", full.names = T), file.remove)

TMB::compile("~/Downloads/tmb_comparison.cpp", flags = "-w")

dyn.load(dynlib("~/Downloads/tmb_comparison"))



f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "tmb_comparison",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})


if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}



obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "tmb_comparison",
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



