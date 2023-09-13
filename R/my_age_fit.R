library(tidyverse)
library(sf)
library(dfertility)
library(multi.utils)
library(moz.utils)
library(countrycode)
library(Matrix)
library(TMB)

iso3 = moz.utils::ssa_iso3()

geographies <- read_sf(moz.utils::national_areas()) %>%
  
  st_make_valid() %>%
  arrange(area_id) %>%
  mutate(
    id.iso3 = row_number()
  )

# nb <- INLA::inla.read.graph(moz.utils::national_adj())

# nb <- geographies %>% 
#   arrange(id.iso3) %>% 
#   spdep::poly2nb() %>% 
#   `names<-`(geographies$area_id)
# 
# nb <- lapply(nb, as.integer)
# class(nb) <-  "nb"
# 
# adj <- spdep::nb2mat(nb, zero.policy = TRUE, style = "B")
# R_spatial <- INLA::inla.scale.model(diag(rowSums(adj)) - 0.99*adj,
#                                     constr = list(A = matrix(1,1, nrow(adj))), e = 0)




nb <- geographies %>%
  arrange(id.iso3) %>%
  spdep::poly2nb() %>%
  `names<-`(geographies$area_id)

nb <- lapply(nb, as.integer)
class(nb) <- "nb"

adj <- spdep::nb2mat(nb, zero.policy=TRUE, style="B")
R_spatial <- INLA::inla.scale.model(diag(rowSums(adj)) - 0.99*adj,
                                    constr = list(A = matrix(1, 1, nrow(adj)), e = 0))

separate_survey_id <- function(df, kp = T) {
  
  if(kp) {
    df %>%
      tidyr::separate(survey_id, into = c("iso3", "year", NA), sep = c(3, 7), remove = F, convert = T) %>%
      tidyr::separate(survey_id, into = c(NA, "kp"), sep = "_", remove = F)
  } else {
    df %>%
      tidyr::separate(survey_id, into = c("iso3", "year", NA), sep = c(3, 7), remove = F, convert = T)
  }
  
}

# dat <- readRDS("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/rds_agedata_1206.rds") %>%
dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agemoddat_20_06.csv", show_col_types = F)  %>% 
  group_by(survey_id) %>% 
  mutate(fullsum = sum(n),
         n = estimate*fullsum) %>%
  ungroup() %>% 
  select(-fullsum) %>% 
# rename(age = category) %>%
  # separate_survey_id() %>%
  # separate(survey_id, into = c("iso3", NA), sep = 3, remove = FALSE) %>% 
  # separate(survey_id, into = c(NA, "kp"), sep = "_", remove = FALSE) %>% 
  moz.utils::separate_survey_id() %>% 
  filter(kp == "FSW",
         age %in% 15:49) %>%
  type.convert(as.is = T) %>% 
  filter(!is.na(age))

dat <- dat %>%
  select(iso3, survey_id, year, age, n, method) %>%
  # dplyr::mutate(age_group_label = cut(age, c(0,
  #                                            seq(5, 85, 5) - 1), c(paste0(seq(0, 79, 5), "-", seq(5,
  #                                                                                                 80, 5) - 1), "80+"), include.lowest = TRUE)) %>%
  # dplyr::left_join(naomi::get_age_groups() %>%
  #                    select(age_group, age_group_label)) %>%
  # dplyr::select(-age_group_label) %>%
  
      # moz.utils::single_year_to_five_year() %>%
  # group_by(iso3, survey_id, age_group) %>%
  group_by(iso3, survey_id, age, method, year) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(id.method = ifelse(method == "convenience", 1, 0))


                                                          ## `mf_model` --> indices we're going to predict at  - similar to blank rows in INLA - this is what we want to get out
mf_model <- crossing(
  iso3 = iso3,
  year = seq(1990,2023, 1),
 #age_group = unique(dat$age_group)
                     age = 15:49
                     ) %>%
  # left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  ungroup() %>%
  mutate(id.age = # factor(to_int(age_group)),
           factor(to_int(age)),                          ##Everything needs to be a factor
         id.iso3 = factor(to_int(iso3)),
         idx = factor(row_number()),
         id.year = as.numeric(factor(year))-1)


dat <- crossing(age = 15:49,
                select(dat, iso3, survey_id, id.method, year)) %>%
  left_join(dat %>% select(survey_id, iso3, age, n, id.method, year)) %>%
  select(iso3, survey_id, age, n, id.method, year) %>%
  arrange(survey_id, age)

dat[is.na(dat)] <- 0


dat <- dat %>%
  left_join(mf_model)


## not too sure I follow these matrices too well
M_obs <- sparse.model.matrix(~0 + idx, dat)             ##n observations long, n columns by age group
Z_spatial <- sparse.model.matrix(~0 + id.iso3, mf_model)

Z_age <- sparse.model.matrix(~0 + id.age, mf_model)     ##n observations long (this time from mf_model), n cols by age group
x <- 0:34
k <- seq(-15, 50, by = 5)
spline_mat <- splines::splineDesign(k, x, ord = 4)
spline_mat <- as(spline_mat, "sparseMatrix")

Z_age <- Z_age %*% spline_mat

Z_survey <- sparse.model.matrix(~0 + survey_id, dat)

X_method <- model.matrix(~0 + id.method, dat)

X_period <- model.matrix(~0 + id.year, mf_model)

Z_interaction3 <-  mgcv::tensor.prod.model.matrix(list(Z_spatial, Z_age))

tmb_int <- list()

# norm_n <- tst %>%
#   group_by(survey_id) %>%
#   # mutate(norm_n = n/sum(n)) %>%
#   pull(norm_n)

observed_x <- matrix(dat$n, nrow = length(unique(dat$survey_id)), byrow = TRUE)    ## Puts n in a matrix - rows for surv id, cols for age group idx


#Everything goes into `data`
tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  
  Z_age = Z_age,
  R_age = dfertility::make_rw_structure_matrix(ncol(Z_age), 1, adjust_diagonal = TRUE),  ##captures relationship between age
  
  Z_spatial = Z_spatial,
  R_spatial = R_spatial,
  
  Z_survey = Z_survey,
  R_survey = as(diag(1, nrow = length(unique(dat$survey_id))), "dgCMatrix"),
  
  X_method = X_method,
  
  X_period = X_period,
  
  Z_interaction3 = Z_interaction3
 
)

tmb_int$par <- list(
  beta_0 = 0,
  u_age = rep(0, ncol(Z_age)),                #Random effect for age
  log_prec_rw_age = 0  ,                     #Manually declare all hyperparameters - here we have log precision for RW over age (standard deviation always fed in as log sigma (or in this case, log precision) so that when exponentiated they are always +ve)
  
  # lag_logit_phi_age = 0                     #Lag logit forces values between -1 and 1
  u_spatial_str = rep(0, ncol(Z_spatial)),
  log_prec_spatial = 0,
  
  u_survey = rep(0, ncol(Z_survey)),
  log_prec_survey = 0,
  
  beta_method = rep(0, ncol(X_method)),
  # log_prec_method = 0
  
  beta_period = rep(0, ncol(X_period)),
  
  eta3 = array(0, c(ncol(Z_spatial), ncol(Z_age))),
  log_prec_eta3 = 0, 
  # logit_eta3_phi_age = 0
  lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c(                          # PUt everything here except hyperparamters
  "beta_0",
  "u_age",
  "u_spatial_str",
  "u_survey",
  "beta_method",
  "beta_period",
  "eta3"
)



library(tidyverse)
library(sf)
library(dfertility)
library(multi.utils)
library(moz.utils)
library(countrycode)
library(Matrix)
library(TMB)

file.remove("src/tmb.dll")
file.remove("src/tmb.o")

# setwd("C:/Users/rla121/Dropbox/KP/Individual KP/Code/KP Code/kp-age/")
TMB::compile("src/tmb.cpp", flags = "-w")
dyn.load(dynlib("src/tmb"))

# gdbsource("src/tmb.cpp")
# 
# f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
#                                           parameters = tmb_int$par,
#                                           DLL = "tmb",
#                                           silent=0,
#                                           checkParameterOrder=FALSE)
# })
# 
# if(is.null(parallel::mccollect(f)[[1]])) {
#   stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
# }


# 
# ### Below produces error: "Error in unserialize(socklist[[n]]) : error reading from connection
# 
# cl <- makeCluster(4)  # Adjust the number of cores as per your system
# registerDoParallel(cl)
# 
# f <- (foreach(i = 1:4) %dopar% {
#   dyn.load(TMB::dynlib("src/tmb"))
# 
#   TMB::MakeADFun(data = tmb_int$data,
#                                           parameters = tmb_int$par,
#                                           DLL = "tmb",
#                                           silent=0,
#                                           checkParameterOrder=FALSE)
# })
# 
# results <- foreach(i = 1:length(f), .export = c("f")) %dopar% {
#   f[[i]]
# }
# 
# # Stop the cluster
# stopCluster(cl)
# registerDoSEQ()  # Set parallel backend to sequential (if needed)
# 
# # Check if the TMB model is valid
# if (is.null(results[[1]])) {
#   stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
# }




## Deparallelised version :( 


# debugonce(TMB::MakeADFun)
f <- TMB::MakeADFun(data = tmb_int$data,
                    parameters = tmb_int$par,
                    DLL = "tmb",
                    silent=0,
                    checkParameterOrder=FALSE)


if(is.null((f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}





obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "tmb",
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

# sd_report2 <- data.frame(sd_report) %>% 
#   filter(Estimate < 1) %>% 
#   mutate(estimate2 = plogis(Estimate),
#          sum = sum(Estimate),
#          norm = estimate2/sum)

class(fit) <- "naomi_fit"
# debugonce(naomi::sample_tmb)
fit <- naomi::sample_tmb(fit, random_only=TRUE)
int <- apply(fit$sample$logit_p, 1, quantile, c(0.025, 0.975))

estimated_mf <- mf_model %>%
  mutate(lower = int[1,],
         mean = rowMeans(fit$sample$logit_p),
         upper = int[2,])

# int <- rowMeans(fit$sample$single_row_of_probs)
# 
# int <- rowMeans(fit$sample$p_norm)
# age <- rep(c(15:49), 14)
# surveys <- rep(1:14, each = 35)
# id.iso3 <- rep(1:39, each = 39)
# 
# ## with iso3
# 
# age <- rep(c(15:49), 546)
# surveys <- rep(1:14, each = 1365)
# id.iso3 <- rep(1:39, each = 490)
# id.iso3 <- rep(c(1:39), 490)
# 
# modelled_outcomes <- data.frame(id.iso3, age, surveys, int) %>% mutate(id.iso3 = as.factor(id.iso3))
# 
# dat2 <- dat %>% 
#   left_join(modelled_outcomes)
# 
# modelled_outcomes <- data.frame(int, age, surveys)



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

# df <- data.frame(age = 15:49, p = int) %>%
# single_year_to_five_year()
# 
# df %>%
#   group_by(age_group) %>%
#   summarise(p = sum(p))
# 
# dat <- dat %>% 
#   mutate(surveys = multi.utils::to_int(survey_id))
# 
# modelled_outcomes_to_plot <- modelled_outcomes %>% 
#   left_join(dat) %>% 
#   select(-id.age, -idx) %>% 
#   group_by(surveys) %>% 
#   mutate(observed_dist = n/sum(n)) %>% 
#   ungroup()
# 
# modelled_outcomes_to_plot %>% 
#   ggplot(aes(x = age)) + 
#   geom_line(aes(y = observed_dist), color = "black") + 
#   geom_line(aes(y = int, color = survey_id)) +
#   facet_wrap(~survey_id)
# 
# mf_model %>%
#   cbind(pred = int) %>%
#   group_by(iso3) %>%
#   mutate(natural_pred = plogis(pred)/sum(plogis(pred)))

dat %>%
  group_by(iso3, survey_id) %>%
  mutate(p = n/sum(n)) %>%
  left_join(estimated_mf %>%
              group_by(iso3, year) %>%
              mutate(across(lower:upper, ~plogis(.x)/sum(plogis(.x))))
            )%>%
  ggplot(aes(x=age)) +
    geom_line(aes(y=mean), size = 1, color = "black") +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y=p, color= survey_id), show.legend = F) +
    facet_wrap(~iso3) +
  moz.utils::standard_theme()
  
