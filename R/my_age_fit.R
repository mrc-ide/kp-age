library(tidyverse)
library(sf)
library(dfertility)
library(multi.utils)
library(moz.utils)
library(countrycode)
library(Matrix)
library(TMB)

# geographies <- read_sf(moz.utils::national_areas()) %>%
#   st_make_valid() %>%
#   arrange(area_id) %>%
#   mutate(
#     id.iso3 = row_number()
#   )
# 
# nb <- INLA::inla.read.graph(moz.utils::national_adj())
# 
# adj <- spdep::nb2mat(nb, zero.policy = TRUE, style = "B")
# R_spatial <- INLA::inla.scale.model(diag(rowSums(adj)) - 0.99*adj,
#                                     constr = list(A = matrix(1,1, nrow(adj))), e = 0)
# 
# 
# nb <- geographies %>%
#   arrange(id.iso3) %>%
#   spdep::poly2nb() %>%
#   `names<-`(geographies$area_id)
# 
# nb <- lapply(nb, as.integer)
# class(nb) <- "nb"
# 
# adj <- spdep::nb2mat(nb, zero.policy=TRUE, style="B")
# R_spatial <- INLA::inla.scale.model(diag(rowSums(adj)) - 0.99*adj,
#                                     constr = list(A = matrix(1, 1, nrow(adj)), e = 0))

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

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/rds_agedata_1206.rds") %>%
  rename(age = category) %>%
  # separate_survey_id() %>%
  separate(survey_id, into = c("iso3", NA), sep = 3, remove = FALSE) %>% 
  separate(survey_id, into = c(NA, "kp"), sep = "_", remove = FALSE) %>% 
  filter(kp == "FSW",
         age %in% 15:49) %>%
  type.convert(as.is = T)

dat <- dat %>%
  select(iso3, survey_id, age, n)

dat <- dat %>%
  select(iso3, survey_id, age, n) %>%
  # dplyr::mutate(age_group_label = cut(age, c(0,
  #                                            seq(5, 85, 5) - 1), c(paste0(seq(0, 79, 5), "-", seq(5,
  #                                                                                                 80, 5) - 1), "80+"), include.lowest = TRUE)) %>%
  # dplyr::left_join(naomi::get_age_groups() %>%
  #                    select(age_group, age_group_label)) %>%
  # dplyr::select(-age_group_label) %>%
  
      # moz.utils::single_year_to_five_year() %>%
  # group_by(iso3, survey_id, age_group) %>%
  group_by(iso3, survey_id, age) %>% 
  summarise(n = sum(n))


                                                          ## `mf_model` --> indices we're going to predict at  - similar to blank rows in INLA - this is what we want to get out
mf_model <- crossing(
  # iso3 = ssa_iso3,
 #age_group = unique(dat$age_group)
                     age = 15:49
                     ) %>%
  # left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  ungroup() %>%
  mutate(id.age = # factor(to_int(age_group)),
           factor(to_int(age)),                          ##Everything needs to be a factor
         # id.iso3 = factor(to_int(iso3)),
         idx = factor(row_number()))


dat <- crossing(age = unique(dat$age),
                # age_group = unique(dat$age_group),
                survey_id = unique(dat$survey_id)) %>%
  left_join(dat %>% select(survey_id, age, n)) %>%
  select(survey_id, age, n) %>%
  arrange(survey_id, age)

dat[is.na(dat)] <- 0


dat <- dat %>%
  left_join(mf_model)


## not too sure I follow these matrices too well
M_obs <- sparse.model.matrix(~0 + idx, dat)             ##n observations long, n columns by age group
Z_age <- sparse.model.matrix(~0 + id.age, mf_model)     ##n observations long (this time from mf_model), n cols by age group    
# Z_spatial <- sparse.model.matrix(~0 + id.iso3, mf_model)

# x <- 0:34
# k <- seq(-15, 50, by = 5)
# spline_mat <- splines::splineDesign(k, x, ord = 4)
# spline_mat <- as(spline_mat, "sparseMatrix")
# Z_age <- Z_age %*% spline_mat

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
  R_age = dfertility::make_rw_structure_matrix(ncol(Z_age), 1, adjust_diagonal = TRUE)  ##captures relationship between age
  # Z_spatial = Z_spatial,
  # R_spatial = R_spatial
  
)

tmb_int$par <- list(
  beta_0 = 0,
  u_age = rep(0, ncol(Z_age)),                #Random effect for age
  log_prec_rw_age = 0                         #Manually declare all hyperparameters - here we have log precision for RW over age (standard deviation always fed in as log sigma (or in this case, log precision) so that when exponentiated they are always +ve)
  
  # lag_logit_phi_age = 0                     #Lag logit forces values between -1 and 1
  # u_spatial_str = rep(0, ncol(Z_spatial)),
  # log_prec_spatial = 0
)

tmb_int$random <- c(                          # PUt everything here except fixed effects (and no hyperparameters)
  "beta_0",
  "u_age"
  # "u_spatial_str"
)

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

int <- rowMeans(fit$sample$single_row_of_probs)

df <- data.frame(age = 15:49, p = int) %>%
  single_year_to_five_year()

df %>%
  group_by(age_group) %>%
  summarise(p = sum(p))
