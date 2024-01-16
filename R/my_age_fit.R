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


dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fullweightedandunweighteddata_0210.rds") %>%
  filter(vars == "age") %>%
  mutate(method = ifelse(is.na(lower), "convenience", "RDS")) %>%
  rename(age = category) %>%
  moz.utils::separate_survey_id() %>%
  filter(kp == "FSW",
         age %in% 15:49) %>%
  type.convert(as.is = T) %>%
  filter(!is.na(age)) %>%
  select(iso3, survey_id, year, age, n)
# group_by(kp, iso3, year, survey_id) %>%
# # mutate(estimate = estimate/sum(estimate))  %>% ## What is this doing? sum(estimate) here should be 1. In fact: what is the estimate column - on second look it's not the age%!
# ungroup()

dat <- dat %>%
  single_year_to_five_year(T) %>%
  group_by(iso3, survey_id, year, age_group) %>%
  summarise(n = sum(n))

## What is this bit doing?
# dat <- dat %>%
#   select(iso3, survey_id, year, age, estimate, method) %>%
#   group_by(iso3, survey_id, age, method, year) %>%
#   summarise(estimate = sum(estimate)) %>%
#   ungroup() %>%
#   mutate(id.method = ifelse(method == "convenience", 1, 0))


## What is this bit doing?
# dat <- dat %>%
#   type.convert(as.is = T) %>%
#   filter(!is.na(age)) %>%
#   mutate(age_group = naomi::cut_naomi_age_group(age = age),
#          id.age =  factor(to_int(age_group))) %>%
#   group_by(age_group, survey_id, iso3, year, id.method, id.age) %>%
#   summarise(estimate = sum(estimate)) %>%
#   ungroup()


## Can you check this maths against my maths? We end up with different numbers for TPa when calculating the % of 15-49
# spectrum_data_grouped <- spectrum_data %>% filter(!age<15, !age>49) %>%
#   group_by(year, iso3) %>%
#   mutate(age_group = naomi::cut_naomi_age_group(age))  %>%
#   ungroup() %>%
#   group_by(year, iso3, age_group) %>%
#   mutate(tpa = sum(tpa)) %>%
#   ungroup() %>%
#   select(-age, -totpop) %>%
#   distinct() %>%
#   mutate(id.age =  (to_int(age_group)))

spectrum_data_f <- readRDS("~/Downloads/spectrum_data_f.rds")
# spec_paths <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2023 final shared/SSA", full.names = T)
# 
# spectrum_data <- lapply(spec_paths, naomi::extract_pjnz_naomi)
# 
# nga_spectrum_dat <- naomi::extract_pjnz_naomi("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2022 final shared/EPP-Gen/Nigeria_National_2022_06_07.PJNZ")
# 
# spectrum_data_f <- spectrum_data %>%
#   bind_rows() %>%
#   bind_rows(nga_spectrum_dat) %>%
#   filter(age %in% 15:49,
#          year > 1992,
#          sex == "female") %>%
#   single_year_to_five_year(T) %>%
#   group_by(iso3, sex, year, age_group) %>%
#   summarise(totpop = sum(totpop)) %>%
#   group_by(iso3, sex, year) %>%
#   mutate(tpa = totpop/sum(totpop)) %>%
#   select(-totpop)

# saveRDS(spectrum_data_f, "~/Downloads/spectrum_data_f.rds")

# spectrum_data_m <- spectrum_data %>%
#   bind_rows() %>%
#   bind_rows(nga_spectrum_dat) %>%
#   filter(age %in% 15:49,
#          year > 1992,
#          sex == "male") %>%
#   single_year_to_five_year(T) %>%
#   group_by(iso3, sex, year, age_group) %>%
#   summarise(totpop = sum(totpop)) %>%
#   group_by(iso3, sex, year) %>%
#   mutate(tpa = totpop/sum(totpop)) %>%
#   select(-totpop)
# spectrum_data_grouped <- readRDS("~/Downloads/specgrouped.rds")

mf_model <- spectrum_data_f %>%
  ungroup() %>%
  mutate(id.age =  factor(to_int(age_group)),
         # id.iso3 = factor(as.numeric(factor(iso3))),            
         idx = factor(row_number()),
         id.year = as.numeric(factor(year))-1) %>% 
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3 = area_id, everything()) %>% mutate(id.iso3 = factor(row_number()))) %>% 
  # filter(!iso3== "NGA") %>% 
  mutate(is15 = ifelse(age_group == "Y015_019", 1, 0))

## What is this for?
# dat <- crossing(id.age = 1:7,
#                 select(dat, iso3, survey_id, id.method, year)) %>%
#   left_join(spectrum_data_grouped) %>% 
#   left_join(dat %>% select(survey_id, iso3, id.age, estimate, id.method, year) %>% type.convert(as.is = T)) %>%
#   select(iso3, survey_id, id.age, estimate, id.method, year, tpa) %>%
#   arrange(survey_id, id.age) %>% 
#   mutate(id.age = factor(id.age))

dat <- dat %>%
  left_join(mf_model)

crossing(age_group = dat$age_group,
         select(dat, iso3, survey_id, year)) %>%
  arrange(survey_id) %>%
  left_join(dat) %>%
  filter(is.na(n))

## Please check the ZAF1998ACA_FSW recoding: apparently it has 1 FSW in it who is age 20... Dropping that survey for now
dat <- crossing(age_group = dat$age_group,
                select(dat, iso3, survey_id, year)) %>%
  arrange(survey_id) %>%
  # filter(!survey_id %in% c("ZAF1998ACA_FSW")) %>%
  filter(!survey_id %in% c("ZAF1998ACA_FSW", "ETH2020ACA_FSW")) %>%
  left_join(dat)

# dat <- dat %>% 
#   filter(!iso3 == "NGA")

dat$n[is.na(dat$n)] <- 0

M_obs <- sparse.model.matrix(~0 + idx, dat)   
Z_spatial <- sparse.model.matrix(~0 + id.iso3, mf_model)


X_stand_in <- sparse.model.matrix(~0 + age_group, mf_model)
# X_stand_in <- X_stand_in[,c(2:7)] 

Z_age <- sparse.model.matrix(~0 + age_group, mf_model)

Z_spaceage <-  mgcv::tensor.prod.model.matrix(list(Z_spatial, Z_age))


mf_model <- mf_model %>% 
  mutate(year = factor(year))

Z_period <- sparse.model.matrix(~0 + year, mf_model)

Z_periodage <-  mgcv::tensor.prod.model.matrix(list(Z_period, Z_age))
dim(Z_periodage)
dim(X_stand_in)
# Dimensions here are causing crash (I think) - but unsure on the fix



observed_x <- matrix(dat$n, nrow = length(unique(dat$survey_id)), byrow = TRUE)

observed_totpop <- matrix(mf_model$tpa, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
logit_totpop <- qlogis(observed_totpop)

tmb_int <- list()

tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  R_beta = dfertility::make_rw_structure_matrix(ncol(X_stand_in), 1, adjust_diagonal = TRUE),  ##captures relationship between age
  
  
  logit_totpop = logit_totpop,
  
  R_survey = as(diag(1, nrow = length(unique(dat$survey_id))), "dgCMatrix"),
  Z_spatial = Z_spatial,
  R_spatial = as(diag(1, nrow = length(unique(mf_model$id.iso3))), "dgCMatrix"),
  Z_spaceage = Z_spaceage,
  Z_period = Z_period,
  R_period = dfertility::make_rw_structure_matrix(ncol(Z_period), 1, adjust_diagonal = TRUE),
  Z_periodage = Z_periodage,
  R_spatial2 = dfertility::make_adjacency_matrix(read_sf(moz.utils::national_areas()) %>% mutate(iso3 = area_id) %>% st_make_valid(), 0)
)


tmb_int$par <- list( 
  beta_0 = rep(0, ncol(X_stand_in)),
  log_prec_rw_beta = 0,
  lag_logit_phi_beta = 0,
  eta3 = array(0, c(ncol(Z_spatial), ncol(Z_age))),
  log_prec_eta3 = 0,
  logit_eta3_phi_age = 0,
  logit_eta3_phi_age = 0,
  logit_eta2_phi_age = 0,
  eta2 = array(0, c(ncol(Z_period), ncol(Z_age))),
  log_prec_eta2 = 0,
  logit_eta2_phi_period = 0
  
)

tmb_int$random <- c(                          
  "beta_0",
  "eta3",
  "eta2"
  
)



tmb_unload <- function(name) {   
  ldll <- getLoadedDLLs()   
  idx  <- grep(name, names(ldll))   
  for (i in seq_along(idx)) 
    dyn.unload(unlist(ldll[[idx[i]]])$path)
  cat('Unload ', length(idx), "loaded versions.\n") 
}

tmb_unload("tmb")

TMB::compile("src/tmb.cpp", flags = "-w")

dyn.load(dynlib("src/tmb"))


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "tmb",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})


if(is.null(parallel::mccollect(f)[[1]])) {
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

class(fit) <- "naomi_fit"
# debugonce(naomi::sample_tmb)
fit <- naomi::sample_tmb(fit, random_only=TRUE)
int <- apply(fit$sample$p_norm, 1, quantile, c(0.025, 0.975))

estimated_mf <- data.frame(matrix(rowMeans(fit$sample$p_norm), nrow = 7, ncol = 63, byrow = T)) %>% 
  rownames_to_column() %>% 
  rename(age_group = rowname) %>% 
  type.convert(as.is = T) %>% 
  pivot_longer(cols = X1:X63, names_to = "survey", values_to = "mean") %>% 
  left_join(
    data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
      pivot_longer(cols = X1:X63, names_to = "survey", values_to = "survey_id")) %>% 
  # data.frame(matrix(basic_age$survey_id, nrow = 7, ncol = 65, byrow = F)) %>% 
  #   pivot_longer(everything(), names_to = "survey", values_to = "survey_id") %>% 
  # distinct()) %>% 
  left_join(
    data.frame(matrix(int[1,], nrow = 7, ncol = 63, byrow = T)) %>% 
      rownames_to_column() %>% 
      rename(age_group = rowname) %>% 
      type.convert(as.is = T) %>% 
      pivot_longer(cols = X1:X63, names_to = "survey", values_to = "lower") %>% 
      left_join(
        data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
          pivot_longer(cols = X1:X63, names_to = "survey", values_to = "survey_id"))) %>% 
  left_join(
    data.frame(matrix(int[2,], nrow = 7, ncol = 63, byrow = T)) %>% 
      rownames_to_column() %>% 
      rename(age_group = rowname) %>% 
      type.convert(as.is = T) %>% 
      pivot_longer(cols = X1:X63, names_to = "survey", values_to = "upper") %>% 
      left_join(
        data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
          pivot_longer(cols = X1:X63, names_to = "survey", values_to = "survey_id")))


dat2 <- dat %>% group_by(survey_id) %>% mutate(pa = n/sum(n)) %>% ungroup() %>% mutate(id.age = as.numeric(id.age)) 


estimated_mf %>%
  ggplot() + 
  geom_line(aes(x = age_group, y = mean)) + 
  geom_ribbon(aes(x = age_group, ymin = lower, ymax = upper)) +
  geom_point(data = (dat2 %>% mutate(age = as.integer(id.age))), aes(x = age, y = pa, color = survey_id), show.legend = F) +
  facet_wrap(~survey_id)

setwd("~/Documents/Github/kp-age")
# library(tidyverse)
# library(sf)
# library(dfertility)
# library(multi.utils)
# library(moz.utils)
# library(countrycode)
# library(Matrix)
# library(TMB)
# 
# iso3 = moz.utils::ssa_iso3()
# 
# geographies <- read_sf(moz.utils::national_areas()) %>%
#   
#   st_make_valid() %>%
#   arrange(area_id) %>%
#   mutate(
#     id.iso3 = row_number()
#   )
# 
# spec_paths <- c(
#   list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2023 final shared/SSA", pattern = "PJNZ", ignore.case = TRUE, full.names = TRUE)
#   # list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2021 final shared/WCA", pattern = "PJNZ", ignore.case = TRUE, full.names = TRUE),
#   # list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2021 final shared/WCA/Nigeria state may 15", pattern = "PJNZ", ignore.case = TRUE, full.names = TRUE)
# )
# 
# debugonce(naomi:::extract_pjnz_one)
# lapply(spec_paths, naomi::extract_pjnz_naomi)
# 
# spectrum_data <- lapply(spec_paths, naomi::extract_pjnz_naomi)
# 
# spectrum_data <- readRDS("C:/Users/rla121/Downloads/specdata_f.rds") %>% 
# # spectrum_data <- map_df(spectrum_data, bind_rows) %>% 
#   filter(sex == "female",
#          !age < 15,
#          !age > 49) %>% 
#   filter(!year<1993) %>% 
#   select(iso3, spectrum_region_name, year, age, totpop) %>% 
#   group_by(year, age, iso3) %>% 
#   mutate(totpop = sum(totpop)) %>% 
#   ungroup() %>% 
#   select(-spectrum_region_name) %>% 
#   distinct() %>% 
#   group_by(iso3, year) %>% 
#   mutate(tpa = totpop/sum(totpop),
#          sum = sum(tpa)) %>% 
#   ungroup() 
#   
#   
# 
# # nb <- INLA::inla.read.graph(moz.utils::national_adj())
# 
# # nb <- geographies %>% 
# #   arrange(id.iso3) %>% 
# #   spdep::poly2nb() %>% 
# #   `names<-`(geographies$area_id)
# # 
# # nb <- lapply(nb, as.integer)
# # class(nb) <-  "nb"
# # 
# # adj <- spdep::nb2mat(nb, zero.policy = TRUE, style = "B")
# # R_spatial <- INLA::inla.scale.model(diag(rowSums(adj)) - 0.99*adj,
# #                                     constr = list(A = matrix(1,1, nrow(adj))), e = 0)
# 
# 
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
# 
# # separate_survey_id <- function(df, kp = T) {
# #   
# #   if(kp) {
# #     df %>%
# #       tidyr::separate(survey_id, into = c("iso3", "year", NA), sep = c(3, 7), remove = F, convert = T) %>%
# #       tidyr::separate(survey_id, into = c(NA, "kp"), sep = "_", remove = F)
# #   } else {
# #     df %>%
# #       tidyr::separate(survey_id, into = c("iso3", "year", NA), sep = c(3, 7), remove = F, convert = T)
# #   }
# #   
# # }
# 
# dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fullweightedandunweighteddata_0210.rds") %>% 
#   filter(vars == "age") %>% 
#   mutate(method = ifelse(is.na(lower), "convenience", "RDS")) %>% 
#   rename(age = category) %>% 
#   moz.utils::separate_survey_id() %>% 
#   filter(kp == "FSW",
#          age %in% 15:49) %>%
#   type.convert(as.is = T) %>% 
#   filter(!is.na(age)) %>% 
#   group_by(kp, iso3, year, survey_id) %>% 
#   mutate(estimate = estimate/sum(estimate))  %>% 
#   ungroup() 
#   
# 
# dat <- dat %>%
#   # select(iso3, survey_id, year, age, n, method) %>%
#   # group_by(iso3, survey_id, age, method, year) %>% 
#   # summarise(n = sum(n)) %>% 
#   # ungroup() %>% 
#   select(iso3, survey_id, year, age, estimate, method) %>%
#   group_by(iso3, survey_id, age, method, year) %>% 
#   summarise(estimate = sum(estimate)) %>% 
#   ungroup() %>% 
#   mutate(id.method = ifelse(method == "convenience", 1, 0))
# 
# 
# 
# dat <- dat %>% 
#   type.convert(as.is = T) %>% 
#   filter(!is.na(age)) %>% 
#   mutate(age_group = naomi::cut_naomi_age_group(age = age),
#          id.age =  factor(to_int(age_group))) %>% 
#   group_by(age_group, survey_id, iso3, year, id.method, id.age) %>% 
#   summarise(estimate = sum(estimate)) %>% 
#   ungroup()
# 
# spectrum_data_grouped <- spectrum_data %>% 
#   group_by(year, iso3) %>% 
#   mutate(age_group = naomi::cut_naomi_age_group(age))  %>% 
#   ungroup() %>% 
#   group_by(year, iso3, age_group) %>% 
#   mutate(tpa_sum = sum(tpa)) %>% 
#   ungroup() %>% 
#   select(-age, -totpop, -tpa) %>% 
#   distinct() %>% 
#   mutate(id.age =  (to_int(age_group))) %>% 
#   group_by(iso3, year) %>% 
#   mutate(sum = sum(tpa_sum))
#                                                           ## `mf_model` --> indices we're going to predict at  - similar to blank rows in INLA - this is what we want to get out
# mf_model <- crossing(
#   iso3 = iso3,
#   year = seq(1993,2023, 1),
#  age_group = unique(dat$age_group)
#                      # age = 15:49
#                      ) %>%
#   left_join(spectrum_data_grouped) %>% 
#   # left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
#   ungroup() %>%
#   mutate(id.age =  factor(to_int(age_group)),
#            # factor(to_int(age)),                          ##Everything needs to be a factor
#          id.iso3 = factor(to_int(iso3)),
#          idx = factor(row_number()),
#          id.year = as.numeric(factor(year))-1) %>% 
#   filter(!iso3== "NGA") %>% 
#   mutate(is15 = ifelse(age_group == "Y015_019", 1, 0))
# 
# 
#  
# 
# dat <- crossing(id.age = 1:7,
#                 select(dat, iso3, survey_id, id.method, year)) %>%
#   left_join(spectrum_data_grouped) %>% 
#   # left_join(dat %>% select(survey_id, iso3, id.age, n, id.method, year) %>% type.convert(as.is = T)) %>%
#   # select(iso3, survey_id, id.age, n, id.method, year, tpa) %>%
#   left_join(dat %>% select(survey_id, iso3, id.age, estimate, id.method, year) %>% type.convert(as.is = T)) %>%
#   select(iso3, survey_id, id.age, estimate, id.method, year, tpa) %>%
#   arrange(survey_id, id.age) %>% 
#   mutate(id.age = factor(id.age))
# 
# dat <- dat %>% 
#   filter(!iso3 == "NGA")
# 
# dat[is.na(dat)] <- 0
# 
# 
# dat <- dat %>%
#   left_join(mf_model) %>% 
#   mutate(estimate = estimate + 0.000000001,
#          estimate = ifelse(estimate == 1, 0.9999999, estimate))
# 
# 
# M_obs <- sparse.model.matrix(~0 + idx, dat)             ##n observations long, n columns by age group
# Z_spatial <- sparse.model.matrix(~0 + id.iso3, mf_model)
# # 
# # Z_age <- sparse.model.matrix(~0 + id.age, mf_model)     ##n observations long (this time from mf_model), n cols by age group
# # x <- 0:34
# # k <- seq(-15, 50, by = 5)
# # spline_mat <- splines::splineDesign(k, x, ord = 4)
# # spline_mat <- as(spline_mat, "sparseMatrix")
# # 
# # Z_age <- Z_age %*% spline_mat
# 
# # Z_survey <- sparse.model.matrix(~0 + survey_id, dat)
# 
# # X_method <- model.matrix(~0 + id.method, dat)
# 
# X_period <- model.matrix(~0 + id.year, mf_model)
# X_15 <- model.matrix(~0 + is15, mf_model)
# 
# 
# X_stand_in <- sparse.model.matrix(~0 + age_group, mf_model)
# X_stand_in <- X_stand_in[,c(2:7)] 
# Y_stand_in <- sparse.model.matrix(~0 + age_group, mf_model)
# X_age_group <- model.matrix(~0 + age_group, mf_model)
# Z_age <- sparse.model.matrix(~0 + age_group, mf_model)
# Z_survey <- sparse.model.matrix(~0 + survey_id, dat)
# Z_survage <- mgcv::tensor.prod.model.matrix(list(Z_survey, Z_age))
# Z_period <- sparse.model.matrix(~0 + id.year, mf_model)
# Z_periodage <- mgcv::tensor.prod.model.matrix(list(Z_period, Z_age))
# # Z_interaction3 <-  mgcv::tensor.prod.model.matrix(list(Z_spatial, Z_age))
# 
# tmb_int <- list()
# 
# 
# # norm_n <- tst %>%
# #   group_by(survey_id) %>%
# #   # mutate(norm_n = n/sum(n)) %>%
# #   pull(norm_n)
# 
# # observed_x <- matrix(dat$n, nrow = length(unique(dat$survey_id)), byrow = TRUE)    ## Puts n in a matrix - rows for surv id, cols for age group idx
# observed_x <- matrix(dat$estimate, nrow = length(unique(dat$survey_id)), byrow = TRUE)
# # observed_totpop <- matrix((dat$totpop), nrow = length(unique(dat$survey_id)), byrow = TRUE)
# observed_totpop <- matrix((mf_model$tpa), ncol = length(unique(mf_model$age_group)), byrow = TRUE)
# logit_totpop <- c(qlogis((observed_totpop)))
# #Everything goes into `data`
# tmb_int$data <- list(   
#   M_obs = M_obs,
#   observed_x = observed_x,
#   X_stand_in = X_stand_in,
#   R_beta = dfertility::make_rw_structure_matrix(ncol(X_stand_in), 1, adjust_diagonal = TRUE),  ##captures relationship between age
#   
#   
#   logit_totpop = logit_totpop,
#   # Z_age = Z_age,
#   # R_age = dfertility::make_rw_structure_matrix(ncol(Z_age), 1, adjust_diagonal = TRUE),  ##captures relationship between age
#   # 
#   # Z_spatial = Z_spatial,
#   # R_spatial = as(diag(1, nrow = length(unique(mf_model$id.iso3))), "dgCMatrix"),
#   # # 
#   # Y_stand_in = Y_stand_in,
#   # Z_survey = Z_survey,
#   R_survey = as(diag(1, nrow = length(unique(dat$survey_id))), "dgCMatrix"),
#   
#   # X_method = X_method,
#   # 
#   # X_period = X_period,
#   # 
#   # Z_interaction3 = Z_interaction3
#   Z_survage = Z_survage,
#   Z_periodage = Z_periodage
#   
#   # X_15 = X_15
#  
# )
# 
# tmb_int$par <- list( 
#   beta_0 = rep(0, ncol(X_stand_in)),
#   # lag_logit_phi_beta = 0
#   log_prec_rw_beta = 0
#   # eta3 = array(0, c(ncol(Z_survey), ncol(Z_age))),
#   # log_prec_eta3 = 0, 
#   # logit_eta3_phi_age = 0,
#   
#   # observed_totpop = c(qlogis(t(observed_totpop)))
#   
#   # eta2 = array(0, c(ncol(Z_survey), ncol(Z_period))),
#   # log_prec_eta2 = 0,
#   # logit_eta2_phi_age = 0
#   # beta_0 = 0,
#   # u_age = rep(0, ncol(Z_age)),                #Random effect for age
#   # log_prec_rw_age = 0  ,                     #Manually declare all hyperparameters - here we have log precision for RW over age (standard deviation always fed in as log sigma (or in this case, log precision) so that when exponentiated they are always +ve)
#   # 
#   # lag_logit_phi_age = 0                     #Lag logit forces values between -1 and 1
#   # u_spatial_str = rep(0, ncol(Z_spatial)),
#   # log_prec_spatial = 0,
#   # 
#   # u_survey = rep(0, ncol(Z_survey)),
#   # log_prec_survey = 0,
#   # 
#   # beta_method = rep(0, ncol(X_method)),
#   # # log_prec_method = 0
#   # 
#   # beta_period = rep(0, ncol(X_period)),
#   # 
#   # eta3 = array(0, c(ncol(Z_spatial), ncol(Z_age))),
#   # log_prec_eta3 = 0, 
#   # # logit_eta3_phi_age = 0
#   # lag_logit_eta3_phi_age = 0
#   
#   # beta15 = rep(0, ncol(X_15)),
#   # log_prec_15 = 0
# )
# 
# tmb_int$random <- c(                          # PUt everything here except hyperparamters
#   "beta_0"
#   # "beta15"
#   # "observed_totpop"
#   # "eta2",
#   # "u_age",
#   # "u_spatial_str",
#   # "u_survey",
#   # "beta_method",
#   # "beta_period",
#   # "eta3"
#   # "eta2"
# )
# 
# 
# 
# library(tidyverse)
# library(sf)
# library(dfertility)
# library(multi.utils)
# library(moz.utils)
# library(countrycode)
# library(Matrix)
# library(TMB)
# 
# tmb_unload <- function(name) {   
#   ldll <- getLoadedDLLs()   
#   idx  <- grep(name, names(ldll))   
#   for (i in seq_along(idx)) 
#     dyn.unload(unlist(ldll[[idx[i]]])$path)
#   cat('Unload ', length(idx), "loaded versions.\n") 
# }
# 
# tmb_unload("tmb")
# 
# # file.remove("src/tmb.dll")
# file.remove("src/tmb.o")
# 
# # setwd("C:/Users/rla121/Dropbox/KP/Individual KP/Code/KP Code/kp-age/")
# TMB::compile("src/tmb.cpp", flags = "-w")
# 
# dyn.load(dynlib("src/tmb"))
# 
# 
# # gdbsource("src/tmb.cpp")
# # 
# # f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
# #                                           parameters = tmb_int$par,
# #                                           DLL = "tmb",
# #                                           silent=0,
# #                                           checkParameterOrder=FALSE)
# # })
# # 
# # if(is.null(parallel::mccollect(f)[[1]])) {
# #   stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
# # }
# 
# 
# # 
# # ### Below produces error: "Error in unserialize(socklist[[n]]) : error reading from connection
# # 
# # cl <- makeCluster(4)  # Adjust the number of cores as per your system
# # registerDoParallel(cl)
# # 
# # f <- (foreach(i = 1:4) %dopar% {
# #   dyn.load(TMB::dynlib("src/tmb"))
# # 
# #   TMB::MakeADFun(data = tmb_int$data,
# #                                           parameters = tmb_int$par,
# #                                           DLL = "tmb",
# #                                           silent=0,
# #                                           checkParameterOrder=FALSE)
# # })
# # 
# # results <- foreach(i = 1:length(f), .export = c("f")) %dopar% {
# #   f[[i]]
# # }
# # 
# # # Stop the cluster
# # stopCluster(cl)
# # registerDoSEQ()  # Set parallel backend to sequential (if needed)
# # 
# # # Check if the TMB model is valid
# # if (is.null(results[[1]])) {
# #   stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
# # }
# 
# 
# 
# 
# ## Deparallelised version :( 
# 
# 
# # debugonce(TMB::MakeADFun)
# f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
#                     parameters = tmb_int$par,
#                     DLL = "tmb",
#                     silent=0,
#                     checkParameterOrder=FALSE)
# })
# 
# 
# if(is.null(parallel::mccollect(f)[[1]])) {
#   stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
# }
# 
# 
# 
# 
# 
# obj <-  TMB::MakeADFun(data = tmb_int$data,
#                        parameters = tmb_int$par,
#                        DLL = "tmb",
#                        random = tmb_int$random,
#                        hessian = FALSE)
# 
# f <- stats::nlminb(obj$par, obj$fn, obj$gr)
# f$par.fixed <- f$par
# f$par.full <- obj$env$last.par
# 
# fit <- c(f, obj = list(obj)) ## taking a while
# 
# fit$sdreport <- sdreport(fit$obj, fit$par) #also taking a while
# sd_report <- fit$sdreport
# sd_report <- summary(sd_report, "all")
# sd_report
# 
# # sd_report2 <- data.frame(sd_report) %>% 
# #   filter(Estimate < 1) %>% 
# #   mutate(estimate2 = plogis(Estimate),
# #          sum = sum(Estimate),
# #          norm = estimate2/sum)
# 
# class(fit) <- "naomi_fit"
# # debugonce(naomi::sample_tmb)
# fit <- naomi::sample_tmb(fit, random_only=TRUE)
# int <- apply(fit$sample$p_norm, 1, quantile, c(0.025, 0.975))
# 
# estimated_mf <- data.frame(matrix(rowMeans(fit$sample$p_norm), nrow = 7, ncol = 65, byrow = T)) %>% 
#   rownames_to_column() %>% 
#   rename(age_group = rowname) %>% 
#   type.convert(as.is = T) %>% 
#   pivot_longer(cols = X1:X65, names_to = "survey", values_to = "mean") %>% 
#   left_join(
#     data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
#       pivot_longer(cols = X1:X65, names_to = "survey", values_to = "survey_id")) %>% 
#     # data.frame(matrix(basic_age$survey_id, nrow = 7, ncol = 65, byrow = F)) %>% 
#     #   pivot_longer(everything(), names_to = "survey", values_to = "survey_id") %>% 
#       # distinct()) %>% 
#   left_join(
#     data.frame(matrix(int[1,], nrow = 7, ncol = 65, byrow = T)) %>% 
#       rownames_to_column() %>% 
#       rename(age_group = rowname) %>% 
#       type.convert(as.is = T) %>% 
#       pivot_longer(cols = X1:X65, names_to = "survey", values_to = "lower") %>% 
#       left_join(
#         data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
#           pivot_longer(cols = X1:X65, names_to = "survey", values_to = "survey_id"))) %>% 
#   left_join(
#     data.frame(matrix(int[2,], nrow = 7, ncol = 65, byrow = T)) %>% 
#       rownames_to_column() %>% 
#       rename(age_group = rowname) %>% 
#       type.convert(as.is = T) %>% 
#       pivot_longer(cols = X1:X65, names_to = "survey", values_to = "upper") %>% 
#       left_join(
#         data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
#           pivot_longer(cols = X1:X65, names_to = "survey", values_to = "survey_id"))) 
#   
# 
# 
# 
# # estimated_mf <- mf_model %>%
# #   mutate(lower = int[1,],
# #          mean = rowMeans(fit$sample$logit_p),
# #          upper = int[2,])
# 
# # int <- rowMeans(fit$sample$single_row_of_probs)
# # 
# # int <- rowMeans(fit$sample$p_norm)
# # age <- rep(c(15:49), 14)
# # surveys <- rep(1:14, each = 35)
# # id.iso3 <- rep(1:39, each = 39)
# # 
# # ## with iso3
# # 
# # age <- rep(c(15:49), 546)
# # surveys <- rep(1:14, each = 1365)
# # id.iso3 <- rep(1:39, each = 490)
# # id.iso3 <- rep(c(1:39), 490)
# # 
# # modelled_outcomes <- data.frame(id.iso3, age, surveys, int) %>% mutate(id.iso3 = as.factor(id.iso3))
# # 
# # dat2 <- dat %>% 
# #   left_join(modelled_outcomes)
# # 
# # modelled_outcomes <- data.frame(int, age, surveys)
# 
# 
# 
# # single_year_to_five_year <- function (df, fifteen_to_49 = TRUE) 
# # {
# #   df <- df %>% dplyr::mutate(age_group_label = cut(age, c(0, 
# #                                                           seq(5, 85, 5) - 1), c(paste0(seq(0, 79, 5), "-", seq(5, 
# #                                                                                                                80, 5) - 1), "80+"), include.lowest = TRUE)) %>% dplyr::left_join(naomi::get_age_groups() %>% 
# #                                                                                                                                                                                    select(age_group, age_group_label)) %>% dplyr::select(-age_group_label)
# #   if (fifteen_to_49) {
# #     df %>% dplyr::filter(age %in% 15:49) %>% dplyr::select(-age)
# #   }
# #   else {
# #     df %>% dplyr::select(-age)
# #   }
# # }
# 
# # df <- data.frame(age = 15:49, p = int) %>%
# # single_year_to_five_year()
# # 
# # df %>%
# #   group_by(age_group) %>%
# #   summarise(p = sum(p))
# # 
# # dat <- dat %>% 
# #   mutate(surveys = multi.utils::to_int(survey_id))
# # 
# # modelled_outcomes_to_plot <- modelled_outcomes %>% 
# #   left_join(dat) %>% 
# #   select(-id.age, -idx) %>% 
# #   group_by(surveys) %>% 
# #   mutate(observed_dist = n/sum(n)) %>% 
# #   ungroup()
# # 
# # modelled_outcomes_to_plot %>% 
# #   ggplot(aes(x = age)) + 
# #   geom_line(aes(y = observed_dist), color = "black") + 
# #   geom_line(aes(y = int, color = survey_id)) +
# #   facet_wrap(~survey_id)
# # 
# # mf_model %>%
# #   cbind(pred = int) %>%
# #   group_by(iso3) %>%
# #   mutate(natural_pred = plogis(pred)/sum(plogis(pred)))
# 
# dat %>%
#   group_by(iso3, survey_id) %>%
#   mutate(p = n/sum(n)) %>% 
#   type.convert(as.is = T) %>% 
#   left_join(estimated_mf %>%
#               group_by(survey_id) %>% 
#               # group_by(iso3, year) %>%
#               mutate(across(lower:upper, ~plogis(.x)/sum(plogis(.x)))) %>% 
#               type.convert(as.is = T) %>% 
#               rename(id.age = age_group)
#             )%>%
#   ggplot(aes(x=id.age)) +
#     geom_line(aes(y=mean), size = 1, color = "black") +
#     # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#     geom_line(aes(y=p, color= survey_id), show.legend = F) +
#     facet_wrap(~survey_id) +
#   moz.utils::standard_theme()
# 
# plotdat <- dat %>% 
#   filter(!is.na(n)) %>% 
#   group_by(survey_id) %>% 
#   mutate(id.age = multi.utils::to_int(age_group), 
#          estimate = n/sum(n)) %>% 
#   ungroup()
#   
# estimated_mf %>%
#   # group_by(survey_id) %>%
#   # mutate(across(lower:upper, ~exp(.x)/sum(exp(.x)))) %>%
#   # select(lower:upper, everything()) %>% 
#   # mutate(across(lower:upper, ~exp(.x))) %>% 
#   # ungroup() %>%
#   mutate(age_group = type.convert(age_group, as.is = T)) %>% 
#   ggplot(aes(x = age_group)) +
#   geom_line(aes(y = mean)) +
#   geom_line(data = plotdat , aes(x = id.age , y = estimate, color = survey_id), show.legend = F) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha =0.2) +
#   facet_wrap(~survey_id) +
#   moz.utils::standard_theme() + 
#   labs(x = "Age Group", y = "Proportion FSW at age") +
#   theme(strip.text = element_text(size = 8),
#         axis.text.y = element_text(size = 8))
# 
