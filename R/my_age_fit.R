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

# aggregate_data <- readxl::read_xlsx("~/Downloads/aggregate_agedat.xlsx") %>% 
#   rename(survey_id = study_id) %>% 
#   separate_survey_id() %>% 
#   mutate(age_group2 = str_remove(age_group, "Y")) %>%
#   separate(age_group2, into = c("age_start", "age_end"), sep = "_", convert = TRUE, remove = T)
# 
# df <- naomi::get_age_groups() %>%
#   filter(age_group_sort_order %in% 18:24) %>%
#   rowwise() %>%
#   mutate(span = list(age_group_start:(age_group_start + age_group_span - 1)),
#          x=1) %>%
#   unnest(span) %>%
#   group_by(age_group) %>%
#   mutate(idx = cur_group_id(),
#          span = span - 14) %>%
#   group_by(span) %>%
#   mutate(idy = cur_group_id()) 
# 
# sparseMatrix(i = df$idx, j = df$idy, x = 1)
# 
# cleanag <- aggregate_data %>% 
#   mutate(n = ifelse(is.na(n), prop*sum, n),
#          n = ifelse(is.na(n), prop, n)) %>% 
#   filter(!is.na(age_start)) %>% 
#   mutate(age_end = ifelse(is.na(age_end), 49, age_end),
#          area = ifelse(is.na(area),iso3, area)) %>% 
#   filter(area == iso3) %>% 
#   select(kp, survey_id, iso3, year, age_group, age_start, age_end, n) %>% 
#   filter(!(age_start > 45 | age_start < 15)) %>% 
#   arrange(age_start, age_end)
# 
# cleanag_matrix <-  cleanag 
#   # rowwise() %>% 
#   # mutate(group_span = (age_end - age_start + 1),
#   #        span = list(age_start:(age_start + group_span - 1)),
#   #                          x=1) %>%
#   # unnest(span) %>%
#   # group_by(age_group) %>%
#   # mutate(idx = cur_group_id(),
#   #        span = span - 14) %>%
#   # group_by(span) %>%
#   # mutate(idy = cur_group_id()) 
# 
# # aggregate <- sparseMatrix(i = cleanag_matrix$idx, j = cleanag_matrix$idy, x = 1)
# 
# age_min <- min(cleanag_matrix$age_start)
# age_max <- max(cleanag_matrix$age_end)
# 
# # Step 2: Initialize an empty sparse matrix
# # The number of rows is the number of age groups, and the number of columns is the range of ages
# mat <- Matrix(0, nrow = nrow(cleanag_matrix), ncol = age_max - age_min + 1,
#               dimnames = list(cleanag_matrix$age_group, paste0(age_min:age_max)))
# 
# # Step 3: Fill the matrix
# for (i in 1:nrow(cleanag_matrix)) {
#   start_index <- cleanag_matrix$age_start[i] - age_min + 1
#   end_index <- cleanag_matrix$age_end[i] - age_min + 1
#   mat[i, start_index:end_index] <- 1
# }
#   #        age_group2 = case_when(age_group %in% c("Y013_019", "Y014_019", "Y015_019", "Y015_020", "Y018_019", "Y018_020") ~ "Y015_019",
#   #                               age_group %in% c("Y018_021", "Y018_024", "Y020_024", "Y021_024", "Y021_025") ~ "Y020_024",
#   #                               age_group %in% c("Y024_029", "Y025_029", "Y025_030", "Y026_030") ~ "Y025_029",
#   #                               age_group == "Y015_024" ~ "Y015_024",
#   #                               age_group == "Y025_034" ~ "Y025_034",
#   #                               age_group == "Y020_029" ~ "Y020_029",
#   #                               age_group == "Y025_049" ~ "Y025_049",
#   #                               age_group == "Y030_034" ~ "Y030_034",
#   #                               age_group == "Y030_039" ~ "Y030_039",
#   #                               age_group %in% c("Y030_049", "Y031_", "Y030_49") ~ "Y030_049",
#   #                               age_group == "Y035_039" ~ "Y035_039",
#   #                               age_group == "Y035_044" ~ "Y035_044",
#   #                               age_group == "Y035_065" ~ "Y035_049",
#   #                               age_group %in% c("Y040_044", "Y040_045") ~ "Y040_044",
#   #                               age_group %in% c("Y040_049", "Y049_049") ~ "Y040_049",
#   #                               age_group %in% c("Y045_049", "Y045_054") ~ "Y045_049")) %>% 
#   # rename(oldage = age_group,
#   #        age_group = age_group2) 
#   # filter(age_group  %in% c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039", "Y040_044", "Y045_049")) %>% #5-year age groups
# 
# 
# twoage <- cleanag %>% 
#   mutate(age_group = case_when(age_group %in% c("Y015_019", "Y015_024", "Y020_024") ~ "Y015_024",
#                                age_group %in% c("Y025_029", "Y025_034", "Y025_049", "Y030_034", "Y030_039", "Y030_049", "Y035_039", "Y035_044", "Y040_044", "Y040_049", "Y045_049") ~ "Y025_049"))%>% 
#   select(-oldage) %>% 
#   mutate(year = factor(year)) %>% 
#   filter(area == iso3) %>% 
#   group_by(survey_id, iso3, year, kp, age_group) %>% 
#   summarise(n = sum(n)) %>% 
#   ungroup() %>% 
#   filter(!is.na(age_group))
# 
# tenyr <- cleanag %>% 
#   filter(age_group  %in% c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039", "Y040_044", "Y045_049")) %>% 
#   mutate(age_group = case_when (age_group %in% c("Y015_019") ~ "Y015_019",
#                                 age_group %in% c("Y020_024", "Y025_029") ~ "Y020_029",
#                                 age_group %in% c("Y030_034", "Y035_039", "Y040_044", "Y045_049", "40_49") ~ "Y030_049")) %>% 
#   bind_rows(cleanag %>% 
#               filter(age_group  %in% c("Y020_029", "Y030_049"))) %>% #10-year age groups
#               group_by(survey_id) %>% 
#               filter(area == iso3) %>% 
#               ungroup() %>% 
#   select(-oldage) %>% 
#   mutate(year = factor(year)) %>% 
#   group_by(survey_id, age_group) %>% 
#   mutate(n = sum(n)) %>% 
#   ungroup()
#   
# fsw_ag <- tenyr %>% filter(kp == "FSW") %>% 
#   # filter(!counter<5) %>% 
#   select(age_group, iso3, year, survey_id, n)  %>% 
#   distinct()  %>% 
#   group_by(survey_id) %>% 
#   mutate(counter = length(iso3)) %>% 
#   ungroup() %>% 
#   filter(!counter == 1)
# 
# fsw_ag <- twoage %>% 
#   filter(kp == "FSW") %>% 
#   select(age_group, iso3, year, survey_id, n)  %>% 
#   distinct()  %>% 
#   group_by(survey_id) %>% 
#   mutate(counter = length(iso3)) %>% 
#   ungroup() 

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fullweightedandunweighteddata_0210.rds") %>%
  filter(vars == "age") %>%
  mutate(method = ifelse(is.na(lower), "convenience", "RDS")) %>%
  rename(age = category) %>%
  moz.utils::separate_survey_id() %>%
  filter(kp == "FSW", 
         # kp == "PWID",
         # kp == "MSM",
         age %in% 15:49) %>%
  type.convert(as.is = T) %>%
  filter(!is.na(age)) %>%
  select(iso3, survey_id, year, age, n) %>% 
  single_year_to_five_year(T) %>%
  mutate(age_group = factor(age_group)) %>% #change to factor(age) for single year of age / factor(age_group) for age groups. 
  # mutate(age_group = case_when (age_group %in% c("Y015_019") ~ "Y015_019",
  #                               age_group %in% c("Y020_024", "Y025_029") ~ "Y020_029",
  #                               age_group %in% c("Y030_034", "Y035_039", "Y040_044", "Y045_049", "40_49") ~ "Y030_049")) %>% 
 # mutate(age_group = case_when(age_group %in% c("Y015_019", "Y015_024", "Y020_024") ~ "Y015_024",
                                      # age_group %in% c("Y025_029", "Y025_034", "Y025_049", "Y030_034", "Y030_039", "Y030_049", "Y035_039", "Y035_044", "Y040_044", "Y040_049", "Y045_049") ~ "Y025_049")) %>% 
  group_by(iso3, survey_id, year, age_group) %>%
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(year = factor(year))

# dat <- dat %>% 
#   bind_rows(fsw_ag)

dat <- crossing(age_group = dat$age_group,
                select(dat, iso3, survey_id, year)) %>%
  arrange(survey_id) %>%
  # filter(!survey_id %in% c("ZAF1998ACA_FSW")) %>%
  # filter(!survey_id %in% c("ZAF1998ACA_FSW", "ETH2020ACA_FSW")) %>%
  left_join(dat)

# dat <- dat %>%
#   filter(survey_id == "NAM2019BBS_FSW")

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

# For age_groups
spectrum_data_f <- readRDS("~/Downloads/spectrum_data_f.rds") %>% 
  filter(year %in% c(1993:2023)) %>% 
  ungroup() 
  # mutate(age_group = case_when (age_group %in% c("Y015_019") ~ "Y015_019",
  #                               age_group %in% c("Y020_024", "Y025_029") ~ "Y020_029",
  #                               age_group %in% c("Y030_034", "Y035_039", "Y040_044", "Y045_049", "40_49") ~ "Y030_049")) %>% 
  # mutate(age_group = case_when(age_group %in% c("Y015_019", "Y015_024", "Y020_024") ~ "Y015_024",
  #                              age_group %in% c("Y025_029", "Y025_034", "Y025_049", "Y030_034", "Y030_039", "Y030_049", "Y035_039", "Y035_044", "Y040_044", "Y040_049", "Y045_049") ~ "Y025_049")) %>%
  # group_by(iso3, year, age_group) %>%
  # summarise(tpa = sum(tpa)) %>% 
  # ungroup()  

# # For single year age
# spectrum_data_f <- readRDS("~/Downloads/spectrum_data_f_il.rds") %>%
#   ungroup()
# 
# # For male single year of age
# spectrum_data_m <- readRDS("~/Downloads/spectrum_data_m.rds") %>% 
#   ungroup()
# 
# # For PWID single year of age
# spectrum_data_pwid <- readRDS("~/Downloads/spectrum_data_pwid.rds") %>% 
#   ungroup() %>% 
#   rename(age_group = age)

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
#          sex == "male") %>%
#   # single_year_to_five_year(T) %>%
#   mutate(age_group = age) %>%
#   group_by(iso3, sex, year, age_group) %>%
#   summarise(totpop = sum(totpop)) %>%
#   group_by(iso3, sex, year) %>%
#   mutate(tpa = totpop/sum(totpop)) %>%
#   select(-totpop) %>%
#   ungroup()
# 
# spectrum_data_m <- spectrum_data_f

# saveRDS(spectrum_data_m, "~/Downloads/spectrum_data_m.rds")
# 
# # saveRDS(spectrum_data_f, "~/Downloads/spectrum_data_f.rds")
# saveRDS(spectrum_data_f, "~/Downloads/spectrum_data_f_il.rds")

# spectrum_data_pwid <- spectrum_data %>%
#   bind_rows() %>%
#   bind_rows(nga_spectrum_dat) %>%
#   filter(age %in% 15:49,
#          year > 1992) %>% 
#          # sex == "male") %>%
#   # single_year_to_five_year(T) %>%
#   # group_by(iso3, sex, year, age_group) %>%
#   group_by(iso3, year, age) %>%
#   summarise(totpop = sum(totpop)) %>%
#   # group_by(iso3, sex, year) %>%
#   group_by(iso3, year) %>%
#   mutate(tpa = totpop/sum(totpop)) %>%
#   select(-totpop)
# 
# saveRDS(spectrum_data_pwid, "~/Downloads/spectrum_data_pwid.rds")
# 
# spectrum_data_grouped <- readRDS("~/Downloads/specgrouped.rds")

# mf_model w/year
mf_model <- spectrum_data_f %>%
# mf_model <- spectrum_data_m %>%
  distinct(age_group, iso3, year, tpa) %>% 
  ungroup() %>%
  mutate(id.age =  factor(to_int(age_group))
         # id.year = factor(to_int(year)),
         ) %>% 
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3 = area_id, everything()) %>% mutate(id.iso3 = factor(row_number()))) %>% 
  filter(year %in% c(1993:2023)) %>% 
  mutate(id.year = factor(year),
         idx = factor(row_number())) %>% 
  left_join(spectrum_data_offsets) #made in local_global_offset.R
  # group_by(iso3, year) %>% 
  # # mutate(
  # #   # tpa = tpa - tpa[age_group == "Y045_049"],
  # #        tpa = ifelse(age_group == "Y045_049", 0.5, tpa)) %>% 
  # ungroup()

naomi <- readRDS("~/Downloads/naomi2022_indicators 1.rds") 

# Let's get some local bits
naomi <- naomi %>% 
  filter(indicator == "population", sex == "female",
         age_group %in% c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039", "Y040_044", "Y045_049"))

area_dictionary <- read_csv("~/Downloads/area_dictionary.csv")


dat2 <- dat %>%
  type.convert(as.is = T) %>%
  left_join(mf_model) 

dat2$n[is.na(dat2$n)] <- 0


# TMB Data objects

M_obs <- sparse.model.matrix(~0 + idx, dat2) 
Z_spatial <- sparse.model.matrix(~0 + id.iso3, mf_model)


X_stand_in <- sparse.model.matrix(~0 + id.age, mf_model)
# X_stand_in[,1] <- 0
X_stand_in <- X_stand_in[,c(1:6)]

##### No splines
Z_age <- sparse.model.matrix(~0 + id.age, mf_model)

# ##### Yes splines
# Z_age <- sparse.model.matrix(~0 + id.age, mf_model)    ##n observations long (this time from mf_model), n cols by age group
# x <- 0:34
# k <- seq(-15, 50, by = 5)
# spline_mat <- splines::splineDesign(k, x, ord = 4)
# spline_mat <- as(spline_mat, "sparseMatrix")
# Z_age <- Z_age %*% spline_mat
# X_stand_in <- X_stand_in %*% spline_mat

# Z_spaceage <-  mgcv::tensor.prod.model.matrix(list(Z_spatial, Z_age)) #wrong way around
Z_spaceage <-  mgcv::tensor.prod.model.matrix(list(Z_age, Z_spatial))

Z_period <- sparse.model.matrix(~0 + id.year, mf_model)

# Z_periodage <-  mgcv::tensor.prod.model.matrix(list(Z_period, Z_age)) #wrong way around
Z_periodage <-  mgcv::tensor.prod.model.matrix(list(Z_age, Z_period)) 
dim(Z_periodage)
dim(X_stand_in)


Z_survey <- sparse.model.matrix(~0 + survey_id, dat2)

Z_age_observations <- sparse.model.matrix(~0 + id.age, dat2)
# Z_age_observations <- Z_age_observations %*% spline_mat
# Z_survage <-  mgcv::tensor.prod.model.matrix(list(Z_survey, Z_age_observations)) 

Z_survage <-  mgcv::tensor.prod.model.matrix(list(Z_age_observations, Z_survey)) 

R_surv <- as(diag(1, nrow = length(unique(dat2$survey_id))), "dgTMatrix")

observed_x <- matrix(dat2$n, nrow = length(unique(dat2$survey_id)), byrow = TRUE)


# To produce logit_totpop with real values
# observed_totpop <- matrix(mf_model$tpa, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
# logit_totpop <- qlogis(observed_totpop)

observed_totpop <- matrix(dat2$tpa2020, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
newoffset2 <- log(observed_totpop) - log(observed_totpop[,7]) # Log-odds using the 7th age group as the base 

# To remove offset
newoffset2 <- log(observed_totpop) - log(observed_totpop)

# SImulated logit_totpop
# logit_totpop <- qlogis(rdirichlet(1209, (seq(1,7,1))))
# observed_totpop <- matrix(mf_model$tpa2, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
# logit_totpop <- qlogis(observed_totpop)

# To remove logit_totpop this produces a matrix of 0
# logit_totpop <- matrix(rep(0, nrow(mf_model)), ncol = length(unique(mf_model$age_group)), byrow = T)

tmb_int <- list()

tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  # R_beta = dfertility::make_rw_structure_matrix(ncol(X_stand_in), 1, adjust_diagonal = TRUE),  ##captures relationship between age
  # R_beta = as(diag(1, nrow = length(unique(dat2$id.age))), "dgTMatrix"), ## we're only estimating 6 coefficients so commenting it out 
  # 
  R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))-1), "dgTMatrix"),
  
  # Z_yeariso = Z_yeariso,
  # logit_totpop = newoffset2
  logit_totpop = as.vector(t(newoffset2)),
  # 
  # R_survey = as(diag(1, nrow = length(unique(dat$survey_id))), "dgCMatrix"),
  # 
  # 
  Z_spatial = Z_spatial,
  R_spatial = dfertility::make_adjacency_matrix(read_sf(moz.utils::national_areas()) %>% mutate(iso3 = area_id) %>% st_make_valid(), 0),

  Z_spaceage = Z_spaceage,
  # 
  Z_period = Z_period,
  R_period = dfertility::make_rw_structure_matrix(ncol(Z_period), 1, adjust_diagonal = TRUE),
  Z_periodage = Z_periodage,
  # 
  # # Z_survage = Z_survage,
  # # R_surv = R_surv,
  # 
  rankdef_R_spatial = 1
)



tmb_int$par <- list( 
  beta_0 = rep(0, ncol(X_stand_in)),
  
  # log_prec_rw_beta = 0
  # lag_logit_phi_beta = 0
  
  # eta3 = array(0, c(ncol(Z_spatial), ncol(Z_age))),
  # log_prec_eta3 = 0,
  # # log_sigma_eta3 = 0,
  # # logit_eta3_phi_age = 0
  # lag_logit_eta3_phi_age = 0
  # 
  # logit_eta2_phi_age = 0,
  lag_logit_eta2_phi_age = 0,
  eta2 = array(0, c(ncol(Z_period), ncol(Z_age))),
  log_prec_eta2 = 0,
  # logit_eta2_phi_period = 0
  lag_logit_eta2_phi_period = 0
  #
  # eta_surv = array(0, c(ncol(Z_survey), ncol(Z_age))),
  # log_prec_eta_surv = 0,
  # logit_eta_surv_phi_age = 0
  
)

tmb_int$random <- c(                          
  "beta_0",
  # "eta3"
  "eta2"
  # "eta_surv"


)

# setwd("~/Documents/Github/kp-age")

tmb_unload <- function(name) {   
  ldll <- getLoadedDLLs()   
  idx  <- grep(name, names(ldll))   
  for (i in seq_along(idx)) 
    dyn.unload(unlist(ldll[[idx[i]]])$path)
  cat('Unload ', length(idx), "loaded versions.\n") 
}


tmb_unload("flib")

lapply(list.files("src/", pattern = "\\.o|\\.so", full.names = T), file.remove)

TMB::compile("src/flib.cpp", flags = "-w")

dyn.load(dynlib("src/flib"))



f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "flib",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})


if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}



obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "flib",
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



estimated_mf <- data.frame(matrix(rowMeans(fit$sample$p_norm), nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>%
  rownames_to_column() %>% 
  rename(age_group = rowname) %>% 
  type.convert(as.is = T) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "survey", values_to = "mean") %>% 
  left_join(
    data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
      pivot_longer(cols = starts_with("X"), names_to = "survey", values_to = "survey_id")) %>% 
  # data.frame(matrix(basic_age$survey_id, nrow = 7, ncol = 65, byrow = F)) %>% 
  #   pivot_longer(everything(), names_to = "survey", values_to = "survey_id") %>% 
  # distinct()) %>% 
  left_join(
    data.frame(matrix(int[1,], nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>% 
      rownames_to_column() %>% 
      rename(age_group = rowname) %>% 
      type.convert(as.is = T) %>% 
      pivot_longer(cols = starts_with("X"), names_to = "survey", values_to = "lower") %>% 
      left_join(
        data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
          pivot_longer(cols = starts_with("X"), names_to = "survey", values_to = "survey_id"))) %>% 
  left_join(
    data.frame(matrix(int[2,], nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>% 
      rownames_to_column() %>% 
      rename(age_group = rowname) %>% 
      type.convert(as.is = T) %>% 
      pivot_longer(cols = starts_with("X"), names_to = "survey", values_to = "upper") %>% 
      left_join(
        data.frame(matrix(unique(dat$survey_id), nrow = 1)) %>% 
          pivot_longer(cols = starts_with("X"), names_to = "survey", values_to = "survey_id"))) %>% 
  separate_survey_id()
  


dat3 <- dat2 %>% group_by(survey_id) %>% mutate(pa = n/sum(n)) %>% ungroup() %>% mutate(id.age = as.numeric(id.age)) %>% 
  mutate(age_group = id.age + 14,
         year = factor(year))


# Offset faff
# otheroffset <- data.frame(matrix(rowMeans(fit$sample$p_norm), nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>%
#   rownames_to_column() %>%
#   rename(age_group = rowname) %>%
#   type.convert(as.is = T) %>%
#   rename(estimate = `matrix.rowMeans.fit.sample.p_norm...nrow...length.unique.dat.age_group....`) %>%
#   left_join(
#     data.frame(matrix(int[1,], nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>%
#       rownames_to_column() %>%
#       rename(age_group = rowname) %>%
#       type.convert(as.is = T) %>%
#       rename(lower = `matrix.int.1.....nrow...length.unique.dat.age_group....ncol...length.unique.dat.survey_id....`)) %>%
#   left_join(
#     data.frame(matrix(int[2,], nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>%
#       rownames_to_column() %>%
#       rename(age_group = rowname) %>%
#       type.convert(as.is = T) %>%
#       rename(upper = `matrix.int.2.....nrow...length.unique.dat.age_group....ncol...length.unique.dat.survey_id....`))
# 
# nooffset <- data.frame(matrix(rowMeans(fit$sample$p_norm), nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>%
#   rownames_to_column() %>%
#   rename(age_group = rowname) %>%
#   type.convert(as.is = T) %>%
#   rename(estimate = `matrix.rowMeans.fit.sample.p_norm...nrow...length.unique.dat.age_group....`) %>%
#   left_join(
#     data.frame(matrix(int[1,], nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>%
#       rownames_to_column() %>%
#       rename(age_group = rowname) %>%
#       type.convert(as.is = T) %>%
#       rename(lower = `matrix.int.1.....nrow...length.unique.dat.age_group....ncol...length.unique.dat.survey_id....`)) %>%
#   left_join(
#     data.frame(matrix(int[2,], nrow = length(unique(dat$age_group)), ncol = length(unique(dat$survey_id)), byrow = T)) %>%
#       rownames_to_column() %>%
#       rename(age_group = rowname) %>%
#       type.convert(as.is = T) %>%
#       rename(upper = `matrix.int.2.....nrow...length.unique.dat.age_group....ncol...length.unique.dat.survey_id....`))
# 
# withoffset %>%
#   mutate(type = "Offset") %>%
#   bind_rows(nooffset %>% mutate(type = "No offset")) %>%
#   bind_rows(otheroffset %>% mutate(type = "Other Offset")) %>% 
#   # bind_rows(no_tpa2 %>% mutate(type = "No genpop")) %>%
# ggplot() +
#     geom_line(aes(x = age_group, y = estimate, color = type))
# 
# dat2 %>%
#   mutate(id.age = as.numeric(id.age)) %>%
#   ggplot() +
#   geom_line(aes(x = id.age, y = tpa1), color = "cornflowerblue") +
#   geom_line(aes(x = id.age, y = tpa2), color = "darkgreen") +
#   geom_hline(yintercept = 0, color = "darkred") +
#   labs(y = "totpop_dist", x = "age_group")
#     
  

estimated_mf %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x=age_group + 14, group = survey_id)) + 
  # geom_point(data = (dat3 %>% mutate(age = as.integer(age_group))), aes(x = age, y = pa, color = id.year), show.legend = F, size = 0.7, alpha = 0.6) +
  geom_point(data = (dat3), aes(x = id.age+14, y = pa, color = survey_id), show.legend = F, size = 0.7, alpha = 0.6) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.75) +  
  geom_line(aes(y = mean, color = survey_id), show.legend = F) +
  # facet_wrap(~iso3) +
  # moz.utils::standard_theme() 
  theme(panel.background = element_rect(fill = NA)) +
  labs(y = "Proportion of FSW")
  # moz.utils::standard_theme() +
  # theme(plot.tag = element_text(size = 12),
  #       plot.title = element_text(size = 12))


estimated_mf %>%
  mutate(year = factor(year)) %>% 
  ggplot(aes(x=age_group + 14, group = survey_id)) + 
  geom_point(data = (dat3 %>% mutate(age = as.integer(age_group))), aes(x = age, y = pa, color = id.year), show.legend = F, size = 0.7, alpha = 0.6) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgrey") + # 
  geom_line(aes(y = mean), show.legend = F) +
  geom_line(data = nooffset, aes(y = mean, x = age_group + 14),  color = "red") +
  facet_wrap(~survey_id) +
  # moz.utils::standard_theme() 
  theme(panel.background = element_rect(fill = NA)) +
  labs(y = "Proportion of sex-workers per age group")

nooffset <- estimated_mf 

as.matrix((data.frame(sd_report) %>% 
  rownames_to_column() %>% 
  filter(str_starts(rowname, "eta2")) %>% 
  select(Estimate) %>% 
    mutate(Estimate = factor(Estimate))), nrow = (ncol(X_stand_in)), ncol = ncol(Z_period), byrow = T)



timetrend <- data.frame(sd_report) %>% 
    rownames_to_column() %>% 
    filter(str_starts(rowname, "eta2")) %>% 
    select(Estimate) 


timetrend <- timetrend$Estimate


estimated_mf %>% 
  mutate(under25 = factor(ifelse(age_group<11, "<25", "25+"))) %>% 
  group_by(survey_id, under25, iso3, year) %>% 
  summarise(cumulative = sum(mean)) %>% 
  ungroup() %>% 
  left_join(dat3 %>% mutate(under25 = factor(ifelse(age_group<25, "<25", "25+"))) %>% group_by(survey_id, under25) %>% summarise(n = sum(n))) %>% 
  ggplot() +
  # geom_line(aes(x = year, y = cumulative, color = under25)) + 
  # facet_wrap(~iso3)
  geom_point(aes(x = year, y = cumulative, color = under25, size = n)) + 
  geom_smooth(aes(x = year, y = cumulative, color = under25, weight = n), method = lm) + 
  moz.utils::standard_theme() +
  labs(x = "Year", y = "Proportion of MSM pop.") + 
  # facet_wrap(~iso3) + 
  lims(y = c(0,1))

  
setwd("~/Documents/Github/kp-age")


data.frame(matrix(timetrend, ncol = (ncol(X_stand_in)), nrow = ncol(Z_period), byrow = T)) %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = starts_with("X"), names_to = "age", values_to = "estimate") %>% 
  type.convert(as.is = T) %>% 
  ggplot() + 
  geom_line(aes(x = rowname, y = estimate, color = age))

data.frame(matrix(timetrend, nrow = (ncol(X_stand_in)), ncol= ncol(Z_period), byrow = T)) %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "estimate") %>% 
  type.convert(as.is = T) %>% 
  mutate(rowname = factor(rowname),
         year = to_int(year)) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = estimate, color = rowname))


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
