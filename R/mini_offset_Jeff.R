library(tidyverse)
library(sf)
library(dfertility)
library(multi.utils)
library(moz.utils)
library(countrycode)
library(Matrix)
library(TMB)
library(VGAM)

tmb_unload <- function(name) {   
  ldll <- getLoadedDLLs()   
  idx  <- grep(name, names(ldll))   
  for (i in seq_along(idx)) 
    dyn.unload(unlist(ldll[[idx[i]]])$path)
  cat('Unload ', length(idx), "loaded versions.\n") 
}
# single_year_to_five_year <- function (df, fifteen_to_49 = TRUE)
# {
#   df <- df %>% dplyr::mutate(age_group_label = cut(age, c(0,
#                                                           seq(5, 85, 5) - 1), c(paste0(seq(0, 79, 5), "-", seq(5,
#                                                                                                                80, 5) - 1), "80+"), include.lowest = TRUE)) %>% dplyr::left_join(naomi::get_age_groups() %>%
#                                                                                                                                                                                    select(age_group, age_group_label)) %>% dplyr::select(-age_group_label)
#   if (fifteen_to_49) {
#     df %>% dplyr::filter(age %in% 15:49) %>% dplyr::select(-age)
#   }
#   else {
#     df %>% dplyr::select(-age)
#   }
# }

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fullweightedandunweighteddata_0210.rds") %>%
  filter(vars == "age") %>%
  mutate(method = ifelse(is.na(lower), "convenience", "RDS")) %>%
  rename(age = category) %>%
  moz.utils::separate_survey_id() %>%
  filter(kp == "FSW", 
         age %in% 15:49) %>%
  type.convert(as.is = T) %>%
  filter(!is.na(age)) %>%
  select(iso3, survey_id, year, age, n) %>% 
  single_year_to_five_year(T) %>%
  mutate(age_group = factor(age_group)) %>% 
  group_by(iso3, survey_id, year, age_group) %>%
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(year = factor(year))

dat <- crossing(age_group = dat$age_group,
                select(dat, iso3, survey_id, year)) %>%
  arrange(survey_id) %>%
  filter(!survey_id %in% c("ZAF1998ACA_FSW", "ETH2020ACA_FSW")) %>%
  left_join(dat)

spectrum_data_f <- readRDS("~/Downloads/spectrum_data_f.rds") %>% 
  filter(year %in% c(1993:2023)) %>% 
  ungroup() 

# mf_model w/year
mf_model <- spectrum_data_f %>%
  distinct(age_group, iso3, year, tpa) %>% 
  ungroup() %>%
  mutate(id.age =  factor(to_int(age_group))
  ) %>% 
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3 = area_id, everything()) %>% mutate(id.iso3 = factor(row_number()))) %>% 
  filter(year %in% c(1993:2023)) %>% 
  mutate(id.year = factor(year),
         idx = factor(row_number()))

# Restrict to 2 surveys
mf_model <- mf_model %>% 
  filter(
    (year == 2021 &
       iso3 == "BDI") |
      (year == 1993 &
         iso3 == "BEN")
  ) %>% 
  droplevels()

dat2 <- dat %>%
  filter(survey_id %in% c("BDI2021BBS_FSW", "BEN1993ACA_FSW")) %>%
  type.convert(as.is = T) %>%
  left_join(mf_model) %>% 
  droplevels() 

dat2$n[is.na(dat2$n)] <- 0



# TMB Data objects

M_obs <- sparse.model.matrix(~0 + idx, dat2) 

X_stand_in <- sparse.model.matrix(~0 + id.age, mf_model)
X_stand_in <- X_stand_in[,c(1:6)]

# Survey data 
observed_x <- matrix(dat2$n, nrow = length(unique(dat2$survey_id)), byrow = TRUE)
# observed_x[1,] = observed_x[2,]



# Offset
observed_totpop <- matrix(dat2$tpa, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
# observed_totpop[1,] = observed_totpop[2,]
newoffset2 <- log(observed_totpop) - log(observed_totpop[,7]) # Log-odds using the 7th age group as the base 


# # To remove the offset this produces a matrix of 0
# newoffset <- matrix(rep(0, nrow(dat2)), ncol = length(unique(mf_model$age_group)), byrow = T)



tmb_int <- list()

tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  
  R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))-1), "dgTMatrix"),
  
  logit_totpop = newoffset2
)


tmb_int$par <- list( 
  beta_0 = rep(0, ncol(X_stand_in))
  
)

tmb_int$random <- c(                        
  
)




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


# VGAM::vglm model

vgam_dat <- data.frame(observed_x) %>% 
  select(Y015_019 = X1, Y020_024 = X2, Y025_029 = X3, Y030_034 = X4, Y035_039 = X5, Y040_044 = X6, Y045_049 = X7)

offset2 <- rbind(newoffset2[,c(1:6)])

# No offset
model1 <-VGAM::vglm(cbind(Y015_019, Y020_024, Y025_029, Y030_034, Y035_039, Y040_044, Y045_049) ~ 1, family = multinomial, data = vgam_dat)
summary(model1)

# Yes offset
model2 <-VGAM::vglm(cbind(Y015_019, Y020_024, Y025_029, Y030_034, Y035_039, Y040_044, Y045_049) ~ 1 + offset(offset2), family = multinomial, data = vgam_dat)

summary(model2)

sd_report

