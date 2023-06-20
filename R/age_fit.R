library(tidyverse)
library(sf)
library(dfertility)
library(multi.utils)
library(countrycode)
library(Matrix)
library(TMB)

geographies <- read_sf(moz.utils::national_areas()) %>%
  st_make_valid() %>%
  arrange(area_id) %>%
  mutate(
    id.iso3 = row_number()
  )

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

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/rds_agedata_1206.rds") %>%
  rename(age = category) %>%
  separate_survey_id() %>%
  filter(kp == "FSW",
         age %in% 15:49) %>%
  type.convert(as.is = T)

dat <- dat %>%
  select(iso3, survey_id, age, n)

dat <- dat %>%
  select(iso3, survey_id, age, n) %>%
  single_year_to_five_year() %>%
  group_by(iso3, survey_id, age_group) %>%
  summarise(n = sum(n))

mf_model <- crossing(
  # iso3 = ssa_iso3,
  # age_group = unique(# dat$age_group)
                     age = 15:49
                     ) %>%
  # left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
  ungroup() %>%
  mutate(id.age = factor(to_int(age)),
         # id.iso3 = factor(to_int(iso3)),
         idx = factor(row_number()))

dat <- dat %>%
  left_join(mf_model)

M_obs <- sparse.model.matrix(~0 + idx, dat)
Z_age <- sparse.model.matrix(~0 + id.age, mf_model)
# Z_spatial <- sparse.model.matrix(~0 + id.iso3, mf_model)

# x <- 0:34
# k <- seq(-15, 50, by = 5)
# spline_mat <- splines::splineDesign(k, x, ord = 4)
# spline_mat <- as(spline_mat, "sparseMatrix")

# Z_age <- Z_age %*% spline_mat

tmb_int <- list()



tst <- crossing(age = 15:49,
                survey_id = unique(dat$survey_id)) %>%
  left_join(dat %>% select(survey_id, age, n)) %>%
  select(survey_id, age, n) %>%
  arrange(survey_id, age)

tst[is.na(tst)] <- 0

norm_n <- tst %>%
  group_by(survey_id) %>%
  mutate(norm_n = n/sum(n)) %>%
  pull(norm_n)

norm_x <- matrix(norm_n, nrow = length(unique(dat$survey_id)), byrow = TRUE)

tmb_int$data <- list(
  M_obs = M_obs,
  observed_x = norm_x,
  Z_age = Z_age,
  R_age = dfertility::make_rw_structure_matrix(ncol(Z_age), 1, adjust_diagonal = TRUE)
  # Z_spatial = Z_spatial,
  # R_spatial = R_spatial
  
)

tmb_int$par <- list(
  beta_0 = 0,
  u_age = rep(0, ncol(Z_age)),
  log_prec_rw_age = 0
  # u_spatial_str = rep(0, ncol(Z_spatial)),
  # log_prec_spatial = 0
)

tmb_int$random <- c(
  "beta_0",
  "u_age"
  # "u_spatial_str"
)


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

hist(fit$sample$p_arr[1,])



x <- c(0, 1, 3, 5 ,12, 12, 14, 11, 10, 8, 5, 2,1, 0, 0)
prob <- c(0.05, 0.05, 0.05, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.05, 0.02, 0.03, 0.03, 0.01, 0.01)
sum(prob)
dmultinom(x, prob = prob)
