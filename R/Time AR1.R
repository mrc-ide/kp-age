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

# dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fullweightedandunweighteddata_0210.rds") %>%
#   filter(vars == "age") %>%
#   mutate(method = ifelse(is.na(lower), "convenience", "RDS")) %>%
#   rename(age = category) %>%
#   moz.utils::separate_survey_id() %>%
#   filter(kp == "FSW", 
#          # kp == "PWID",
#          # kp == "MSM",
#          age %in% 15:49) %>%
#   type.convert(as.is = T) %>%
#   filter(!is.na(age)) %>%
#   select(iso3, survey_id, year, age, n) %>% 
#   single_year_to_five_year(T) %>%
#   mutate(age_group = factor(age_group)) %>% #change to factor(age) for single year of age / factor(age_group) for age groups. 
#   group_by(iso3, survey_id, year, age_group) %>%
#   summarise(n = sum(n)) %>% 
#   ungroup() %>% 
#   mutate(year = factor(year))
# 
# 
# spectrum_data_f <- readRDS("~/Downloads/spectrum_data_f.rds") %>% 
#   filter(year %in% c(1993:2023)) %>% 
#   ungroup() 


############## Extract stratified by survey_city

kpdat <- readRDS("~/Downloads/all_recoded.rds") %>% 
  moz.utils::separate_survey_id() %>% 
  filter(kp == "FSW") %>% 
  group_by(survey_id, iso3, year, survey_city, age) %>% 
  summarise(fswcount = n()) %>% 
  ungroup() %>% 
  # single_year_to_five_year() %>% 
  mutate(age_group = age) %>% 
  filter(!age_group < 15 | age_group > 49) %>% 
  rename(area_name = survey_city) %>% 
  group_by(survey_id, iso3, year, area_name, age_group) %>% 
  summarise(fswcount = sum(fswcount)) %>% 
  ungroup() %>% 
  group_by(survey_id, iso3, year, area_name) %>% 
  mutate(estimate = fswcount/sum(fswcount) ) %>% 
  ungroup()


# spectrum_data_multipliers <- spectrum_data_f %>% 
#   filter(year %in% c(2005:2021)) %>% 
#   group_by(iso3, age_group) %>% 
#   mutate(ratio = tpa/tpa[year == 2021]) %>% 
#   ungroup() %>% 
#   group_by(iso3, year) %>% 
#   mutate(sum_tpa = sum(tpa)) %>% 
#   ungroup() %>% 
#   select(iso3, year, age_group, tpa, sum_tpa, ratio) %>% 
#   filter(iso3 == "MWI")
  
area_dictionary <- read_csv("~/Downloads/area_dictionary.csv")

areas3 <- readRDS("~/Downloads/areas 4.rds") 

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
  


rightareas1 <- kpdat %>% 
  # filter(survey_id %in% c("MWI2006BBS_FSW", "ZAF2018BBS_FSW", "BDI2021BBS_FSW", "BEN1993ACA_FSW", "BEN2005ACA_FSW", "BEN2008ACA_FSW", "BEN2012BBS_FSW", "MWI2013BBS_FSW", "MWI2019BBS_FSW", "ZAF2014BBS_FSW", "ZAF2017ACA_FSW", "ZAF2012ACA_FSW", "BEN2015BBS_FSW", "BEN2017ACA_FSW")) %>% 
  select(iso3, area_name) %>% 
  arrange(iso3, area_name) %>% 
  distinct() %>% 
  mutate(area_name = case_when(area_name == "Johannesburg" ~ "Johannesburg MM",
                               area_name == "Durban" ~ "eThekwini MM",
                               area_name == "Cape Town" ~ "Cape Town MM",
                               area_name == "DJOUGOU MADINA" ~ "DJOUGOU",
                               area_name == " ATHIEME" ~ "ATHIEME",
                               TRUE ~ area_name)) %>% 
  left_join(areas3$MWI %>% 
              bind_rows(areas3$ZAF) %>% 
              bind_rows(areas3$BDI) %>% 
              bind_rows(areas3$BEN) %>% 
              bind_rows(areas3$BEN %>% mutate(area_name = toupper(area_name))) %>% 
              select(area_name, area_id) %>% 
              sf::st_drop_geometry() %>% 
              separate(area_id, into = c("iso3", NA), sep = "_", remove = F) %>% 
              distinct()) 

rightareas2 <- rightareas1 %>% 
  filter(is.na(area_id)) %>% select(-area_id) %>% 
  mutate(area_name = ifelse(area_name == " ATHIEME", "ATHIEME", area_name)) %>% 
  left_join(area_dictionary %>% select(iso3 , area_name = area_name2, area_id) %>% sf::st_drop_geometry()) %>% 
  bind_rows(rightareas1 %>% filter(!is.na(area_id))) %>% 
  separate(area_id, into = c(NA, "area_level", NA), sep = "_", remove = F) %>% 
  mutate(area_id = ifelse(is.na(area_id), iso3, area_id),
         area_name = ifelse(area_name == "", iso3, area_name)) %>% 
  type.convert(as.is = T) %>% 
  group_by(area_name) %>% 
  filter(area_level == max(area_level)) %>% 
  ungroup()
  

parent_areas <- areas3 %>% lapply(select, "area_id", "parent_area_id") %>% lapply(st_drop_geometry) %>% bind_rows()


# Make simulated data: 

# # Set seed for reproducibility
# set.seed(123)
# 
# weights <- c(0.8, 1.2, 1.2, 1.0, 0.9, 0.7, 0.5)  # agegp weighting
# 
# survey_data <- data.frame(survey_id = character(), age_group = character(), count = integer(), stringsAsFactors = FALSE)
# 
# for (survey_id in survey_ids) {
#   # Generate base counts with weights
#   base_counts <- sample(100:300, length(age_groups), replace = TRUE)
#   weighted_counts <- round(base_counts * weights)
#   
#   # Ensure total count is greater than 500
#   while (sum(weighted_counts) <= 500) {
#     base_counts <- sample(100:300, length(age_groups), replace = TRUE)
#     weighted_counts <- round(base_counts * weights)
#   }
#   
#   temp_data <- data.frame(
#     survey_id = survey_id,
#     age_group = age_groups,
#     count = weighted_counts
#   )
#   
#   survey_data <- rbind(survey_data, temp_data)
# }
# 
# survey_data <- survey_data %>% 
  # mutate(area_id = case_when(survey_id %in% c("Survey_2", "Survey_1", "Survey_3", "Survey_4", "Survey_5") ~ "ZAF",
  #                            TRUE ~ "BEN")) %>%
  # mutate(year = case_when(survey_id %in% c("Survey_2", "Survey_1") ~ 2000,
  #                            survey_id %in% c("Survey_3", "Survey_4") ~ 2003, survey_id %in% c("Survey_5") ~ 2009,
  #                            survey_id %in% c("Survey_6") ~ 2014,
  #                            survey_id %in% c("Survey_7") ~ 2019,
  #                            survey_id %in% c("Survey_8", "Survey_9") ~2021,
  #                            TRUE ~ 1995)) %>%
#   mutate(area_name = area_id,
#          iso3 = area_id)

survey_data <- data.frame(rmultinom(n = 23, size = 1000, prob = c(0.12, 0.2, 0.25, 0.17, 0.12, 0.08, 0.06))) %>% 
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
                          TRUE ~ 1995)) %>% 
  mutate(area_name = area_id,
         iso3 = area_id)

restricted_kpdat <- survey_data 

restricted_kpdat <- kpdat %>% 
  # filter(survey_id %in% c("BEN1993ACA_FSW", "BEN1995ACA_FSW", "BEN1998ACA_FSW", "BEN2002ACA_FSW", "BEN2005ACA_FSW", "BFA2022BBS_FSW", "BFA2005BBS_FSW", "BFA2002BBS_FSW", "CIV2017ACA_FSW", "CIV2020BBS_FSW", "COD2022BBS_FSW",  "ZAF2018BBS_FSW", "ZAF2012ACA_FSW", "ZAF2014BBS_FSW")) %>%
  # filter(!survey_id %in% c("BEN2008ACA_FSW", "BEN2015BBS_FSW", "ZAF1998ACA_FSW")) %>% 
  filter(survey_id %in% c("BEN1993ACA_FSW", "ZAF2014BBS_FSW", "ZAF2018BBS_FSW")) %>%
  # filter(survey_id == "MWI2006BBS_FSW" | survey_id == "ZAF2018BBS_FSW") %>% 
  # left_join(area_dictionary %>% select(iso3, area_name = area_name2, area_id)) 
  mutate(area_name = ifelse(area_name == " ATHIEME", "ATHIEME", area_name)) %>% 
  left_join(rightareas2 %>% select(iso3, area_name , area_id)) %>% 
  mutate(area_id = ifelse(is.na(area_id), iso3, area_id),
         area_id = ifelse(iso3 == "BDI", iso3, area_id),
         area_name = ifelse(is.na(area_name), iso3, area_name)) %>% 
  ungroup() %>% 
  distinct() #%>% 
  # left_join(parent_areas) %>% 
  # group_by(survey_id, area_id) %>% 
  # mutate(sum = sum(fswcount)) %>% 
  # ungroup() %>% 
  # mutate(area_id2 = ifelse(sum < 30, parent_area_id, area_id)) %>% 
  # group_by(iso3, survey_id, year, area_id2, age_group) %>% 
  # summarise(fswcount = sum(fswcount)) %>% 
  # ungroup() %>% 
  # group_by(iso3, survey_id, year, area_id2) %>% 
  # mutate(denominator = sum(fswcount),
  #        estimate = fswcount/sum(fswcount)) %>% 
  # ungroup() %>% 
  # rename(area_id = area_id2) %>% 
  # left_join(parent_areas) %>% 
  # group_by(survey_id, area_id) %>% 
  # mutate(sum = sum(fswcount)) %>% 
  # ungroup() %>% 
  # mutate(area_id2 = ifelse(sum < 30, parent_area_id, area_id),
  #        area_id2 = ifelse(is.na(area_id2), iso3, area_id2)) %>% 
  # group_by(iso3, survey_id, year, area_id2, age_group) %>% 
  # summarise(fswcount = sum(fswcount)) %>% 
  # ungroup() %>% 
  # group_by(iso3, survey_id, year, area_id2) %>% 
  # mutate(denominator = sum(fswcount),
  #        estimate = fswcount/sum(fswcount)) %>% 
  # ungroup() %>% 
  # rename(area_id = area_id2) %>% 
  # left_join(parent_areas) %>% 
  # group_by(survey_id, area_id) %>% 
  # mutate(sum = sum(fswcount)) %>% 
  # ungroup() %>% 
  # mutate(area_id2 = ifelse(sum < 30, parent_area_id, area_id),
  #        area_id2 = ifelse(is.na(area_id2), iso3, area_id2)) %>% 
  # group_by(iso3, survey_id, year, area_id2, age_group) %>% 
  # summarise(fswcount = sum(fswcount)) %>% 
  # ungroup() %>% 
  # group_by(iso3, survey_id, year, area_id2) %>% 
  # mutate(denominator = sum(fswcount),
  #        estimate = fswcount/sum(fswcount)) %>% 
  # ungroup()  %>% 
  # rename(area_id = area_id2) %>% 
  # mutate(area_name = area_id)

restricted_kpdat %>% ungroup() %>% group_by(survey_id, area_id) %>% summarise(sum = sum(fswcount))


dat <- crossing(age_group = unique(restricted_kpdat$age_group),
                select(restricted_kpdat, iso3, survey_id, area_name, area_id, year)) %>%
  arrange(survey_id) %>%
  left_join(restricted_kpdat)
# %>% 
  # mutate(age = age_group,
  #        age2 = age) %>% 
  # select(-age_group) %>% 
  # single_year_to_five_year() %>% 
  # rename(age = age2)
  



mf_model <- crossing(distinct(dat, iso3, area_id),
         year = 1993:2023,
         age_group = unique(pop5$age_group)
         ) %>%
  left_join(distinct(dat, age_group)) %>% 
  # filter(iso3 == "BEN", year == 1993, area_id == "BEN") %>% 
  left_join(pop5) %>%
  left_join(
    pop5 %>%
      filter(area_id == iso3) %>% 
      # filter(area_id == "ZAF") %>%
      rename(nat_tpa = local_tpa) %>%
      select(iso3, year, age_group, nat_tpa)
  ) %>%
  mutate(id.age =  factor(to_int(age_group))) %>% 
  # mutate(id.age =  factor(to_int(age))) %>% 
  ungroup() %>% 
  mutate(idx = factor(row_number()),
         local_tpa = ifelse(is.na(local_tpa), nat_tpa, local_tpa),
         id.year = factor(year)
         # local_tpa2 = local_tpa/5
         )
  
  
  

dat2 <- dat %>%
  type.convert(as.is = T) %>%
  left_join(mf_model) %>% 
  rename(n = fswcount) %>%
  # filter(iso3 == "ZAF") %>%
  arrange(survey_id, area_name) %>% 
  # filter(!survey_id %in% c("BEN2008ACA_FSW", "BEN2012BBS_FSW", "BEN2022BBS_FSW")) %>% 
  mutate(area_name_matrix = paste0(survey_id, "_", area_name)) %>%
  # arrange(area_name_matrix) %>% 
  # filter(!(area_name == "")) %>% 
  # group_by(area_name_matrix) %>% 
  # mutate(sum = sum(n)) %>% 
  # filter(!sum<50) %>% 
  ungroup()
  # rownames_to_column() 
  # #%>% 
# droplevels() 

dat2$n[is.na(dat2$n)] <- 0


######## TMB Inputs

M_obs <- sparse.model.matrix(~0 + idx, dat2) 

X_stand_in <- sparse.model.matrix(~0 + id.age, mf_model)
X_stand_in <- X_stand_in[,c(1,2, 4:7)]
# X_stand_in <- X_stand_in[,c(1:6)]
X_stand_in <- X_stand_in[,c(1:34)]

# Survey data 
observed_x <- matrix(dat2$n, nrow = length(unique(dat2$area_name_matrix)), byrow = TRUE)
# observed_x[1,] = observed_x[2,]
# set rownames 


# Offset
observed_totpop <- matrix(dat2$local_tpa, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
observed_totpop <- matrix(dat2$local_tpa2, ncol = length(unique(mf_model$age)), byrow = TRUE)
# observed_totpop[1,] = observed_totpop[2,]
newoffset2 <- log(observed_totpop)
newoffset2 <- log(observed_totpop) - log(observed_totpop[,7]) # Log-odds using the 7th age group as the base 
newoffset2 <- log(observed_totpop) - log(observed_totpop)
newoffset2 <- log(observed_totpop) - log(observed_totpop[,3]) 


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
  logit_eta2_phi_age = 0,
  lag_logit_eta2_phi_age = 0,
  eta2 = array(0, c(ncol(Z_period), ncol(Z_age))),
  log_sigma_eta2 = 0,
  # log_prec_eta2 = 0,
  # logit_eta2_phi_period = 0
  lag_logit_eta2_phi_period = 0
  
  
)

tmb_int$random <- c(                        
  "beta_0",
  "eta2"
  
)




tmb_unload("flib_time_AR1_breaking.cpp")

lapply(list.files("src/", pattern = "\\.o|\\.so", full.names = T), file.remove)


TMB::compile("src/flib_time_AR1_breaking.cpp", flags = "-w")

dyn.load(dynlib("src/flib_time_AR1_breaking"))



f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "flib_time_AR1_breaking",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})


if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}



obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "flib_time_AR1_breaking",
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

# local_rpt <- sd_report 
# nat_rpt <- sd_report


mat <- matrix(1:64, nrow = 8)
dimnames(estimates) <- list("area_name_matrix" = unique(dat2$area_name_matrix),
                      "age_group" = unique(dat2$age_group))

modpreds <- as.data.frame.table(estimates, responseName = "est") %>% 
  separate(area_name_matrix, into = c("iso3", NA), sep = 3, remove = F)


dat2 %>%
  left_join(modpreds) %>% 
ggplot(aes(group = area_name_matrix)) +
  geom_point(aes(x = age_group, y = estimate, size = n, color = area_name_matrix), show.legend = F) + 
  geom_line(aes(x = age_group, y = est, color = area_name_matrix), show.legend =F) + 
  facet_wrap(~iso3)
  



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