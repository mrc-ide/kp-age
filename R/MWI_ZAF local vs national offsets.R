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
         # kp == "PWID",
         # kp == "MSM",
         age %in% 15:49) %>%
  type.convert(as.is = T) %>%
  filter(!is.na(age)) %>%
  select(iso3, survey_id, year, age, n) %>% 
  single_year_to_five_year(T) %>%
  mutate(age_group = factor(age_group)) %>% #change to factor(age) for single year of age / factor(age_group) for age groups. 
  group_by(iso3, survey_id, year, age_group) %>%
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(year = factor(year))

dat <- crossing(age_group = dat$age_group,
                select(dat, iso3, survey_id, year)) %>%
  arrange(survey_id) %>%
  left_join(dat)


spectrum_data_f <- readRDS("~/Downloads/spectrum_data_f.rds") %>% 
  filter(year %in% c(1993:2023)) %>% 
  ungroup() 

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


############## Extract stratified by survey_city

kpdat <- readRDS("~/Downloads/all_recoded.rds") %>% 
  moz.utils::separate_survey_id() %>% 
  filter(kp == "FSW") %>% 
  group_by(survey_id, iso3, year, survey_city, age) %>% 
  summarise(fswcount = n()) %>% 
  ungroup() %>% 
  single_year_to_five_year() %>% 
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
  



restricted_kpdat <- kpdat %>% 
  # filter(survey_id == "MWI2006BBS_FSW" | survey_id == "ZAF2018BBS_FSW") %>% 
  # left_join(area_dictionary %>% select(iso3, area_name = area_name2, area_id)) 
  left_join(rightareas2 %>% select(iso3, area_name , area_id)) %>% 
  mutate(area_id = ifelse(is.na(area_id), iso3, area_id),
         area_id = ifelse(iso3 == "BDI", iso3, area_id),
         area_name = ifelse(is.na(area_name), iso3, area_name))

dat <- crossing(age_group = unique(restricted_kpdat$age_group),
                select(restricted_kpdat, iso3, survey_id, area_name, area_id, year)) %>%
  arrange(survey_id) %>%
  left_join(restricted_kpdat)



mf_model <- crossing(distinct(dat, iso3, area_id),
         year = 1993:2023,
         age_group = unique(pop5$age_group)
         ) %>%
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
  ungroup() %>% 
  mutate(idx = factor(row_number()),
         local_tpa = ifelse(is.na(local_tpa), nat_tpa, local_tpa))
  
  
  

dat2 <- dat %>%
  type.convert(as.is = T) %>%
  left_join(mf_model) %>% 
  rename(n = fswcount) %>%
  # filter(iso3 == "ZAF") %>%
  arrange(survey_id, area_name) %>% 
  filter(!survey_id %in% c("BEN2008ACA_FSW", "BEN2012BBS_FSW", "BEN2022BBS_FSW")) %>% 
  mutate(area_name_matrix = paste0(survey_id, "_", area_name)) %>% 
  arrange(area_name_matrix)
  # rownames_to_column() 
  # #%>% 
# droplevels() 

dat2$n[is.na(dat2$n)] <- 0


######## TMB Inputs

M_obs <- sparse.model.matrix(~0 + idx, dat2) 

X_stand_in <- sparse.model.matrix(~0 + id.age, mf_model)
# X_stand_in <- X_stand_in[,c(1,2, 4:7)]
X_stand_in <- X_stand_in[,c(1:6)]

# Survey data 
observed_x <- matrix(dat2$n, nrow = length(unique(dat2$area_name_matrix)), byrow = TRUE)
# observed_x[1,] = observed_x[2,]



# Offset
observed_totpop <- matrix(dat2$local_tpa, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
# observed_totpop[1,] = observed_totpop[2,]
newoffset2 <- log(observed_totpop)
newoffset2 <- log(observed_totpop) - log(observed_totpop[,7]) # Log-odds using the 7th age group as the base 
newoffset2 <- log(observed_totpop) - log(observed_totpop)

tmb_int <- list()

tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  
  R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))-1), "dgTMatrix"),
  # R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))), "dgTMatrix"),
  
  # Z_yeariso = Z_yeariso,
  # logit_totpop = newoffset2
  logit_totpop = as.vector(t(newoffset2))
)

tmb_int$par <- list( 
  beta_0 = rep(0, ncol(X_stand_in))
  # interaction = rep(0, ncol(Z_yeariso))
  
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

local_rpt <- sd_report 
nat_rpt <- sd_report
