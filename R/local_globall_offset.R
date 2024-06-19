library(tidyverse)
library(sf)
library(dfertility)
library(moz.utils)
library(countrycode)
library(Matrix)
library(TMB)
library(VGAM)
library(multi.utils)

tmb_unload <- function(name) {   
  ldll <- getLoadedDLLs()   
  idx  <- grep(name, names(ldll))   
  for (i in seq_along(idx)) 
    dyn.unload(unlist(ldll[[idx[i]]])$path)
  cat('Unload ', length(idx), "loaded versions.\n") 
}

spec_paths <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2023 final shared/SSA", full.names = T)

spectrum_data <- lapply(spec_paths, naomi::extract_pjnz_naomi)

nga_spectrum_dat <- naomi::extract_pjnz_naomi("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2022 final shared/EPP-Gen/Nigeria_National_2022_06_07.PJNZ")

spectrum_data_offsets <- spectrum_data %>%
  bind_rows() %>%
  bind_rows(nga_spectrum_dat) %>%
  filter(age %in% 15:49,
         year > 1992,
         sex == "female") %>%
  single_year_to_five_year(T) %>%
  # mutate(age_group = age) %>%
  group_by(iso3, sex, year, age_group) %>%
  summarise(totpop = sum(totpop)) %>%
  group_by(iso3, sex, year) %>%
  mutate(national_tpa = totpop/sum(totpop)) %>%
  left_join(
    spectrum_data %>%
      bind_rows() %>%
      bind_rows(nga_spectrum_dat) %>%
      filter(age %in% 15:49,
             year > 1992,
             sex == "female") %>%
      single_year_to_five_year(T) %>%
      # mutate(age_group = age) %>%
  group_by(sex, year, age_group) %>%
  summarise(fulltotpop = sum(totpop)) %>%
  group_by(sex, year) %>%
  mutate(global_tpa = fulltotpop/sum(fulltotpop))) %>%
  left_join(
    spectrum_data %>%
      bind_rows() %>%
      bind_rows(nga_spectrum_dat) %>%
      filter(age %in% 15:49,
             year > 1992,
             sex == "female") %>%
      single_year_to_five_year(T) %>%
      # mutate(age_group = age) %>%
      group_by(sex, age_group) %>%
      summarise(yearlesstotpop = sum(totpop)) %>%
      group_by(sex) %>%
      mutate(yearless_tpa = yearlesstotpop/sum(yearlesstotpop))) %>%
  select(-totpop) %>%
  ungroup() %>% 
  left_join(
    spectrum_data %>%
      bind_rows() %>%
      bind_rows(nga_spectrum_dat) %>%
      filter(age %in% 15:49,
             year  == 2020,
             sex == "female") %>%
      single_year_to_five_year(T) %>%
      # mutate(age_group = age) %>%
      group_by(sex, age_group) %>%
      summarise(totpop2020 = sum(totpop)) %>%
      group_by(sex) %>%
      mutate(tpa2020 = totpop2020/sum(totpop2020)) %>% 
      ungroup())


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
  # mutate(age_group = age) %>%
  mutate(age_group = factor(age_group)) %>% 
  group_by(iso3, survey_id, year, age_group) %>%
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(year = factor(year)) 
  # filter(survey_id %in% c("BFA2022BBS_FSW",  "CIV2020BBS_FSW",  "COD2019BBS1_FSW", "KEN2012ACA_FSW", "NGA2014BBS_FSW", "ZAF1998ACA_FSW", "ZMB2023BBS_FSW", "BEN2015BBS_FSW", "BDI2021BBS_FSW", "BEN1993ACA_FSW", "GIN2005BBS_FSW", "GIN2022BBS_FSW", "MWI2006BBS_FSW",  "MWI2013BBS_FSW",  "MWI2019BBS_FSW",  "NAM2019BBS_FSW", "NGA2020BBS_FSW", "SEN2002BBS_FSW",  "SEN2005BBS_FSW"))


dat2 <- crossing(age_group = dat$age_group,
                 select(dat, iso3, survey_id, year)) %>%
  left_join(dat %>%
              # mutate(age_group = factor(age_group)) %>% 
  select(-year)) %>%
  separate_survey_id() %>%
  # mutate(year = as.integer(year)) %>% 
  left_join(spectrum_data_offsets) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(survey_id) %>% 
  mutate(pa = n/sum(n)) %>% 
  ungroup()

# FOr full age dist plots
dat2 <- crossing(age_group = extract_numeric(dat$age_group),
                 select(dat, iso3, survey_id, year)) %>%
  left_join(dat %>%
              mutate(age_group = extract_numeric(dat$age_group)) %>%
              select(-year)) %>%
  separate_survey_id() %>%
  # mutate(year = as.integer(year)) %>% 
  left_join(spectrum_data_offsets) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(survey_id) %>% 
  mutate(pa = n/sum(n)) %>% 
  ungroup()

dat2[dat2 == 0] <- 0.00000001


test <- dat2 %>% 
  group_by(survey_id) %>% 
  mutate(nationallor = log((pa/(1-pa))/(national_tpa/(1-national_tpa))),
         globallor = log((pa/(1-pa))/(global_tpa/(1-global_tpa))),
         yearlesslor = log((pa/(1-pa))/(yearless_tpa/(1-yearless_tpa))),
         national_lor = log((pa/(pa[age_group == "Y045_049"]))/(national_tpa/national_tpa[age_group == "Y045_049"])),
         global_lor = log((pa/pa[age_group == "Y045_049"])/(global_tpa/global_tpa[age_group == "Y045_049"])),
         global2020_lor = log((pa/pa[age_group == "Y045_049"])/(tpa2020/tpa2020[age_group == "Y045_049"]))) %>% 
         # step1 = (pa/(1-pa))/((pa[age_group == "Y045_049"])/(1-pa[age_group == "Y045_049"])),
         # step2 = (national_tpa/(1-national_tpa))/((national_tpa[age_group == "Y045_049"])/(1-national_tpa[age_group == "Y045_049"])),
         # national_base = log((pa/(1-pa))/((pa[age_group == "Y045_049"])/(1-pa[age_group == "Y045_049"]))/(national_tpa/(1-national_tpa))/((national_tpa[age_group == "Y045_049"])/(1-national_tpa[age_group == "Y045_049"]))),
         # global_base = log((pa/(1-pa))/((pa[age_group == "Y045_049"])/(1-pa[age_group == "Y045_049"]))/(global_tpa/(1-global_tpa))/((global_tpa[age_group == "Y045_049"])/(1-global_tpa[age_group == "Y045_049"]))),
         # yearless_base = log((pa/(1-pa))/((pa[age_group == "Y045_049"])/(1-pa[age_group == "Y045_049"]))/(yearless_tpa/(1-yearless_tpa))/((yearless_tpa[age_group == "Y045_049"])/(1-yearless_tpa[age_group == "Y045_049"])))) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(national_lor, global_lor, global2020_lor), names_to = "offset", values_to = "lor")

summ <- test %>% 
  filter(!is.na(lor)) %>% 
  group_by(age_group, offset) %>% 
  summarize(mean = mean(lor), median = median(lor), sd = sd(lor)) %>% 
  ungroup()

test %>% 
  ggplot() +
  # lims(y = c(0,500)) +
  geom_boxplot(aes(y = lor, x = offset)) + 
  facet_wrap(~age_group) + 
  geom_label(data = summ, aes(x = offset, y = -5, 
                              label = paste("Mean: ", round(mean, 1), "\nMedian: ", round(median, 1), "\nSD: ", round(sd, 1))), size = 3) + 
  lims(y = c(-7, 3))


mf_model <- spectrum_data_offsets %>%
  distinct(age_group, iso3, year, national_tpa, global_tpa, tpa2020) %>% 
  ungroup() %>%
  mutate(id.age =  factor(to_int(age_group))
  ) %>% 
  left_join(read_sf(moz.utils::national_areas()) %>% select(iso3 = area_id, everything()) %>% mutate(id.iso3 = factor(row_number()))) %>% 
  filter(year %in% c(1993:2023)) %>% 
  mutate(id.year = factor(year),
         idx = factor(row_number()))
  

mf_model <- mf_model %>% 
  droplevels()

dat2 <- crossing(age_group = dat$age_group,
                 select(dat, iso3, survey_id, year)) %>%
  arrange(survey_id) %>%
  left_join(dat) %>% 
  filter(survey_id %in% c(
    #   "BDI2021BBS_FSW",
    "ZAF2018BBS_FSW"
  )) %>%
  type.convert(as.is = T) %>%
  left_join(mf_model) #%>% 
# droplevels() 

dat2$n[is.na(dat2$n)] <- 0




M_obs <- sparse.model.matrix(~0 + idx, dat2) 

X_stand_in <- sparse.model.matrix(~0 + id.age, mf_model)
X_stand_in <- X_stand_in[,c(1:6)]

# Survey data 
observed_x <- matrix(dat2$n, nrow = length(unique(dat2$survey_id)), byrow = TRUE)
# observed_x[1,] = observed_x[2,]



# Offset
observed_totpop <- matrix(dat2$tpa2020, ncol = length(unique(mf_model$age_group)), byrow = TRUE)
# observed_totpop[1,] = observed_totpop[2,]
newoffset2 <- log(observed_totpop) - log(observed_totpop[,7]) # Log-odds using the 7th age group as the base 
newoffset2 <- log(observed_totpop) - log(observed_totpop)

tmb_int <- list()

tmb_int$data <- list(   
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  
  R_beta = as(diag(1, nrow = (length(unique(dat2$id.age)))-1), "dgTMatrix"),
  
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

tpa2020_rpt <- sd_report
global_tparpt <- sd_report
national_tparpt <- sd_report


# comparing genpop distributions

dat2 %>% 
  filter(iso3 == "MWI" | iso3 == "MLI") %>%
  ggplot() + 
  geom_line(aes(x = age_group, y = national_tpa, color = survey_id), show.legend = T) + 
  geom_line(aes(x = age_group, y = tpa2020), color = "black")  
  # geom_line(aes(x = age_group, y = global_tpa, color = year ))

# ZAF2018BBS_FSW ; ZAF2014BBS_FSW
# MLI2005BBS_FSW ; MWI2006BBS_FSW 