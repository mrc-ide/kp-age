library(tidyverse)
library(RDS)


dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agehiv_data_extract_160925.rds")

dat %>% filter(kp == "FSW") %>% count(survey_id)

#FSW surveys to add

c("AGO2018PLACE_ALL", "BFA2001PLACE_KP", "COD2012BBS_FSW", "COD2019BBS_FSW", "GHA2005PLACE_FSW", "GNB2018BBS_FSW", "GNB2021BBS_FSW", "GNB2022BBS_FSW", "GNQ2021BBS_FSW", "KEN2016PLACE_FSW", "KEN2021ACA_FSW", "KEN_McClelland", "MLI2005BBS_FSW", "MOZ2012ACA_FSW", "MOZ2017PLACE_FSW", "MOZ2021BBS_FSW", "NAM2017PLACE_KP", "NGA2020BBS_FSW", "SEN2002BBS_FSW", "SEN2005BBS_FSW", "SEN2019BBS_FSW", "TGO2002BBS_FSW", "TGO2005BBS_FSW", "UGA2018PLACE_KP", "ZAF1998ACA_FSW", "ZAF1999PLACE_FSW", "ZAF2002PLACE_FSW", "ZAF2010ACA2_FSW", "ZAF2012ACA_FSW", "ZAF2017ACA_FSW", "ZAF2018BBS_FSW", "ZAF2019ACA_FSW", "ZMB2017BBS_FSW" )

# MSM surveys to add

c("AGO2011BBS_MSM", "AGO2018PLACE_ALL", "BFA2017ACA_MSM", "CIV2017ACA_MSM", "CIV2018PLACE_FSW", "GNB2018BBS_MSM", "GNB2021BBS_MSM", "GNB2022BBS_MSM", "GNQ2021BBS_MSM", "KEN2019ACA_MSM", "LSO2019BBS_MSM", "MLI2017ACA_MSM", "MOZ2017PLACE_MSM", "MOZ2021BBS_MSM", "NAM2017PLACE_KP", "NGA2020BBS_MSM", "RWA2015BBS_MSM", "SEN2007ACA_MSM", "TGO2017ACA_MSM", "UGA2018PLACE_KP", "ZAF2013BBS_MSM", "ZAF2017ACA_MSM", "ZAF2017BBS_MSM", "ZAF2019BBS_MSM", "ZMB2021BBS_MSM", "ZWE2019BBS_MSM")

# TGW Surveys to add
c("AGO2018PLACE_ALL", "MOZ2017PLACE_TGM", "MOZ2017PLACE_TGW", "NAM2017PLACE_KP", "NGA2020BBS_TG", "UGA2018PLACE_KP", "UGA2021BBS_TGW")

# CFSW Surveys to add
c("BEN2008BBS_CFSW", "CIV1999ACA_CFSW", "GHA2011BBS_CFSW", "GHA2015BBS_NPP", )

#RM c("KEN2021ACA_FSW", "BFA2017ACA_MSM", "CIV2017ACA_MSM", "KEN2016ACA_MSM", "MLI2017ACA_MSM", "TGO2017ACA_MSM", "ZAF2016ACA_MSM")

# Check what
# Benin Data Demonstration is
#PLACE surveys with client data


dat <- dat %>% 
  group_split(survey_id)

compact_dat <- lapply(dat, function(df) {
  df %>% select(where(~ !all(is.na(.x)))) %>% 
    rowid_to_column() 
})


process_one_survey <- function(df,
                               id_var = "rowid",
                               own_coupon_var = "own_coupon",
                               network_size_var = "network_size",
                               N = 3200,
                               ss_per_iter = 1000) {
  # Check required columns (per your request)
  needed <- c(own_coupon_var, "coupon1", network_size_var)
  if (!all(needed %in% names(df))) {
    return(NULL)  # skip this survey
  }
  # Also ensure subject_id exists, since you pass it to rid.from.coupons()
  if (!id_var %in% names(df)) {
    return(NULL)  # skip if subject_id is missing
  }
  
  if (unique(df$survey_id) %in% c("COD2022PSE_FSW", "COD2022PSE_MSM", "COD2022PSE_PWID", "COG2017BBS_MSM", "COG2017BBS_PWID", "UGA2021BBS_MSM")) {
    return(NULL)  # skip if subject_id is missing
  }
  
  # Detect all coupon columns present: coupon1, coupon2, ..., coupon8 (or more)
  coupon_cols <- grep("^coupon\\d+$", names(df), value = TRUE)
  # Sort them numerically: coupon1, coupon2, ...
  coupon_cols <- coupon_cols[order(as.integer(gsub("^coupon", "", coupon_cols)))]
  
  if (length(coupon_cols) == 0) {
    return(NULL)  # nothing to do if no coupon columns detected
  }
  
  print(unique(df$survey_id))
  # Compute recruiter id from coupons
  df$recruiter.id <- rid.from.coupons(
    df,
    subject.id      = id_var,
    subject.coupon  = own_coupon_var,
    coupon.variables= coupon_cols
  )
  
  # Set max.coupons to the number we actually detected
  max_coupons <- length(coupon_cols)
  
  # Convert to rds.data.frame
  rds_df <- as.rds.data.frame(
    df,
    id             = id_var,
    recruiter.id   = "recruiter.id",
    network.size   = network_size_var,
    population.size= c(NA, NA, NA),
    max.coupons    = max_coupons,
    notes          = NULL,
    time           = NULL
  )
  
  # Derive seed and wave
  rds_df$seed <- get.seed.id(rds_df)
  rds_df$wave <- get.wave(rds_df)
  
  # Weights (Gile's SS)
  rds_df$weights_comp <- compute.weights(
    rds_df,
    weight.type = "Gile's SS",
    N = N,
    outcome.variable = "",
    number.ss.samples.per.iteration = ss_per_iter
  )
  
  rds_df
}


process_surveys <- lapply(compact_dat, process_one_survey)


process_surveys <- bind_rows(process_surveys) %>% as.data.frame()

rds_survs <- unique(process_surveys$survey_id)

process_surveys %>% filter(!is.na(weights), !survey_id == "BDI2021BBS_MSM") %>% pivot_longer(cols = c("weights", "weights_comp"), names_to = "weight_type", values_to = "value") %>% ggplot(aes(x = rowid, y = value)) + geom_point(aes(color = weight_type), position = position_dodge(width = 0.1)) + facet_wrap(~survey_id)

process_surveys %>% filter(survey_id == "ZMB2021BBS_PWID") %>% pivot_longer(cols = c("weights", "weights_comp"), names_to = "weight_type", values_to = "value") %>% ggplot(aes(x = rowid, y = value)) + geom_point(aes(color = weight_type), position = position_dodge(width = 0.1))

process_surveys %>% filter(survey_id == "ZMB2021BBS_PWID") %>% mutate(weight_diff = weights - weights_comp) %>% ggplot(aes(x = rowid, y = weight_diff)) + geom_point()


process_surveys %>% filter(survey_id == "BFA2022BBS_FSW") %>% mutate(weight_diff = weights - weights_comp) %>% ggplot(aes(x = rowid, y = weight_diff)) + geom_point()


fulldat <- compact_dat %>% bind_rows() %>% filter(!(survey_id %in% rds_survs)) %>% mutate(method = "other") %>% 
  bind_rows(process_surveys %>% as.data.frame() %>% 
              select(-rowid) %>% 
              mutate(method = "RDS")) 

