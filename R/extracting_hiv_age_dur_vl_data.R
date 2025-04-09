library(orderly2)
library(tidyverse)
library(glue)

# Pull all new data from Packit (this includes everything in both 0_extract_raw and 1_recode)
orderly2::orderly_metadata_extract(
  name = "kipling",
  allow_remote = TRUE, fetch_metadata = TRUE)

orderly2::orderly_location_pull(NULL, location = "kipling_packit")


# View the latest version of your recoded dataset[s]
# specify your survey_id e.g. 
survey_id <- read_csv("src/0_extract_raw/survey_paths.csv") %>% 
  # filter(survey_id == "BDI2021BBS_PWID") %>% 
  pull(survey_id) %>% unique()

survey_id <- read_csv("src/0_extract_raw/survey_paths.csv") %>% 
  filter(!survey_id %in% c("KEN2021ACA_FSW", "BFA2017ACA_MSM", "CIV2017ACA_MSM", "MLI2017ACA_MSM", "TGO2017ACA_MSM")) %>% 
  pull(survey_id) %>% unique()

survey_id <- extract_surveys

survey_id <- "NGA2014BBS_FSW"

# Gives you the most recent packet from the 1_recode archive for the sepecified survey_id(s)
files <- lapply(survey_id, function(x) {
  wow <- glue::glue('latest(name == "1_recode" && parameter:survey_id == "{x}")')
  orderly2::orderly_search(wow)
})

files <- lapply(survey_id, function(x) {
  wow <- glue::glue('latest(name == "0_extract_raw" && parameter:survey_id == "{x}")')
  orderly2::orderly_search(wow)
})

files <- Filter(Negate(is.na), files)

# Pulls out the relevant bits

# rawdatas <- list()
cleandatas <- list()
for (i in files){
  id = i

  path = paste0(getwd(), "/archive/1_recode/", id)
# 
#   raw_data <- readRDS(paste0(path, "/raw_data.rds"))
#   rawdatas[[i]] <- raw_data

  clean_data <- read_csv(paste0(path, "/dat.csv"))
  cleandatas[[i]] <- clean_data
}


# cleandatas <- list()
# rawdatas <- list()
# for (i in files){
#   id = i
#   
#   path = paste0(getwd(), "/archive/0_extract_raw/", id)
#   
#   raw_data <- readRDS(paste0(path, "/raw_data.rds"))
#   raw_data <- readRDS(paste0(path, "/dat.rds"))
#   rawdatas[[i]] <- raw_data
#   
#   # clean_data <- read_csv(paste0(path, "/dat.csv"))
#   # cleandatas[[i]] <- clean_data
# }
cleandatas_compact <- cleandatas
cleandatas_compact <-  Filter(function(df) all(c("age", "hiv") %in% colnames(df)), cleandatas)

desired_columns <- c("survey_id", "survey_city", "area_id", "age", "sex", "gender","hiv", 
                     "coupon1", "coupon2", "coupon3", "coupon4", "coupon5", "coupon6", 
                     "network_size", "own_coupon", "vl", "vl_result_count", "vl_result_detectable", "vl_results_suppressed", 
                     "age_fs_gift", "age_fs_paid", "age_fs_paidfor_anal", "age_fs_paidfor_vag", "age_startsw", "age_fs_paidorgift","duration_yr", "duration_mnth", 
                     "age_fs_man", "age_fs_man_anal", "age_fs_man_anal_cat", "age_fs_man_cat",
                     "age_inject", "inject_dur")

# Filter columns in each data.frame
filteredcleandata <- lapply(cleandatas_compact, function(df) df[ , intersect(names(df), desired_columns), drop = FALSE])

filteredcleandata <- lapply(cleandatas_compact, function(df) {
  # Select only available desired columns
  df <- df[ , intersect(names(df), desired_columns), drop = FALSE]
  
  # Find coupon columns that exist in the data.frame
  coupon_columns <- intersect(names(df), c("coupon1", "coupon2", "coupon3", 
                                           "coupon4", "coupon5", "coupon6", "own_coupon"))
  
  # Convert coupon columns to character
  df[coupon_columns] <- lapply(df[coupon_columns], as.character)
  
  coupon_columns <- intersect(names(df), c("network_size"))
  
  # Convert coupon columns to character
  df[coupon_columns] <- lapply(df[coupon_columns], as.numeric)
  
  coupon_columns <- intersect(names(df), c("hiv"))
  
  # Convert coupon columns to character
  df[coupon_columns] <- lapply(df[coupon_columns], as.numeric)
  
  
  coupon_columns <- intersect(names(df), c("gender"))
  
  # Convert coupon columns to character
  df[coupon_columns] <- lapply(df[coupon_columns], as.factor)
  
  
  coupon_columns <- intersect(names(df), c("vl"))
  
  # Convert coupon columns to character
  df[coupon_columns] <- lapply(df[coupon_columns], as.numeric)
  
  return(df)
})

filteredcleandata <- bind_rows(filteredcleandata)

filteredcleandata <- filteredcleandata %>% 
  moz.utils::separate_survey_id()

saveRDS(filteredcleandata, "~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/age_duration_hiv_data_extract_1703.rds")

meta_data <- read_csv(paste0(path,  "/",  survey_id, "_meta.csv"))

### If extracting multiple files: 


metadatas <- list()

# for (i in survey_id) & for (i in files){
# # for (i in files){
#   id = i
#   
#   path = paste0(getwd(), "/archive/1_recode/", id)
#   
#   raw_data <- readRDS(paste0(path, "/raw_data.rds"))
#   rawdatas[[i]] <- raw_data
#   
#   clean_data <- read_csv(paste0(path, "/dat.csv"))
#   cleandatas[[i]] <- clean_data
#   
#   meta_data <- read_csv(paste0(path,  "/",  survey_id, "_meta.csv"))
#   metadatas[[i]] <- meta_data
#   
# }

for (i in seq_along(survey_id)) {
  id <- survey_id[i]
  file <- files[i]
  
  path <- paste0(getwd(), "/archive/1_recode/", file)
  
  # Read raw data
  raw_data <- readRDS(paste0(path, "/raw_data.rds"))
  rawdatas[[i]] <- raw_data
  
  # Read clean data
  clean_data <- read_csv(paste0(path, "/dat.csv"))
  cleandatas[[i]] <- clean_data
  
  # Read metadata
  meta_data <- read_csv(paste0(path, "/", id, "_meta.csv"))
  metadatas[[id]] <- meta_data
}
