msmtg <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/msmtg_age_df_1211.csv")

msmtg %>% 
  separate_survey_id()

msmtg %>% 
  separate_survey_id() %>% 
  mutate(tgw = case_when(gender == 1 ~ 1,
                         gender == 0 ~ 0,
                         gender == 3 ~ 1,
                         gender == 4 ~ NA_integer_,
                         gender == 5 ~ NA_integer_),
         trans = case_when(gender == 1 ~ 1,
                           gender == 0 ~ 0,
                           gender == 3 ~ 1,
                           gender == 4 ~ 1,
                           gender == 5 ~ 1),
         tgm = case_when(gender == 1 ~ NA_integer_,
                         gender == 0 ~ 0,
                         gender == 3 ~ NA_integer_,
                         gender == 4 ~ 1,
                         gender == 5 ~ NA_integer_)) %>% 
  filter(kp == "MSM") %>% 
  filter(!is.na(tgw)) %>% 
  filter(!survey_id == "NAM2019BBS_MSM") %>% 
  mutate(tgw = factor(tgw),
         trans = factor(trans)) %>% 
  ggplot() + 
  geom_density(aes(x = age, color = trans)) +
  facet_wrap(~survey_id) 
  

msmtg %>% 
  separate_survey_id() %>% 
  filter(iso3 %in% c("COD", "LSO", "MWI", "SLE", "SWZ", "ZAF"),
         !survey_id %in%  c("MWI2016ACA_MSM", "ZAF2016ACA_MSM", "ZAF2013BBS_MSM")) %>% 
  mutate(year = ifelse(survey_id == "MWI2019BBS_MSM", 2020, year)) %>% 
  group_by(iso3, year, kp) %>% 
  ggplot() +
  geom_density(aes(x = age, color = kp)) +
  facet_wrap(~ iso3+ year)
  
  