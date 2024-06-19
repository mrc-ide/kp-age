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
  single_year_to_five_year() %>% 
  group_by(survey_id, iso3, year, trans, age_group) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id, iso3, year, trans) %>% 
  mutate(full_sample = sum(n)) %>% 
  ungroup() %>%  
  mutate(estimate = n/full_sample) %>% 
  # distinct() %>% 
  # bind_rows(data.frame(survey_id = "MWI2016ACA_MSM", iso3 = "MWI", year = 2016, age = 29, trans = factor(0), estimate = 0, n = 0)) %>% 
  # bind_rows(data.frame(survey_id = "RWA2015BBS_MSM", iso3 = "MWI", year = 2016, age = 41, trans = factor(0), estimate = 0, n = 0)) %>% 
  # bind_rows(data.frame(survey_id = "SLE2021BBS_MSM", iso3 = "MWI", year = 2016, age = 49, trans = factor(0), estimate = 0, n = 0)) %>% 
  # bind_rows(data.frame(survey_id = "UGA2012BBS_MSM", iso3 = "MWI", year = 2016, age = 54, trans = factor(0), estimate = 0, n = 0)) %>% 
  group_by(survey_id, age_group) %>% 
  mutate(normalised = estimate/estimate[trans == 0]) %>% 
  mutate(log_or = log((estimate/(1-estimate))/(estimate[trans == 0]/(1 - estimate[trans == 0])))) %>% 
  ungroup() %>% 
  select(-n) %>% 
  distinct() %>% 
  mutate(agegroup = to_int(age_group)) %>% 
  filter(trans == 1) %>% 
  ggplot() +
  # geom_boxplot(aes(x = age_group, y = normalised)) +
  # geom_boxplot(aes(x = age_group, y = log_or)) +
  geom_line(aes(x = agegroup, y = log_or, color = survey_id)) +
  # geom_line(aes(x = agegroup, y = normalised, color = survey_id)) +
  geom_hline(yintercept = log(1), linewidth = 0.5) +
  moz.utils::standard_theme() +
  labs(y = "TGW:MSM Log OR", x = "Age Group") +
  theme(aspect.ratio=1) + 
  theme(axis.text.x = element_text(angle = 45))
  # faceta wrap(~survey_id)
  
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
  
  