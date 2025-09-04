fswdur_dat <- dat %>% 
  filter(kp == "FSW", !survey_id == "ETH2020ACA_FSW") %>% 
  mutate(duration_calc = age - age_fs_paid,
         duration_calc = ifelse(duration_calc < 0 , NA_integer_, duration_calc),
         duration_calc2 = age - age_fs_gift,
         duration_calc2 = ifelse(duration_calc2 < 0 , NA_integer_, duration_calc2),
         duration_calc3 = age - age_fs_paidorgift,
         duration_calc3 = ifelse(duration_calc3 < 0 , NA_integer_, duration_calc3),
         duration_calc4 = age - age_startsw,
         duration_calc4 = ifelse(duration_calc4 < 0 , NA_integer_, duration_calc4),
         duration_yr = ifelse(duration_yr > age, NA_integer_, duration_yr),
         duration_estimate = ifelse(is.na(duration_calc), duration_calc2, duration_calc),
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc3, duration_estimate),
         duration_estimate = ifelse(is.na(duration_estimate), duration_calc4, duration_estimate))


fswdur_dat %>% 
  ggplot(aes(x = duration_yr, y = duration_estimate)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)


fswdur_dat %>% 
  group_by(survey_id) %>% 
  summarise(median_duration_yr = median(duration_yr, na.rm = T),
            median_age_first = median(duration_estimate, na.rm = T),
            iso3 = iso3) %>% 
  ungroup() %>% 
  droplevels() %>% 
  ggplot(aes(x = median_duration_yr, y = median_age_first, color = iso3), show.legend = F) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  moz.utils::standard_theme() 

fswdur_dat %>% 
  mutate(diff = duration_yr - duration_estimate) %>% 
  group_by(survey_id) %>% 
  summarise(median_duration_yr = median(duration_yr, na.rm = T),
            median_age_first = median(duration_estimate, na.rm = T),
            mean_diff = mean(diff, na.rm = T),
            survey_id = survey_id,
            iso3 = iso3) %>% 
  ungroup() %>% 
  distinct() %>% 
  filter(!is.na(mean_diff)) %>% 
  droplevels() %>% 
  ggplot(aes(y = survey_id, x = mean_diff, fill = survey_id), show.legend = F) + 
  geom_col() + 
  # geom_abline(slope = 1, intercept = 0) + 
  moz.utils::standard_theme() 
  # theme(axis.text.x = element_text(angle = ))/



fswdur_dat %>% 
  ggplot() +
  geom_density(aes(x = duration_yr))

fswdur_dat %>% 
  pivot_longer(cols = c(duration_yr, duration_estimate), names_to = "indicator", values_to = "duration") %>% 
  mutate(indicator = ifelse(indicator == "duration_estimate", "age first SW", "duration of SW")) %>% 
  ggplot() +
  geom_density(aes(x = duration, color = indicator)) + moz.utils::standard_theme()
