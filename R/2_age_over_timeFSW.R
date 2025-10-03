library(tidyverse)
library(INLA)
library(matrixStats)

dat <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/agehiv_data_extract_240925.rds") 

spectrum_dat <- readRDS("~/Downloads/2024_spectrum_data.rds") %>%
  bind_rows() %>%
  filter(age > 14,
         iso3 %in% moz.utils::ssa_iso3()) %>%
  group_by(iso3, year, age, sex) %>%
  summarise(across(c(totpop:infections), sum)) %>%
  ungroup() %>%
  mutate(tot_prev = hivpop/totpop)


genpop_median_ages <- spectrum_dat %>%
  group_by(iso3, year, sex) %>%
  summarise(
    median_age = weightedMedian(age, w = totpop, na.rm = TRUE),
    mean_age   = weighted.mean(age, w = totpop, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(year %in% 1993:2023) %>% ungroup() %>% 
  group_by(year, sex) %>%
  mutate(megamedian = median(median_age),
         megamean = mean(mean_age)) %>% 
  ungroup()

genpop_median_ages %>% 
  select(year, sex, Median = megamedian, Mean = megamean) %>% 
  distinct() %>% 
  pivot_longer(cols = c(Median, Mean), names_to = "Measure", values_to = "Value") %>% 
  ggplot() +
  geom_line(aes(x = year, y = Value, color = sex), linewidth = 0.75) + 
  facet_wrap(~Measure) + 
  moz.utils::standard_theme()


# genpop_median_ages2 <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/genpop_median_ages.rds")
# # 
# genpop_median_ages2 %>%
#   select(year, sex, megamedian, megamean) %>%
#   distinct() %>%
#   ggplot() +
#   geom_line(aes(x = year, y = megamean, color = sex))

KEN1993 <- dat %>% 
  filter(survey_id == "KEN1993ACA_FSW") %>% 
  filter(visit == 0) %>% 
  separate(survey_day, into = c(NA, NA, "survey_year"), sep = "/") %>% 
  mutate(year = paste0(19, survey_year),
         survey_id = paste0(survey_id, "_", survey_year)) %>% 
  select(survey_year, everything()) %>% 
  type.convert(as.is = T) %>% 
  mutate(year = ifelse(year < 1993, year + 100, year))

# KEN1993 %>% 
#   ggplot() + 
#   geom_density(aes(x = age, color = factor(year))) +
#   theme_minimal()

#FSW : 108 surveys 
fsw_age_dat <- dat %>% 
  select(kp, year, sex, survey_id, iso3, age) %>% 
  filter(kp == "FSW", !survey_id %in% c("ETH2020ACA_FSW")) %>% 
  filter(sex == 1 | is.na(sex)) %>% 
  filter(!survey_id == "KEN1993ACA_FSW") %>% 
  bind_rows(KEN1993 %>% mutate(survey_id = "KEN1993ACA_FSW") %>% 
              select(kp, year, sex, survey_id, iso3, age)) %>% 
  filter(!(age <10 | age > 70)) %>% 
  filter(!is.na(age)) %>% 
  group_by(survey_id, year) %>% 
  mutate(mean_kp_age = mean(age),
         denom = n()) %>%
  ungroup() %>% 
  left_join(genpop_median_ages %>% filter(sex == "female") %>% select(-sex)) %>% 
  select(kp, year, survey_id, iso3, age, mean_kp_age, genpop_mean = mean_age, denom)



inla_fsw_age <- crossing(year = 1993:2023,
                         iso3 = unique(fsw_age_dat$iso3),
                         model = "full_sample") %>% 
  bind_rows(crossing(year = 1993:2023) %>%
              left_join(genpop_median_ages %>% filter(sex == "female") %>% select(-sex)) %>% 
              select(year, genpop_median = megamedian, genpop_mean = megamean) %>% 
              distinct() %>% 
              mutate(model = "ssa_genpop")) %>% 
  bind_rows(crossing(year = 1993:2023) %>%
              left_join(genpop_median_ages  %>% filter(sex == "female") %>% select(-sex)) %>% 
              select(year, iso3, genpop_median = median_age, genpop_mean = mean_age) %>% 
              distinct() %>% 
              mutate(model = "country_slopes"))



inla_fsw_age <- inla_fsw_age %>% 
  select(year, model, iso3, genpop_median, genpop_mean) %>%
  distinct() %>% 
  bind_rows(fsw_age_dat) %>% 
  left_join(genpop_median_ages %>% filter(sex == "female") %>% select(-sex)) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-2000) %>% 
  mutate(year_min = year - min(year),
         iso3_id = iso3,
         mean_year = (year - 2012)/5) %>% 
  left_join(sf::read_sf(moz.utils::national_areas()) %>% select(iso3, id.iso3) %>% sf::st_drop_geometry()) 

inla_fsw_age2 <- inla_fsw_age %>% 
  filter(!year < 2005)

fsw_age_formula1 <- age ~ 1 + mean_year + f(iso3_id, mean_year, model = "iid") + f(id.iso3, model = "besag", graph = moz.utils::national_adj()) + offset(log(genpop_mean))

fsw_agemod_gamma_offset <- INLA::inla(formula = fsw_age_formula1,
                                      family = "gamma",
                                      control.compute=list(config = TRUE, waic = T, dic = T),
                                      data = inla_fsw_age)

summary(fsw_agemod_gamma_offset)

fsw_agemod_gamma_offset_samples <- moz.utils::sample_model(fsw_agemod_gamma_offset, inla_fsw_age, col = "survey_id")


(fsw_agecount_gamma_offset <- fsw_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper),
           kp = "FSW") %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean)), linewidth = 0.75) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_line(data = genpop_median_ages %>% filter(sex == "female") %>% select(year, megamean) %>% distinct(), aes(x = year, y = megamean), color = "darkred", linetype = "dashed", linewidth = 0.75) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    annotate("text", x = 1993, y = 35.7, label = "Matched population mean age", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "", y = "Age \n ") +
    theme(aspect.ratio = 1) +
    facet_wrap(~kp) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm")))


(fsw_ageratio_gamma_offset <- fsw_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper),
           kp = "FSW") %>% 
    ggplot() + 
    geom_line(aes(x = year , y = (exp(mean))-genpop_mean)) +
    geom_ribbon(aes(x = year, ymin = (exp(lower))- genpop_mean, ymax = (exp(upper))-genpop_mean), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age-genpop_mean, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.75) +
    moz.utils::standard_theme() + 
    lims(y = c(-15, 5)) +
    labs(x = "", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = 3.5, label = "KP older than total \npopulation", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -10.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = -11.3, label = "KP younger than total \npopulation", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    facet_wrap(~kp) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))) 


fsw_agetime_plots <- ggpubr::ggarrange(fsw_agecount_gamma_offset, fsw_ageratio_gamma_offset, nrow = 2)

(fsw_ageratio_gamma_offset_countries <- fsw_agemod_gamma_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% fsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    filter(iso3 %in% c("KEN", "BEN", "ZAF", "SEN")) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_mean, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_mean, ymax = exp(upper)-genpop_mean, fill = iso3), alpha = 0.05) +
    geom_point(data = fsw_age_dat %>% filter(iso3 %in% c("KEN", "BEN", "ZAF", "SEN")), aes(x = year, y = (mean_kp_age)-genpop_mean, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-7, 10)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 4), color = guide_legend(title = "Country", nrow = 4)) +
    theme(legend.position = "right"))
