library(tidyverse)
library(INLA)

inla_summary <-  function(inlamod_summary, model_description, hyper) {
  df <- data.frame(inlamod_summary$fixed) %>% 
    select(mean = mean, lower = X0.025quant, upper = X0.975quant)
  
  if (hyper) {
    df %>% 
      bind_rows(data.frame(inlamod_summary$hyperpar) %>%
                  select(mean = mean, lower = X0.025quant, upper = X0.975quant)) %>% 
      mutate(model = model_description) %>% 
      rownames_to_column()
  } else {
    df %>% 
      mutate(model = model_description) %>% 
      rownames_to_column()
  }
}

inla_pwid_age <- crossing(year = 2014:2023,
                          sex = c("male", "female"),
                          iso3 = unique(pwid_age_dat$iso3),
                          model = "full_sample") %>%
  bind_rows(crossing(year = 2014:2023) %>%
              left_join(genpop_median_ages %>% group_by(year, sex) %>% mutate(megamedian = median(median_age)) %>% ungroup()) %>%
              # filter(sex == "male")%>%
              select(year, sex, genpop_median = megamedian) %>%
              distinct() %>%
              mutate(model = "ssa_genpop")) %>%
              # filter(sex == "male")) %>%
  bind_rows(crossing(year = 2014:2023) %>%
              left_join(genpop_median_ages) %>%
              # filter(sex == "male") %>%
              select(year, iso3, sex, genpop_median = median_age) %>%
              distinct() %>%
              filter(iso3 %in% pwid_age_dat$iso3) %>%
              mutate(model = "country_slopes") )


inla_pwid_age <- inla_pwid_age %>%
  dplyr::select(year, model, sex, iso3, genpop_median) %>%
  distinct() %>%
  bind_rows(pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) %>%
  filter(!age<15 | is.na(age)) %>%
  mutate(year_centre = year-2014) %>%
  mutate(year_min = year - min(year),
         iso3_id = iso3)

# inla_pwid_age <- readRDS("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/inla_pwid_age2705.rds")
#######################
# Gamma with offset and slopes and prior on slopes


ref.iid.prec.prior <- list(prec = list(prior = "normal", param = c(3, 2)))

pwid_age_formula1_prior <- age ~ 1 + 
  year_min + 
  sex + 
  f(iso3, model = "iid")  + 
  # f(iso3_id, year_min, model = "iid") +
  f(iso3_id, year_min, model = "iid", hyper = ref.iid.prec.prior) +
  offset(log(genpop_median))


pwid_agemod_gamma_offset_prior <- INLA::inla(formula = pwid_age_formula1_prior,
                                       family = "gamma",
                                       control.compute=list(config = TRUE),
                                       data = inla_pwid_age)

pwid_gamma_offset_prior <- summary(pwid_agemod_gamma_offset_prior)
pwid_gamma_offset_prior
pwid_agemod_gamma_offset_prior$internal.summary.hyperpar

mod1sum_gammapwid_prior <- inla_summary(pwid_gamma_offset_prior, "PWID, gamma, slopes w/prior, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2014", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_gammapwid_prior

pwid_agemod_gamma_offset_prior_samples <- moz.utils::sample_model(pwid_agemod_gamma_offset_prior, inla_pwid_age, col = "survey_id")

(pwid_ageratio_gamma_offset_prior <- pwid_agemod_gamma_offset_prior_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)), aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(-10, 10)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than \ntotal population", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(pwid_ageratio_gamma_offset_prior_countries <- pwid_agemod_gamma_offset_prior_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% pwid_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(-10, 10)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 2), color = guide_legend(title = "Country", nrow = 2)))



(pwid_agecount_gamma_offset_prior <- pwid_agemod_gamma_offset_prior_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(20, 38)) +
    labs(x = "Year", y = "PWID age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(pwid_gammaoffsetslopes_plots <- ggpubr::ggarrange(pwid_agecount_gamma_offset_prior, pwid_ageratio_gamma_offset_prior, pwid_ageratio_gamma_offset_prior_countries, nrow = 1, common.legend = T, legend = "bottom"))

###########
ref.iid.prec.prior <- list(prec = list(prior = "normal", param = c(1,)))

pwid_age_formula1 <- age ~ 1 + 
  year_min + 
  sex + 
  f(iso3, model = "iid")  + 
  f(iso3_id, year_min, model = "iid") +
  # f(iso3_id, year_min, model = "iid", hyper = ref.iid.prec.prior) +
  offset(log(genpop_median))


pwid_agemod_gamma_offset <- INLA::inla(formula = pwid_age_formula1,
                                       family = "gamma",
                                       control.compute=list(config = TRUE),
                                       data = inla_pwid_age)

pwid_gamma_offset <- summary(pwid_agemod_gamma_offset)

mod1sum_gammapwid <- inla_summary(pwid_gamma_offset, "PWID, gamma, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2014", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_gammapwid


pwid_agemod_gamma_offset_samples <- moz.utils::sample_model(pwid_agemod_gamma_offset, inla_pwid_age, col = "survey_id")

(pwid_ageratio_gamma_offset <- pwid_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)), aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(-10, 10)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than \ntotal population", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(pwid_ageratio_gamma_offset_countries <- pwid_agemod_gamma_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% pwid_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(-10, 10)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 2), color = guide_legend(title = "Country", nrow = 2)))



(pwid_agecount_gamma_offset <- pwid_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(20, 38)) +
    labs(x = "Year", y = "PWID age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(pwid_gammaoffsetslopes_plots <- ggpubr::ggarrange(pwid_agecount_gamma_offset, pwid_ageratio_gamma_offset, pwid_ageratio_gamma_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))



######## Gamma No Offset

pwid_age_formula2 <- age ~ 1 + sex + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid")

pwid_agemod_gamma_no_offset <- INLA::inla(formula = pwid_age_formula2,
                                          family = "gamma",
                                          control.compute=list(config = TRUE),
                                          data = inla_pwid_age)

pwid_gamma_no_offset <- summary(pwid_agemod_gamma_no_offset)

mod2sum_gammapwid <- inla_summary(pwid_gamma_no_offset, "PWID, gamma, slopes, NO offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2014", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod2sum_gammapwid


pwid_agemod_gamma_no_offset_samples <- moz.utils::sample_model(pwid_agemod_gamma_no_offset, inla_pwid_age, col = "survey_id")

(pwid_ageratio_gamma_no_offset <- pwid_agemod_gamma_no_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(-10, 10)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than \ntotal population", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


(pwid_ageratio_gamma_no_offset_countries <- pwid_agemod_gamma_no_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% pwid_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(-10, 10)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than \ntotal population", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 2), color = guide_legend(title = "Country", nrow = 2)))



(pwid_agecount_gamma_no_offset <- pwid_agemod_gamma_no_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(20, 38)) +
    labs(x = "Year", y = "PWID age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(pwid_gamma_no_offsetslopes_plots <- ggpubr::ggarrange(pwid_agecount_gamma_no_offset, pwid_ageratio_gamma_no_offset, pwid_ageratio_gamma_no_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))


###### Gamma, offset, no country slopes

pwid_age_formula3 <- age ~ 1 + year_min + sex + f(iso3, model = "iid") + offset(log(genpop_median))

pwid_agemod_gamma_offset_noslopes <- INLA::inla(formula = pwid_age_formula3,
                                                family = "gamma",
                                                control.compute=list(config = TRUE),
                                                data = inla_pwid_age)

pwid_gamma_offset_noslopes <- summary(pwid_agemod_gamma_offset_noslopes)

mod3sum_gammapwid <- inla_summary(pwid_gamma_offset_noslopes, "PWID, gamma, NO slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2014", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod3sum_gammapwid

pwid_agemod_gamma_offset_noslopes_samples <- moz.utils::sample_model(pwid_agemod_gamma_offset_noslopes, inla_pwid_age, col = "survey_id")

(pwid_ageratio_gamma_offset_noslopes <- pwid_agemod_gamma_offset_noslopes_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(-10, 10)) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than \ntotal population", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(pwid_ageratio_gamma_offset_noslopes_countries <- pwid_agemod_gamma_offset_noslopes_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% pwid_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    facet_wrap(~sex, nrow = 2) +
    coord_cartesian(ylim = c(-10, 10)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than \ntotal population", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 2), color = guide_legend(title = "Country", nrow = 2)))



(pwid_agecount_gamma_offset_noslopes <- pwid_agemod_gamma_offset_noslopes_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,38)) +
    facet_wrap(~sex, nrow = 2) +
    labs(x = "Year", y = "PWID age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(pwid_gammaoffset_noslopes_plots <- ggpubr::ggarrange(pwid_agecount_gamma_offset_noslopes, pwid_ageratio_gamma_offset_noslopes, pwid_ageratio_gamma_offset_noslopes_countries, nrow = 1, common.legend = T, legend = "bottom"))

pwid_gammamodsums <- mod1sum_gammapwid %>% bind_rows(mod2sum_gammapwid) %>% bind_rows(mod3sum_gammapwid)
write_csv(pwid_gammamodsums, "~/Downloads/pwid_modsums2605.csv", na = "")


###############################################################################
############################## LOG-NORMAL MODELS ##############################

### Offset

pwid_agemod_lognormal_offset <- INLA::inla(formula = pwid_age_formula1,
                                           family = "lognormal",
                                           control.compute=list(config = TRUE),
                                           data = inla_pwid_age)

pwid_lognormal_offset <- summary(pwid_agemod_lognormal_offset)

mod1sum_pwidln <- inla_summary(pwid_lognormal_offset, "PWID, lognormal, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2014", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_pwidln


pwid_agemod_lognormal_offset_samples <- moz.utils::sample_model(pwid_agemod_lognormal_offset, inla_pwid_age, col = "survey_id")

(pwid_ageratio_lognormal_offset <- pwid_agemod_lognormal_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-8, 8)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(pwid_ageratio_lognormal_offset_countries <- pwid_agemod_lognormal_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% pwid_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(pwid_agecount_lognormal_offset <- pwid_agemod_lognormal_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "PWID age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(pwid_lognormaloffsetslopes_plots <- ggpubr::ggarrange(pwid_agecount_lognormal_offset, pwid_ageratio_lognormal_offset, pwid_ageratio_lognormal_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))


######## Log-Normal No Offset

pwid_age_formula2 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid")

pwid_agemod_lognorm_no_offset <- INLA::inla(formula = pwid_age_formula2,
                                            family = "lognormal",
                                            control.compute=list(config = TRUE),
                                            data = inla_pwid_age)

pwid_lognorm_no_offset <- summary(pwid_agemod_lognorm_no_offset)

mod2sum_pwidln <- inla_summary(pwid_lognorm_no_offset, "PWID, lognorm, slopes, NO offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2014", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod2sum_pwidln


pwid_agemod_lognorm_no_offset_samples <- moz.utils::sample_model(pwid_agemod_lognorm_no_offset, inla_pwid_age, col = "survey_id")

(pwid_ageratio_lognorm_no_offset <- pwid_agemod_lognorm_no_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    coord_cartesian(ylim = c(-8, 8)) +
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


(pwid_ageratio_lognorm_no_offset_countries <- pwid_agemod_lognorm_no_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% pwid_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(pwid_agecount_lognorm_no_offset <- pwid_agemod_lognorm_no_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "PWID age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(pwid_lognorm_no_offsetslopes_plots <- ggpubr::ggarrange(pwid_agecount_lognorm_no_offset, pwid_ageratio_lognorm_no_offset, pwid_ageratio_lognorm_no_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))

###### Lognorm, offset, no country slopes


pwid_agemod_lognormal_offset_noslopes <- INLA::inla(formula = pwid_age_formula3,
                                                    family = "lognormal",
                                                    control.compute=list(config = TRUE),
                                                    data = inla_pwid_age)

pwid_lognormal_offset_noslopes <- summary(pwid_agemod_lognormal_offset_noslopes)

mod3sum_pwidln <- inla_summary(pwid_lognormal_offset_noslopes, "PWID, lognormal, NO slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2014", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod3sum_pwidln

pwid_agemod_lognormal_offset_noslopes_samples <- moz.utils::sample_model(pwid_agemod_lognormal_offset_noslopes, inla_pwid_age, col = "survey_id")

(pwid_ageratio_lognormal_offset_noslopes <- pwid_agemod_lognormal_offset_noslopes_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-8, 8)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(pwid_ageratio_lognormal_offset_noslopes_countries <- pwid_agemod_lognormal_offset_noslopes_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% pwid_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2014, xend = 2014, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = 7.3, label = "PWID older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2014, xend = 2014, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2014, y = -7.3, label = "PWID younger than \ntotal population", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(pwid_agecount_lognormal_offset_noslopes <- pwid_agemod_lognormal_offset_noslopes_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = (pwid_age_dat %>% select(-genpop_median, -megamedian) %>% left_join(genpop_median_ages) %>% rename(genpop_median = median_age)) , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "PWID age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(pwid_lognormaloffset_noslopes_plots <- ggpubr::ggarrange(pwid_agecount_lognormal_offset_noslopes, pwid_ageratio_lognormal_offset_noslopes, pwid_ageratio_lognormal_offset_noslopes_countries, nrow = 1, common.legend = T, legend = "bottom"))

pwid_lognormalmodsums <- mod1sum_pwidln %>% bind_rows(mod2sum_pwidln) %>% bind_rows(mod3sum_pwidln)
write_csv(pwid_lognormalmodsums, "~/Downloads/pwid_modsums_lognorm2605.csv", na = "")

