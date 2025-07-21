
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

inla_cfsw_age <- crossing(year = 1998:2023,
                         iso3 = unique(cfsw_age_dat$iso3),
                         model = "full_sample") %>% 
  bind_rows(crossing(year = 1998:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "male")%>%
              select(year, sex, genpop_median = megamedian) %>% 
              distinct() %>% 
              mutate(model = "ssa_genpop") %>% 
              filter(sex == "male")) %>% 
  bind_rows(crossing(year = 1998:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "male") %>%
              select(year, iso3, sex, genpop_median = median_age) %>% 
              distinct() %>% 
              mutate(model = "country_slopes") )


inla_cfsw_age <- inla_cfsw_age %>% 
  dplyr::select(year, model, iso3, genpop_median) %>%
  distinct() %>% 
  bind_rows(cfsw_age_dat) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-1998) %>% 
  mutate(year_min = year - min(year),
         iso3_id = iso3)

ref.iid.prec.prior <- list(prec = list(prior = "normal", param = c(3, 2)))

cfsw_age_formula1 <- age ~ 1 + year_min + 
  f(iso3, model = "iid") + 
  # f(iso3_id, year_min, model = "iid") +
  f(iso3_id, year_min, model = "iid", hyper = ref.iid.prec.prior) +
  offset(log(genpop_median))

cfsw_agemod_gamma_offset <- INLA::inla(formula = cfsw_age_formula1,
                                      family = "gamma",
                                      control.compute=list(config = TRUE),
                                      data = inla_cfsw_age)

cfsw_gamma_offset <- summary(cfsw_agemod_gamma_offset)

mod1sum_gammacfsw <- inla_summary(cfsw_gamma_offset, "CFSW, gamma, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 1998", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_gammacfsw


cfsw_agemod_gamma_offset_samples <- moz.utils::sample_model(cfsw_agemod_gamma_offset, inla_cfsw_age, col = "survey_id")

(cfsw_ageratio_gamma_offset <- cfsw_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-8, 8)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(cfsw_ageratio_gamma_offset_countries <- cfsw_agemod_gamma_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% cfsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(cfsw_agecount_gamma_offset <- cfsw_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "CFSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(cfsw_gammaoffsetslopes_plots <- ggpubr::ggarrange(cfsw_agecount_gamma_offset, cfsw_ageratio_gamma_offset, cfsw_ageratio_gamma_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))



######## Gamma No Offset

cfsw_age_formula2 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid")

cfsw_agemod_gamma_no_offset <- INLA::inla(formula = cfsw_age_formula2,
                                         family = "gamma",
                                         control.compute=list(config = TRUE),
                                         data = inla_cfsw_age)

cfsw_gamma_no_offset <- summary(cfsw_agemod_gamma_no_offset)

mod2sum_gammacfsw <- inla_summary(cfsw_gamma_no_offset, "CFSW, gamma, slopes, NO offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 1998", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod2sum_gammacfsw


cfsw_agemod_gamma_no_offset_samples <- moz.utils::sample_model(cfsw_agemod_gamma_no_offset, inla_cfsw_age, col = "survey_id")

(cfsw_ageratio_gamma_no_offset <- cfsw_agemod_gamma_no_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-8, 8)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


(cfsw_ageratio_gamma_no_offset_countries <- cfsw_agemod_gamma_no_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% cfsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-8, 8)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(cfsw_agecount_gamma_no_offset <- cfsw_agemod_gamma_no_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(ylim = c(20,35)) +
    labs(x = "Year", y = "CFSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(cfsw_gamma_no_offsetslopes_plots <- ggpubr::ggarrange(cfsw_agecount_gamma_no_offset, cfsw_ageratio_gamma_no_offset, cfsw_ageratio_gamma_no_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))


###### Gamma, offset, no country slopes

cfsw_age_formula3 <- age ~ 1 + year_min + f(iso3, model = "iid") + offset(log(genpop_median))

cfsw_agemod_gamma_offset_noslopes <- INLA::inla(formula = cfsw_age_formula3,
                                               family = "gamma",
                                               control.compute=list(config = TRUE),
                                               data = inla_cfsw_age)

cfsw_gamma_offset_noslopes <- summary(cfsw_agemod_gamma_offset_noslopes)

mod3sum_gammacfsw <- inla_summary(cfsw_gamma_offset_noslopes, "CFSW, gamma, NO slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 1998", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod3sum_gammacfsw

cfsw_agemod_gamma_offset_noslopes_samples <- moz.utils::sample_model(cfsw_agemod_gamma_offset_noslopes, inla_cfsw_age, col = "survey_id")

(cfsw_ageratio_gamma_offset_noslopes <- cfsw_agemod_gamma_offset_noslopes_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    coord_cartesian(ylim = c(-8, 8)) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(cfsw_ageratio_gamma_offset_noslopes_countries <- cfsw_agemod_gamma_offset_noslopes_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% cfsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(cfsw_agecount_gamma_offset_noslopes <- cfsw_agemod_gamma_offset_noslopes_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "CFSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(cfsw_gammaoffset_noslopes_plots <- ggpubr::ggarrange(cfsw_agecount_gamma_offset_noslopes, cfsw_ageratio_gamma_offset_noslopes, cfsw_ageratio_gamma_offset_noslopes_countries, nrow = 1, common.legend = T, legend = "bottom"))

cfsw_gammamodsums <- mod1sum_gammacfsw %>% bind_rows(mod2sum_gammacfsw) %>% bind_rows(mod3sum_gammacfsw)
write_csv(cfsw_gammamodsums, "~/Downloads/cfsw_modsums2605.csv", na = "")


###############################################################################
############################## LOG-NORMAL MODELS ##############################

### Offset

cfsw_agemod_lognormal_offset <- INLA::inla(formula = cfsw_age_formula1,
                                          family = "lognormal",
                                          control.compute=list(config = TRUE),
                                          data = inla_cfsw_age)

cfsw_lognormal_offset <- summary(cfsw_agemod_lognormal_offset)

mod1sum_cfswln <- inla_summary(cfsw_lognormal_offset, "CFSW, lognormal, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 1998", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_cfswln


cfsw_agemod_lognormal_offset_samples <- moz.utils::sample_model(cfsw_agemod_lognormal_offset, inla_cfsw_age, col = "survey_id")

(cfsw_ageratio_lognormal_offset <- cfsw_agemod_lognormal_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-8, 8)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(cfsw_ageratio_lognormal_offset_countries <- cfsw_agemod_lognormal_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% cfsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(cfsw_agecount_lognormal_offset <- cfsw_agemod_lognormal_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "CFSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(cfsw_lognormaloffsetslopes_plots <- ggpubr::ggarrange(cfsw_agecount_lognormal_offset, cfsw_ageratio_lognormal_offset, cfsw_ageratio_lognormal_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))


######## Log-Normal No Offset

cfsw_age_formula2 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid")

cfsw_agemod_lognorm_no_offset <- INLA::inla(formula = cfsw_age_formula2,
                                           family = "lognormal",
                                           control.compute=list(config = TRUE),
                                           data = inla_cfsw_age)

cfsw_lognorm_no_offset <- summary(cfsw_agemod_lognorm_no_offset)

mod2sum_cfswln <- inla_summary(cfsw_lognorm_no_offset, "CFSW, lognorm, slopes, NO offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 1998", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod2sum_cfswln


cfsw_agemod_lognorm_no_offset_samples <- moz.utils::sample_model(cfsw_agemod_lognorm_no_offset, inla_cfsw_age, col = "survey_id")

(cfsw_ageratio_lognorm_no_offset <- cfsw_agemod_lognorm_no_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    coord_cartesian(ylim = c(-8, 8)) +
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


(cfsw_ageratio_lognorm_no_offset_countries <- cfsw_agemod_lognorm_no_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% cfsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(cfsw_agecount_lognorm_no_offset <- cfsw_agemod_lognorm_no_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "CFSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(cfsw_lognorm_no_offsetslopes_plots <- ggpubr::ggarrange(cfsw_agecount_lognorm_no_offset, cfsw_ageratio_lognorm_no_offset, cfsw_ageratio_lognorm_no_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))

###### Lognorm, offset, no country slopes


cfsw_agemod_lognormal_offset_noslopes <- INLA::inla(formula = cfsw_age_formula3,
                                                   family = "lognormal",
                                                   control.compute=list(config = TRUE),
                                                   data = inla_cfsw_age)

cfsw_lognormal_offset_noslopes <- summary(cfsw_agemod_lognormal_offset_noslopes)

mod3sum_cfswln <- inla_summary(cfsw_lognormal_offset_noslopes, "CFSW, lognormal, NO slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 1998", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod3sum_cfswln

cfsw_agemod_lognormal_offset_noslopes_samples <- moz.utils::sample_model(cfsw_agemod_lognormal_offset_noslopes, inla_cfsw_age, col = "survey_id")

(cfsw_ageratio_lognormal_offset_noslopes <- cfsw_agemod_lognormal_offset_noslopes_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-8, 8)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(cfsw_ageratio_lognormal_offset_noslopes_countries <- cfsw_agemod_lognormal_offset_noslopes_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% cfsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(cfsw_agecount_lognormal_offset_noslopes <- cfsw_agemod_lognormal_offset_noslopes_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "CFSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(cfsw_lognormaloffset_noslopes_plots <- ggpubr::ggarrange(cfsw_agecount_lognormal_offset_noslopes, cfsw_ageratio_lognormal_offset_noslopes, cfsw_ageratio_lognormal_offset_noslopes_countries, nrow = 1, common.legend = T, legend = "bottom"))

cfsw_lognormalmodsums <- mod1sum_cfswln %>% bind_rows(mod2sum_cfswln) %>% bind_rows(mod3sum_cfswln)
write_csv(cfsw_lognormalmodsums, "~/Downloads/cfsw_modsums_lognorm2605.csv", na = "")

##### Weibull 

ref.iid.prec.prior <- list(prec = list(prior = "normal", param = c(3, 2)))

cfsw_age_formula1 <- age ~ 1 + year_min + 
  f(iso3, model = "iid") + 
  # f(iso3_id, year_min, model = "iid") +
  # f(iso3_id, year_min, model = "iid", hyper = ref.iid.prec.prior) +
  offset(log(genpop_median))

cfsw_agemod_gamma_offset <- INLA::inla(formula = cfsw_age_formula1,
                                       family = "weibull",
                                       control.compute=list(config = TRUE),
                                       data = inla_cfsw_age)

cfsw_gamma_offset <- summary(cfsw_agemod_gamma_offset)

mod1sum_gammacfsw <- inla_summary(cfsw_gamma_offset, "CFSW, gamma, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 1998", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_gammacfsw


cfsw_agemod_gamma_offset_samples <- moz.utils::sample_model(cfsw_agemod_gamma_offset, inla_cfsw_age, col = "survey_id")

(cfsw_ageratio_gamma_offset <- cfsw_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-8, 8)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(cfsw_ageratio_gamma_offset_countries <- cfsw_agemod_gamma_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% cfsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-8, 8)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1998, xend = 1998, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = 6.3, label = "CFSW older than total \npopulation men", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1998, xend = 1998, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1998, y = -6.3, label = "CFSW younger than total \npopulation men", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(cfsw_agecount_gamma_offset <- cfsw_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = cfsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    coord_cartesian(y = c(20,35)) +
    labs(x = "Year", y = "CFSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(cfsw_gammaoffsetslopes_plots <- ggpubr::ggarrange(cfsw_agecount_gamma_offset, cfsw_ageratio_gamma_offset, cfsw_ageratio_gamma_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))

#####

eta <- cfsw_agemod_gamma_offset$summary.linear.predictor$mean

shape_post <- inla.tmarginal(exp, cfsw_agemod_gamma_offset$marginals.hyperpar$`Log shape parameter for weibull`)
shape_mean <- inla.emarginal(function(x) x, shape_post)



