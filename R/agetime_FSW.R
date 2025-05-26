
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

inla_fsw_age <- crossing(year = 1993:2023,
                         iso3 = unique(fsw_age_dat$iso3),
                         model = "full_sample") %>% 
  bind_rows(crossing(year = 1993:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "female")%>%
              select(year, sex, genpop_median = megamedian) %>% 
              distinct() %>% 
              mutate(model = "ssa_genpop") %>% 
              filter(sex == "female")) %>% 
  bind_rows(crossing(year = 1993:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "female") %>%
              select(year, iso3, sex, genpop_median = median_age) %>% 
              distinct() %>% 
              mutate(model = "country_slopes") )


inla_fsw_age <- inla_fsw_age %>% 
  dplyr::select(year, model, iso3, genpop_median) %>%
  distinct() %>% 
  bind_rows(fsw_age_dat) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-2000) %>% 
  mutate(year_min = year - min(year),
         iso3_id = iso3)


fsw_age_formula1 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid") + offset(log(genpop_median))

fsw_agemod_gamma_offset <- INLA::inla(formula = fsw_age_formula1,
                         family = "gamma",
                         control.compute=list(config = TRUE),
                         data = inla_fsw_age)

fsw_gamma_offset <- summary(fsw_agemod_gamma_offset)

mod1sum <- inla_summary(fsw_gamma_offset, "FSW, gamma, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2000", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
  

fsw_agemod_gamma_offset_samples <- moz.utils::sample_model(fsw_agemod_gamma_offset, inla_fsw_age, col = "survey_id")

(fsw_ageratio_gamma_offset <- fsw_agemod_gamma_offset_samples %>% 
  filter(model == "ssa_genpop") %>% 
  mutate(exp_mean = exp(mean),
         exp_lower = exp(lower),
         exp_upper = exp(upper)) %>% 
  ggplot() + 
  geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
  geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
  geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
  geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
  moz.utils::standard_theme() + 
  labs(x = "Year", y = "Number of years older \nthan the total population") +
  theme(aspect.ratio = 1) + 
  annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
           arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
  annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
  annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
           arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
  annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(fsw_ageratio_gamma_offset_countries <- fsw_agemod_gamma_offset_samples %>% 
  filter(model == "country_slopes",
         iso3 %in% fsw_age_dat$iso3) %>% 
  mutate(exp_mean = exp(mean),
         exp_lower = exp(lower),
         exp_upper = exp(upper)) %>% 
  ggplot() + 
  geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
  geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
  geom_point(data = fsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
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
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(fsw_agecount_gamma_offset <- fsw_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "Year", y = "FSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


fsw_gammaoffsetslopes_plots <- ggpubr::ggarrange(fsw_agecount_gamma_offset, fsw_ageratio_gamma_offset, fsw_ageratio_gamma_offset_countries, nrow = 1, common.legend = T, legend = "bottom")



######## Gamma No Offset

fsw_age_formula2 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid")

fsw_agemod_gamma_no_offset <- INLA::inla(formula = fsw_age_formula2,
                                      family = "gamma",
                                      control.compute=list(config = TRUE),
                                      data = inla_fsw_age)

fsw_gamma_no_offset <- summary(fsw_agemod_gamma_no_offset)

mod2sum <- inla_summary(fsw_gamma_no_offset, "FSW, gamma, slopes, NO offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2000", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod2sum


fsw_agemod_gamma_no_offset_samples <- moz.utils::sample_model(fsw_agemod_gamma_no_offset, inla_fsw_age, col = "survey_id")

(fsw_ageratio_gamma_no_offset <- fsw_agemod_gamma_no_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


(fsw_ageratio_gamma_no_offset_countries <- fsw_agemod_gamma_no_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% fsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = fsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
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
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(fsw_agecount_gamma_no_offset <- fsw_agemod_gamma_no_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "Year", y = "FSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


fsw_gamma_no_offsetslopes_plots <- ggpubr::ggarrange(fsw_agecount_gamma_no_offset, fsw_ageratio_gamma_no_offset, fsw_ageratio_gamma_no_offset_countries, nrow = 1, common.legend = T, legend = "bottom")


###### Gamma, offset, no country slopes

fsw_age_formula3 <- age ~ 1 + year_min + f(iso3, model = "iid") + offset(log(genpop_median))

fsw_agemod_gamma_offset_noslopes <- INLA::inla(formula = fsw_age_formula3,
                                      family = "gamma",
                                      control.compute=list(config = TRUE),
                                      data = inla_fsw_age)

fsw_gamma_offset_noslopes <- summary(fsw_agemod_gamma_offset_noslopes)

mod3sum <- inla_summary(fsw_gamma_offset_noslopes, "FSW, gamma, NO slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2000", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod3sum

fsw_agemod_gamma_offset_noslopes_samples <- moz.utils::sample_model(fsw_agemod_gamma_offset_noslopes, inla_fsw_age, col = "survey_id")

(fsw_ageratio_gamma_offset_noslopes <- fsw_agemod_gamma_offset_noslopes_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(fsw_ageratio_gamma_offset_noslopes_countries <- fsw_agemod_gamma_offset_noslopes_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% fsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = fsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
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
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(fsw_agecount_gamma_offset_noslopes <- fsw_agemod_gamma_offset_noslopes_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "Year", y = "FSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


fsw_gammaoffset_noslopes_plots <- ggpubr::ggarrange(fsw_agecount_gamma_offset_noslopes, fsw_ageratio_gamma_offset_noslopes, fsw_ageratio_gamma_offset_noslopes_countries, nrow = 1, common.legend = T, legend = "bottom")

fsw_gammamodsums <- mod1sum %>% bind_rows(mod2sum) %>% bind_rows(mod3sum)
write_csv(fsw_gammamodsums, "~/Downloads/fsw_modsums2605.csv", na = "")


###############################################################################
############################## LOG-NORMAL MODELS ##############################

### Offset

fsw_agemod_lognormal_offset <- INLA::inla(formula = fsw_age_formula1,
                                      family = "lognormal",
                                      control.compute=list(config = TRUE),
                                      data = inla_fsw_age)

fsw_lognormal_offset <- summary(fsw_agemod_lognormal_offset)

mod1sum_fswln <- inla_summary(fsw_lognormal_offset, "FSW, lognormal, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2000", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_fswln


fsw_agemod_lognormal_offset_samples <- moz.utils::sample_model(fsw_agemod_lognormal_offset, inla_fsw_age, col = "survey_id")

(fsw_ageratio_lognormal_offset <- fsw_agemod_lognormal_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(fsw_ageratio_lognormal_offset_countries <- fsw_agemod_lognormal_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% fsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = fsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
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
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(fsw_agecount_lognormal_offset <- fsw_agemod_lognormal_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "Year", y = "FSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


fsw_lognormaloffsetslopes_plots <- ggpubr::ggarrange(fsw_agecount_lognormal_offset, fsw_ageratio_lognormal_offset, fsw_ageratio_lognormal_offset_countries, nrow = 1, common.legend = T, legend = "bottom")


######## Log-Normal No Offset

fsw_age_formula2 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid")

fsw_agemod_lognorm_no_offset <- INLA::inla(formula = fsw_age_formula2,
                                         family = "lognormal",
                                         control.compute=list(config = TRUE),
                                         data = inla_fsw_age)

fsw_lognorm_no_offset <- summary(fsw_agemod_lognorm_no_offset)

mod2sum_fswln <- inla_summary(fsw_lognorm_no_offset, "FSW, lognorm, slopes, NO offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2000", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod2sum_fswln


fsw_agemod_lognorm_no_offset_samples <- moz.utils::sample_model(fsw_agemod_lognorm_no_offset, inla_fsw_age, col = "survey_id")

(fsw_ageratio_lognorm_no_offset <- fsw_agemod_lognorm_no_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


(fsw_ageratio_lognorm_no_offset_countries <- fsw_agemod_lognorm_no_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% fsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = fsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
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
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(fsw_agecount_lognorm_no_offset <- fsw_agemod_lognorm_no_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "Year", y = "FSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


fsw_lognorm_no_offsetslopes_plots <- ggpubr::ggarrange(fsw_agecount_lognorm_no_offset, fsw_ageratio_lognorm_no_offset, fsw_ageratio_lognorm_no_offset_countries, nrow = 1, common.legend = T, legend = "bottom")

###### Lognorm, offset, no country slopes


fsw_agemod_lognormal_offset_noslopes <- INLA::inla(formula = fsw_age_formula3,
                                               family = "lognormal",
                                               control.compute=list(config = TRUE),
                                               data = inla_fsw_age)

fsw_lognormal_offset_noslopes <- summary(fsw_agemod_lognormal_offset_noslopes)

mod3sum_fswln <- inla_summary(fsw_lognormal_offset_noslopes, "FSW, lognormal, NO slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2000", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod3sum_fswln

fsw_agemod_lognormal_offset_noslopes_samples <- moz.utils::sample_model(fsw_agemod_lognormal_offset_noslopes, inla_fsw_age, col = "survey_id")

(fsw_ageratio_lognormal_offset_noslopes <- fsw_agemod_lognormal_offset_noslopes_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 1993, xend = 1993, y = 0.5, yend = 5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = 6.7, label = "FSW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 1993, xend = 1993, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 1993, y = -6.3, label = "FSW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(fsw_ageratio_lognormal_offset_noslopes_countries <- fsw_agemod_lognormal_offset_noslopes_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% fsw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = fsw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
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
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(fsw_agecount_lognormal_offset_noslopes <- fsw_agemod_lognormal_offset_noslopes_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = fsw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "Year", y = "FSW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


fsw_lognormaloffset_noslopes_plots <- ggpubr::ggarrange(fsw_agecount_lognormal_offset_noslopes, fsw_ageratio_lognormal_offset_noslopes, fsw_ageratio_lognormal_offset_noslopes_countries, nrow = 1, common.legend = T, legend = "bottom")

fsw_lognormalmodsums <- mod1sum_fswln %>% bind_rows(mod2sum_fswln) %>% bind_rows(mod3sum_fswln)
write_csv(fsw_lognormalmodsums, "~/Downloads/fsw_modsums_lognorm2605.csv", na = "")
