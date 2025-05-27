
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

inla_tgw_age <- crossing(year = 2010:2023,
                         iso3 = unique(tgw_age_dat$iso3),
                         model = "full_sample") %>% 
  bind_rows(crossing(year = 2010:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "female")%>%
              select(year, sex, genpop_median = megamedian) %>% 
              distinct() %>% 
              mutate(model = "ssa_genpop") %>% 
              filter(sex == "female")) %>% 
  bind_rows(crossing(year = 2010:2023) %>%
              left_join(genpop_median_ages) %>% 
              filter(sex == "female") %>%
              select(year, iso3, sex, genpop_median = median_age) %>% 
              distinct() %>% 
              mutate(model = "country_slopes") )


inla_tgw_age <- inla_tgw_age %>% 
  dplyr::select(year, model, iso3, genpop_median) %>%
  distinct() %>% 
  bind_rows(tgw_age_dat) %>% 
  filter(!age<15 | is.na(age)) %>% 
  mutate(year_centre = year-2010) %>% 
  mutate(year_min = year - min(year),
         iso3_id = iso3)


tgw_age_formula1 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid") + offset(log(genpop_median))

tgw_agemod_gamma_offset <- INLA::inla(formula = tgw_age_formula1,
                                      family = "gamma",
                                      control.compute=list(config = TRUE),
                                      data = inla_tgw_age)

tgw_gamma_offset <- summary(tgw_agemod_gamma_offset)

mod1sum_gammatgw <- inla_summary(tgw_gamma_offset, "TGW, gamma, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2010", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_gammatgw


tgw_agemod_gamma_offset_samples <- moz.utils::sample_model(tgw_agemod_gamma_offset, inla_tgw_age, col = "survey_id")

(tgw_ageratio_gamma_offset <- tgw_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-12, 3)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.7, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(tgw_ageratio_gamma_offset_countries <- tgw_agemod_gamma_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% tgw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = tgw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-12, 3)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(tgw_agecount_gamma_offset <- tgw_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,30)) +
    labs(x = "Year", y = "TGW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(tgw_gammaoffsetslopes_plots <- ggpubr::ggarrange(tgw_agecount_gamma_offset, tgw_ageratio_gamma_offset, tgw_ageratio_gamma_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))



######## Gamma No Offset

tgw_age_formula2 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid")

tgw_agemod_gamma_no_offset <- INLA::inla(formula = tgw_age_formula2,
                                         family = "gamma",
                                         control.compute=list(config = TRUE),
                                         data = inla_tgw_age)

tgw_gamma_no_offset <- summary(tgw_agemod_gamma_no_offset)

mod2sum_gammatgw <- inla_summary(tgw_gamma_no_offset, "TGW, gamma, slopes, NO offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2010", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod2sum_gammatgw


tgw_agemod_gamma_no_offset_samples <- moz.utils::sample_model(tgw_agemod_gamma_no_offset, inla_tgw_age, col = "survey_id")

(tgw_ageratio_gamma_no_offset <- tgw_agemod_gamma_no_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-12, 3)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


(tgw_ageratio_gamma_no_offset_countries <- tgw_agemod_gamma_no_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% tgw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = tgw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-12, 3)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(tgw_agecount_gamma_no_offset <- tgw_agemod_gamma_no_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,30)) +
    labs(x = "Year", y = "TGW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(tgw_gamma_no_offsetslopes_plots <- ggpubr::ggarrange(tgw_agecount_gamma_no_offset, tgw_ageratio_gamma_no_offset, tgw_ageratio_gamma_no_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))


###### Gamma, offset, no country slopes

tgw_age_formula3 <- age ~ 1 + year_min + f(iso3, model = "iid") + offset(log(genpop_median))

tgw_agemod_gamma_offset_noslopes <- INLA::inla(formula = tgw_age_formula3,
                                               family = "gamma",
                                               control.compute=list(config = TRUE),
                                               data = inla_tgw_age)

tgw_gamma_offset_noslopes <- summary(tgw_agemod_gamma_offset_noslopes)

mod3sum_gammatgw <- inla_summary(tgw_gamma_offset_noslopes, "TGW, gamma, NO slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2010", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod3sum_gammatgw

tgw_agemod_gamma_offset_noslopes_samples <- moz.utils::sample_model(tgw_agemod_gamma_offset_noslopes, inla_tgw_age, col = "survey_id")

(tgw_ageratio_gamma_offset_noslopes <- tgw_agemod_gamma_offset_noslopes_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    coord_cartesian(ylim = c(-12, 3)) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.7, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(tgw_ageratio_gamma_offset_noslopes_countries <- tgw_agemod_gamma_offset_noslopes_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% tgw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = tgw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-12, 3)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.7, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(tgw_agecount_gamma_offset_noslopes <- tgw_agemod_gamma_offset_noslopes_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,30)) +
    labs(x = "Year", y = "TGW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(tgw_gammaoffset_noslopes_plots <- ggpubr::ggarrange(tgw_agecount_gamma_offset_noslopes, tgw_ageratio_gamma_offset_noslopes, tgw_ageratio_gamma_offset_noslopes_countries, nrow = 1, common.legend = T, legend = "bottom"))

tgw_gammamodsums <- mod1sum_gammatgw %>% bind_rows(mod2sum_gammatgw) %>% bind_rows(mod3sum_gammatgw)
write_csv(tgw_gammamodsums, "~/Downloads/tgw_modsums2605.csv", na = "")


###############################################################################
############################## LOG-NORMAL MODELS ##############################

### Offset

tgw_agemod_lognormal_offset <- INLA::inla(formula = tgw_age_formula1,
                                          family = "lognormal",
                                          control.compute=list(config = TRUE),
                                          data = inla_tgw_age)

tgw_lognormal_offset <- summary(tgw_agemod_lognormal_offset)

mod1sum_tgwln <- inla_summary(tgw_lognormal_offset, "TGW, lognormal, slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2010", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)

mod1sum_tgwln


tgw_agemod_lognormal_offset_samples <- moz.utils::sample_model(tgw_agemod_lognormal_offset, inla_tgw_age, col = "survey_id")

(tgw_ageratio_lognormal_offset <- tgw_agemod_lognormal_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-12, 3)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(tgw_ageratio_lognormal_offset_countries <- tgw_agemod_lognormal_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% tgw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = tgw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-12, 3)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(tgw_agecount_lognormal_offset <- tgw_agemod_lognormal_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,30)) +
    labs(x = "Year", y = "TGW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(tgw_lognormaloffsetslopes_plots <- ggpubr::ggarrange(tgw_agecount_lognormal_offset, tgw_ageratio_lognormal_offset, tgw_ageratio_lognormal_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))


######## Log-Normal No Offset

tgw_age_formula2 <- age ~ 1 + year_min + f(iso3, model = "iid") + f(iso3_id, year_min, model = "iid")

tgw_agemod_lognorm_no_offset <- INLA::inla(formula = tgw_age_formula2,
                                           family = "lognormal",
                                           control.compute=list(config = TRUE),
                                           data = inla_tgw_age)

tgw_lognorm_no_offset <- summary(tgw_agemod_lognorm_no_offset)

mod2sum_tgwln <- inla_summary(tgw_lognorm_no_offset, "TGW, lognorm, slopes, NO offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2010", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod2sum_tgwln


tgw_agemod_lognorm_no_offset_samples <- moz.utils::sample_model(tgw_agemod_lognorm_no_offset, inla_tgw_age, col = "survey_id")

(tgw_ageratio_lognorm_no_offset <- tgw_agemod_lognorm_no_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    coord_cartesian(ylim = c(-12, 3)) +
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 


(tgw_ageratio_lognorm_no_offset_countries <- tgw_agemod_lognorm_no_offset_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% tgw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = tgw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-12, 3)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(tgw_agecount_lognorm_no_offset <- tgw_agemod_lognorm_no_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,30)) +
    labs(x = "Year", y = "TGW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(tgw_lognorm_no_offsetslopes_plots <- ggpubr::ggarrange(tgw_agecount_lognorm_no_offset, tgw_ageratio_lognorm_no_offset, tgw_ageratio_lognorm_no_offset_countries, nrow = 1, common.legend = T, legend = "bottom"))

###### Lognorm, offset, no country slopes


tgw_agemod_lognormal_offset_noslopes <- INLA::inla(formula = tgw_age_formula3,
                                                   family = "lognormal",
                                                   control.compute=list(config = TRUE),
                                                   data = inla_tgw_age)

tgw_lognormal_offset_noslopes <- summary(tgw_agemod_lognormal_offset_noslopes)

mod3sum_tgwln <- inla_summary(tgw_lognormal_offset_noslopes, "TGW, lognormal, NO slopes, offset", hyper = T) %>% 
  mutate(rowname = ifelse(rowname == "year_min", "Year, centred at 2010", rowname)) %>% 
  mutate(Mean = round(mean, 2),
         CrI = paste0(round(lower, 1), " - ", round(upper, 1))) %>% 
  select(Parameter = rowname, Mean, CrI, model)
mod3sum_tgwln

tgw_agemod_lognormal_offset_noslopes_samples <- moz.utils::sample_model(tgw_agemod_lognormal_offset_noslopes, inla_tgw_age, col = "survey_id")

(tgw_ageratio_lognormal_offset_noslopes <- tgw_agemod_lognormal_offset_noslopes_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)- genpop_median, ymax = exp(upper)-genpop_median), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    coord_cartesian(ylim = c(-12, 3)) +
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("SSA trend")) 



(tgw_ageratio_lognormal_offset_noslopes_countries <- tgw_agemod_lognormal_offset_noslopes_samples %>% 
    filter(model == "country_slopes",
           iso3 %in% tgw_age_dat$iso3) %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = exp(mean)-genpop_median, color = iso3)) +
    geom_ribbon(aes(x = year, ymin = exp(lower)-genpop_median, ymax = exp(upper)-genpop_median, fill = iso3), alpha = 0.05) +
    geom_point(data = tgw_age_dat , aes(x = year, y = (mean_kp_age)-genpop_median, size = denom, color = iso3), show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.7) +
    moz.utils::standard_theme() + 
    labs(x = "Year", y = "Number of years older \nthan the total population") +
    coord_cartesian(ylim = c(-12, 3)) +
    theme(aspect.ratio = 1) + 
    annotate("segment", x = 2010, xend = 2010, y = 0.5, yend = 2,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = 2.9, label = "TGW older than total \npopulation women", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    annotate("segment", x = 2010, xend = 2010, y = -0.5, yend = -5.5,
             arrow = arrow(length = unit(0.15, "inches")), color = "darkred") +
    annotate("text", x = 2010, y = -6.3, label = "TGW younger than total \npopulation women", size = 4, fontface = "bold", hjust =0, color = "darkred" ) + 
    ggtitle("Country-level trends") +
    guides(fill = guide_legend(title = "Country", nrow = 3), color = guide_legend(title = "Country", nrow = 3)))



(tgw_agecount_lognormal_offset_noslopes <- tgw_agemod_lognormal_offset_noslopes_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper)) %>%
    ggplot() +
    geom_line(aes(x = year , y = exp(mean))) +
    # geom_smooth(aes(x = year , y = exp(mean), color = iso3), method = "lm") +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_point(data = tgw_age_dat , aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    moz.utils::standard_theme() +
    lims(y = c(15,30)) +
    labs(x = "Year", y = "TGW age") +
    theme(aspect.ratio = 1) + 
    ggtitle("SSA"))


(tgw_lognormaloffset_noslopes_plots <- ggpubr::ggarrange(tgw_agecount_lognormal_offset_noslopes, tgw_ageratio_lognormal_offset_noslopes, tgw_ageratio_lognormal_offset_noslopes_countries, nrow = 1, common.legend = T, legend = "bottom"))

tgw_lognormalmodsums <- mod1sum_tgwln %>% bind_rows(mod2sum_tgwln) %>% bind_rows(mod3sum_tgwln)
write_csv(tgw_lognormalmodsums, "~/Downloads/tgw_modsums_lognorm2605.csv", na = "")
