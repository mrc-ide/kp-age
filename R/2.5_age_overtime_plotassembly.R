

ggpubr::ggarrange(fsw_agetime_plots, msm_agetime_plots, nrow = 1, widths = c(1.2, 2))


ggpubr::ggarrange(fsw_agecount_gamma_offset, msm_agecount_gamma_offset, fsw_ageratio_gamma_offset, msm_ageratio_gamma_offset)


count_plot_labels <- data.frame(
  kp = factor("Female sex\nworkers"),
  x = 1993,
  y = 37, 
  label = "Matched total adult \npopulation mean age"
)


(agecount_plots <- fsw_agemod_gamma_offset_samples %>%
    filter(model == "ssa_genpop") %>%
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper),
           kp = "FSW") %>%
    bind_rows(msm_agemod_gamma_offset_samples %>%
                filter(model == "ssa_genpop") %>%
                mutate(exp_mean = exp(mean),
                       exp_lower = exp(lower),
                       exp_upper = exp(upper)) %>%
                mutate(mean = ifelse(kp == "TGW" & year < 2016, NA_integer_, mean),
                       lower = ifelse(kp == "TGW" & year < 2016, NA_integer_, lower),
                       upper = ifelse(kp == "TGW" & year < 2016, NA_integer_, upper))) %>% 
    bind_rows(cfsw_agemod_gamma_offset_samples %>%
                filter(model == "ssa_genpop") %>%
                mutate(exp_mean = exp(mean),
                       exp_lower = exp(lower),
                       exp_upper = exp(upper)) %>% 
                mutate(kp = "CFSW")) %>% 
    bind_rows(pwid_agemod_gamma_offset_samples %>%
                filter(model == "ssa_genpop") %>%
                mutate(exp_mean = exp(mean),
                       exp_lower = exp(lower),
                       exp_upper = exp(upper),
                       kp = ifelse(sex == "female", "Female PWID", "Male PWID"))) %>% 
    mutate(sex = ifelse(kp %in%  c("MSM", "Male PWID", "CFSW"), "male", "female")) %>% 
    moz.utils::name_kp() %>% 
    mutate(kp = case_when(kp == "Male PWID" ~ "Males who \ninject drugs", 
                          kp == "Female PWID" ~ "Females who \ninject drugs",
                          TRUE ~ kp),
           kp = factor(kp, levels = c("Female sex\nworkers", "Men who have\nsex with men", "Transgender\nwomen", "Males who \ninject drugs", "Females who \ninject drugs", "Clients of female\nsex workers"))) %>% 
    ggplot() +
    geom_line(aes(x = year , y = exp(mean)), linewidth = 0.75) +
    geom_ribbon(aes(x = year, ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
    geom_line(data = genpop_median_ages %>% 
                mutate(kp = ifelse(sex == "female", "Female sex\nworkers", "Men who have\nsex with men")) %>% 
                bind_rows(genpop_median_ages %>% 
                            mutate(kp = ifelse(sex == "female", "Females who \ninject drugs", "Males who \ninject drugs"))) %>% 
                bind_rows(genpop_median_ages %>% mutate(kp = ifelse(sex == "female", "Transgender\nwomen", "Clients of female\nsex workers"))) %>% 
                select(year, kp, megamean) %>% 
                distinct() %>% 
                mutate(kp = factor(kp, levels = c("Female sex\nworkers", "Men who have\nsex with men", "Transgender\nwomen", "Males who \ninject drugs", "Females who \ninject drugs", "Clients of female\nsex workers"))), 
              aes(x = year, y = megamean), 
              color = "darkred", 
              linetype = "dashed", 
              linewidth = 0.75) +
    geom_point(data = fsw_age_dat %>% 
                 bind_rows(msmtg_age_dat %>% filter(!kp == "TGM_other")) %>% 
                 bind_rows(pwid_age_dat %>% mutate(kp = ifelse(sex == "female", "Female PWID", "Male PWID"))) %>% 
                 bind_rows(cfsw_age_dat) %>% 
                 moz.utils::name_kp() %>% 
                 mutate(kp = case_when(kp == "Male PWID" ~ "Males who \ninject drugs", 
                                       kp == "Female PWID" ~ "Females who \ninject drugs",
                                       TRUE ~ kp),
                        kp = factor(kp, levels = c("Female sex\nworkers", "Men who have\nsex with men", "Transgender\nwomen", "Males who \ninject drugs", "Females who \ninject drugs", "Clients of female\nsex workers"))) , 
               aes(x = year, y = mean_kp_age, size = denom, color = iso3), show.legend = F) +
    geom_text(data = count_plot_labels, aes(x = x, y = y, label = label), size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    # annotate("text", x = 1993, y = 37, label = "Matched population mean \nage", size = 4, fontface = "bold", hjust =0 , color = "darkred") +
    moz.utils::standard_theme() +
    lims(y = c(15,40)) +
    labs(x = "", y = "Age \n ") +
    theme(aspect.ratio = 1) +
    facet_wrap(~kp, nrow = 1) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm")))





plot_labels <- data.frame(
  kp = factor("Female sex\nworkers"),
  x = 1993, xend = 1993, 
  y = c(0.5, -0.5), 
  yend = c(2, -12),
  label = c("KP older than total \npopulation",
            "KP younger than total \npopulation"),
  label_y = c(3.5, -13)
)



(ageratio_plots <- fsw_agemod_gamma_offset_samples %>% 
    filter(model == "ssa_genpop") %>% 
    mutate(exp_mean = exp(mean),
           exp_lower = exp(lower),
           exp_upper = exp(upper),
           kp = "FSW") %>% 
    bind_rows(msm_agemod_gamma_offset_samples %>% 
                filter(model == "ssa_genpop") %>% 
                mutate(exp_mean = exp(mean),
                       exp_lower = exp(lower),
                       exp_upper = exp(upper)) %>% 
                mutate(mean = ifelse(kp == "TGW" & year < 2016, NA_integer_, mean),
                       lower = ifelse(kp == "TGW" & year < 2016, NA_integer_, lower),
                       upper = ifelse(kp == "TGW" & year < 2016, NA_integer_, upper))) %>% 
    bind_rows(cfsw_agemod_gamma_offset_samples %>% 
                filter(model == "ssa_genpop") %>% 
                mutate(exp_mean = exp(mean),
                       exp_lower = exp(lower),
                       exp_upper = exp(upper),
                       kp = "CFSW")) %>% 
    bind_rows(pwid_agemod_gamma_offset_samples %>% 
                filter(model == "ssa_genpop") %>% 
                mutate(exp_mean = exp(mean),
                       exp_lower = exp(lower),
                       exp_upper = exp(upper),
                       kp = ifelse(sex == "female", "Female PWID", "Male PWID"))) %>% 
  mutate(sex = ifelse(kp %in%  c("MSM", "Male PWID", "CFSW"), "male", "female")) %>% 
  moz.utils::name_kp() %>% 
  mutate(kp = case_when(kp == "Male PWID" ~ "Males who \ninject drugs", 
                        kp == "Female PWID" ~ "Females who \ninject drugs",
                        TRUE ~ kp),
         kp = factor(kp, levels = c("Female sex\nworkers", "Men who have\nsex with men", "Transgender\nwomen", "Males who \ninject drugs", "Females who \ninject drugs", "Clients of female\nsex workers"))) %>% 
    ggplot() + 
    geom_line(aes(x = year , y = (exp(mean))-genpop_mean)) +
    geom_ribbon(aes(x = year, ymin = (exp(lower))- genpop_mean, ymax = (exp(upper))-genpop_mean), alpha = 0.3) +
    geom_point(data = fsw_age_dat %>% 
                 bind_rows(msmtg_age_dat %>% filter(!kp == "TGM_other")) %>% 
                 bind_rows(pwid_age_dat %>% mutate(kp = ifelse(sex == "female", "Female PWID", "Male PWID"))) %>% 
                 bind_rows(cfsw_age_dat) %>% 
                 moz.utils::name_kp() %>% 
                 mutate(kp = case_when(kp == "Male PWID" ~ "Males who \ninject drugs", 
                                       kp == "Female PWID" ~ "Females who \ninject drugs",
                                       TRUE ~ kp),
                        kp = factor(kp, levels = c("Female sex\nworkers", "Men who have\nsex with men", "Transgender\nwomen", "Males who \ninject drugs", "Females who \ninject drugs", "Clients of female\nsex workers"))), 
               aes(x = year, 
                   y = mean_kp_age-genpop_mean, 
                   size = denom, 
                   color = iso3), 
               show.legend = F) + 
    geom_hline(yintercept = 0 , color = "darkred", linetype = "dotted", linewidth = 0.75) +
    moz.utils::standard_theme() + 
    lims(y = c(-15, 5)) +
    labs(x = "", y = "Number of years older \nthan the total population") +
    theme(aspect.ratio = 1) + 
    geom_segment(
      data = plot_labels,
      aes(x = x, xend = xend, y = y, yend = yend),
      arrow = arrow(length = unit(0.15, "inches")),
      color = "darkred",
      inherit.aes = FALSE) +
    geom_text(
      data = plot_labels,
      aes(x = x, y = label_y, label = label), size = 4, fontface = "bold", hjust = 0, color = "darkred",
      inherit.aes = FALSE) + 
    facet_wrap(~kp, nrow = 1) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))) 


ggpubr::ggarrange(agecount_plots, ageratio_plots, nrow = 2)
