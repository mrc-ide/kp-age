# install.packages("nnet")
library(nnet)
library(tidyverse)
library(TMB)
library(Matrix)
# nga <- readRDS("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/NGA/NGA2020BBS_FSW/NGA2020BBS_FSW.rds")
# nga <- nga %>% 
#   mutate(age2 = age,
#   age = as.factor(age))
# 
# model <- multinom(age_group ~ 1 + year * iso , data = new_agedata)
# 
# plot(model$fitted.values[1,])
# 
# age_values <- as.factor(seq(from = min(nga$age2), to = max(nga$age2), by = 1))
# predicted_probs <- predict(model, newdata = data.frame(age = age_values), type = "probs")
# total_counts <- 4974 
# expected_counts <- data.frame(predicted_probs * total_counts)
# 
# expected_counts <- expected_counts[1,] %>% 
#   pivot_longer(cols = everything(), names_to = "age", values_to = "count") %>% 
#   mutate(id.age = row_number())
# 
# 
# nga2 <- nga %>% 
#   group_by(age2) %>% 
#   mutate(count = n()) %>% 
#   ungroup() %>% 
#   select(age2, count) %>% 
#   distinct() %>% 
#   arrange(age2) %>% 
#   mutate(age.id = row_number()) %>% 
#   select(age.id, count)
# 
# ggplot(expected_counts, aes(x = id.age, y = count)) +
#   geom_line() +
#   labs(x = "Age", y = "Number of People", color = "Age Group") +
#   scale_color_discrete(name = "Age Group") +
#   theme_minimal() + 
#   geom_line(aes(x = nga2$age.id, y = nga2$count, color = "orange"))
# 
# 
# 
# # mnl_pred_ova(model = model,
# #              data = ages_to_predict,
# #              x = "age",
# #              by = 1,
# #              seed = "random", # default
# #              nsim = 100, # faster
# #              probs = c(0.025, 0.975))
# # # Print the results
# # print(results)
# 
# 
# model <-VGAM::vglm(age ~ 1, family = multinomial, data = nga)
# plot(model@fitted.values[1,])
# 
# 
# # predict(model, newdata = data.frame(age = age_values), type = "probs")
# # VGAM::predict.vglm(model, newdata = data.frame(age = agevalues), type = "response", se.fit = T)
# 
# prediction <- predictvglm(object = model, newdata = data.frame(age = age_values), 
#              type =  "link", se.fit = T)
# 
# plot(prediction[1,])
# 
# full_dat
# a <-  data.frame(prediction[["fitted.values"]][1,],  prediction[["se.fit"]][1,]) %>% 
#   mutate(age.id = row_number()) %>% 
#   rename(logestimate = prediction...fitted.values....1..., se = prediction...se.fit....1...) %>% 
#   mutate(loglower = logestimate - 1.96*se,
#          logupper = logestimate + 1.96*se, 
#          lower = exp(loglower),
#          upper = exp(logupper),
#          estimate = exp(logestimate)) 
# 
# nga3 <- nga2 %>% 
#   filter(!age.id == 48)
# 
# full_data %>% 
#   ggplot() + 
#   geom_line(aes(x = age.id, y = estimate)) +
#   geom_ribbon(aes(x = age.id, ymin = lower, ymax = upper), alpha = 0.2) + 
#   moz.utils::standard_theme() +
#   geom_point(aes(x = nga3$age.id, y = nga3$count)) + 
#   coord_cartesian(ylim = c(0, 1000))
# 


basic_age <- readxl::read_xlsx("data (public)/basic_age_mod.xlsx") %>% 
  filter(!age_group == 6) %>% 
  mutate(iso = factor(multi.utils::to_int(country))) %>% 
  mutate(odds = prop/(1- prop)) %>% 
  group_by(survey_id) %>% 
  mutate(odds_ratio = odds/odds[age_group == 1]) %>% 
  ungroup()

basic_age2 <- basic_age


# new_agedata <- data.frame(
#   age_group = rep(basic_age$age_group, basic_age$count),
#   iso = rep(basic_age$iso, basic_age$count)
# ) %>% 
#   left_join((basic_age %>% select(country, iso, year, old_age, age_group, survey_id))) %>% 
#   mutate(age_group = as.factor(age_group),
#          iso = as.factor(iso))
# 
# # model <-VGAM::vglm(age_group ~ 1 + year, family = "multinomial", data = new_agedata)
# # plot(model@fitted.values[1,])
# # model
# 
# model <- multinom(age_group ~ 1 , data = new_agedata)
# summary(model)
# exp(coef(model))
# 
# predicted_probs <- predict(model, newdata = new_agedata, type = "probs")
# 
# predicted_probs <- data.frame(predicted_probs) %>% distinct() %>% pivot_longer(cols = everything(), names_to = "age_group", values_to = "prob")
# 
# prediction_data <- basic_age %>% 
#   left_join(predicted_probs %>% mutate(age_group = case_when(age_group == "X1" ~ 1,
#                                                              age_group == "X2" ~ 2,
#                                                              age_group == "X3" ~ 3,
#                                                              age_group == "X4" ~ 4,
#                                                              age_group == "X5" ~ 5)))
# 
# intercept_only <- prediction_data %>% 
#   ggplot(aes(x = age_group)) + 
#   geom_line(aes(y = prob)) + 
#   geom_point(aes(y = prop)) + 
#   facet_wrap(~year+country) +
#   moz.utils::standard_theme()
# 
# 
# model2 <- multinom(age_group ~ 1 + year , data = new_agedata)
# summary(model2)
# exp(coef(model2))
# 
# predicted_probs2 <- predict(model2, newdata = new_agedata, type = "probs")
# 
# predicted_probs2 <- data.frame(predicted_probs2) %>% distinct() %>% rownames_to_column() %>% left_join(new_agedata %>% rownames_to_column() %>% select(rowname, year)) %>% select(-rowname) %>% pivot_longer(cols = X1:X5, names_to = "age_group", values_to = "prob")
# 
# prediction_data2 <- basic_age %>% 
#   left_join(predicted_probs2 %>% mutate(age_group = case_when(age_group == "X1" ~ 1,
#                                                              age_group == "X2" ~ 2,
#                                                              age_group == "X3" ~ 3,
#                                                              age_group == "X4" ~ 4,
#                                                              age_group == "X5" ~ 5)))
# 
# year_fe <- prediction_data2 %>% 
#   ggplot(aes(x = age_group)) + 
#   geom_line(aes(y = prob)) + 
#   geom_point(aes(y = prop)) + 
#   facet_wrap(~country+year) +
#   moz.utils::standard_theme()
# 
# 
# model3 <- multinom(age_group ~ 1 + year + iso, data = new_agedata)
# summary(model3)
# exp(coef(model3))
# 
# predicted_probs3 <- predict(model3, newdata = new_agedata, type = "probs")
# 
# predicted_probs3 <- data.frame(predicted_probs3) %>% distinct() %>% rownames_to_column() %>% left_join(new_agedata %>% rownames_to_column() %>% select(rowname, year, country)) %>% select(-rowname) %>% pivot_longer(cols = X1:X5, names_to = "age_group", values_to = "prob")
# 
# prediction_data3 <- basic_age %>% 
#   left_join(predicted_probs3 %>% mutate(age_group = case_when(age_group == "X1" ~ 1,
#                                                               age_group == "X2" ~ 2,
#                                                               age_group == "X3" ~ 3,
#                                                               age_group == "X4" ~ 4,
#                                                               age_group == "X5" ~ 5)))
# 
# year_iso_fe <- prediction_data3 %>% 
#   ggplot(aes(x = age_group)) + 
#   geom_line(aes(y = prob)) + 
#   geom_point(aes(y = prop)) + 
#   facet_wrap(~country+year) +
#   moz.utils::standard_theme()
# 
# 
# 
# model4 <- multinom(age_group ~ 1 + iso, data = new_agedata)
# summary(model4)
# exp(coef(model4))
# 
# predicted_probs4 <- predict(model4, newdata = new_agedata, type = "probs")
# 
# predicted_probs4 <- data.frame(predicted_probs4) %>% distinct() %>% rownames_to_column() %>% left_join(new_agedata %>% rownames_to_column() %>% select(rowname, country)) %>% select(-rowname) %>% pivot_longer(cols = X1:X5, names_to = "age_group", values_to = "prob")
# 
# prediction_data4 <- basic_age %>% 
#   left_join(predicted_probs4 %>% mutate(age_group = case_when(age_group == "X1" ~ 1,
#                                                               age_group == "X2" ~ 2,
#                                                               age_group == "X3" ~ 3,
#                                                               age_group == "X4" ~ 4,
#                                                               age_group == "X5" ~ 5)))
# 
# prediction_data4 %>% 
#   ggplot(aes(x = age_group)) + 
#   geom_line(aes(y = prob)) + 
#   geom_point(aes(y = prop)) + 
#   facet_wrap(~country+year) +
#   moz.utils::standard_theme()
# 
# 
# model5.a <- multinom(cbind(age1, age2, age3, age4, age5) ~ 1 , data = counts)
# summary(model5.a)
# exp(coef(model5))
# 
# 
# counts <- basic_age %>% 
#   select(age_group, survey_id, age_group, count, year) %>% 
#   pivot_wider(names_from = age_group, values_from = count, names_prefix = "age") 
# 
# counts 
# 
# predicted_probs5 <- predict(model5, newdata = counts, type = "probs")
# 
# # predicted_probs5 <- data.frame(predicted_probs5) %>% distinct() %>% rownames_to_column() %>% left_join(new_agedata %>% rownames_to_column() %>% select(rowname, country, year)) %>% select(-rowname) %>% pivot_longer(cols = X1:X5, names_to = "age_group", values_to = "prob")
# # 
# # prediction_data5 <- basic_age %>% 
# #   left_join(predicted_probs5 %>% mutate(age_group = case_when(age_group == "X1" ~ 1,
# #                                                               age_group == "X2" ~ 2,
# #                                                               age_group == "X3" ~ 3,
# #                                                               age_group == "X4" ~ 4,
# #                                                               age_group == "X5" ~ 5)))
# # 
# # prediction_data5 %>% 
# #   ggplot(aes(x = age_group)) + 
# #   geom_line(aes(y = prob)) + 
# #   geom_point(aes(y = prop)) + 
# #   facet_wrap(~country+year) +
# #   moz.utils::standard_theme()
# # 
# # 
# # model6 <- multinom(age_group ~ 1 + survey_id, data = new_agedata)
# # summary(model6)
# # exp(coef(model6))
# # 
# # predicted_probs6 <- predict(model6, newdata = new_agedata, type = "probs")
# # 
# # predicted_probs6 <- data.frame(predicted_probs6) %>% distinct() %>% rownames_to_column() %>% left_join(new_agedata %>% rownames_to_column() %>% select(rowname, country, year, survey_id)) %>% select(-rowname) %>% pivot_longer(cols = X1:X5, names_to = "age_group", values_to = "prob")
# # 
# # prediction_data6 <- basic_age %>% 
# #   left_join(predicted_probs6 %>% mutate(age_group = case_when(age_group == "X1" ~ 1,
# #                                                               age_group == "X2" ~ 2,
# #                                                               age_group == "X3" ~ 3,
# #                                                               age_group == "X4" ~ 4,
# #                                                               age_group == "X5" ~ 5)))
# # 
# # prediction_data6 %>% 
# #   ggplot(aes(x = age_group)) + 
# #   geom_line(aes(y = prob)) + 
# #   geom_point(aes(y = prop)) + 
# #   facet_wrap(~country+year) +
# #   moz.utils::standard_theme()
# # 
# # 
# # 
# # multinom(age_group )
# # 
# # model6 <- multinom(age_group ~ 1 + survey_id + year, data = new_agedata)
# # summary(model6)
# # exp(coef(model6))
# # 
# # predicted_probs6 <- predict(model6, newdata = new_agedata, type = "probs")
# # 
# # predicted_probs6 <- data.frame(predicted_probs6) %>% distinct() %>% rownames_to_column() %>% left_join(new_agedata %>% rownames_to_column() %>% select(rowname, country, year, survey_id)) %>% select(-rowname) %>% pivot_longer(cols = X1:X5, names_to = "age_group", values_to = "prob")
# # 
# # prediction_data6 <- basic_age %>% 
# #   left_join(predicted_probs6 %>% mutate(age_group = case_when(age_group == "X1" ~ 1,
# #                                                               age_group == "X2" ~ 2,
# #                                                               age_group == "X3" ~ 3,
# #                                                               age_group == "X4" ~ 4,
# #                                                               age_group == "X5" ~ 5)))
# # 
# # prediction_data6 %>% 
# #   ggplot(aes(x = age_group)) + 
# #   geom_line(aes(y = prob)) + 
# #   geom_point(aes(y = prop)) + 
# #   facet_wrap(~country+year) +
# #   moz.utils::standard_theme()
# 
# 
# 
# # model <- multinom(age_group ~ 1 )
# # 
# # basic_model <- data.frame(model@fitted.values[1,]) %>% 
# #   rownames_to_column() %>% 
# #   mutate(age_group = as.numeric(rowname),
# #          value = as.numeric(`model.fitted.values.1...`)) %>% 
# #   right_join(basic_age) %>% 
# #   ggplot() + 
# #   geom_line(aes(x = age_group, y = value), linewidth = 1) + 
# #   geom_point(aes(x = age_group, y = prop, color = iso)) +
# #   moz.utils::standard_theme()
# # 
# # basic_age <- basic_age %>% 
# #   mutate(age_group = factor(age_group))
# # 
# # 
# # model2 <-VGAM::vglm(age_group ~ 1 + year, family = "multinomial", data = new_agedata)
# # plot(model2@fitted.values[1,])
# # model2
# # 
# # yearfe_model <- data.frame(model2@fitted.values[1,]) %>% 
# #   rownames_to_column() %>% 
# #   mutate(age_group = as.numeric(rowname),
# #          value = as.numeric(`model2.fitted.values.1...`)) %>% 
# #   right_join(basic_age) %>% 
# #   ggplot() + 
# #   geom_line(aes(x = age_group, y = value), linewidth = 1) + 
# #   geom_point(aes(x = age_group, y = prop, color = iso)) +
# #   moz.utils::standard_theme()
# # 
# # 
# # model3 <-VGAM::vglm(age_group ~ 1 + year + country, family = "multinomial", data = new_agedata)
# # plot(model3@fitted.values[1,])
# # model3
# # 
# # ##Error in vglm.fitter(x = x, y = y, w = w, offset = offset, Xm2 = Xm2,  : 
# # #vglm() only handles full-rank models (currently)
# # 
# # 
# # yearfe_model <- data.frame(model3@fitted.values[1,]) %>% 
# #   rownames_to_column() %>% 
# #   mutate(age_group = as.numeric(rowname),
# #          value = as.numeric(`model3.fitted.values.1...`)) %>% 
# #   right_join(basic_age) %>% 
# #   ggplot() + 
# #   geom_line(aes(x = age_group, y = value), linewidth = 1) + 
# #   geom_point(aes(x = age_group, y = prop, color = iso)) +
# #   moz.utils::standard_theme()
# # 
# # ggpubr::ggarrange(basic_model, yearfe_model)
# # 
# # ggplot(
# #   geom_point(aes(model, y = model@fitted.values[1,]))
# # )
# 
# 
# 
# 
# 
# 
# ## TMB 
# library(Matrix)
# library(TMB)
# 
# # mf_model <- crossing(
# #   iso = c(1,2,3),
# #   age_group = c(1,2,3,4,5)
# # ,
# # year = c(2006, 2009, 2011, 2014, 2017)) %>% 
# #   mutate(iso = factor(iso),
# #          age_group = factor(age_group),
# #          idx = factor(row_number()))
# # 
# # new_agedata <- new_agedata %>% 
# #   left_join(mf_model)
# 
# # for running one survey:
# # basic_age <- basic_age2 %>% 
# #   filter(area == "Adama") %>% 
# #   mutate(idx = factor(row_number()),
# #          age_group = factor(age_group),
# #          year = factor(year))

# for running multiple surveys:
basic_age <- basic_age2 %>% 
    mutate(idx = factor(row_number()),
           age_group = factor(age_group),
           year = factor(year),
           id.iso3 = factor(iso),
           id.age = factor(age_group))

M_obs <- sparse.model.matrix(~0 + idx, basic_age)
Z_age <- sparse.model.matrix(~0 + age_group, basic_age)
# Z_survey <- sparse.model.matrix(~0 + iso, basic_age)

# X_year <- model.matrix(~0 + year, basic_age)

X_age_group <- model.matrix(~0 + age_group, basic_age)

X_stand_in <- sparse.model.matrix(~0 + age_group, basic_age)
# X_stand_in[,1] <- 0
X_stand_in <- X_stand_in[,c(2:5)] 

# X_stand_in <- model.matrix(~1, basic_age)

Z_spatial <- sparse.model.matrix(~0 + id.iso3, basic_age)
# Z_spatial <- sparse.model.matrix(~0 + age_group, basic_age)

Z_age <- sparse.model.matrix(~0 + id.age, basic_age)

Z_spaceage <- mgcv::tensor.prod.model.matrix(list(Z_spatial, Z_age))

observed_x <- matrix(basic_age$count, nrow = length(unique(basic_age$survey_id)), byrow = TRUE)

Z_survey <- sparse.model.matrix(~0 + survey_id, basic_age)

Z_survage <- mgcv::tensor.prod.model.matrix(list(Z_survey, Z_age))

tmb_int <- list()


tmb_int$data <- list(
  M_obs = M_obs,
  observed_x = observed_x,
  X_stand_in = X_stand_in,
  
  # age as a FE 
  # X_age_group = X_age_group
  #year as a FE
  # X_year = X_year

  #age as a rw 
  # Z_age = Z_age,
  # R_age = dfertility::make_rw_structure_matrix(ncol(Z_age), 1, adjust_diagonal = TRUE)
  
  # space
  Z_spatial = Z_spatial,
  R_spatial = as(diag(1, nrow = length(unique(basic_age$id.iso3))), "dgCMatrix"),
  
  # Z_survey = Z_survey,
  R_survey = as(diag(1, nrow = length(unique(basic_age$survey_id))), "dgCMatrix"),
  

  # Z_age = Z_age,
  # R_age = as(diag(1, nrow = length(unique(basic_age$id.age))), "dgCMatrix"),
  
  Z_survage = Z_survage
  # Z_spatial = Z_spatial,
  # R_spatial = R_spatial
  
  )

tmb_int$par <- list(
  beta_0 = rep(0, ncol(X_stand_in)),
  
  # u_spatial = rep(0, ncol(Z_spatial)),
  # log_prec_spatial = 0
  
  # beta_age = rep(0, ncol(X_age_group))
  
  
  # beta_year = rep(0, ncol(X_year))
  
  # u_age = rep(0, ncol(Z_age)),
  # log_prec_rw_age = 0,
  # 
  # u_survey = rep(0, ncol(Z_survey)),
  # log_prec_survey = 0,
  # 
  eta3 = array(0, c(ncol(Z_survey), ncol(Z_age))),
  log_prec_eta3 = 0, 
  logit_eta3_phi_age = 0
)

tmb_int$random <- c(               # Put everything here except hyperparamters
  "beta_0",
  # "beta_age"
  #"beta_year"
  # "u_age"
  # "u_survey",
  # "u_spatial"
  "eta3"
)

file.remove("src/tmb_sample.dll")
file.remove("src/tmb_sample.o")
file.remove("src/tmb_sample.so")

tmb_unload <- function(name) {   
  ldll <- getLoadedDLLs()   
  idx  <- grep(name, names(ldll))   
  for (i in seq_along(idx)) 
    dyn.unload(unlist(ldll[[idx[i]]])$path)
  cat('Unload ', length(idx), "loaded versions.\n") 
  }

tmb_unload("tmb_sample")

TMB::compile("src/tmb_sample.cpp", flags = "-w")
dyn.load(dynlib("src/tmb_sample"))

f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                    parameters = tmb_int$par,
                    random = tmb_int$random,
                    DLL = "tmb_sample",
                    silent=0,
                    checkParameterOrder=FALSE)
})



if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "tmb_sample",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par


fit <- c(f, obj = list(obj))

fit$sdreport <- sdreport(fit$obj, fit$par)
sd_report <- fit$sdreport
sd_report <- summary(sd_report, "all")
sd_report

c(0, 0.5280903, -0.3171204, -1.7408908, -1.8740775) %>% plogis
# TMB [1] 1.0000000 1.6956910 0.7282431 0.1753641 0.1534965 TMB 

# nnet:    1.   1.2580862 0.8472631. 0.29839. 0.2661504  - recovers the correct probs

basic_age %>% mutate(newodds = prop/prop[age_group == 1], newOR = newodds/newodds[age_group == 1]) %>% select(newOR, everything())  #nnet odds

# TMB Doesn't perform so well when we use ETH, Adama data. 
c(0 , 8.3998734, -1.7693821, -0.9977373, -3.2756717) %>% plogis
# [1] 1.000000e+00 4.446504e+03 1.704383e-01 3.687128e-01 3.779148e-02
# Real ORs: 1.      6.           0.3516.      0.698.       0.083. 

class(fit) <- "naomi_fit"
# debugonce(naomi::sample_tmb)
fit <- naomi::sample_tmb(fit, random_only=TRUE)  # Error in -r : invalid argument to unary operator

# int <- apply(fit$sample$logit_p, 1, quantile, c(0.025, 0.975))
int <- apply(fit$sample$p_norm, 1, quantile, c(0.025, 0.975))

estimated_mf <- data.frame(matrix(rowMeans(fit$sample$p_norm), nrow = 5, ncol = 7, byrow = T)) %>% 
  rownames_to_column() %>% 
  rename(age_group = rowname) %>% 
  type.convert(as.is = T) %>% 
  pivot_longer(cols = X1:X7, names_to = "survey", values_to = "mean") %>% 
  left_join(
data.frame(matrix(basic_age$survey_id, nrow = 5, ncol = 7, byrow = F)) %>% 
  pivot_longer(everything(), names_to = "survey", values_to = "survey_id") %>% 
  distinct()) %>% 
  left_join(
data.frame(matrix(int[1,], nrow = 5, ncol = 7, byrow = T)) %>% 
  rownames_to_column() %>% 
  rename(age_group = rowname) %>% 
  type.convert(as.is = T) %>% 
  pivot_longer(cols = X1:X7, names_to = "survey", values_to = "lower") %>% 
  left_join(
    data.frame(matrix(basic_age$survey_id, nrow = 5, ncol = 7, byrow = F)) %>% 
      pivot_longer(everything(), names_to = "survey", values_to = "survey_id") %>% 
      distinct()) 
) %>% 
  left_join(
    data.frame(matrix(int[2,], nrow = 5, ncol = 7, byrow = T)) %>% 
  rownames_to_column() %>% 
  rename(age_group = rowname) %>% 
  type.convert(as.is = T) %>% 
  pivot_longer(cols = X1:X7, names_to = "survey", values_to = "upper") %>% 
  left_join(
    data.frame(matrix(basic_age$survey_id, nrow = 5, ncol = 7, byrow = F)) %>% 
      pivot_longer(everything(), names_to = "survey", values_to = "survey_id") %>% 
      distinct())
  )


estimated_mf <- basic_age %>%
  mutate(lower = int[1,],
         mean = rowMeans(fit$sample$p_norm),
         upper = int[2,])


estimated_mf %>%
  # group_by(survey_id) %>%
  # mutate(across(lower:upper, ~exp(.x)/sum(exp(.x)))) %>%
  # select(lower:upper, everything()) %>% 
  # mutate(across(lower:upper, ~exp(.x))) %>% 
  # ungroup() %>%
  mutate(age_group = type.convert(age_group, as.is = T)) %>% 
  ggplot(aes(x = age_group)) +
  geom_line(aes(y = mean)) +
  geom_line(data = basic_age %>% mutate(age_group = as.numeric(age_group)), aes(x = age_group , y = prop, color = survey_id)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =0.2) +
  facet_wrap(~survey_id) +
  moz.utils::standard_theme()
