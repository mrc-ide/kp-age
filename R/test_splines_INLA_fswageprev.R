library(tidyverse)
library(dfertility) 

####

# years <- 0:54 ## Length of variable here
years <- length(unique(pred$id.year))
year_knots <- seq(-15, 70, by = 5)
year_spline_mat <- splines::splineDesign(year_knots, years, ord = 4)
year_spline_mat <- as(year_spline_mat, "sparseMatrix")

# ages <- 0:54 ## Length of variable here
ages <- length(unique(pred$age))
age_knots <- seq(-15, 70, by = 5)
age_spline_mat <- splines::splineDesign(age_knots, ages, ord = 4)
age_spline_mat <- as(age_spline_mat, "sparseMatrix")


Z_year <-  sparse.model.matrix(~0 + id.year, data = inla_dat %>% mutate(id.year = factor(id.year)))
Z_year <- Z_year %*% year_spline_mat

Z_age <-  sparse.model.matrix(~0 + age, data = dat %>% mutate(age = factor(age)))
Z_age <- Z_age %*% age_spline_mat

A_year <- Z_year
idx_year <- inla.spde.make.index("year_spde", n.spde = ncol(Z_year))

A_age <- Z_age
idx_age <- inla.spde.make.index("age_spde", n.spde = ncol(Z_age))

A_year_age <- mgcv::tensor.prod.model.matrix(list(Z_age, Z_year))
idx_year_age <- inla.spde.make.index("year_age_spde", n.spde = ncol(A_year_age))


## Constraint matrices
n_years <- ncol(year_spline_mat)
n_ages <- ncol(age_spline_mat)
n_agetime_interactions <- n_years * n_ages

A_sum_age_year <- matrix(0, nrow = n_years, ncol = n_agetime_interactions)

for (year in 1:n_years) {
  A_sum_age_year[year, ((year - 1) * n_ages + 1):(year * n_ages)] <- 1
}

A_sum_year_age <- matrix(0, nrow = n_ages, ncol = n_agetime_interactions)

for (age in 1:n_ages) {
  A_sum_year_age[age, ((age - 1) * n_years + 1):(age * n_years)] <- 1
}

A_combined3 <- rbind(A_sum_year_age, A_sum_age_year)
e3 <- matrix(0, nrow(A_combined3), nrow = 1)

R_age <- dfertility::make_rw_structure_matrix(n_ages, 2, adjust_diagonal = F)
R_year <- dfertility::make_rw_structure_matrix(n_years, 2, adjust_diagonal = F)
Q3 <- kronecker(R_age, R_year)
diag(Q3) <- diag(Q3 + 1E-6)


############

stack <- inla.stack(tag = 'est', ## tag
                    data = list(n = dat$n, ## For some reason you need to name these differently.
                                denom = dat$denom),
                    A = list(
                      A_year,
                      A_age,
                      A_year_age,
                      1
                    ),
                    effects=list(
                      year_spde = idx_year,
                      age_spde = idx_age,
                      year_age_spde = idx_year_age,
                      dat
                    ))

form18 <- positives ~ 1 + f(year_spde, model = "rw2") + f(age_spde, model = "rw2") +  f(id.age.year, 
                                                                                        model = "generic0", 
                                                                                        Cmatrix = Q3, 
                                                                                        extraconstr = list(A = A_combined3, e = e3), 
                                                                                        rankdef = n_ages + n_years -1) 


fit18_spde <- inla(form18,
                   data = inla.stack.data(stack),
                   family = "binomial",
                   Ntrials = n,
                   # control.family = list(link = "logit"),
                   control.predictor = list(compute = TRUE,
                                            # link = 1,
                                            A = inla.stack.A(stack)),
                   control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
                   inla.mode = "experimental",
                   verbose = F)

summary(fit18_spde)

res18_spde <- sample_model(fit18_spde, dat, "positive") %>%
  mutate(across(c(mean:upper), invlogit))