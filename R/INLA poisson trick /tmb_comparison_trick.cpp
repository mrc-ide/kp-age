#include <TMB.hpp>

// template<class Type>
// Type logit(const Type x) {
//   return log(x/(1-x));
// }

// template<class Type>
// Type invlogit(const Type x) {
//   return exp(x)/(1+exp(x));
// }

template<class Type>
Type objective_function<Type>::operator() ()




{

  using namespace density;
  using namespace Eigen;

  Type nll = 0;

  PARAMETER_VECTOR(beta_0);

  DATA_SPARSE_MATRIX(M_obs);
  DATA_VECTOR(observed_x);
  DATA_SPARSE_MATRIX(X_stand_in);
  DATA_SPARSE_MATRIX(R_beta);
  DATA_VECTOR(logit_totpop)

  int number_surveys = observed_x.rows(); // Number of rows (surveys)
  int number_age = observed_x.cols(); // Number of cols (age categories)

  // nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true).sum();  //Prior for the intercept, v diffuse prior

  PARAMETER(log_sigma_rw_beta);
  Type sigma_rw_beta = exp(log_sigma_rw_beta);
  nll -= dnorm(sigma_rw_beta, Type(0), Type(2.5), true) + log_sigma_rw_beta;
  // // Run these two if you want iid/RW instead of AR1
    nll -= Type(-0.5) * (beta_0 * (R_beta * beta_0)).sum();
  nll -= dnorm(beta_0.sum(), Type(0), Type(0.01) * beta_0.size(), true);

  // /// Make AR1 on beta 0 - Run 50-56
  // PARAMETER(lag_logit_phi_beta);
  // nll -= dnorm(lag_logit_phi_beta, Type(0), Type(sqrt(1/0.15)), true);
  // Type phi_beta = 2*exp(lag_logit_phi_beta)/(1+exp(lag_logit_phi_beta))-1;
  // 
  // nll+= AR1(Type(phi_beta))(beta_0);

//   ///////////// Time * Age
  DATA_SPARSE_MATRIX(Z_periodage);
  // DATA_SPARSE_MATRIX(R_period);
  DATA_SPARSE_MATRIX(R_age);

  PARAMETER_ARRAY(eta2);
  PARAMETER(log_sigma_eta2);
  // PARAMETER(logit_eta2_phi_age);
  // PARAMETER(logit_eta2_phi_period)
  PARAMETER(lag_logit_eta2_phi_age);
  PARAMETER(lag_logit_eta2_phi_period)

    //
  Type sigma_eta2 = exp(log_sigma_eta2);
  nll -= dnorm(sigma_eta2, Type(0), Type(2.5), true) + log_sigma_eta2;

  nll -= dnorm(lag_logit_eta2_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  Type eta2_phi_age = 2*exp(lag_logit_eta2_phi_age)/(1+exp(lag_logit_eta2_phi_age))-1;

  nll -= dnorm(lag_logit_eta2_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  Type eta2_phi_period = 2*exp(lag_logit_eta2_phi_period)/(1+exp(lag_logit_eta2_phi_period))-1;

  nll += SEPARABLE(AR1(Type(eta2_phi_period)), AR1(Type(eta2_phi_age)))(eta2);

  for (int i = 0; i < eta2.cols(); i++) {
    nll -= dnorm(eta2.col(i).sum(), Type(0), Type(0.001) * eta2.col(i).size(), true);}

  vector<Type> eta2_v(eta2);

  //// survey iid
  
  // PARAMETER(log_sigma_survey);
  PARAMETER_VECTOR(u_survey);
  DATA_SPARSE_MATRIX(Z_survey);
  DATA_SPARSE_MATRIX(R_survey);
  // Type sigma_survey = exp(log_sigma_survey);
  // nll -= dnorm(sigma_survey, Type(0), Type(2.5), true) + log_sigma_survey;
  
  nll -= Type(-0.5) * (u_survey * (R_survey * u_survey)).sum();

  /////////// 

  vector<Type> log_p(
      // X_stand_in * beta_0 //intercept only
      X_stand_in * beta_0 * sigma_rw_beta
  // + Z_yeariso * interaction
    // + Z_spaceage * eta3_v * sigma_eta3
    // + Z_spaceage * eta3_v * sigma_eta3
    + Z_periodage * eta2_v * sigma_eta2
  );

  vector<Type> log_p2(M_obs*log_p 
                          + Z_survey * u_survey * Type(1000)
                          // + logit_totpop
    );
                          // + Z_survage * eta_surv_v * sqrt(1/prec_eta_surv));

  nll -= dpois(observed_x, exp(log_p2), true).sum();

  return nll;




  // vector<Type> logit_p(
  //     X_stand_in * beta_0 * sqrt(1/prec_rw_beta)
  //   // + Z_spaceage * eta3_v * sqrt(1/prec_eta3)
  //   // + Z_periodage * eta2_v * sqrt(1/prec_eta2)
  //   + logit_totpop
  // );
  //
  // vector<Type> logit_p2((M_obs*logit_p)
  //                         + Z_survage * eta_surv_v * sqrt(1/prec_eta_surv));
  //
  // vector<Type> p(exp(logit_p2));
  //
  // array<Type> p_arr(number_surveys,number_age);
  //
  // for(int i=0; i<number_surveys; i++) {
  //   for(int j=0; j<number_age; j++) {
  //     p_arr(i,j) = p((i*number_age) + j);
  //   }
  // }
  //
  // matrix<Type> p_norm(number_surveys, number_age);
  //
  // for (int i=0; i<number_surveys; i++) {
  //   vector<Type> p_row(p_arr.matrix().row(i));
  //   vector<Type> p_row_norm(p_row/p_row.sum());
  //   vector<Type> x_row(observed_x.row(i));
  //
  //   nll -= dmultinom(x_row, p_row_norm, true);
  //
  //   p_norm.row(i) = p_row_norm;
  //
  // }
  //
  //
  // REPORT(p_norm);
  // REPORT(logit_p);
  //
  // return nll;

}

