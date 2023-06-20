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

  PARAMETER(beta_0);

  DATA_SPARSE_MATRIX(M_obs);
  DATA_ARRAY(observed_x);

  int number_surveys = observed_x.rows(); // Number of rows (surveys)
  int number_age = observed_x.cols(); // Number of cols (age categories)

  // DATA_SCALAR(n_surveys);

  // DATA_SPARSE_MATRIX(Z_spatial);
  // DATA_SPARSE_MATRIX(R_spatial);
  // DATA_SCALAR(rankdef_R_spatial); // rank deficiency of the R_spatial structure matrix

  nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true);

  ///////////////////

  // PARAMETER_VECTOR(u_spatial_str);
  // PARAMETER(log_prec_spatial);

  // Type prec_spatial = exp(log_prec_spatial);
  // nll -= dgamma(prec_spatial, Type(1), Type(2000), true);

  // nll -= Type(-0.5) * (u_spatial_str * (R_spatial * u_spatial_str)).sum();

  // nll -= dnorm(u_spatial_str.sum(), Type(0), Type(0.01) * u_spatial_str.size(), 1);


  ///////////////////

  // DATA_SPARSE_MATRIX(Z_period);
  // DATA_SPARSE_MATRIX(R_period);
  // PARAMETER(log_prec_rw_period);
  // PARAMETER_VECTOR(u_period);

  // // nll -= dlgamma(log_prec_rw_period, Type(1), Type(20000), true);
  // Type prec_rw_period = exp(log_prec_rw_period);
  // nll -= dgamma(prec_rw_period, Type(1), Type(2000), true);

  // PARAMETER(lag_logit_phi_period);
  // nll -= dnorm(lag_logit_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type phi_period = 2*exp(lag_logit_phi_period)/(1+exp(lag_logit_phi_period))-1;
  // nll += AR1(Type(phi_period))(u_period);


  ///////////////////

  DATA_SPARSE_MATRIX(Z_age);
  DATA_SPARSE_MATRIX(R_age);
  PARAMETER(log_prec_rw_age);
  PARAMETER_VECTOR(u_age);


  Type prec_rw_age = exp(log_prec_rw_age);
  nll -= dgamma(prec_rw_age, Type(1), Type(2000), true);
  nll -= dnorm(u_age.sum(), Type(0), Type(0.01) * u_age.size(), true);


  ////////////////////
  // ETA-1 - Age x time interaction

  // PARAMETER_ARRAY(eta1);
  // PARAMETER(log_prec_eta1);
  // PARAMETER(logit_eta1_phi_age);
  // PARAMETER(logit_eta1_phi_period);

  // Type prec_eta1 = exp(log_prec_eta1);
  // nll -= dgamma(prec_eta1, Type(1), Type(2000), true);

  // Type eta1_phi_age(exp(logit_eta1_phi_age)/(1+exp(logit_eta1_phi_age)));
  // nll -= log(eta1_phi_age) +  log(1 - eta1_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta1_phi_age, Type(0.5), Type(0.5), true);

  // Type eta1_phi_period(exp(logit_eta1_phi_period)/(1+exp(logit_eta1_phi_period)));
  // nll -= log(eta1_phi_period) +  log(1 - eta1_phi_period); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta1_phi_period, Type(0.5), Type(0.5), true);

  // nll += SEPARABLE(AR1(Type(eta1_phi_age)), SEPARABLE(AR1(Type(eta1_phi_period)), GMRF(R_country)))(eta1);
  // vector<Type> eta1_v(eta1);

  ///////////////////
   // ETA-2 - Space x time interaction
//
  // PARAMETER_ARRAY(eta2);
  // PARAMETER(log_prec_eta2);
  // PARAMETER(logit_eta2_phi_period);

  // Type prec_eta2 = exp(log_prec_eta2);
  // nll -= dgamma(prec_eta2, Type(1), Type(2000), true);

  // Type eta2_phi_period(exp(logit_eta2_phi_period)/(1+exp(logit_eta2_phi_period)));
  // nll -= log(eta2_phi_period) +  log(1 - eta2_phi_period); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta2_phi_period, Type(0.5), Type(0.5), true);

  // nll += SEPARABLE(AR1(Type(eta2_phi_period)), GMRF(R_spatial))(eta2);

  // Type log_det_Qar1_eta2((eta2.cols() - 1) * log(1 - eta2_phi_period * eta2_phi_period));
  // nll -= rankdef_R_spatial * 0.5 * (log_det_Qar1_eta2 - log(2 * PI));

  // for (int i = 0; i < eta2.cols(); i++) {
  //   nll -= dnorm(eta2.col(i).sum(), Type(0), Type(0.01) * eta2.col(i).size(), true);}

  // vector<Type> eta2_v(eta2);


  ////////////////////

  // PARAMETER_ARRAY(eta3);
  // PARAMETER(log_prec_eta3);
  // PARAMETER(logit_eta3_phi_age);

  // Type prec_eta3 = exp(log_prec_eta3);
  // nll -= dgamma(prec_eta3, Type(1), Type(2000), true);

  // Type eta3_phi_age(exp(logit_eta3_phi_age)/(1+exp(logit_eta3_phi_age)));
  // nll -= log(eta3_phi_age) +  log(1 - eta3_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta3_phi_age, Type(0.5), Type(0.5), true);

  // nll += SEPARABLE(AR1(Type(eta3_phi_age)), GMRF(R_spatial))(eta3);

  // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
  // nll -= rankdef_R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));

  // for (int i = 0; i < eta3.cols(); i++) {
  //   nll -= dnorm(eta3.col(i).sum(), Type(0), Type(0.01) * eta3.col(i).size(), true);}

  // vector<Type> eta3_v(eta3);

  //Smooth iid

  // PARAMETER(log_prec_smooth_iid);
  // DATA_SPARSE_MATRIX(R_smooth_iid);

  // DATA_SPARSE_MATRIX(Z_smooth_iid);
  // PARAMETER_VECTOR(u_smooth_iid);

  // // nll=- dnorm(log_prec_smooth_iid, Type(3.5), Type(0.5), true);
  // Type prec_smooth_iid = exp(log_prec_smooth_iid);
  // nll -= dgamma(prec_smooth_iid, Type(1), Type(2000), true);

  // nll -= Type(-0.5) * (u_smooth_iid * (R_smooth_iid * u_smooth_iid)).sum();

  ///////////////////////
 
  vector<Type> logit_p(
                     beta_0
                     + Z_age * u_age * sqrt(1/prec_rw_age)
                     // + Z_period * u_period * sqrt(1/prec_rw_period)
                     // + X_period * beta_period
                     // + Z_spatial * spatial
                     // + Z_spatial * u_spatial_str * sqrt(1/prec_spatial)
                     // + X_urban_dummy * beta_urban_dummy
                     // + Z_country * u_country * sqrt(1/prec_country)
                     // + Z_omega1 * omega1_v * sqrt(1/prec_omega1)
                     // + Z_omega2 * omega2_v * sqrt(1/prec_omega2)
                     // + Z_interaction1 * eta1_v * sqrt(1/prec_eta1)
                     // + Z_interaction2 * eta2_v * sqrt(1/prec_eta2)
                     // + Z_interaction3 * eta3_v * sqrt(1/prec_eta3)
                     );

  vector<Type> p(invlogit(logit_p));

  vector<Type> p_pred(M_obs * p
                          // + log_offset_naomi
                          );

  array<Type> p_arr(number_surveys,number_age);

  for(int i=0; i<number_surveys; i++) {
    for(int j=0; j<number_age; j++) {
      p_arr(i,j) = p_pred((i*35) + j);
    }
  }

  // vector<Type> births(lambda * pop);

  // vector<Type> births_full(A_full_obs * births);
  // vector<Type> pop_full(A_full_obs * pop);
  // vector<Type> lambda_out(births_full/pop_full);

  // vector<Type> tfr_out(A_tfr_out * lambda_out);

  // vector<Type> observed_x(x);

  for (int i=0; i<number_surveys; i++) {
    vector<Type> p_row(p_arr.matrix().row(i));
    // vector<Type> p_row_norm(p_row/p_row.sum());
    vector<Type> x_row(observed_x.matrix().row(i));

    nll -= dmultinom(x_row, p_row, true);
  }

  // REPORT(p);
  REPORT(p_arr);
  // REPORT(log_prec_rw_age);

  return nll;

}
