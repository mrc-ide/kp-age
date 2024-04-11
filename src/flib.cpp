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
  DATA_MATRIX(observed_x);
  DATA_SPARSE_MATRIX(X_stand_in);
  DATA_SPARSE_MATRIX(R_beta);
  
  // DATA_SCALAR(rankdef_R_spatial);
  
  DATA_VECTOR(logit_totpop);
  
  int number_surveys = observed_x.rows(); // Number of rows (surveys)
  int number_age = observed_x.cols(); // Number of cols (age categories)
  
  // nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true).sum();  //Prior for the intercept, v diffuse prior
  
  // PARAMETER(log_prec_rw_beta);
  // Type prec_rw_beta = exp(log_prec_rw_beta);
  // nll -= dgamma(prec_rw_beta, Type(1), Type(2000), true);
  // // Run these two if you want iid/RW instead of AR1
  //   nll -= Type(-0.5) * (beta_0 * (R_beta * beta_0)).sum();
  // nll -= dnorm(beta_0.sum(), Type(0), Type(0.01) * beta_0.size(), true);

  // /// Make AR1 on beta 0 - Run 50-54
  // PARAMETER(lag_logit_phi_beta);
  // nll -=dnorm(lag_logit_phi_beta, Type(0), Type(sqrt(1/0.15)), true);
  // Type phi_beta = 2*exp(lag_logit_phi_beta)/(1+exp(lag_logit_phi_beta))-1;
  // 
  // nll+= AR1(Type(phi_beta))(beta_0);

  
  // // /////////// SPACE AGE INTERACTION
  // //
  // DATA_SPARSE_MATRIX(Z_spaceage);
  // DATA_SPARSE_MATRIX(R_spatial);
  // 
  // PARAMETER_ARRAY(eta3);
  // PARAMETER(log_prec_eta3);
  // PARAMETER(logit_eta3_phi_age);
  // // PARAMETER(lag_logit_eta3_phi_age);
  // 
  // Type prec_eta3 = exp(log_prec_eta3);
  // nll -= dgamma(prec_eta3, Type(1), Type(2000), true);
  // 
  // // nll -= dnorm(logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);
  // 
  // Type eta3_phi_age(exp(logit_eta3_phi_age)/(1+exp(logit_eta3_phi_age)));
  // //
  // nll -= log(eta3_phi_age) +  log(1 - eta3_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta3_phi_age, Type(0.5), Type(0.5), true); // 69-72 remove if swapping to lag logit instead of logit
  // 
  // // nll -=dnorm(lag_logit_eta3_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  // // Type eta3_phi_age = 2*exp(lag_logit_eta3_phi_age)/(1+exp(lag_logit_eta3_phi_age))-1;
  // 
  // 
  // nll += SEPARABLE(AR1(Type(eta3_phi_age)), GMRF(R_spatial))(eta3);
  // 
  // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
  // nll -= rankdef_R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));
  // 
  // for (int i = 0; i < eta3.cols(); i++) {
  //   nll -= dnorm(eta3.col(i).sum(), Type(0), Type(0.01) * eta3.col(i).size(), true);}
  // 
  // vector<Type> eta3_v(eta3);


  // ///////////// Time * Age
  // DATA_SPARSE_MATRIX(Z_periodage);
  // // DATA_SPARSE_MATRIX(R_period);
  // 
  // PARAMETER_ARRAY(eta2);
  // PARAMETER(log_prec_eta2);
  // PARAMETER(logit_eta2_phi_age);
  // PARAMETER(logit_eta2_phi_period)
  // 
  //   //
  //   Type prec_eta2 = exp(log_prec_eta2);
  // nll -= dgamma(prec_eta2, Type(1), Type(2000), true);
  // 
  // // nll -= dnorm(logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);
  // 
  // Type eta2_phi_age(exp(logit_eta2_phi_age)/(1+exp(logit_eta2_phi_age)));
  // nll -= log(eta2_phi_age) +  log(1 - eta2_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta2_phi_age, Type(0.5), Type(0.5), true);
  // 
  // 
  // Type eta2_phi_period(exp(logit_eta2_phi_period)/(1+exp(logit_eta2_phi_period)));
  // nll -= log(eta2_phi_period) +  log(1 - eta2_phi_period); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta2_phi_period, Type(0.5), Type(0.5), true);
  // 
  // nll += SEPARABLE(AR1(Type(eta2_phi_age)), AR1(Type(eta2_phi_period)))(eta2);
  // 
  // vector<Type> eta2_v(eta2);
  
  
  // // ///////////// Survey Age interaction ///////////
  // //
  // DATA_SPARSE_MATRIX(Z_survage);
  // DATA_SPARSE_MATRIX(R_surv);
  // //
  // PARAMETER_ARRAY(eta_surv);
  // PARAMETER(log_prec_eta_surv);
  // PARAMETER(logit_eta_surv_phi_age);
  // // PARAMETER(lag_logit_eta_surv_phi_age);
  // //
  // Type prec_eta_surv = exp(log_prec_eta_surv);
  // nll -= dgamma(prec_eta_surv, Type(1), Type(2000), true);
  // //
  // // // nll -= dnorm(logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);
  // //
  // Type eta_surv_phi_age(exp(logit_eta_surv_phi_age)/(1+exp(logit_eta_surv_phi_age)));
  // nll -= log(eta_surv_phi_age) +  log(1 - eta_surv_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta_surv_phi_age, Type(0.5), Type(0.5), true); // 69-72 remove if swapping to lag logit instead of logit
  // //
  // // nll -=dnorm(lag_logit_eta3_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  // // Type eta3_phi_age = 2*exp(lag_logit_eta3_phi_age)/(1+exp(lag_logit_eta3_phi_age))-1;
  // //
  // //
  // nll += SEPARABLE(AR1(Type(eta_surv_phi_age)), GMRF(R_surv))(eta_surv);
  // //
  // // // // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
  // // // // nll -= R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));
  // //
  // // for (int i = 0; i < eta_surv.cols(); i++) {
  // //   nll -= dnorm(eta_surv.col(i).sum(), Type(0), Type(0.01) * eta_surv.col(i).size(), true);}
  // //
  // vector<Type> eta_surv_v(eta_surv);


  /////////// Multinomial model --> Logit is our link 
  
  vector<Type> logit_p(
      X_stand_in * beta_0
    // + Z_spaceage * eta3_v * sqrt(1/prec_eta3)
    // + Z_periodage * eta2_v * sqrt(1/prec_eta2)
    + logit_totpop
  );
  
  vector<Type> logit_p2(M_obs*logit_p);
                          // + Z_survage * eta_surv_v * sqrt(1/prec_eta_surv));

  vector<Type> p(exp(logit_p2));
  // vector<Type> p(invlogit(logit_p2));

  array<Type> p_arr(number_surveys,number_age);

  for(int i=0; i<number_surveys; i++) {
    for(int j=0; j<number_age; j++) {
      p_arr(i,j) = p((i*number_age) + j);
    }
  }

  matrix<Type> p_norm(number_surveys, number_age);

  for (int i=0; i<number_surveys; i++) {
    vector<Type> p_row(p_arr.matrix().row(i));
    vector<Type> p_row_norm(p_row/p_row.sum());
    vector<Type> x_row(observed_x.row(i));

    nll -= dmultinom(x_row, p_row_norm, true);

    p_norm.row(i) = p_row_norm;

  }
  

  REPORT(p_norm);
  REPORT(logit_p);
  REPORT(logit_p2);

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

