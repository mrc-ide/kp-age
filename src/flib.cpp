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
  
  DATA_VECTOR(logit_totpop);
  
  int number_surveys = observed_x.rows(); // Number of rows (surveys)
  int number_age = observed_x.cols(); // Number of cols (age categories)
  
  // nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true).sum();  //Prior for the intercept, v diffuse prior
  
  /// Make RW on beta 0
  PARAMETER(log_prec_rw_beta);
  Type prec_rw_beta = exp(log_prec_rw_beta);
  nll -= dgamma(prec_rw_beta, Type(1), Type(2000), true);
  // //
  //   nll -= Type(-0.5) * (beta_0 * (R_beta * beta_0)).sum();
  // nll -= dnorm(beta_0.sum(), Type(0), Type(0.01) * beta_0.size(), true);
  
  /// Make AR1 on beta 0 
  PARAMETER(lag_logit_phi_beta);
  nll -=dnorm(lag_logit_phi_beta, Type(0), Type(sqrt(1/0.15)), true);
  Type phi_beta = 2*exp(lag_logit_phi_beta)/(1+exp(lag_logit_phi_beta))-1;
  
  nll+= AR1(Type(phi_beta))(beta_0);
  
  
  /////////// SPACE AGE INTERACTION
  
  DATA_SPARSE_MATRIX(Z_spaceage);
  // DATA_SPARSE_MATRIX(R_spatial2); //this is ICAR
  DATA_SPARSE_MATRIX(R_spatial);

  PARAMETER_ARRAY(eta3);
  PARAMETER(log_prec_eta3);
  PARAMETER(logit_eta3_phi_age);
  // PARAMETER(lag_logit_eta3_phi_age);

  Type prec_eta3 = exp(log_prec_eta3);
  nll -= dgamma(prec_eta3, Type(1), Type(2000), true);

  // nll -= dnorm(logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);

  Type eta3_phi_age(exp(logit_eta3_phi_age)/(1+exp(logit_eta3_phi_age)));
  //
  nll -= log(eta3_phi_age) +  log(1 - eta3_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  nll -= dbeta(eta3_phi_age, Type(0.5), Type(0.5), true); // 69-72 remove if swapping to lag logit instead of logit

  // nll -=dnorm(lag_logit_eta3_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  // Type eta3_phi_age = 2*exp(lag_logit_eta3_phi_age)/(1+exp(lag_logit_eta3_phi_age))-1;


  nll += SEPARABLE(AR1(Type(eta3_phi_age)), GMRF(R_spatial))(eta3);

  // // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
  // // nll -= R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));

  for (int i = 0; i < eta3.cols(); i++) {
    nll -= dnorm(eta3.col(i).sum(), Type(0), Type(0.01) * eta3.col(i).size(), true);}

  vector<Type> eta3_v(eta3);


  // ///////////// Time * Age
  DATA_SPARSE_MATRIX(Z_periodage);
  DATA_SPARSE_MATRIX(R_period);

  PARAMETER_ARRAY(eta2);
  PARAMETER(log_prec_eta2);
  PARAMETER(logit_eta2_phi_age);
  PARAMETER(logit_eta2_phi_period)

    //
    Type prec_eta2 = exp(log_prec_eta2);
  nll -= dgamma(prec_eta2, Type(1), Type(2000), true);

  // nll -= dnorm(logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);

  Type eta2_phi_age(exp(logit_eta2_phi_age)/(1+exp(logit_eta2_phi_age)));
  nll -= log(eta2_phi_age) +  log(1 - eta2_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  nll -= dbeta(eta2_phi_age, Type(0.5), Type(0.5), true);


  Type eta2_phi_period(exp(logit_eta2_phi_period)/(1+exp(logit_eta2_phi_period)));
  nll -= log(eta2_phi_period) +  log(1 - eta2_phi_period); // Jacobian adjustment for inverse logit'ing the parameter...
  nll -= dbeta(eta2_phi_period, Type(0.5), Type(0.5), true);

  nll += SEPARABLE(AR1(Type(eta2_phi_age)), AR1(Type(eta2_phi_period)))(eta2);

  // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
  // nll -= R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));

  for (int i = 0; i < eta2.cols(); i++) {
    nll -= dnorm(eta2.col(i).sum(), Type(0), Type(0.01) * eta2.col(i).size(), true);}

  vector<Type> eta2_v(eta2);
  
  
  // ///////////// Survey Age interaction ///////////
  // 
  // DATA_SPARSE_MATRIX(Z_survage);
  // DATA_SPARSE_MATRIX(R_surv);
  // 
  // PARAMETER_ARRAY(eta_surv);
  // PARAMETER(log_prec_eta_surv);
  // PARAMETER(logit_eta_surv_phi_age);
  // // PARAMETER(lag_logit_eta_surv_phi_age);
  // 
  // Type prec_eta3 = exp(log_prec_eta_durv);
  // nll -= dgamma(prec_eta_surv, Type(1), Type(2000), true);
  // 
  // // nll -= dnorm(logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);
  // 
  // Type eta3_phi_age(exp(logit_eta_surv_phi_age)/(1+exp(logit_eta_surv_phi_age)));
  // //
  // nll -= log(eta_surv_phi_age) +  log(1 - eta_surv_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(eta_surv_phi_age, Type(0.5), Type(0.5), true); // 69-72 remove if swapping to lag logit instead of logit
  // 
  // // nll -=dnorm(lag_logit_eta3_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  // // Type eta3_phi_age = 2*exp(lag_logit_eta3_phi_age)/(1+exp(lag_logit_eta3_phi_age))-1;
  // 
  // 
  // nll += SEPARABLE(AR1(Type(eta_surv_phi_age)), GMRF(R_surv))(eta_surv);
  // 
  // // // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
  // // // nll -= R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));
  // 
  // for (int i = 0; i < eta_surv.cols(); i++) {
  //   nll -= dnorm(eta_surv.col(i).sum(), Type(0), Type(0.01) * eta_surv.col(i).size(), true);}
  // 
  // vector<Type> eta_surv_v(eta_surv);


  /////////// Multinomial model --> Logit is our link 
  
  vector<Type> logit_p(
      X_stand_in * beta_0 * sqrt(1/prec_rw_beta) 
    + Z_spaceage * eta3_v * sqrt(1/prec_eta3)
    + Z_periodage * eta2_v * sqrt(1/prec_eta2)
    + logit_totpop
  );
  
  vector<Type> logit_p2(M_obs*logit_p);
  
  vector<Type> p(exp(logit_p2));
  
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
  
  return nll;
  
}


// #include <TMB.hpp>  
// 
// // template<class Type>
// // Type logit(const Type x) {
// //   return log(x/(1-x));
// // }
// 
// // template<class Type>
// // Type invlogit(const Type x) {
// //   return exp(x)/(1+exp(x));
// // }
// 
// template<class Type>
// Type objective_function<Type>::operator() ()
// 
// 
// 
// {
// 
//   using namespace density;
//   using namespace Eigen;
// 
//   Type nll = 0;
// 
//   PARAMETER_VECTOR(beta_0);
//   // PARAMETER(lag_logit_phi_beta);
//   PARAMETER(log_prec_rw_beta);
// 
//   DATA_SPARSE_MATRIX(M_obs);
//   DATA_MATRIX(observed_x);
//   DATA_SPARSE_MATRIX(X_stand_in);
//   DATA_SPARSE_MATRIX(R_beta);
//   
//   DATA_VECTOR(logit_totpop);
//   // DATA_SPARSE_MATRIX(Y_stand_in);
// 
//   int number_surveys = observed_x.rows(); // Number of rows (surveys)
//   int number_age = observed_x.cols(); // Number of cols (age categories)
// 
//   // nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true).sum();  //Prior for the intercept, v diffuse prior
//   
//   Type prec_rw_beta = exp(log_prec_rw_beta);
//   nll -= dgamma(prec_rw_beta, Type(1), Type(2000), true);
//   // 
//   nll -= Type(-0.5) * (beta_0 * (R_beta * beta_0)).sum();
//   nll -= dnorm(beta_0.sum(), Type(0), Type(0.01) * beta_0.size(), true);         //Sum to zero constraint on u_age (putting a prior on all the age groups to sum to zero with some small standard deviation)
//   
//  
//   
//   // nll -=dnorm(lag_logit_phi_beta, Type(0), Type(sqrt(1/0.15)), true);
//   // Type phi_beta = 2*exp(lag_logit_phi_beta)/(1+exp(lag_logit_phi_beta))-1;
//   // 
//   // nll+= AR1(Type(phi_beta))(beta_0);
// 
//   ///////////////////
// 
//   // DATA_SPARSE_MATRIX(Z_age);
//   // DATA_SPARSE_MATRIX(R_age);
//   // PARAMETER(log_prec_rw_age);
//   // PARAMETER_VECTOR(u_age);
//   // 
//   // Type prec_rw_age = exp(log_prec_rw_age);
//   // nll -= dgamma(prec_rw_age, Type(1), Type(2000), true);
//   // 
//   // nll -= Type(-0.5) * (u_age * (R_age * u_age)).sum();
//   // nll -= dnorm(u_age.sum(), Type(0), Type(0.01) * u_age.size(), true);         //Sum to zero constraint on u_age (putting a prior on all the age groups to sum to zero with some small standard deviation)
// 
// 
//   ///////////////////////
//   
//   // DATA_SPARSE_MATRIX(Z_spatial);
//   // DATA_SPARSE_MATRIX(R_spatial);
//   // DATA_SCALAR(rankdef_R_spatial); // rank deficiency of the R_spatial structure matrix
//   
//   ///////////////////
//   
//   // PARAMETER_VECTOR(u_spatial_str);
//   // PARAMETER(log_prec_spatial);
//   // 
//   // Type prec_spatial = exp(log_prec_spatial);
//   // nll -= dgamma(prec_spatial, Type(1), Type(2000), true);
//   // 
//   // nll -= Type(-0.5) * (u_spatial_str * (R_spatial * u_spatial_str)).sum();
//   // 
//   // nll -= dnorm(u_spatial_str.sum(), Type(0), Type(0.01) * u_spatial_str.size(), 1);
//   // 
//   //////////// YEAR FIXED EFFECTS
//   
//   // DATA_MATRIX(X_period);
//   // PARAMETER_VECTOR(beta_period);
//   // 
//   // nll -= dnorm(beta_period, Type(0), Type(sqrt(1/0.001)), true).sum();
//   // 
//   // 
//   
//   ///////////// OSBERVATION EFFECTS
//   
//   // DATA_SPARSE_MATRIX(Z_survey);
//   // DATA_SPARSE_MATRIX(R_survey);
//   // PARAMETER(log_prec_survey);
//   // PARAMETER_VECTOR(u_survey);
//   // 
//   // Type prec_survey = exp(log_prec_survey);
//   // nll -= dgamma(prec_survey, Type(1), Type(2000), true);
//   // 
//   // nll -= Type(-0.5) * (u_survey * (R_survey * u_survey)).sum();
//   // nll -= dnorm(u_survey.sum(), Type(0), Type(0.01) * u_survey.size(), true);         //Sum to zero constraint on u_age (putting a prior on all the age groups to sum to zero with some small standard deviation)
//   // 
//   //////////// RECRUITMENT METHOD EFFECTS
//   
//   // DATA_MATRIX(X_method);
//   // PARAMETER_VECTOR(beta_method);
//   // 
//   // nll -= dnorm(beta_method, Type(0), Type(sqrt(1/0.001)), true).sum();
//   // // Type prec_method = exp(log_prec_method);
//   // // nll -= dgamma(prec_method, Type(1), Type(2000), true);
// 
//   // 
//   // nll -= Type(-0.5) * (u_method * (R_method * u_method)).sum();
//   // nll -= dnorm(u_method.sum(), Type(0), Type(0.01) * u_method.size(), true);         //Sum to zero constraint on u_age (putting a prior on all the age groups to sum to zero with some small standard deviation)
// 
//   /////////// SPACE AGE INTERACTION
//   // 
//   // DATA_SPARSE_MATRIX(Z_interaction3);
//   // 
//   // PARAMETER_ARRAY(eta3);
//   // PARAMETER(log_prec_eta3);
//   // PARAMETER(logit_eta3_phi_age);
//   // 
//   // 
//   // Type prec_eta3 = exp(log_prec_eta3);
//   // nll -= dgamma(prec_eta3, Type(1), Type(2000), true);
//   // 
//   // // nll -= dnorm(logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);
//   // 
//   // Type eta3_phi_age(exp(logit_eta3_phi_age)/(1+exp(logit_eta3_phi_age)));
//   // nll -= log(eta3_phi_age) +  log(1 - eta3_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
//   // nll -= dbeta(eta3_phi_age, Type(0.5), Type(0.5), true);
//   // 
//   // nll += SEPARABLE(AR1(Type(eta3_phi_age)), GMRF(R_spatial))(eta3);
//   // 
//   // // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
//   // // nll -= R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));
//   // 
//   // for (int i = 0; i < eta3.cols(); i++) {
//   //   nll -= dnorm(eta3.col(i).sum(), Type(0), Type(0.01) * eta3.col(i).size(), true);}
//   // 
//   // vector<Type> eta3_v(eta3);
//   // 
//   
//   // /////////// SURVEY AGE INTERACTION
//   
//   // DATA_SPARSE_MATRIX(Z_survage);
//   // DATA_SPARSE_MATRIX(R_survey);
//   // 
//   // PARAMETER_ARRAY(eta3);
//   // PARAMETER(log_prec_eta3);
//   // PARAMETER(logit_eta3_phi_age);
//   // 
//   // 
//   // Type prec_eta3 = exp(log_prec_eta3);
//   // nll -= dgamma(prec_eta3, Type(1), Type(2000), true);
//   // 
//   // 
//   // Type eta3_phi_age(exp(logit_eta3_phi_age)/(1+exp(logit_eta3_phi_age)));
//   // nll -= log(eta3_phi_age) +  log(1 - eta3_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
//   // nll -= dbeta(eta3_phi_age, Type(0.5), Type(0.5), true);
//   // 
//   // nll += SEPARABLE(AR1(Type(eta3_phi_age)), GMRF(R_survey))(eta3);
//   // 
//   // 
//   // for (int i = 0; i < eta3.cols(); i++) {
//   //   nll -= dnorm(eta3.col(i).sum(), Type(0), Type(0.01) * eta3.col(i).size(), true);}
//   // 
//   // vector<Type> eta3_v(eta3);
//   
//   // /////////// YEAR AGE INTERACTION
//   // 
//   // DATA_SPARSE_MATRIX(Z_periodage);
//   // // DATA_SPARSE_MATRIX(R_survey);
//   // 
//   // PARAMETER_ARRAY(eta2);
//   // PARAMETER(log_prec_eta2);
//   // PARAMETER(logit_eta2_phi_age);
//   // 
//   // 
//   // Type prec_eta2 = exp(log_prec_eta2);
//   // nll -= dgamma(prec_eta2, Type(1), Type(2000), true);
//   // 
//   // 
//   // Type eta2_phi_age(exp(logit_eta2_phi_age)/(1+exp(logit_eta2_phi_age)));
//   // nll -= log(eta2_phi_age) +  log(1 - eta2_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
//   // nll -= dbeta(eta2_phi_age, Type(0.5), Type(0.5), true);
//   // 
//   // nll += SEPARABLE(AR1(Type(eta2_phi_age)), GMRF(R_survey))(eta2);
//   // 
//   // 
//   // for (int i = 0; i < eta2.cols(); i++) {
//   //   nll -= dnorm(eta2.col(i).sum(), Type(0), Type(0.01) * eta2.col(i).size(), true);}
//   // 
//   // vector<Type> eta2_v(eta2);
//   
//   ////// 15 ///////
//   // DATA_MATRIX(X_15);
//   // PARAMETER_VECTOR(beta15);
//   // PARAMETER(log_prec_15);
//   // 
//   // nll -= dnorm(beta15, Type(0), Type(sqrt(1/0.001)), true).sum();
//   // Type prec_15 = exp(log_prec_15);
//   // nll -= dgamma(prec_15, Type(1), Type(2000), true);
// 
//   
//   /////////// Multinomial model --> Logit is our link 
// 
//   vector<Type> logit_p(
//                      X_stand_in * beta_0 * sqrt(1/prec_rw_beta)
//                        // + Z_survage * eta3_v * sqrt(1/prec_eta3)
//   // Parameter of length 1
//                      // + Z_age * u_age * sqrt(1/prec_rw_age)
//                      // // + Z_period * u_period * sqrt(1/prec_rw_period)
//                      // + X_period * beta_period
//                      // // + Z_spatial * spatial
//                      // + Z_spatial * u_spatial_str * sqrt(1/prec_spatial)
//                      // // + X_urban_dummy * beta_urban_dummy
//                      // // + Z_country * u_country * sqrt(1/prec_country)
//                      // // + Z_omega1 * omega1_v * sqrt(1/prec_omega1)
//                      // // + Z_omega2 * omega2_v * sqrt(1/prec_omega2)
//                      // // + Z_interaction1 * eta1_v * sqrt(1/prec_eta1)
//                      // + Z_periodage * eta2_v * sqrt(1/prec_eta2)
//                      // + Z_survage * eta3_v * sqrt(1/prec_eta3)
//                      // + X_15 * beta15
//                      + logit_totpop
//                      );
// 
//   // vector<Type> p_pred((M_obs * logit_p)      
//   //                     + Z_survey * u_survey * sqrt(1/prec_survey)
//   //                     + X_method * beta_method  
//   //                         );           //get me the perfect p_vector and get me the ones that relate to the data --> which comes from M_obs
//   
//   // vector<Type> p(invlogit(p_pred));  //ip dealised set of P
//   vector<Type> p(exp(logit_p));
// 
//   array<Type> p_arr(number_surveys,number_age);
// 
//   for(int i=0; i<number_surveys; i++) {
//     for(int j=0; j<number_age; j++) {
//       p_arr(i,j) = p((i*number_age) + j);       
//     }
//   }
//   
//   matrix<Type> p_norm(number_surveys, number_age);
// 
//   for (int i=0; i<number_surveys; i++) {
//     vector<Type> p_row(p_arr.matrix().row(i));   //p_row is composed of the i'th row of p_arr
//     vector<Type> p_row_norm(p_row/p_row.sum());  // we normalise it
//     vector<Type> x_row(observed_x.row(i));       //i_th row of the data 
// 
//     nll -= dmultinom(x_row, p_row_norm, true);
//     
//     p_norm.row(i) = p_row_norm;
//     
//   }
//   
//   // vector<Type> single_row_of_probs;
//   // single_row_of_probs = p_norm.row(1);
//   
// // 
// //   int num_rows = p_norm.rows();
// //   int num_cols = p_norm.cols();
// //   
// //   vector<Type> p_norm_vector(num_rows * num_cols);
// //   for (int i = 0; i < num_rows; ++i) {
// //     for (int j = 0; j < num_cols; ++j) {
// //       p_norm_vector[i * num_cols + j] = p_norm(i, j);
// //     }
// //   }
//   
//   REPORT(p_norm);
//   REPORT(logit_p);
//   
//   return nll;
// 
// }
