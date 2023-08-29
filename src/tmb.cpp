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
  DATA_MATRIX(observed_x);

  int number_surveys = observed_x.rows(); // Number of rows (surveys)
  int number_age = observed_x.cols(); // Number of cols (age categories)

  nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true);  //Prior for the intercept, v diffuse prior

  ///////////////////

  DATA_SPARSE_MATRIX(Z_age);
  DATA_SPARSE_MATRIX(R_age);
  PARAMETER(log_prec_rw_age);
  PARAMETER_VECTOR(u_age);

  Type prec_rw_age = exp(log_prec_rw_age);
  nll -= dgamma(prec_rw_age, Type(1), Type(2000), true);

  nll -= Type(-0.5) * (u_age * (R_age * u_age)).sum();
  nll -= dnorm(u_age.sum(), Type(0), Type(0.01) * u_age.size(), true);         //Sum to zero constraint on u_age (putting a prior on all the age groups to sum to zero with some small standard deviation)


  ///////////////////////
  
  DATA_SPARSE_MATRIX(Z_spatial);
  DATA_SPARSE_MATRIX(R_spatial);
  // DATA_SCALAR(rankdef_R_spatial); // rank deficiency of the R_spatial structure matrix
  
  ///////////////////
  
  PARAMETER_VECTOR(u_spatial_str);
  PARAMETER(log_prec_spatial);

  Type prec_spatial = exp(log_prec_spatial);
  nll -= dgamma(prec_spatial, Type(1), Type(2000), true);

  nll -= Type(-0.5) * (u_spatial_str * (R_spatial * u_spatial_str)).sum();

  nll -= dnorm(u_spatial_str.sum(), Type(0), Type(0.01) * u_spatial_str.size(), 1);
  
  //////////// YEAR FIXED EFFECTS
  
  DATA_MATRIX(X_period);
  PARAMETER_VECTOR(beta_period);
  
  nll -= dnorm(beta_period, Type(0), Type(sqrt(1/0.001)), true).sum();
  
  
  
  ///////////// OSBERVATION EFFECTS
  
  DATA_SPARSE_MATRIX(Z_survey);
  DATA_SPARSE_MATRIX(R_survey);
  PARAMETER(log_prec_survey);
  PARAMETER_VECTOR(u_survey);

  Type prec_survey = exp(log_prec_survey);
  nll -= dgamma(prec_survey, Type(1), Type(2000), true);

  nll -= Type(-0.5) * (u_survey * (R_survey * u_survey)).sum();
  nll -= dnorm(u_survey.sum(), Type(0), Type(0.01) * u_survey.size(), true);         //Sum to zero constraint on u_age (putting a prior on all the age groups to sum to zero with some small standard deviation)
  
  //////////// RECRUITMENT METHOD EFFECTS
  
  DATA_MATRIX(X_method);
  PARAMETER_VECTOR(beta_method);

  nll -= dnorm(beta_method, Type(0), Type(sqrt(1/0.001)), true).sum();
  // Type prec_method = exp(log_prec_method);
  // nll -= dgamma(prec_method, Type(1), Type(2000), true);

  // 
  // nll -= Type(-0.5) * (u_method * (R_method * u_method)).sum();
  // nll -= dnorm(u_method.sum(), Type(0), Type(0.01) * u_method.size(), true);         //Sum to zero constraint on u_age (putting a prior on all the age groups to sum to zero with some small standard deviation)

  /////////// SPACE AGE INTERACTION
  
  DATA_SPARSE_MATRIX(Z_interaction3);
  
  PARAMETER_ARRAY(eta3);
  PARAMETER(log_prec_eta3);
  PARAMETER(eta3_phi_age);
  PARAMETER(logit_eta3_phi_age);
  
  Type prec_eta3 = exp(log_prec_eta3);
  nll -= dgamma(prec_eta3, Type(1), Type(2000), true);
  
  // nll -= dnorm(lag_logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);
  
  // Type eta3_phi_age(exp(logit_eta3_phi_age)/(1+exp(logit_eta3_phi_age)));
  nll -= log(eta3_phi_age) +  log(1 - eta3_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  nll -= dbeta(eta3_phi_age, Type(0.5), Type(0.5), true);
  
  nll += SEPARABLE(AR1(Type(eta3_phi_age)), GMRF(R_spatial))(eta3);
  
  // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
  // nll -= R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));

  for (int i = 0; i < eta3.cols(); i++) {
    nll -= dnorm(eta3.col(i).sum(), Type(0), Type(0.01) * eta3.col(i).size(), true);}
  
  vector<Type> eta3_v(eta3);
  
  
  
  /////////// Multinomial model --> Logit is our link 

  vector<Type> logit_p(
                     beta_0                                  // Parameter of length 1
                     + Z_age * u_age * sqrt(1/prec_rw_age)   
                     // + Z_period * u_period * sqrt(1/prec_rw_period)
                     + X_period * beta_period
                     // + Z_spatial * spatial
                     + Z_spatial * u_spatial_str * sqrt(1/prec_spatial)
                     // + X_urban_dummy * beta_urban_dummy
                     // + Z_country * u_country * sqrt(1/prec_country)
                     // + Z_omega1 * omega1_v * sqrt(1/prec_omega1)
                     // + Z_omega2 * omega2_v * sqrt(1/prec_omega2)
                     // + Z_interaction1 * eta1_v * sqrt(1/prec_eta1)
                     // + Z_interaction2 * eta2_v * sqrt(1/prec_eta2)
                     + Z_interaction3 * eta3_v * sqrt(1/prec_eta3)
                     );
  
  vector<Type> p_pred((M_obs * logit_p)      
                      + Z_survey * u_survey * sqrt(1/prec_survey)
                      + X_method * beta_method  
                          );           //get me the perfect p_vector and get me the ones that relate to the data --> which comes from M_obs
  
  vector<Type> p(invlogit(p_pred));  //ip dealised set of P
  

  array<Type> p_arr(number_surveys,number_age);

  for(int i=0; i<number_surveys; i++) {
    for(int j=0; j<number_age; j++) {
      p_arr(i,j) = p((i*number_age) + j);       
    }
  }
  
  matrix<Type> p_norm(number_surveys, number_age);

  for (int i=0; i<number_surveys; i++) {
    vector<Type> p_row(p_arr.matrix().row(i));   //p_row is composed of the i'th row of p_arr
    vector<Type> p_row_norm(p_row/p_row.sum());  // we normalise it
    vector<Type> x_row(observed_x.row(i));       //i_th row of the data 

    nll -= dmultinom(x_row, p_row_norm, true);
    
    p_norm.row(i) = p_row_norm;
    
  }
  
  // vector<Type> single_row_of_probs;
  // single_row_of_probs = p_norm.row(1);
  
// 
//   int num_rows = p_norm.rows();
//   int num_cols = p_norm.cols();
//   
//   vector<Type> p_norm_vector(num_rows * num_cols);
//   for (int i = 0; i < num_rows; ++i) {
//     for (int j = 0; j < num_cols; ++j) {
//       p_norm_vector[i * num_cols + j] = p_norm(i, j);
//     }
//   }
  
  // REPORT(p_norm);
  REPORT(logit_p);
  
  return nll;

}
