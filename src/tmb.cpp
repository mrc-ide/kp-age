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
  
  // Multinomial model --> Logit is our link 

  vector<Type> logit_p(
                     beta_0                                  // Parameter of length 1
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

  vector<Type> p(invlogit(logit_p));  //ip dealised set of P
  
  
  vector<Type> p_pred(M_obs * p      
                          // + log_offset_naomi
                          );           //get me the perfect p_vector and get me the ones that relate to the data --> which comes from M_obs
  
  

  array<Type> p_arr(number_surveys,number_age);

  for(int i=0; i<number_surveys; i++) {
    for(int j=0; j<number_age; j++) {
      p_arr(i,j) = p_pred((i*number_age) + j);       
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
  // 
  // single_row_of_probs = p_norm.row(1);

  // REPORT(p);
  // REPORT(p_arr);
  // REPORT(single_row_of_probs);
  REPORT(p_norm);
  // REPORT(log_prec_rw_age);
  


  return nll;

}
