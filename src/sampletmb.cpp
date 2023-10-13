#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;
  using namespace Eigen;
  
  // Data
  DATA_VECTOR(sample); // Observed counts (data)
  
  // Parameters
  PARAMETER_VECTOR(log_probs); // Log probabilities for each category
  
  // Transform log probabilities to probabilities using softmax function
  vector<Type> probs = exp(log_probs);
  probs /= probs.sum();
  
  // Negative log-likelihood
  Type nll = -dmultinom(sample, probs, true);
  
  // Return the negative log-likelihood
  return nll;
}