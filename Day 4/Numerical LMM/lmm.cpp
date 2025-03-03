
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double log_like_fun(NumericVector response, double beta_mu, double beta_re, double sigma_beta, double sigma){
  double like = 1;
  for(int i = 0; i < response.size(); i++){
    like *= 1/(sqrt(2 * M_PI) * sigma) * exp( - (pow(response[i] - beta_mu - beta_re * sigma_beta, 2) / (2 * pow(sigma, 2) )) );
  }
  like *= 1/sqrt(2 * M_PI) * exp(-pow(beta_re, 2) / 2);
  return(like);
}

// [[Rcpp::export]]
double mixed_nll(NumericVector response, IntegerVector group, double beta_mu, NumericVector beta_re, double sigma_beta, double sigma){
  // Parameters
  sigma_beta = exp(sigma_beta);
  sigma = exp(sigma);
  
  // Model objects
  IntegerVector groups = unique(group);
  int n_group = groups.size();
  double integral =0;
  double h = 0;
  
  // Likelihood objects
  NumericVector group_like(n_group);
  double nll = 1;
  
  // Get likelihoods
  for(int j = 0; j < n_group; j++){
    NumericVector response_sub = response[group == groups[j]];
    for(int k = 1; k < beta_re.size(); k++){
      integral = 0;// Initialize
      h = (beta_re[k] - beta_re[k-1]) / 2;
      integral = log_like_fun( response_sub, beta_mu, beta_re[k-1], sigma_beta, sigma); // F(a)
      integral += 4 * log_like_fun( response_sub, beta_mu,(beta_re[k-1] + h), sigma_beta, sigma); // F(a + h)
      integral += log_like_fun( response_sub, beta_mu, beta_re[k], sigma_beta, sigma); // F(b)
      integral *= h/3;
      group_like(j) += integral; // Add integral part to likelihood
    }
    nll *= group_like(j);
  }

  // Return nll
  nll = -log(nll);
  return(nll);
}