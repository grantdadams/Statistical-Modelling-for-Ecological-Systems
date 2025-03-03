#include <TMB.hpp>


template<class Type>
Type objective_function<Type>::operator() ()
{
 DATA_VECTOR(Y);
 DATA_MATRIX(X);

 PARAMETER_VECTOR(Beta);
 PARAMETER(ln_sd);

 Type nll= 0;
 vector<Type> mu = X*Beta;

 nll = - dnorm(Y, mu, exp(ln_sd), true).sum();
 SIMULATE{
   Y = rnorm(mu,exp(ln_sd));
   REPORT(Y);
}


 return(nll);
}
