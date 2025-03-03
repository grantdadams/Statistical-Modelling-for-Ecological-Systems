#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(x);
  DATA_VECTOR(y);
  int n = y.size();

  PARAMETER(b0);
  PARAMETER(b1);
  PARAMETER(logSigma);
  vector<Type> yfit(n);

  REPORT(b0);

  Type neglogL = 0.0;

  yfit = b0 + b1*x;
  neglogL = -sum(dnorm(y, yfit, exp(logSigma), true));
  SIMULATE {
	for (int i=0;i<n;i++) y(i) = rnorm(yfit(i),exp(logSigma));
   }

  SIMULATE {
	REPORT(y)
   }


  return neglogL;
}
