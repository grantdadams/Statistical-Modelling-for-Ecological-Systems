#include <TMB.hpp>

template <class Type> 
vector<Type> myline(vector<Type> x, Type a, Type b){return a+b*x;}

// R-interface til sub-funktion
extern "C" SEXP call_myline(SEXP x, SEXP a, SEXP b){ 
  vector<double> y = myline(asVector<double>(x), (double)*REAL(a), (double)*REAL(b));
  return asSEXP(y);
}

// "asMatrix"

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Y);
  DATA_VECTOR(x);
  PARAMETER(a);
  PARAMETER(b);
  PARAMETER(logSigma);
  Type nll = -sum(dnorm(Y, myline(x,a,b), exp(logSigma), true));    
  return nll;
}

