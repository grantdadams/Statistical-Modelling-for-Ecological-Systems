#include <TMB.hpp>


template<class Type>
Type posfun(Type x, Type eps, Type &pen)
{
  if ( x >= eps ){
    return x;
  } else {
    pen += Type(0.01) * pow(x-eps,2);
    return eps/(Type(2.0)-x/eps);
  }
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(C);
  DATA_VECTOR(I1);
  int n = C.size();

  PARAMETER(logR);
  PARAMETER(logK);
  PARAMETER(logQ1);
  PARAMETER(logQ2);
  PARAMETER(logSigma);
  PARAMETER_VECTOR(FF);
  PARAMETER_VECTOR(Eps);
  PARAMETER(LogSigmaR)
  Type r = exp(logR);
  Type k = exp(logK);
  Type q1 = exp(logQ1);
  Type q2 = exp(logQ2);
  Type sigma = exp(logSigma);
  Type SigmaR = exp(LogSigmaR);

  int n1 = 0;
  n1 = n + 1;

  vector<Type> B(n1);
  vector<Type> Ihat1(n);
  vector<Type> Chat(n);
  vector<Type> ExpOut(n);
  Type f;

  B(0) = k;
  f = 0;
  for(int t=0; t<n; t++)
  {
    Type Expl = 1.0/(1.0+exp(-FF(t)));
    B(t+1) = B(t) + r*B(t)*(1-B(t)/k) - Expl*B(t);
    //if (B(t+1) < 0.01) B(t+1) = 0.001;
    Chat(t) = Expl*B(t);
    ExpOut(t) = Expl;
    Ihat1(t) = q1*B(t);
  }
  f -= sum(dnorm(log(C), log(Chat), Type(0.05), true));
  f -= sum(dnorm(log(I1), log(Ihat1), sigma, true));

  //ADREPORT(log(B));    // to account for uncertainty
  REPORT(f);
  REPORT(B);
  REPORT(Chat);
  REPORT(ExpOut);
  REPORT(Ihat1);
  REPORT(sigma);
  REPORT(SigmaR);
  REPORT(r);
  REPORT(k);
  REPORT(q1);
  REPORT(q2);
  REPORT(Eps);

  return f;
}




