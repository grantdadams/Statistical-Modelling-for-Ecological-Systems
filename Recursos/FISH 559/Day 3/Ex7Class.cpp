#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
 {
  DATA_INTEGER(Nbatch);
  DATA_INTEGER(Nunit);
  DATA_IVECTOR(Batches);
  DATA_IVECTOR(Units);
  DATA_IVECTOR(Treat);
  DATA_VECTOR(Original);
  DATA_VECTOR(Final);
  DATA_INTEGER(IsRandom);                                                  // 0 is fixed only 1 is full model

  PARAMETER(Control);
  PARAMETER(Treatment);
  PARAMETER_VECTOR(EpsB);
  PARAMETER_VECTOR(EpsU);
  PARAMETER(logSigmaB);
  Type SigmaB = exp(logSigmaB);
  PARAMETER(logSigmaU);
  Type SigmaU = exp(logSigmaU);

  Type f;
  vector<Type>Prob(Nunit);

  f = 0;

  REPORT(f);
  ADREPORT(SigmaB);
  ADREPORT(SigmaU);
  REPORT(Prob);

  return f;
}




