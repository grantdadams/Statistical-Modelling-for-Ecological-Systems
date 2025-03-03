#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Age);
  DATA_VECTOR(Length);
  DATA_INTEGER(Ndata)
  DATA_INTEGER(Model_type)

  PARAMETER(LogLinf);
  PARAMETER(Loga50);
  PARAMETER(LogDelta);
  PARAMETER(LogKappa);
  PARAMETER(t0);
  PARAMETER(LogSigma);

  Type Linf = exp(LogLinf);
  Type a50 = exp(Loga50);
  Type Delta = exp(LogDelta);
  Type Kappa = exp(LogKappa);

  vector<Type> Pred(Ndata);
  vector<Type> PredY(20);
  int II;

  Type neglogL = 0.0;

  if (Model_type == 1)
   {
	Pred = Linf/(1+exp(-log(19)*(Age-a50)/Delta));
	for (II=1;II<=20;II++)
	 PredY(II-1) = Linf/(1+exp(-log(19)*(float(II)-a50)/Delta));
   }
  if (Model_type == 2)
   {
    Pred = Linf*(1-exp(-Kappa*(Age-t0)));
	for (II=1;II<=20;II++)
	 PredY(II-1) = Linf*(1-exp(-Kappa*(float(II)-t0)));
   }
  neglogL = -sum(dnorm(Length, Pred, exp(LogSigma), true));
  //std::cout << Linf << " " << a50 << " " << Delta << " " << exp(LogSigma) << " " << neglogL  << "\n";

  REPORT(PredY);

  return neglogL;
}
