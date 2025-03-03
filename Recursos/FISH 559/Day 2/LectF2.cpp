#include <TMB.hpp>

template <class Type> Type square(Type x){return x*x;}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Length);
  DATA_VECTOR(Prob);
  DATA_VECTOR(Prob2);
  int n = Length.size();

  PARAMETER(LogA50);
  PARAMETER(LogA95);
  PARAMETER(LogSigma);

  Type A50;
  Type A95;
  Type Sigma;
  A50 = exp(LogA50);
  A95 = exp(LogA95);
  Sigma = exp(LogSigma);

  vector<Type> yfit(n);
  Type SS = 0;
  Type neglogL;

  // prediction
  for (int II=0;II<n;II++) yfit(II) = 1/(1+exp(-1*log(19)*(Length(II)-A50)/(A95-A50)));
  for (int II=0;II<n;II++) yfit(II) = log(yfit(II)/(1.0-yfit(II)));
  for (int II=0;II<n;II++) SS += square(yfit(II)-Prob2(II));
  neglogL = float(n)*log(Sigma) + SS/(2.0*square(Sigma));
  //std::cout << neglogL << "\n";

  return neglogL;
}
