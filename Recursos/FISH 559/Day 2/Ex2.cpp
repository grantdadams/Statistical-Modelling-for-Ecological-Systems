#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(Nprey)
  DATA_INTEGER(NData)
  DATA_MATRIX(N);
  DATA_INTEGER(Model_Num);

  PARAMETER_VECTOR(alpha);
  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(gamma);

  matrix<Type> Pred(NData,Nprey);
  Type obj_fun;

  int Idata,Iprey;

  for (Idata=0;Idata<NData;Idata++)
   for (Iprey=0;Iprey<Nprey;Iprey++)
    {
     // Model 1
     if (Model_Num == 1) Pred(Idata,Iprey) = alpha(Iprey)*N(Idata,0);

     // Model 2
     if (Model_Num == 2) Pred(Idata,Iprey) = alpha(Iprey)*N(Idata,0)/(1.0+beta(Iprey)*N(Idata,1+Iprey));

     // Model 3
     if (Model_Num == 3) Pred(Idata,Iprey) = alpha(Iprey)*N(Idata,0)*pow(N(Idata,1+Iprey),gamma(Iprey)-1.0)/(1.0+beta(Iprey)*pow(N(Idata,1+Iprey),gamma(Iprey)));

     // Model 4
     if (Model_Num == 4) Pred(Idata,Iprey) = alpha(Iprey)*N(Idata,0)/(1+beta(Iprey)*N(Idata,1+Iprey)+gamma(Iprey)*N(Idata,0));

     // Residuals
     obj_fun = obj_fun + pow(log(Pred(Idata,Iprey))-log(N(Idata,4+Iprey)),2.0);
    }
  return(obj_fun);

}
