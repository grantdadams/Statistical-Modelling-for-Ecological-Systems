#include <TMB.hpp>

template <class Type> Type square(Type x){return x*x;}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(m)
  DATA_IVECTOR(TT)
  DATA_INTEGER(Tmax)
  DATA_MATRIX(B)
  DATA_MATRIX(R)
  DATA_VECTOR(Phi0)

  // End of data section

  PARAMETER(dummy);
  PARAMETER(mu);
  PARAMETER(log_tau);
  PARAMETER_VECTOR(B0);
  PARAMETER_VECTOR(log_sigR);
  PARAMETER_VECTOR(eta);
  // End of the estimated parameters

  vector<Type> R0(m);
  vector<Type> SigR(m);
  vector<Type> h(m);
  Type tau;
  Type beta;
  Type BH;
  Type obj_fun;
  // End of the temporary variables

  // Transform the parameters
  R0 = B0/Phi0;
  tau = exp(log_tau);
  SigR = exp(log_sigR);

  obj_fun = 0;
  for (int k=0;k<m;k++) {

   // Extract beta and define h
   beta = mu + tau*eta(k);
   h(k) = (exp(beta) + 0.2) / (1.0 + exp(beta));
   obj_fun = obj_fun - dnorm(eta(k),Type(0.0),Type(1.0),true);
   //std::cout << h(k) << "\n";

   // Likelihood
   for (int i=0;i<TT(k);i++) {
	 BH = 4.0*R0(k)*h(k)*B(k,i) / (B0(k)*(1-h(k)) + (5*h(k)-1)*B(k,i));
	 obj_fun += square(log(R(k,i)) - log(BH) + square(SigR(k))/2) / (2*square(SigR(k))) + log(SigR(k));
	 //std::cout << obj_fun << " " << R0(k) << " " << BH << " " << B(k,i) << "\n";
    }
   }

  obj_fun += dummy*dummy;
  ADREPORT(h);
  ADREPORT(R0);
  ADREPORT(tau);

  return(obj_fun);
}
