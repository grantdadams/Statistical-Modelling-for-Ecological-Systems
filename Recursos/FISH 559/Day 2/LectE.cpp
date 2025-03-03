#include <TMB.hpp>

template <class Type> Type square(Type x){return x*x;}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(Nyear)
  DATA_INTEGER(Nage)
  DATA_SCALAR(M)
  DATA_VECTOR(Wght)
  DATA_SCALAR(SigCatch)
  DATA_SCALAR(SigCPUE)
  DATA_SCALAR(Omega)
  DATA_VECTOR(Catch)
  DATA_VECTOR(CPUE)
  DATA_MATRIX(Propn)
  // End of data section

  PARAMETER(dummy);
  PARAMETER_VECTOR(LogN);
  PARAMETER(Sel50);
  PARAMETER(Sel95);
  PARAMETER_VECTOR(LogFish);
  PARAMETER(logq);

  matrix<Type> N(Nyear+1,Nage);
  matrix<Type> F(Nyear,Nage);
  matrix<Type> Z(Nyear,Nage);
  vector<Type> S(Nage);
  vector<Type> FullF(Nyear);

  vector<Type> CPred(Nyear);
  vector<Type> CPUEPred(Nyear);
  matrix<Type> PropnPred(Nyear,Nage);
  vector<Type> Bio(Nyear);

  Type Like1;
  Type Like2;
  Type Like3;
  Type obj_fun;

  // End of specifications section
  // =============================

  // Specify selectivity
  for (int Age=0;Age<Nage;Age++)
    S(Age) = 1.0/(1+exp(-log(19)*(Age-Sel50)/(Sel95-Sel50)));

  // Compute the F and Z matrices
  for (int Year=0;Year<Nyear;Year++)
   for (int Age=0;Age<Nage;Age++)
    {
     FullF(Year) = exp(LogFish(Year));
     F(Year,Age) = exp(LogFish(Year))*S(Age);
     Z(Year,Age) = M + F(Year,Age);
    }

  // Insert all the recritments (watch out for the index pointers)
  for (int Age=0;Age<Nage;Age++) N(0,Age) = exp(LogN(Nage-Age-1));
  for (int Year=1;Year<Nyear;Year++) N(Year,0) = exp(LogN(Nage+Year-1));
  N(Nyear,0) = 0;

  // Project the whole N-matrix
  for (int Year=0;Year<Nyear;Year++)
   for (int Age=0;Age<Nage-1;Age++)
    N(Year+1,Age+1) = N(Year,Age)*exp(-Z(Year,Age));

  // Compute the predicted exploitable biomass, catch-at-age and catch
  Type TotCAA;
  for (int Year=0;Year<Nyear;Year++)
   {
    Bio(Year) = 0; CPred(Year) = 0; TotCAA = 0;
    for (int Age=0;Age<Nage;Age++)
     {
      PropnPred(Year,Age) = F(Year,Age)/Z(Year,Age)*N(Year,Age)*(1.0-exp(-Z(Year,Age)));
      CPred(Year) += Wght(Age)*PropnPred(Year,Age);
      Bio(Year) += Wght(Age)*S(Age)*N(Year,Age)*exp(-Z(Year,Age)/2.0);
      TotCAA += PropnPred(Year,Age);
     }
    CPUEPred(Year) = exp(logq)*Bio(Year);
    for (int Age=0;Age<Nage;Age++) PropnPred(Year,Age) /= TotCAA;
   }

  // Likelihood components

  // Catch data
  Like1 = 0;
  for (int Year=0;Year<Nyear;Year++)
   Like1 += square( (Catch(Year)-CPred(Year))/CPred(Year));
  Like1 = Like1 / (2.0*square(SigCatch));

  // CPUE data
  Like2 = 0;
  for (int Year=0;Year<Nyear;Year++)
   Like2 += square( log(CPUE(Year)) - log(CPUEPred(Year)));
  Like2 = Like2 / (2.0*square(SigCPUE));

  // Catch-at-age data
  Like3 = 0;
  for (int Year=0;Year<Nyear;Year++)
   for (int Age=0;Age<Nage;Age++)
    if (Propn(Year,Age) >0)
     Like3 += Propn(Year,Age)*log(PropnPred(Year,Age)/Propn(Year,Age));
  Like3 = -1*Omega*Like3;

  obj_fun = dummy*dummy +Like1 + Like2 + Like3;

  Type Ratio;
  Ratio = Bio(Nyear-1)/Bio(0);

  REPORT(S);
  REPORT(N);
  //REPORT(FullF);
  ADREPORT(FullF);
  REPORT(CPUEPred);
  REPORT(Like1);
  REPORT(Like2);
  REPORT(Like3);
  ADREPORT(Ratio);
  REPORT(obj_fun);


  return(obj_fun);

}
