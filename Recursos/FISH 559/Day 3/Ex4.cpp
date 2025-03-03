#include <TMB.hpp>

template <class Type> Type square(Type x){return x*x;}

template <class Type> vector<Type> square(vector<Type> x){return x*x;}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(Nyear)
  DATA_INTEGER(Nclass)
  DATA_VECTOR(Length)
  DATA_VECTOR(Weight)
  DATA_MATRIX(X);
  DATA_VECTOR(S)
  DATA_VECTOR(SurveyS)
  DATA_SCALAR(M)
  DATA_VECTOR(CWObs)
  DATA_MATRIX(CALObs);
  DATA_SCALAR(Neff)
  DATA_VECTOR(BioIndex)
  DATA_SCALAR(BioSig)
  DATA_INTEGER(Nproj)
  DATA_SCALAR(Fproj)

  // End of data section

  PARAMETER(dummy);
  PARAMETER(LogRbar);
  PARAMETER_VECTOR(LogNinit);
  PARAMETER_VECTOR(LogFullF);
  PARAMETER_VECTOR(Eps);

  matrix<Type> N(Nyear+Nproj+1,Nclass);
  matrix<Type> F(Nyear+Nproj,Nclass);
  matrix<Type> Z(Nyear+Nproj,Nclass);
  matrix<Type> CAL(Nyear+Nproj,Nclass);
  vector<Type> CW(Nyear+Nproj);
  vector<Type> BioPred(Nyear+Nproj);

  Type CALtot;

  Type Penal;
  Type LikeCatch;
  Type LikeBio;
  Type LikeCAL;
  Type obj_fun;

  // End of specifications section
  // =============================

  // First set F and Z by size-classs (note that Fproj applies after year Nyear)
  for (int Iyear=0; Iyear<Nyear+Nproj; Iyear++)
   for (int Iclass=0;Iclass<Nclass;Iclass++)
    {
     if (Iyear < Nyear)
 	  F(Iyear,Iclass) = exp(LogFullF(Iyear))*S(Iclass);
 	 else
 	  F(Iyear,Iclass) = Fproj*S(Iclass);
	 Z(Iyear,Iclass) = M + F(Iyear,Iclass);

    }

  // Now set the N matrix
  for (int Iclass=0;Iclass<Nclass;Iclass++) N(0,Iclass) = exp(LogNinit(Iclass));
  for (int Iyear=0;Iyear<Nyear+Nproj;Iyear++)
   {
    // Catch-at-length
    CALtot = 0; CW(Iyear) = 0;
    for (int Iclass=0;Iclass<Nclass;Iclass++)
     {
      CAL(Iyear,Iclass) = F(Iyear,Iclass)/Z(Iyear,Iclass)*N(Iyear,Iclass)*(1.0-exp(-Z(Iyear,Iclass)));
      CALtot += CAL(Iyear,Iclass);
      CW(Iyear) += Weight(Iclass)*CAL(Iyear,Iclass);
     }
    CALtot = (CAL.row(Iyear)).sum();
    for (int Iclass=0;Iclass<Nclass;Iclass++) CAL(Iyear,Iclass) /= CALtot;

    // Numbers-at-age
    for (int Iclass=0;Iclass<Nclass;Iclass++)
     {
      N(Iyear+1,Iclass) = 0;
      for (int Jclass=0;Jclass<Nclass;Jclass++)
       N(Iyear+1,Iclass) += N(Iyear,Jclass)*exp(-Z(Iyear,Jclass))*X(Jclass,Iclass);
     }

    // Recruitment (watch for the index for Eps - and N)
    N(Iyear+1,0) += exp(LogRbar)*exp(Eps[Iyear]);
   }

  // Catch Likelihood (two versions)
  Type SS = 0;
  for (int Iyear=0; Iyear<Nyear; Iyear++) SS += square(log(CWObs(Iyear)) - log(CW(Iyear)));
  LikeCatch = SS /(2.0*0.05*0.05);
  
  LikeCatch = 0;
  for (int Iyear=0; Iyear<Nyear; Iyear++) LikeCatch += -1*dnorm(log(CWObs(Iyear)),log(CW(Iyear)),Type(0.05),true);

  // Biomass predictions
  for (int Iyear=0; Iyear<(Nyear+Nproj); Iyear++)
   {
    BioPred(Iyear) = 0;
    for (int Iclass=0;Iclass<Nclass;Iclass++) BioPred(Iyear) += N(Iyear,Iclass)*SurveyS(Iclass)*Weight(Iclass);
   }
  Type Top = 0; Type Bot = 0; Type q;
  for (int Iyear=0; Iyear<Nyear; Iyear++)
   { Top += log(BioIndex(Iyear)/BioPred(Iyear)); Bot += 1.0; }
  q = exp(Top/Bot);

  // Likelihood
  SS = 0;
  for (int Iyear=0; Iyear<Nyear; Iyear++) SS += square(log(BioIndex(Iyear))-log(q*BioPred(Iyear)));
  LikeBio = SS/(2.0*BioSig*BioSig);
  
  LikeBio = 0;
  for (int Iyear=0; Iyear<Nyear; Iyear++) LikeBio += -1*dnorm(log(BioIndex(Iyear)),log(q*BioPred(Iyear)),BioSig,true);

  // CAL Likelihood
  LikeCAL = 0;
  for (int Iyear=0; Iyear<Nyear; Iyear++)
   for (int Iclass=0;Iclass<Nclass;Iclass++)
    if (CALObs(Iyear,Iclass) > 0)
     LikeCAL -= Neff*CALObs(Iyear,Iclass)*log(CAL(Iyear,Iclass)/CALObs(Iyear,Iclass));

  // Recruitment penalty (include years after Nyear)
  Penal = 0;
  for (int Iyear=0; Iyear<(Nyear+Nproj); Iyear++) Penal += Eps(Iyear)*Eps(Iyear);
  Penal = Penal / (2.0*0.6*0.6);
  
  Penal= 0;
  for (int Iyear=0; Iyear<(Nyear+Nproj); Iyear++) Penal += -1*dnorm(Eps(Iyear),Type(0),Type(0.6),true);

  obj_fun = dummy*dummy + LikeCatch+LikeBio+LikeCAL+Penal;
  // obj_fun = dummy*dummy;

  // Stuff to report
  REPORT(F);
  REPORT(Z);
  REPORT(N);
  REPORT(LikeCatch);
  REPORT(LikeBio);
  REPORT(LikeCAL);
  REPORT(Penal);
  REPORT(CAL);
  REPORT(CW);
  REPORT(BioPred);
  REPORT(obj_fun);

  return(obj_fun);
}
