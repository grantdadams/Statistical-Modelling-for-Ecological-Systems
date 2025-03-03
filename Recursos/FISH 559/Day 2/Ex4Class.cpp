#include <TMB.hpp>

template <class Type> Type square(Type x){return x*x;}

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

    }

  // Now set the N matrix
  for (int Iclass=0;Iclass<Nclass;Iclass++) N(0,Iclass) = exp(LogNinit(Iclass));
  for (int Iyear=0;Iyear<Nyear+Nproj;Iyear++)
   {
    // Catch-at-length
    // Numbers-at-length

    // Recruitment (watch for the index for Eps - and N)
    N(Iyear+1,0) += exp(LogRbar)*exp(Eps[Iyear]);
   }

  // Catch Likelihood
  Type SS = 0;

  // Biomass predictions

  // Index Likelihood
  SS = 0;

  // CAL Likelihood
  LikeCAL = 0;

  // Recruitment penalty (include years after Nyear)
  Penal = 0;

  obj_fun = dummy*dummy + LikeCatch+LikeBio+LikeCAL+Penal;

  // Stuff to report
  REPORT(N);
  REPORT(LikeCatch);
  REPORT(LikeBio);
  REPORT(LikeCAL);
  REPORT(Penal);
  REPORT(BioPred);
  REPORT(obj_fun);

  return(obj_fun);
}
