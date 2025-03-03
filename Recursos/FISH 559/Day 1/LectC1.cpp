#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
 DATA_VECTOR(v1);
  DATA_VECTOR(v2);
  DATA_VECTOR(v3);

  DATA_MATRIX(m1);
  DATA_MATRIX(m2);
  //DATA_MATRIX(m3);
  //DATA_MATRIX(m4);
  //DATA_ARRAY(a1);
  //DATA_ARRAY(a2);

  PARAMETER(x);
  // Vectors ========================================================
  int v1_size = v1.size();              // Length of v1
  REPORT(v1_size);
  vector<Type> v2_head = v2.head(2);    // First 2 elements of v2
  REPORT(v2_head);                      //  v2.tail(2): last 2 elem.
  vector<Type> v3_segment = v3.segment(2,3); //segment of 3 elements,
  REPORT(v3_segment);                     // starting at 3rd element
  Type v1_sum = v1.sum();               // sum of all cells in v1
  REPORT(v1_sum);
  Type sum_v1 = sum(v1);                // alternative summation
  REPORT(sum_v1);
  Type v1_prod = v1.prod();             // product of all cells in v1
  REPORT(v1_prod);
  vector<Type> v1_plus_v1 = v1 + v1;    // elementwise addition
  REPORT(v1_plus_v1);                   //   (-, *, / similar)
  Type v1_times_v1_sum = (v1*v1).sum(); // inner product
  REPORT(v1_times_v1_sum);
  vector<Type> exp_v1 = exp(v1);        // exp of v1 (also log)
  REPORT(exp_v1);
  // min() and max(). NB! Non-differentiable functions
  Type v1_mincoeff = min(v1);    // minimum value in v1
  REPORT(v1_mincoeff);
  matrix<Type>  m4 = m1*m2;
  REPORT(m4);
  Type m5 = atomic::logdet(m1);
  REPORT(m5);
  matrix<Type> m6 = atomic::matinv(m1);
  REPORT(m6);
  matrix<Type> m7 =  m1.transpose();
  REPORT(m7);
  return(x*x);


}
