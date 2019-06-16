
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>
SEXP vecSum_cpp(SEXP x){
SEXP result;
PROTECT(result = allocVector(REALSXP,1));
double *input, value = 0;
input = REAL(x);
int n = length(x);
for (int i=0; i<n; i++) value += input[i];
REAL(result)[0] = value;
UNPROTECT(1);
return result;
}