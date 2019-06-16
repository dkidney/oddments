
#include <Rcpp.h> 
double timestwo(double x){

    return 2*x ;

}

//' A silly Rcpp function that uses another Rcpp function inside it. 
// [[Rcpp::export]]
double timesfour(double x){

    return timestwo(timestwo(x)) ;

}
