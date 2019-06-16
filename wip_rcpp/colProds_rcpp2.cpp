
#include <Rcpp.h> 
using namespace Rcpp ;

// [[Rcpp::export]]
NumericVector colProds_rcpp2(NumericMatrix x){

    NumericVector col_prods(x.ncol()) ;

    col_prods.fill(1) ;

    for (int col = 0 ; col < x.ncol() ; col++) {

        for (int row = 0 ; row < x.nrow() ; row++) {

            col_prods(col) *= x(row,col) ;

        }

    }

    return col_prods ;

}