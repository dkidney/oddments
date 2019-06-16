#################################################################################################################
#################################################################################################################

# Non-standard variance relationships : approximation with splines

#################################################################################################################
#################################################################################################################


require(Rcpp)
require(RcppArmadillo)
require(RcppEigen)
require(inline)

#################################################################################################################

# Rcpp version of dmvnorm

code <- '
    using arma::mat; 
    using arma::vec; 
    using arma::as_scalar; 
    using arma::trans; 
    using arma::solve;
    mat Y = as<mat>(y);
    mat MU = as<mat>(mu);
    mat SIGMA = as<mat>(sigma);
    mat RESIDUAL = Y - MU;
    double DISTVAL = as_scalar(trans(RESIDUAL) * solve( SIGMA, RESIDUAL ));
    vec eigval;
    eig_sym(eigval, SIGMA);
    double LOGDET = as_scalar(sum(log(eigval)));
    double LOGRETVAL = -(Y.n_elem * log(M_2PI) + LOGDET + DISTVAL)/2;
    return wrap(LOGRETVAL);'
dmvnorm_RcppArmadillo <- cxxfunction( signature(y="numeric", mu="numeric", sigma="numeric"), code, plugin="RcppArmadillo")

#################################################################################################################

# testing

if(0){
    
    n <- 1000
    Beta_0 <- 1
    Beta_1 <- 2
    Beta <- as.matrix(c(Beta_0,Beta_1)) ; Beta ; dim(Beta) ; class(Beta)
    x <- seq(0, 10, length=n)
    X <- as.matrix(cbind(1,x)) ; head(X,5) ; dim(X) ; class(X)
    sigma <- 2
    Sigma <- sigma^2*diag(n) ; Sigma[1:min(n,5),1:min(n,5)] ; dim(Sigma) ; class(Sigma)
    y <- t(rmvnorm(1, mean = X %*% Beta, sigma = Sigma)) ; head(y, 10) ; dim(y) ; class(y)
    
    require(mvtnorm)
    dmvnorm(as.numeric(y), as.numeric(X %*% Beta), Sigma, log = TRUE)
    
    dmvnorm_RcppArmadillo(y, X %*% Beta, Sigma)
    
    require(rbenchmark)
    benchmark(dmvnorm(t(y), mean = X %*% Beta, sigma = Sigma, log = TRUE),
              dmvnorm_RcppArmadillo(y, X %*% Beta, Sigma),
              columns=c("test", "replications","elapsed", "relative"), order="relative", replications=1)
    
}

#################################################################################################################
#################################################################################################################
