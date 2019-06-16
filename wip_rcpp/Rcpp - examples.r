##################################################################################
##################################################################################

# Examples using Rcpp

##################################################################################
##################################################################################

# see also:
    # 'how to - set up Rcpp on windows.r' 
    # 'examples - Rcpp.r' 
    # http://cran.r-project.org/doc/manuals/R-exts.html#The-R-API
    # http://dirk.eddelbuettel.com/code/rcpp/html/namespaceRcpp.html

##################################################################################

library(inline) # where the cxxfunction lives
library(Rcpp) # for the 'plugin'
library(rbenchmark) # for comparing performance

# Rcpp includes:
#   - Member functions - these have a dot and then a name and are prefixed by the name of an existing object
#   - Normal fuctions - the object upon which these functions act are placed inside parentheses after the function name
#   - vector / matrix classes - these help make the syntax and handling easier than in C++

# In the code below, every class or function which is specific to Rcpp has the prefix 'Rcpp::' 
# This is so you can tell the difference between ordinary C++ code and things that are only available in Rcpp
# The code will still run without this prefix 

##################################################################################

# CONSTANTS

# pi
pi_Rcpp <- cxxfunction(signature(), body='return Rcpp::wrap(M_PI);', plugin="Rcpp")
pi_Rcpp()

# others include M_E - 
# see 'Writing R Extensions', CHapter 6 for morealso 

##################################################################################

# VECTORS

code <- '

    Rprintf("Rcpp examples: working with vectors\\n\\n");

    Rcpp::NumericVector VECTOR(x); 

    // length
    double SIZE = VECTOR.size();
    double LENGTH = VECTOR.length();

    // fist and last elements
    double FIRST_ELEMENT = VECTOR(0);
    double LAST_ELEMENT = VECTOR(VECTOR.size()-1); 
    double *FIRST_POINTER = VECTOR.begin();
    double *LAST_POINTER = VECTOR.end(); 

    // max and min
    Rcpp::NumericVector::iterator MAX = std::max_element(VECTOR.begin(), VECTOR.end());  
    Rcpp::NumericVector::iterator MIN = std::min_element(VECTOR.begin(), VECTOR.end());  

    // sum and prod
    double SUM = sum(VECTOR);
    double SUM_ACCUMULATE = std::accumulate(VECTOR.begin(), VECTOR.end(), 0);
    double PROD = 1; for (int i=0; i<VECTOR.size(); i++) PROD *= VECTOR[i];
    double PROD_ACCUMULATE = std::accumulate(VECTOR.begin(), VECTOR.end(), 1, std::multiplies<int>());
    double PROD_NA_IGNORE = 1; for (int i=0; i<VECTOR.size(); i++) if(!ISNA(VECTOR[i])) PROD_NA_IGNORE *= VECTOR[i];

    // vector functions
    Rcpp::NumericVector LOG = log(VECTOR);

    return Rcpp::List::create(Named("NumericVector", VECTOR),
                              Named("NumericVector.size()", SIZE),
                              Named("NumericVector.length()", LENGTH),
                              Named("NumericVector(0))", FIRST_ELEMENT),
                              Named("NumericVector(NumericVector.size()-1)", LAST_ELEMENT),
                              Named("NumericVector(*first)", *FIRST_POINTER),
                              Named("NumericVector(*last)", *LAST_POINTER),
                              Named("max_element(NumericVector.begin(), NumericVector.end())", *MAX),
                              Named("min_element(NumericVector.begin(), NumericVector.end())", *MIN),
                              Named("sum(NumericVector)", SUM),
                              Named("accumulate(NumericVector.begin(), NumericVector.end(), 0)", SUM_ACCUMULATE),
                              Named("vector product using for loop", PROD),
                              Named("accumulate(NumericVector.begin(), NumericVector.end(), 1, multiplies)", PROD_ACCUMULATE),
                              Named("vector product using for loop and !ISNA()", PROD_NA_IGNORE),
                              Named("log(NumericVector)", LOG)
                             );
'
length_Rcpp <- cxxfunction(signature(x="numeric"), body=code, plugin="Rcpp")
length_Rcpp(1:10)

#---------------------------------------------------------------------------------# functions that change the vector itself

code <- '
    Rcpp::NumericVector VECTOR(x);
    VECTOR.fill(1);
    return Rcpp::wrap(VECTOR);
'
fill_Rcpp <-  cxxfunction(signature(x="numeric"), body=code, plugin="Rcpp")
fill_Rcpp(1:5)

##################################################################################

# MATRICES

code <- '
    Rprintf("Rcpp examples: working with matrices\\n\\n");

    // input matrix
    NumericMatrix MATRIX(x);

    // calculate dimensions
    Rcpp::IntegerVector DIM(2);
    DIM(0) = MATRIX.nrow();
    DIM(1) = MATRIX.ncol();

    // explicit indexing
    double ROW1_COL1 = MATRIX(0,0);
    Rcpp::NumericVector ROW1 = MATRIX(0,_);
    Rcpp::NumericVector COL2 = MATRIX(_,1);

    // indexing using member functions
    double *FIRST_POINTER = MATRIX.begin();
    double *LAST_POINTER = MATRIX.end();
    Rcpp::NumericVector ROW1_MEMBER = MATRIX.row(0);
    Rcpp::NumericVector COL2_MEMBER = MATRIX.column(1);

    // make a matrix of zeros
    NumericMatrix ZEROS(DIM(0),DIM(1));

    // function of a matrix
    Rcpp::NumericVector LOG = NumericMatrix (x);
    std::transform(LOG.begin(), LOG.end(), LOG.begin(), ::log);

    return Rcpp::List::create(Named("NumericMatrix", MATRIX),
                              Named("NumericMatrix.nrow()", MATRIX.nrow()),
                              Named("NumericMatrix.ncol()", MATRIX.ncol()),
                              Named("vector of dimensions", DIM),
                              Named("NumericMatrix(0,0)", ROW1_COL1),
                              Named("NumericMatrix(0,_)", ROW1),
                              Named("NumericMatrix(_,1)", COL2),
                              Named("NumericMatrix.begin()", *FIRST_POINTER),
                              Named("NumericMatrix.end()", *LAST_POINTER),
                              Named("NumericMatrix.row(0)", ROW1_MEMBER),
                              Named("NumericMatrix.column(1)", COL2_MEMBER),
                              Named("ZEROS(NumericMatrix.nrow(),NumericMatrix.ncol())", ZEROS),
                              Named("transform(LOG.begin(), LOG.end(), LOG.begin(), log))", LOG)
                             );
'
matrix_Rcpp <- cxxfunction(signature(x="double"), body=code, plugin="Rcpp")
matrix_Rcpp(matrix(1:9,3))

#---------------------------------------------------------------------------------# colSums
code <- '
    NumericMatrix MATRIX(x);
    int NROWS = MATRIX.nrow();
    int NCOLS = MATRIX.ncol();
    Rcpp::NumericVector COLSUMS(NCOLS);
    for (int col=0; col<NCOLS; col++) {
        for (int row=0; row<NROWS; row++) {
		    COLSUMS(col) += MATRIX(row,col);
	    }
    }
    return Rcpp::wrap(COLSUMS);
'
colSums_Rcpp <- cxxfunction(signature(x="numeric"), body=code, plugin="Rcpp")
X <- matrix(1:9, 3) ; X
apply(X,2,sum)
colSums(X)
colSums_Rcpp(X)

# alternative version - more compact code but the same speed
code <- '
    NumericMatrix MATRIX(x);
    int NCOLS = MATRIX.ncol();
    Rcpp::NumericVector COLSUMS(NCOLS);
    for (int col=0; col<NCOLS; col++) {
	    COLSUMS(col) = sum(MATRIX.column(col));
    }
    return Rcpp::wrap(COLSUMS);
'
colSums2_Rcpp <- cxxfunction(signature(x="numeric"), body=code, plugin="Rcpp")
X <- matrix(1:9, 3) ; X
apply(X,2,sum)
colSums(X)
colSums2_Rcpp(X)

benchmark(apply(X,2,sum),
          colSums(X),
          colSums_Rcpp(X),
          colSums2_Rcpp(X),
          columns=c("test", "replications","elapsed", "relative"), order="relative", replications=10000)

#---------------------------------------------------------------------------------# colProds
code <- '
    NumericMatrix MATRIX(x);
    int NROWS = MATRIX.nrow();
    int NCOLS = MATRIX.ncol();
    Rcpp::NumericVector COLPRODS(NCOLS);
    COLPRODS.fill(1);
    for (int col=0; col<NCOLS; col++) {
        for (int row=0; row<NROWS; row++) {
    	    COLPRODS(col) *= MATRIX(row,col);
	    }
    }
    return Rcpp::wrap(COLPRODS);
'
colProds_Rcpp <- cxxfunction(signature(x="numeric"), body=code, plugin="Rcpp")
X <- matrix(1:9, 3) ; X
apply(X,2,prod)
colProds_Rcpp(X)

benchmark(apply(X,2,prod),
          colProds_Rcpp(X),
          columns=c("test", "replications","elapsed", "relative"), order="relative", replications=10000)

##################################################################################

# DATA FRAMES

# make a data frame
code <- '
    Rcpp::NumericVector X1(x);
    Rcpp::NumericVector X2(clone(x));
    X1[0] = 999;
    X2[1] = 999;
    return(DataFrame::create(Named("original", x),
                             Named("X1", X1),
                             Named("x2", X2)));
'
data.frame_Rcpp <- cxxfunction(signature(x="numeric"), body=code, plugin="Rcpp")
data.frame_Rcpp(1:10)

##################################################################################

# LISTS

# extract an element with name 'getThis' from a list
code <- '
    List LIST(x);
    Rcpp::NumericVector RESULT = LIST["getThis"];
    return RESULT;
'
getThis_Rcpp <- cxxfunction(signature(x="list"), body=code, plugin="Rcpp")
x <- list(something=1, getThis=1:10, stuff="A") ; x
getThis_Rcpp(x)

##################################################################################

# DISTRIBUTIONS

# runif(n,a,b)
code <- '
    int N = Rcpp::as<int>(n);
    double A = Rcpp::as<double>(a);
    double B = Rcpp::as<double>(b);
    return Rcpp::wrap(runif(N, A, B));
'
runif_Rcpp <-  cxxfunction(signature(n = "integer", a="numeric", b="numeric"), body=code, plugin="Rcpp")
runif_Rcpp(10,1,2)

#---------------------------------------------------------------------------------# dpois(x, lambda, log)
code <- '
    Rcpp::NumericVector X(x);
    double LAMBDA = Rcpp::as<double>(lambda);
    bool LOG = Rcpp::as<bool>(log);
    return Rcpp::wrap(dpois(X, LAMBDA, LOG));
'
dpois_Rcpp <-  cxxfunction(signature(x="numeric", lambda="numeric",log="logical"), body=code, plugin="RcppArmadillo")
dpois(1:5,10)
dpois_Rcpp(1:5,10,FALSE)
dpois(1:5,10,TRUE)
dpois_Rcpp(1:5,10,TRUE)

#---------------------------------------------------------------------------------# dnorm(x, mu, sd, log)
code <- '
    Rcpp::NumericVector X(x);
    double MU = Rcpp::as<double>(mu);
    double SD = Rcpp::as<double>(sd);
    bool LOG = Rcpp::as<bool>(log);
    return Rcpp::wrap(dnorm(X, MU, SD, LOG));
'
dnorm_Rcpp <-  cxxfunction(signature(x="numeric", mu="numeric", sd="numeric", log="logical"), body=code, plugin="Rcpp")
dnorm(seq(0.1,0.9,length=10), 1, 5, FALSE)
dnorm_Rcpp(seq(0.1,0.9,length=10), 1, 5, FALSE)
dnorm(seq(0.1,0.9,length=10), 1, 5, TRUE)
dnorm_Rcpp(seq(0.1,0.9,length=10), 1, 5, TRUE)

#---------------------------------------------------------------------------------# qnorm(p, mu, sd, lowerTail, log)
code <- '
    Rcpp::NumericVector P(p);
    double MU = Rcpp::as<double>(mu);
    double SD = Rcpp::as<double>(sd);
    bool LOWERTAIL = Rcpp::as<bool>(lowerTail);
    bool LOG = Rcpp::as<bool>(log);
    return Rcpp::wrap(qnorm(P, MU, SD, LOWERTAIL, LOG));
'
qnorm_Rcpp <-  cxxfunction(signature(p="numeric", mu="numeric", sd="numeric", lowerTail="logical", log="logical"), body=code, plugin="Rcpp")
qnorm(seq(0.1,0.9,length=10), 1, 5, TRUE, FALSE)
qnorm_Rcpp(seq(0.1,0.9,length=10), 1, 5, TRUE, FALSE)
qnorm(seq(0.1,0.9,length=10), 1, 5, FALSE, FALSE)
qnorm_Rcpp(seq(0.1,0.9,length=10), 1, 5, FALSE, FALSE)

#---------------------------------------------------------------------------------# pbinom(q, size, prob, lowerTail, log)
code <- '
    Rcpp::NumericVector Q(q);
    int SIZE = Rcpp::as<int>(size);
    double PROB = Rcpp::as<double>(prob);
    bool LOWERTAIL = Rcpp::as<bool>(lowerTail);
    bool LOG = Rcpp::as<bool>(log);
    return Rcpp::wrap(pbinom(Q, SIZE, PROB, LOWERTAIL, LOG));
'
pbinom_Rcpp <-  cxxfunction(signature(q="numeric", size="integer", prob="numeric", lowerTail="logical", log="logical"), body=code, plugin="Rcpp")
pbinom(0:10, 10, 0.5)
pbinom_Rcpp(0:10, 10, 0.5, TRUE, FALSE)
pbinom(0:10, 10, 0.5, FALSE)
pbinom_Rcpp(0:10, 10, 0.5, FALSE, FALSE)

##################################################################################

# USING RCPP FUNCTION INSIDE OTHER RCPP FUNCTIONS

inc <- '
    double norm( double x, double y ){
        return sqrt( x*x + y*y ) ;
    }
'

body <- '
    double X = Rcpp::as<double>(x), Y = Rcpp::as<double>(y);
    double res = norm(X,Y) ;
    return Rcpp::wrap(res) ;
'

norm_Rcpp <- cxxfunction(signature(x="numeric", y="numeric" ), body=body, includes=inc, plugin="Rcpp" )
norm_Rcpp(sqrt(2), sqrt(2))

##################################################################################

# SYNTATIC 'SUGAR'

# There are lots of convenient vectorised functions available in Rcpp that look like R functions but are much faster

# ifelse
code <- '
    Rcpp::NumericVector X(x), Y(y) ;
    Rcpp::NumericVector s = ifelse( X < Y, X*X, -(Y*Y) ) ;
    return Rcpp::wrap(s);
'
ifelse_Rcpp <- cxxfunction(signature(x="numeric", y="integer"), body=code, plugin="Rcpp")
x <- 1:10
y <- 10:1
ifelse(x<y, X*X, -(y*y))
ifelse_Rcpp(x,y)

benchmark(ifelse(x<y, X*X, -(y*y)),
          ifelse_Rcpp(x,y),
          columns=c("test", "replications","elapsed", "relative"), order="relative", replications=10000)

#---------------------------------------------------------------------------------# sapply
inc <- '
    double square( double x ){
        return x*x ;
    }
'
code <- '
    Rcpp::NumericVector X(x);
    Rcpp::NumericVector RESULT = sapply( X, square ) ;
    return RESULT;
'
sapply_Rcpp <- cxxfunction(signature(x="numeric"), body=code, inc=inc, plugin="Rcpp")
sapply_Rcpp(1:10)


#---------------------------------------------------------------------------------# some other functions 
code <- '
    Rcpp::NumericVector X(x);
    Rcpp::NumericVector IFELSE = ifelse( X < 0, X*0, pow(X,0)) ;
    Rcpp::NumericVector ABS = abs(X);
    bool ALL = all(X<0).is_true();
    bool ANY = any(X<0).is_true();
    Rcpp::NumericVector CEIL = ceil(X);
    Rcpp::NumericVector CEILING = ceiling(X);
    Rcpp::NumericVector FLOOR = floor(X);
    Rcpp::NumericVector EXP = exp(X);
    Rcpp::NumericVector HEAD = head(X, 4);
    Rcpp::IntegerVector SEQ_LEN = seq_len(10);
    Rcpp::IntegerVector SEQ_ALONG = seq_along(X);
    Rcpp::NumericVector REP = rep(X, 2);
    Rcpp::NumericVector REP_LEN = rep_len(X, 7);
    Rcpp::NumericVector REP_EACH = rep_each(X, 2);
    Rcpp::NumericVector REV = rev(X);
    return Rcpp::List::create(Named("ifelse( NumericVector < 0, NumericVector*0, pow(NumericVector,0))", IFELSE),
                              Named("abs(NumericVector)",ABS),
                              Named("all(NumericVector<0).is_true()", ALL),
                              Named("any(NumericVector<0).is_true()", ANY), 
                              Named("ceil(NumericVector)", CEIL), 
                              Named("ceiling(NumericVector)", CEILING),
                              Named("floor(NumericVector)",FLOOR), 
                              Named("exp(NumericVector)", EXP), 
                              Named("head(NumericVector, 4)", HEAD), 
                              Named("seq_len(10)", SEQ_LEN), 
                              Named("seq_along(NumericVector)", SEQ_ALONG), 
                              Named("rep(NumericVector, 2)", REP),
                              Named("rep_len(NumericVector, 7)", REP_LEN), 
                              Named("rep_each(NumericVector, 2)", REP_EACH), 
                              Named("rev(NumericVector)", REV));
'
sugar_Rcpp <- cxxfunction(signature(x="numeric"), body=code, plugin="Rcpp")
sugar_Rcpp(seq(-1,1,by=0.5))

##################################################################################

# USING R FUNCTIONS

# Suppose you can't find an Rcpp equivalent to the dt() function
code <- '
    Rcpp::NumericVector X(x);
    Environment stats("package:stats");
    Function dt = stats["dt"];
    return dt(X[0], Named("df", 100));
'
function_Rcpp <- cxxfunction(signature(x="numeric"), body=code, plugin="Rcpp")
function_Rcpp(1:10)


##################################################################################
##################################################################################
