##################################################################################
##################################################################################

# Examples using RcppArmadillo

##################################################################################
##################################################################################

# Rcpp Armadillo includes some other object classes which makes it easier to work with vectors and matrices
# it also has a 3D array object type, called 'cube'
# It also has some additional functions and sightly different ways of accessing matrix elements etc.

# Bear in mind, Armadillo uses BLAS for matrix multiplication, so speed is dependant on the implementation of BLAS

# see also: http://arma.sourceforge.net/docs.html

##################################################################################

library(inline) 
library(Rcpp) 
require(RcppArmadillo) 
library(rbenchmark) 

##################################################################################

#---------------------------------------------------------------------------------# Member functions
#---------------------------------------------------------------------------------

# .begin()      
# .begin_col()      
# .begin_row()      

# .col() (mat)
# .cols() (mat)
# .copy_size()

# .each_col()
# .each_row() 
# .elem()
# .end()      
# .end_col() 	 
# .end_row()      
# .eye()

# .diag()

# .fill()

# .insert_col()
# .insert_cols()     
# .insert_row()
# .insert_rows()     
# .insert_slice()
# .insert_slices() 	
# .is_colvec()
# .is_empty()
# .is_finite()
# .is_rowvec()
# .is_square()
# .is_vec()

# .max()     
# .min()     

# .n_elem 
# .n_cols 
# .n_rows 
# .n_slices 
# .n_nonzero (spmat)

# .ones()

# .print()

# .randn()
# .randu()
# .reshape()
# .reset()
# .resize()
# .row() 
# .rows() 

# .set.size()
# .shed_col()
# .shed_cols()     
# .shed_row()
# .shed_rows()     
# .shed_slice()
# .shed_slices() 	
# .slice()
# .slices()
# .subcube()
# .submat() - e.g. X.submat(span(first,last),span::all) 
# .subvec() - e.g. V.subvec(span(first,last))
# .swap_cols() 
# .swap_rows()

# .t()

# .zeros()


#---------------------------------------------------------------------------------# Generated Vectors/Matrices 
#---------------------------------------------------------------------------------

# eye()

# linespace()

# ones()

# randn()
# randu()
# repmat()

# zeros()


#---------------------------------------------------------------------------------# Functions applied to each element in a vector/matrix
#---------------------------------------------------------------------------------

# abs()
# acos()
# acosh()
# asin()
# asinh()
# atan()
# atanh()

# ceil()
# cos()
# cosh()

# exp()

# floor()

# log()

# pow()

# sin()
# sinh()
# square()
# sqrt()

# tan()
# tanh()


#---------------------------------------------------------------------------------# Scalar Valued Functions of Vectors/Matrices/Cubes 
#---------------------------------------------------------------------------------
# accu() 
# as_scalar()

# cdot()

# det()
# dot() 

# log_det()

# norm()
# norm_dot()

# rank()

# trace()


#---------------------------------------------------------------------------------# Scalar/Vector Valued Functions of Vectors/Matrices
#---------------------------------------------------------------------------------
# diagvec()

# max()
# mean()
# median()
# min()

# prod()

# stddev()
# sum()

# var()


#---------------------------------------------------------------------------------# Vector/Matrix/Cube Valued Functions of Vectors/Matrices/Cubes
#---------------------------------------------------------------------------------
# cor()
# cov()
# cross() 
# cumsum()

# diagmat()

# find()
# fliplr(mat)
# flipud(mat)

# join_rows()
# join_cols()
# join_slices()

# reshape()
# resize()

# shuffle()
# sort()
# sort_index()

# trans()

# unique()


#---------------------------------------------------------------------------------# Decompositions, Factorisations, Inverses and Equation Solvers
#---------------------------------------------------------------------------------
# chol()

# eig_gen()
# eig_sym()

# inv()

# princomp()

# qr()
# qr_econ()

# solve()
# svd()
# svd_econ()


#---------------------------------------------------------------------------------# Miscellaneous 
#---------------------------------------------------------------------------------
# is_finite()


##################################################################################
##################################################################################

# VECTORS

#---------------------------------------------------------------------------------# construction

code <- '
    // column vectors by default

    arma::vec VEC(5);
    arma::vec ZEROS = arma::zeros(5);
    arma::vec ONES = arma::ones(5);
    arma::vec LINSPACE = arma::linspace(10, 20, 5);
    arma::vec RANDU = arma::randu(5);
    arma::vec RANDN = arma::randn(5);

    return Rcpp::List::create(Named("vec(5)", VEC),
                        Named("ones(5)", ONES),
                        Named("zeros(5)", ZEROS),
                        Named("linspace(10, 20, 5)", LINSPACE),
                        Named("randu(5)", RANDU),
                        Named("randn(5)", RANDN)
                       );
'
makeVectors_RcppArmadillo <- cxxfunction(signature(), body=code, plugin="RcppArmadillo")
makeVectors_RcppArmadillo()

code <- '
    // row vectors

    arma::rowvec ZEROS = arma::zeros<arma::rowvec>(5);
    arma::rowvec ONES = arma::ones<arma::rowvec>(5);
    arma::rowvec LINSPACE = arma::linspace<arma::rowvec>(10, 20, 5);
    arma::rowvec RANDU = arma::randu<arma::rowvec>(5);
    arma::rowvec RANDN = arma::randn<arma::rowvec>(5);

    return Rcpp::List::create(Named("ones(5)", ONES),
                        Named("zeros(5)", ZEROS),
                        Named("linspace(10, 20, 5)", LINSPACE),
                        Named("randu(5)", RANDU),
                        Named("randn(5)", RANDN)
                       );
'
vectorConstruction_RcppArmadillo <- cxxfunction(signature(), body=code, plugin="RcppArmadillo")
vectorConstruction_RcppArmadillo()

#---------------------------------------------------------------------------------# indexing with member functions

code <- '
    arma::vec VEC = Rcpp::as<arma::vec>(x);

    int N_ELEM = VEC.n_elem;
    int N_ROWS = VEC.n_rows;
    int N_COLS = VEC.n_cols;
    double FIRST = VEC(0); 
    double LAST = VEC(VEC.n_elem-1); 
    arma::vec VEC_SPAN = VEC(arma::span(0,1)); 
    arma::vec SUBVEC_FIRST = VEC.subvec(0, 0); 
    arma::vec SUBVEC_LAST = VEC.subvec(VEC.n_elem-1, VEC.n_elem-1); 
    arma::vec SUBVEC_RANGE = VEC.subvec(1, 3); 
    arma::vec SUBVEC_SPAN = VEC.subvec(arma::span(1,3)); 
    double MIN = VEC.min();
    double MAX = VEC.max();

    return Rcpp::List::create(Named("vec", VEC),
                        Named("vec.n_elem", N_ELEM),
                        Named("vec.n_rows", N_ROWS),
                        Named("vec.n_cols", N_COLS),
                        Named("vec(0)", FIRST),
                        Named("vec(VEC.n_elem-1)", LAST),
                        Named("vec(span(0,1))", VEC_SPAN),
                        Named("vec.subvec(0,0)", SUBVEC_FIRST),
                        Named("vec.subvec(VEC.n_elem-1, VEC.n_elem-1)", SUBVEC_LAST),
                        Named("vec.subvec(1,3)", SUBVEC_RANGE),
                        Named("vec.subvec(span(1,3))", SUBVEC_SPAN),
                        Named("vec.min()", MIN),
                        Named("vec.max()", MAX)
                        );
'
vectorIndexing_RcppArmadillo <- cxxfunction(signature(x="numeric"), body=code, plugin="RcppArmadillo")
vectorIndexing_RcppArmadillo(1:5)

#---------------------------------------------------------------------------------# other member function examples

code <- '
    arma::rowvec VEC = Rcpp::as<arma::rowvec>(x); VEC.print();
    VEC.fill(10); VEC.print();
    VEC.ones(); VEC.print();
    VEC.zeros(); VEC.print();
'
vectorMemberFunctions_RcppArmadillo <- cxxfunction(signature(x="numeric"), body=code, plugin="RcppArmadillo")
vectorMemberFunctions_RcppArmadillo(1:10)


#---------------------------------------------------------------------------------# normal functions

code <- '
    arma::vec VEC = Rcpp::as<arma::vec>(x);

    double MEAN = mean(VEC);
    double MAX = max(VEC);
    double MIN = min(VEC);
    double SUM = sum(VEC);
    arma::vec COS = cos(VEC); 
    bool IS_FINITE = is_finite(VEC);

    return Rcpp::List::create(Named("vec", VEC),
                        Named("mean(vec)", MEAN),
                        Named("max(vec)", MAX),
                        Named("min(vec)", MIN),
                        Named("sum(vec)", SUM),
                        Named("cos(vec)", COS),
                        Named("is_finite(vec)", IS_FINITE)
                        );
'
vectorFunctions_RcppArmadillo <- cxxfunction(signature(x="numeric"), body=code, plugin="RcppArmadillo")
vectorFunctions_RcppArmadillo(1:10)


##################################################################################
##################################################################################

# MATRICES

#---------------------------------------------------------------------------------# construction

code <- '
    arma::mat MAT(3,3);
    arma::mat ZEROS = arma::zeros(3,3);
    arma::mat ONES = arma::ones(3,3);
    arma::mat RANDU = arma::randu(3,3);
    arma::mat RANDN = arma::randn(3,3);
    arma::mat EYE = arma::eye(3,3);
    arma::mat DIAG = ONES;
    DIAG.diag() += 9;

    return Rcpp::List::create(Named("mat(3,3)", MAT),
                        Named("ones()", ONES),
                        Named("zeros()", ZEROS),
                        Named("random U(0,1)", RANDU),
                        Named("random N(0,1)", RANDN),
                        Named("eye()", EYE),
                        Named("mat.diag()", DIAG)
                       );
'
makeMatrices_RcppArmadillo <- cxxfunction(signature(), body=code, plugin="RcppArmadillo")
makeMatrices_RcppArmadillo()

#---------------------------------------------------------------------------------# indexing

code <- '
    arma::mat MAT = Rcpp::as<arma::mat>(x);

    int N_ELEM = MAT.n_elem;
    int N_ROWS = MAT.n_rows;
    int N_COLS = MAT.n_cols;

    double MAT_0 = MAT(0);
    double MAT_0_0 = MAT(0,0);
    double MAT_N = MAT(MAT.n_elem-1);
    double MAT_N_N = MAT(MAT.n_rows-1, MAT.n_cols-1);

    arma::mat MAT_SPAN_SPAN = MAT(arma::span(0,1), arma::span(0,1));
    arma::mat MAT_SPAN_COL = MAT(arma::span::all, 0);
    arma::mat MAT_ROW_SPAN = MAT(0, arma::span::all);

    arma::mat SUBMAT_FIRST_ELEMENT = MAT.submat(0,0,0,0); 
    arma::mat SUBMAT_LAST_ELEMENT = MAT.submat(MAT.n_rows-1, MAT.n_rows-1, MAT.n_cols-1, MAT.n_cols-1); 
    arma::mat SUBMAT_SPAN_SPAN = MAT.submat(arma::span(1,2),arma::span::all); 

    arma::mat FIRST_ROW = MAT.row(0);
    arma::mat LAST_COL = MAT.col(MAT.n_cols-1);

    double MAX = MAT.max();
    double MIN = MAT.min();

    arma::vec DIAG = MAT.diag();
    arma::vec DIAG_1 = MAT.diag(1);

    return Rcpp::List::create(Named("mat", MAT),
                        Named("mat.n_elem", N_ELEM),
                        Named("mat.n_rows", N_ROWS),
                        Named("mat.n_cols", N_COLS),
                        Named("mat(0)", MAT_0),
                        Named("mat(0,0)", MAT_0_0),
                        Named("mat(mat.n_elem-1)", MAT_N),
                        Named("mat(mat.n_rows-1, mat.n_cols-1)", MAT_N_N),
                        Named("mat(span(0,1), span(0,1))", MAT_SPAN_SPAN),
                        Named("mat(span::all, 0)", MAT_SPAN_COL),
                        Named("mat(0, span::all)", MAT_ROW_SPAN),
                        Named("mat.submat(0,0,0,0)", SUBMAT_FIRST_ELEMENT),
                        Named("mat.submat(mat.n_rows-1, mat.n_rows-1, mat.n_cols-1, mat.n_cols-1)", SUBMAT_LAST_ELEMENT),
                        Named("mat.submat(span(1,2),span::all)", SUBMAT_SPAN_SPAN),
                        Named("mat.row(0)", FIRST_ROW),
                        Named("mat.col(mat.n_cols-1)", LAST_COL),
                        Named("mat.max()", MAX),
                        Named("mat.min()", MIN),
                        Named("mat.diag()", DIAG),
                        Named("mat.diag(1)", DIAG_1)
                       );
'
matrixIndexing_RcppArmadillo <- cxxfunction(signature(x="numeric"), body=code, plugin="RcppArmadillo")
matrixIndexing_RcppArmadillo(matrix(1:9,3))

#---------------------------------------------------------------------------------# other member functions

code <- '
    arma::mat MAT = arma::randu(3,3); MAT.print();
    MAT.zeros(); MAT.print();
    MAT.ones(); MAT.print();
    MAT.resize(4,4); MAT.print();
    MAT.reshape(2,8); MAT.print();
    MAT.reset(); MAT.print();
'
matrixFunctions_RcppArmadillo <- cxxfunction(signature(), body=code, plugin="RcppArmadillo")
matrixFunctions_RcppArmadillo()

#---------------------------------------------------------------------------------# other functions

code <- '
    arma::mat MAT = Rcpp::as<arma::mat>(x);
    arma::mat TRANS = trans(MAT);
    arma::mat INV = inv(MAT);
    double DET = det(MAT);
    return Rcpp::List::create(Named("mat", MAT),
                        Named("trans(mat)", TRANS),
                        Named("inv(mat)", INV),
                        Named("det(mat)", DET));
'
matrixFunctions_RcppArmadillo <- cxxfunction(signature(x="numeric"), body=code, plugin="RcppArmadillo")
matrixFunctions_RcppArmadillo(matrix(runif(9),3))

#---------------------------------------------------------------------------------# operations 

code <- '
    arma::mat MAT = Rcpp::as<arma::mat>(x);
    arma::mat SUM = MAT + MAT;
    arma::mat ELEM_MULT = MAT % MAT;
    arma::mat MAT_MULT = MAT * MAT;
    return Rcpp::List::create(Named("mat", MAT),
                        Named("mat + mat", SUM),
                        Named("mat % mat", ELEM_MULT),
                        Named("mat * mat", MAT_MULT));
'
matrixOperations_Rcpp <- cxxfunction(signature(x="numeric"), body=code, plugin="RcppArmadillo")
A <- matrix(1:9,3) ; A
A + A
A * A
A %*% A
matrixOperations_Rcpp(A)


##################################################################################
##################################################################################

# ARRAYS

#---------------------------------------------------------------------------------# construction

code <- '
    arma::cube CUBE(2,2,2);
    arma::cube ZEROS = arma::zeros(2,2,2);
    arma::cube ONES = arma::ones(2,2,2);
    arma::cube RANDU = arma::randu(2,2,2);
    arma::cube RANDN = arma::randn(2,2,2);

    return Rcpp::List::create(Named("cube(1,2,3)", CUBE),
                        Named("ones()", ONES),
                        Named("zeros()", ZEROS),
                        Named("random U(0,1)", RANDU),
                        Named("random N(0,1)", RANDN)
                       );
'
makeCubes_RcppArmadillo <- cxxfunction(signature(), body=code, plugin="RcppArmadillo")
makeCubes_RcppArmadillo()

#---------------------------------------------------------------------------------# indexing

code <- '
    Rcpp::NumericVector VECTOR(x);
    Rcpp::IntegerVector DIM(dim);
    arma::cube CUBE(VECTOR.begin(), DIM[0], DIM[1], DIM[2], false);

    int N_ELEM = CUBE.n_elem;
    int N_ROWS = CUBE.n_rows;
    int N_COLS = CUBE.n_cols;
    int N_SLICES = CUBE.n_slices;

    return Rcpp::List::create(Named("cube", CUBE),
                        Named("cube.n_elem", N_ELEM),
                        Named("cube.n_rows", N_ROWS),
                        Named("cube.n_cols", N_COLS),
                        Named("cube.n_slices", N_SLICES)
                       );
'
cubeIndexing_RcppArmadillo <- cxxfunction(signature(x="numeric", dim="integer"), body=code, plugin="RcppArmadillo")
x <- array(1:8, dim=c(2,2,2)) ; x
cubeIndexing_RcppArmadillo(x, dim(x))

#------------------------------------------------------------------------------------------
# e.g. array row sums
code <- '
    Rcpp::NumericVector VECTOR(array);
    Rcpp::IntegerVector DIM(dim);

    arma::cube ARRAY(VECTOR.begin(), DIM[0], DIM[1], DIM[2], false);

    int n = DIM[0];
    int S = DIM[1];
    int K = DIM[2];

    Rcpp::NumericVector ROWSUMS(n);
    std::fill(ROWSUMS.begin(), ROWSUMS.end(), 0);
    for (int i=0; i<n; i++) {
        for (int s=0; s<S; s++) {
    	    for (int k=0; k<K; k++) {
			    ROWSUMS(i) += ARRAY(i,s,k);
            }
    	}
    }
    return Rcpp::wrap(ROWSUMS);
'
rowSumsArray <-  cxxfunction(signature(array="numeric", dim="integer"), body=code, plugin="RcppArmadillo")
x <- array(1:8, dim=c(2,2,2)) ; x
apply(x, 1, sum)
rowSumsArray(x, dim(x))

##################################################################################
##################################################################################
