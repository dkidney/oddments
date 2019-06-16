################################################################################
################################################################################

# Simulate errors

################################################################################
################################################################################

make.error.model <- function(var.model=c("const","exp"), cor.model=c("ind","ar1","cos"), var.pars=1, cor.pars=0){
    
    cor.model <- match.arg(cor.model) ; cor.model
    
    var.model <- match.arg(var.model) ; var.model

    if(var.model == "exp" & length(var.pars) != 2) stop("2 parameters needed for var.model = exp", call. = FALSE)
    
    if(cor.model == "ar1" & length(cor.pars) != 1) stop("1 parameter needed for cor.model = ar1", call. = FALSE)

    if(cor.model == "cos" & length(cor.pars) != 2) stop("2 parameters needed for cor.model = cos", call. = FALSE)

    mod <- list(var = list(model = var.model, pars = var.pars),
                cor = list(model = cor.model, pars = cor.pars))
    
    return(mod)
    
}

make.covmat <- function(t, error.model){ 
    
    em <- error.model
    
    sigma <- em$var$pars ; sigma
    
    rho <- em$cor$pars ; rho
    
    n <- length(t) ; n
    
    varmat <- switch(em$var$model,
                     const = sigma^2 * diag(n),
                     exp = sigma[1]^2 * diag(exp(sigma[2] * t))) ; varmat
    
    distmat <- outer(t, t, function(x,y) abs(x-y)) ; distmat
    
    cormat <- switch(em$cor$model,
                     ind = diag(n),
                     ar1 = rho ^ distmat,
                     cos = cos(rho[1]*distmat) * exp(rho[2]*distmat) ) ; cormat
    
    covmat <- sqrt(varmat) %*% cormat %*% sqrt(varmat) ; covmat
    
    attr(covmat, "distmat") <- distmat
    
    return(covmat)
    
}

plot.error.model <- function(error.model, tlim=c(0,1), nt = 100){
    
    t <- seq(tlim[1], tlim[2], length = nt) ; t
    
    covmat <- make.covmat(t, error.model) ; covmat
    
    distmat <- attr(covmat, "distmat") ; distmat
    
    cormat <- cov2cor(covmat) ; cormat
    
    plot(t, diag(covmat), type="l", ylab="Variance", main="Variance function", ylim=c(0, max(diag(covmat))))
    
    i <- upper.tri(distmat, diag=TRUE) ; i
    
    j <- order(distmat[i]) ; j
    
    plot(distmat[i][j], cormat[i][j], type="l", ylab="Correlation", xlab="t", main="Correlation function")
    
}

################################################################################

if(0){
    
    require(mvtnorm)
    
    error.model <- make.error.model() ; error.model
    par(mfrow=c(1,3))
    plot.error.model(error.model)
    t <- seq(0,1,length=100)
    covmat <- make.covmat(t, error.model)
    errors <- as.numeric(rmvnorm(1, rep(0,nrow(covmat)), covmat))
    plot(t, errors, main="Simulated errors") ; abline(h=0) 
    
    error.model <- make.error.model(var.model="exp", var.pars = c(0.5,3)) ; error.model
    par(mfrow=c(1,3))
    plot.error.model(error.model)
    t <- seq(0,1,length=100)
    covmat <- make.covmat(t, error.model)
    errors <- as.numeric(rmvnorm(1, rep(0,nrow(covmat)), covmat))
    plot(t, errors, main="Simulated errors") ; abline(h=0) 
    
    error.model <- make.error.model(var.model="exp", var.pars = c(0.5,3), cor.model="ar1", cor.pars=0.5) ; error.model
    par(mfrow=c(1,3))
    plot.error.model(error.model)
    t <- seq(0,1,length=100)
    covmat <- make.covmat(t, error.model)
    errors <- as.numeric(rmvnorm(1, rep(0,nrow(covmat)), covmat))
    plot(t, errors, main="Simulated errors") ; abline(h=0) 
    
    error.model <- make.error.model(var.model="exp", var.pars = c(0.5,3), cor.model="cos", cor.pars=c(15,-4)) ; error.model
    par(mfrow=c(1,3))
    plot.error.model(error.model)
    t <- seq(0,1,length=100)
    covmat <- make.covmat(t, error.model)
    errors <- as.numeric(rmvnorm(1, rep(0,nrow(covmat)), covmat))
    plot(t, errors, main="Simulated errors") ; abline(h=0) 

}


################################################################################
################################################################################
