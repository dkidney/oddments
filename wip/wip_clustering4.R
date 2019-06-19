################################################################################
################################################################################

# clustering functions

################################################################################
################################################################################

#' step1
#'
#' Function to fit a series of spline models to the data
#' 
#' When fpc=TRUE the fpcs are calculated by using the b-spline specification determined by the input arguments. The first step is to fit the b-spline model to all profiles/subjects separately. The second step is to use the coefficients from the individual fits to
#'
#' @param x description
#' @return value
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' @export

if(0){
    
    x               = data$x
    y               = data$y
    subject         = data$subject
    degree.range    = 3
    df.range        = 3:5
    re.structs      = "ident"
    var.structs     = "const"
    cor.structs     = "ind"
    ncores          = 4
    results.path    = "~/PhD/Chapters/1 Clustering/Results/trig50/step1"
    dir.create(results.path, FALSE)
    
}

step1 <- function(x, y, subject, 
                  degree.range = 1:3, 
                  df.range     = 1:16, 
                  re.structs   = c("ident","diag","cs","un"), 
                  var.structs  = c("const", "exp", "pow"), 
                  cor.structs  = c("ind", "car1"), 
                  ncores       = getOption("cl.cores", detectCores()), 
                  results.path = tempdir()){
    
    data <- data.frame(y = y, x = x, subject = subject)
    
    mod.sum <- get.model.summary(degree.range, df.range, re.structs, var.structs, cor.structs) ; head(mod.sum) ; dim(mod.sum)
    
    mod.sum <- mod.sum[mod.sum$bspl.df > mod.sum$bspl.degree,] ; head(mod.sum) ; dim(mod.sum)
    
    message("Step 1: fitting ", nrow(mod.sum), " b-spline models...")
    
    myCluster <- try(makeCluster(ncores))
    
    clusterExport(myCluster, c("mod.sum", "data", "results.path"), envir = environment())
    
    clusterEvalQ(myCluster, {
        source('~/PhD/Chapters/1 Clustering/R code/clustering functions.R')
        require(nlme)
        require(MuMIn)
        require(splines)
    })
    
    results <- do.call(rbind, parLapply(myCluster, 1:nrow(mod.sum), function(i){ # i=1
        
        model.name <- paste("bspl_fit_",i,sep="")
        
        res <- fit.lme(data         = data,
                       X            = bs(x          = data$x, 
                                         df         = mod.sum$bspl.df[i], 
                                         degree     = mod.sum$bspl.degree[i], 
                                         intercept  = TRUE),
                       var.struct   = mod.sum$var.struct[i],
                       cor.struct   = mod.sum$cor.struct[i], 
                       re.struct    = mod.sum$re.struct[i], 
                       name         = model.name,
                       path         = results.path) 
        
        for(par in colnames(mod.sum)) res$fit[[par]] <- mod.sum[[par]][i]
        
        fit <- res$fit
        
        save(fit, file = file.path(results.path, paste(model.name,".RData", sep = "")))
        
        return(res$sum)
        
    }))
    
    stopCluster(myCluster)
    
    mod.sum <- cbind(mod.sum, results)
    
    rownames(mod.sum) <- NULL
    
    return(mod.sum)
    
}

################################################################################
################################################################################

if(0){
    
    fit          = step1.best.fit 
    re.structs   = "cs"
    var.structs  = "const"
    cor.structs  = "ind"
    ncores       = 4
    results.path = "~/PhD/Chapters/1 Clustering/Results/trig50/step2"
    
}

step2 <- function(fit, 
                  re.structs     = c("ident","diag","cs","un"), 
                  var.structs    = c("const", "exp", "pow"), 
                  cor.structs    = c("ind", "car1"), 
                  ncores         = getOption("cl.cores", detectCores()), 
                  results.path   = tempdir()){
    
    data <- fit$data
    
    X <- get.fpc.matrix(data$x, data$y, data$subject, fit$bspl.degree, fit$bspl.df)
    
    data <- data[,c("x","y","subject")]
    
    mod.sum <- get.model.summary(fit$bspl.degree, fit$bspl.df, re.structs, var.structs, cor.structs, nfpcs = 1:(ncol(X)-1)) ; head(mod.sum) ; dim(mod.sum)
    
    mod.sum <- mod.sum[!(mod.sum$nfpcs == 1 & mod.sum$re.struct == "cs"),] ; dim(mod.sum)
    
    message("Step 2: fitting ", nrow(mod.sum), " fpc models...")
    
    myCluster <- try(makeCluster(ncores)) ; myCluster
    
    clusterExport(myCluster, c("mod.sum", "data", "results.path", "X"), envir = environment())
    
    clusterEvalQ(myCluster, {
        source('~/PhD/Chapters/1 Clustering/R code/clustering functions.R')
        require(nlme)
        require(MuMIn)
    })
    
    results <- do.call(rbind, parLapply(myCluster, 1:nrow(mod.sum), function(i){ # i=1
        
        model.name <- paste("fpc_fit_",i, sep="")
        
        res <- fit.lme(data         = data,
                       X            = X[,1:mod.sum$nfpcs[i], drop=FALSE],
                       var.struct   = mod.sum$var.struct[i],
                       cor.struct   = mod.sum$cor.struct[i], 
                       re.struct    = mod.sum$re.struct[i], 
                       name         = model.name,
                       path         = results.path) 
        
        for(par in colnames(mod.sum)) res$fit[[par]] <- mod.sum[[par]][i]
        
        fit <- res$fit
        
        res$sum
        
        save(fit, file = file.path(results.path, paste(model.name,".RData", sep = "")))
        
        return(res$sum)
        
    }))
    
    stopCluster(myCluster)
    
    mod.sum <- cbind(mod.sum, results)
    
    rownames(mod.sum) <- NULL
    
    return(mod.sum)
    
}

################################################################################
################################################################################

if(0){
    
    fit         = step2.best.fit 
    G           = 2:4
    re.structs  = "ident"
    var.structs = "const"
    cor.structs = "ind"
    re.blocks   = FALSE
    var.blocks  = FALSE
    cor.blocks  = FALSE
    ncores      = getOption("cl.cores", detectCores())    
#     results.path = "~/PhD/Chapters/1 Clustering/Results/trig50/step3"
    
}

step3 <- function(fit, 
                  G             = 2:4, 
                  re.structs    = c("ident","diag","cs","un"), 
                  var.structs   = c("const","exp","pow"), 
                  cor.structs   = c("ind","car1"), 
                  re.blocks     = c(FALSE, TRUE), 
                  var.blocks    = c(FALSE, TRUE), 
                  cor.blocks    = c(FALSE, TRUE),
                  ncores        = getOption("cl.cores", detectCores()), 
                  results.path  = tempdir()){
    
    G <- G[G>1] ; G
    
    if(length(G) == 0) stop("min G must be at least 2")
    
    X <- as.matrix(fit$data[,names(fit$fixDF$terms)]) ; head(X)
    
    data <- fit$data[,c("x","y","subject")] ; head(data)
    
    mod.sum <- get.model.summary(fit$bspl.degree, fit$bspl.df, re.structs, var.structs, cor.structs, fit$nfpcs, G, re.blocks, var.blocks, cor.blocks) ; head(mod.sum) ; dim(mod.sum)
    
    # if cor struct is independent, then no need to block 
    mod.sum <- mod.sum[!(mod.sum$cor.struct == "ind" & mod.sum$cor.block),] ; head(mod.sum) ; dim(mod.sum)
    
    # when using lme, need to use the same grouping variable for correlation and weights as for random
    mod.sum <- mod.sum[!(mod.sum$cor.block & !mod.sum$re.block),] ; head(mod.sum) ; dim(mod.sum)
    mod.sum <- mod.sum[!(mod.sum$var.block & !mod.sum$re.block),] ; head(mod.sum) ; dim(mod.sum)
    
    # if no models left, then stop
    if(nrow(mod.sum) == 0) stop("No valid model definitions supplied")
    
    # get grouping structures 
    nsubs <- length(unique(data$subject)) ; nsubs
    
    nobs <- sapply(1:nsubs, function(i) sum(data$subject == unique(data$subject)[i])) ; nobs
    
    ranefs <- ranef(fit) ; head(ranefs)
    
    ranefs <- ranefs[order(as.numeric(rownames(ranefs))),] ; head(ranefs)
    
    group.struct <- lapply(G, function(g){ # g=2
        
        output <- Mclust(ranefs, G = g)
        
        attr(output, "group") <- factor(rep(output$classification, times = nobs))
        
        return(output)
        
    })
    
    message("Step 3: fitting ", nrow(mod.sum), " models...")
    
    myCluster <- makeCluster(ncores, outfile = file.path(results.path, "step3.log"))
    
    clusterExport(myCluster, c("mod.sum","data","results.path","X","group.struct","G"), envir = environment())
    
    clusterEvalQ(myCluster, {
        source('~/PhD/Chapters/1 Clustering/R code/clustering functions.R')
        require(nlme)
        require(MuMIn)
        require(mclust)
    })
    
    results <- do.call(rbind, parLapply(myCluster, 1:nrow(mod.sum), function(i){ # i=1
        
        model.name <- paste("fit_",i, sep="") ; model.name
        
        data <- cbind(data, group = attr(group.struct[[which(G == mod.sum$ngroups[i])]], "group")) ; head(data)
        
        res <- fit.lme(data         = data,
                       X            = X,
                       re.struct    = mod.sum$re.struct[i],
                       var.struct   = mod.sum$var.struct[i],
                       cor.struct   = mod.sum$cor.struct[i], 
                       re.block     = mod.sum$re.block[i],
                       var.block    = mod.sum$var.block[i],
                       cor.block    = mod.sum$cor.block[i],
                       name         = model.name,
                       path         = results.path) 
        
        for(par in colnames(mod.sum)) res$fit[[par]] <- mod.sum[[par]][i]
        
        fit <- res$fit
        
        res$sum
        
        save(fit, file = file.path(results.path, paste(model.name,".RData", sep = "")))
        
        return(res$sum)
        
    }))
    
    stopCluster(myCluster)
    
    mod.sum <- cbind(mod.sum, results)
    
    rownames(mod.sum) <- NULL
    
    return(mod.sum)
    
}

################################################################################
################################################################################

#' get.fpc.matrix
#'
#' Make a design matrix of functional pricipal components
#' 
#' details
#'
#' @param x description
#' @return value
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' @export

get.fpc.matrix <- function(x, y, subject, degree, df){
    
    data <- data.frame(y = y, x = x, subject = subject)
    
    basis <- create.bspline.basis(nbasis = df, norder = degree + 1, rangeval = range(data$x))
    
    X <- eval.basis(data$x, basis) ; head(X) ; dim(X)
    
    data <- groupedData(y ~ x | subject, data = cbind(data, X)) ; head(data)
    
    form <- formula(paste("y ~ -1 +", paste(colnames(X), collapse=" + "), "| subject")) ; form
    
    lmList.coefs <- coef(lmList(form, data)) ; head(lmList.coefs)
    
    lmList.coefs <- as.matrix(lmList.coefs[order(as.factor(rownames(lmList.coefs))),]) ; head(lmList.coefs)
    
    if(all(rownames(lmList.coefs) != as.character(unique(data$subject)))) stop("Something funny is going on with the subject names in the lmList step...")
    
    functional.data.object <- fd(coef = t(lmList.coefs), basisobj = basis)
    
    pca.functional.data.object <- pca.fd(fdobj = functional.data.object, nharm = df, centerfns = FALSE) # 
    
    fpcX <- sapply(1:df, function(comp) X %*% pca.functional.data.object$harmonics$coefs[,comp]) ; head(fpcX) ; dim(fpcX)
    
    attr(fpcX, "varprop") <- pca.functional.data.object$varprop
    
    return(fpcX)
    
}

################################################################################
################################################################################

fit.lme <- function(data, X, var.struct = "const", cor.struct = "ind", re.struct = "ident", var.block = FALSE, cor.block = FALSE, re.block = FALSE, name = NA, path = NA){
    
    if(is.null(data$group)) data$group <- rep(1, nrow(data))
    
    group <- data$group
    
    ngroups <- length(unique(data$group)) 
    
    Z <- X    
    
    if(ngroups > 1){
        
        colnames(X) <- colnames(Z) <- 1:ncol(X)
        
        X <- model.matrix(~ -1 + X:group) # head(X) ; dim(X)
        
        colnames(X) <- gsub(":","_", colnames(X)) # colnames(X)
        
        Z <- if(re.block) model.matrix(~ -1 + Z:group) else model.matrix(~ -1 + Z)
        
        colnames(Z) <- gsub(":","_", colnames(Z)) # head(Z) ; dim(Z)
        
    }else{
        
        colnames(X) <- paste("X", 1:ncol(X), sep="")    
        
        colnames(Z) <- paste("Z", 1:ncol(Z), sep="")    
        
    }
    
    newdata <- groupedData(y ~ x | subject, data = cbind(data, X, Z)) # head(newdata)
    
    fixed <- get.fixed.formula(X) # fixed
    
    random <- get.random.formula(Z, re.struct, re.block) # attr(random, "formula")
    
    correlation <- get.correlation.formula(cor.struct, cor.block) # correlation
    
    weights <- get.weights.formula(var.struct, var.block) # weights
    
    fit <- try(lme(fixed, newdata, random, correlation, weights, method = "ML"), TRUE)
    
    sum <- if(class(fit) == "try-error"){
        
        fit <- list()
        
        data.frame(npars = NA, BIC = NA, AICc = NA, AIC = NA)
        
    }else{
        
        data.frame(npars   = attr(logLik(fit), "df"),
                   BIC     = BIC(fit), 
                   AICc    = AICc(fit), 
                   AIC     = AIC(fit),
                   stringsAsFactors = FALSE)
        
    }
    
    sum$name <- fit$name <- name
    sum$path <- fit$path <- path
    
    return(list(fit = fit, sum = sum))
    
}

################################################################################
################################################################################

get.model.summary <- function(degree.range  = 1, 
                              df.range      = 5, 
                              re.structs    = "ind", 
                              var.structs   = "const", 
                              cor.structs   = "ind", 
                              nfpcs         = NA, 
                              ngroups       = 1, 
                              re.blocks     = NA, 
                              var.blocks    = NA, 
                              cor.blocks    = NA){
    
    mod.sum <- expand.grid(bspl.degree = degree.range, 
                           bspl.df     = df.range,
                           nfpcs       = nfpcs,
                           ngroups     = ngroups,
                           re.struct   = re.structs, 
                           var.struct  = var.structs, 
                           cor.struct  = cor.structs,
                           re.block    = re.blocks, 
                           var.block   = var.blocks, 
                           cor.block   = cor.blocks,
                           stringsAsFactors = FALSE)
    
    return(mod.sum)
    
}

################################################################################
################################################################################

classification.success <- function(true, est){
    
    true <- outer(true, true, function(x, y) x == y) ; true
    
    est <- outer(est, est, function(x, y) x == y) ; est
    
    same <- true == est
    
    return(mean(same[upper.tri(same)]))
    
}

################################################################################
################################################################################

choose.best.model <- function(summary, criterion = "BIC"){
    
    delta <- summary[,criterion] - min(summary[,criterion], na.rm = TRUE) ; delta
    
    within <- which(delta < 2) ; within
    
    best <- if(length(within) == 1){
        
        within
        
    }else{        
        
        minpars <- which(summary[within, "npars"] == min(summary[within, "npars"])) ; minpars
        
        if(length(minpars) == 1){
            
            within[minpars]
            
        }else{
            
            minscore <- which(summary[within, criterion][minpars] == min(summary[within, criterion][minpars])) ; minscore
            
            within[minpars][minscore]
            
        }
        
    } ; best
    
    return(best)
    
}

################################################################################
################################################################################

get.fixed.formula <- function(X){
    
    form <- paste("y ~ -1 ", paste("+", colnames(X), sep=" ", collapse=" "), sep="") ; form
    
    fixed <- eval(parse(text = form))
    
    return(fixed)
    
}

################################################################################
################################################################################

get.random.formula <- function(Z, re.struct, re.block = FALSE){
    
    z.names <- colnames(Z)
    
    n.names <- length(z.names) ; n.names
    
    ngroups <- if(!grepl("group", z.names[1])) 1 else{
        
        temp.names <- do.call(rbind, strsplit(z.names, ":"))
        
        length(unique(temp.names[,grepl("group", temp.names[1,])]))
        
    } ; ngroups
    
    pdClass <- switch(re.struct, ident = "pdIdent", diag = "pdDiag", cs = "pdCompSymm", un = "pdSymm") ; pdClass
    
    blocks <- if(re.block) cut(1:n.names, seq(0, n.names, by = n.names / ngroups)) else
        rep(1, n.names) ; blocks
    
    form <- lapply(unique(blocks), function(block){ # block = unique(blocks)[2]
        
        term.names <- paste("+", z.names[which(blocks == block)], sep=" ", collapse=" ")
        
        form <- paste(pdClass,"(~ -1 ", term.names, ")", sep="") ; form
        
        eval(parse(text = form))
        
    }) ; form
    
    random <- if(re.block) pdBlocked(form) else form[[1]]
    
    return(random)
    
}

################################################################################
################################################################################

get.correlation.formula <- function(cor.struct = "ar1", cor.block = FALSE){
    
    correlation <- if(cor.struct == "ind") NULL else{
        
        corClass <- switch(cor.struct, ar1  = "corAR1", car1 = "corCAR1") ; corClass
        
        form <- paste(corClass, "(form = ~ x") ; form
        
        if(cor.block) form <- paste(form, "| group") ; form
        
        form <- paste(form, ")", sep="") ; form
        
        eval(parse(text = form))
        
    }
    
    return(correlation)
    
}

################################################################################
################################################################################

get.weights.formula <- function(var.struct = "const", var.block = FALSE){
    
    varClass <- switch(var.struct, const = "varIdent", exp  = "varExp", pow  = "varPower") ; varClass
    
    form <- paste(varClass, "(form = ~", sep="") ; form
    
    term <- switch(var.struct, const = "1", "x") ; term
    
    form <- paste(form, term) ; form
    
    if(var.block) form <- paste(form, "| group") ; form
    
    form <- paste(form, ")", sep="") ; form
    
    weights <- eval(parse(text = form))
    
    return(weights)
    
}

################################################################################
################################################################################

#' sim.trig.data
#'
#' Function to generate artificial curve data for multiple subjects from three different functions/groups
#' 
#' detaiils
#'
#' @param nobs the number of observations per subject
#' @param nsubs the number of subjects
#' @param beta a vector of mean parameter values using in the three underlying functions (the same paramters are used by each function)
#' @param beta.cov.pars parameters determining the corvariance matrix for the betas from which betas for individual subects are drawn. The first parameter gives the variance, the second parmater gives the correlation (the off-diagonals in a compound symmetric correlation structure)
#' @param error.cov.pars parameters determining the corvariance matrix for the errors. The first parameter gives the variance, the second parmater gives the ar1 correlation parameter
#' @param xlim the range of the x-values
#' @param p.missing
#' @param regular.x
#' @return Returns a data frame with the following columns:
#' \itemize{
#' \item y
#' \item x
#' \item subject
#' \item group
#' \item Ey0 - the group mean function values
#' \item Ey1 - the subject-specific mean function values
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' require(ggplot2)
#' require(mvtnorm)
#' data.data <- sim.trig.data()
#' head(data.data) ; dim(data.data)
#' ggplot(data.data, aes(x,y, group = subject, colour = group)) + 
#'   geom_point(alpha=0.3) + 
#'   geom_path(aes(x,Ey1)) + 
#'   geom_line(aes(x, Ey0, group = group), col=1, lwd=1) + 
#'   facet_wrap(~group)     
#' @export

sim.trig.data <- function(nobs = 20, nsubs.per.group = 10, beta = c(0,1,10), xlim = c(0,1), beta.cov.pars = c(0.01,0), error.cov.pars = c(0.01, 0), regular.x = TRUE, p.missing = 0.0){
    
    nsubs <- nsubs.per.group * 3
    
    group <- rep(1:3, each = nsubs.per.group) ; group <- group[order(group)] ; group    
    
    red   <- function(x, pars, xlim) pars[1] + pars[2] * sin(pars[3] * x) * (xlim[2] - x) 
    green <- function(x, pars, xlim) pars[1] + pars[2] * cos(pars[3] * (x + xlim[2] / 4)) * x 
    blue  <- function(x, pars, xlim) pars[1] + pars[2]/2 * cos(pars[3] * x) 
    
    beta.varmat <- diag(3) * beta.cov.pars[1] ; beta.varmat
    beta.cormat <- matrix(beta.cov.pars[2], nc=3, nr=3) ; diag(beta.cormat) <- 1 ; beta.cormat
    beta.covmat <- sqrt(beta.varmat) %*% beta.cormat %*% sqrt(beta.varmat) ; beta.covmat
    
    data <- do.call(rbind, lapply(1:nsubs, function(i){ # i=1
        
        x <- if(regular.x){
            
            seq(xlim[1], xlim[2], length = nobs)
            
        } else { 
            
            sort(runif(nobs, xlim[1], xlim[2]))
            
        } ; x
        
        beta.i <- as.numeric(rmvnorm(1, beta, beta.covmat)) ; beta.i
        
        colour <- switch(group[i], "red", "green", "blue")
        
        Ey0 <- eval(parse(text = paste(colour,"(x, beta, xlim)",sep=""))) ; Ey0
        
        Ey1 <- eval(parse(text = paste(colour,"(x, beta.i, xlim)",sep=""))) ; Ey1
        
        errors <- if(is.null(error.cov.pars)){
            
            rep(0, nobs)
            
        }else{            
            
            error.varmat <- diag(nobs) * error.cov.pars[1] ; error.varmat
            error.distmat <- outer(x, x, function(x,y) abs(x-y)) ; error.distmat
            error.cormat <- error.cov.pars[2] ^ error.distmat ; error.cormat
            error.covmat <- sqrt(error.varmat) %*% error.cormat %*% sqrt(error.varmat) ; error.covmat
            
            as.numeric(rmvnorm(1, rep(0,nobs), error.covmat))
            
        }
        
        data <- data.frame(y = Ey1+errors, x = x, subject = i, group = group[i], Ey1 = Ey1, Ey0 = Ey0)
        
        data[rbinom(nobs, 1, p.missing) == 0,]
        
    }))
    
    data$group <- as.factor(data$group)
    
    attr(data, "nobs.per.sub") <- nobs
    
    attr(data, "nsubs.per.group") = nsubs.per.group
        
    attr(data, "beta") <- beta

    attr(data, "beta.cov.pars") <- beta.cov.pars
    
    attr(data, "error.cov.pars") <- error.cov.pars

    attr(data, "regular.x") = regular.x
    
    attr(data, "p.missing") <- p.missing
    
    return(data)
    
}

################################################################################
################################################################################

#' sim.luan.li.data

if(0){
    require(splines)
    require(mvtnorm)
    ngroups = 4
    nsubs.per.group = 10
    nobs.per.sub = 18
    ranef.correlation = 0.5
    ranef.variance = 0.4
    error.variance = 0.25   
}

sim.luanli.data <- function(ngroups = 4, nsubs.per.group = 200, nobs.per.sub = 18, ranef.correlation = 0.5, ranef.variance = 0.4, error.variance = 0.25){
    
    nsubs <- ngroups * nsubs.per.group ; nsubs
    
    beta <- list(c(-2.3, -0.5,  1.5, -1.0,  0.5,  0.4, -0.8,  1.2),
                 c(-0.5, -0.3, -1.5,  1.7, -0.5, -0.4,  1.2,  0.5),
                 c( 2.5, -1.5,  2.4, -0.6, -1.5,  0.5,  1.5, -1.0),
                 c( 1.2, -2.5,  0.8, -0.6, -1.4,  1.5, -0.4,  2.4))
    
    x <- seq(0, 1, length = nobs.per.sub)
    
    X <- bs(x, df = 8, degree = 3, intercept  = TRUE) ; dim(X) ; attr(X, "knots")
    
    group <- rep(1:ngroups, each = nsubs.per.group) ; group
    
    ranef.varmat <- ranef.variance * diag(8) ; ranef.varmat
    
    ranef.cormat <- matrix(ranef.correlation, 8, 8) ; diag(ranef.cormat) <- 1 ; ranef.cormat
    
    ranef.covmat <- sqrt(ranef.varmat) %*% ranef.cormat %*% sqrt(ranef.varmat) ; ranef.covmat
    
    error.covmat <- error.variance * diag(nobs.per.sub) ; error.covmat
    
    data <- do.call(rbind, lapply(1:nsubs, function(i){ # i=1
        
        Ey0 <- X %*% beta[[group[i]]]
        
        ranefs <- as.numeric(rmvnorm(1, sigma = ranef.covmat)) ; ranefs
        
        Ey1 <- Ey0 + X %*% ranefs
        
        errors <- as.numeric(rmvnorm(1, sigma = error.covmat)) ; errors
        
        y <- Ey1 + errors
        
        return(data.frame(y = y, x = x, subject = i, group = group[i], Ey1 = Ey1, Ey0 = Ey0))
        
    })) ; head(data) ; dim(data)
    
    for(i in c("subject", "group")) data[[i]] <- as.factor(data[[i]])
    
    return(data)
    
}

if(0){
    
    require(splines)
    require(mvtnorm)
    require(ggplot2)
    
    luanli <- sim.luanli.data() ; head(luanli)
        
    myplot <- ggplot(luanli, aes(x, y, group = subject, colour = group)) + 
        geom_line(alpha = 0.3) + 
        geom_line(aes(x, Ey0, group = group), col=1, lwd=1) + 
        facet_wrap(~ group) + 
        ggtitle("Luan and Li sim data\n")

    print(myplot)
    
}

################################################################################
################################################################################
