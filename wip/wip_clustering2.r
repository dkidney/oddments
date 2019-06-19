################################################################################
################################################################################
################################################################################
################################################################################

#' @export

add.ellipse = function(mu, sigma, label = NULL, ...){
    
    ev <- eigen(sigma, symmetric = TRUE)

    s <- sqrt(rev(sort(ev$values)))
    
    V <- t(ev$vectors[, rev(order(ev$values))])
    
    theta <- (0:15) * (pi/(2 * 15))
    
    x <- s[1] * cos(theta)
    
    y <- s[2] * sin(theta)
    
    xy <- cbind(c(x, -x, -x, x), c(y, y, -y, -y))
    
    xy <- xy %*% V
    
    xy <- sweep(xy, MARGIN = 2, STATS = mu, FUN = "+")
    
    l <- length(x)
    
    i <- 1:l
    
    for(j in 1:4){
    
        lines(xy[i, ], ...)
        
        i <- i + l
    
    }
    
    if(is.null(label)){
    
        x <- s[1]
        
        y <- s[2]
        
        xy <- cbind(c(x, -x, 0, 0), c(0, 0, y, -y))
        
        xy <- xy %*% V
        
        xy <- sweep(xy, MARGIN = 2, STATS = mu, FUN = "+")
        
        lines(xy[1:2, ], lty = 2)
        
        lines(xy[3:4, ], lty = 2)
        
        points(mu[1], mu[2], pch = 8)
        
    }else{     
        
        text(mu[1], mu[2], label = label, ...)
        
    }
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Classification success of clustering algorithm
#'
#' Description
#' 
#' Details.
#'
#' @param truth true group classification
#' @param estimate estimated group classification
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

classification.success = function(truth, estimate){
    
    matching = function(x){
        
        grid = outer(x, x, function(x, y) x == y)
        
        grid[!lower.tri(grid)] = NA
        
        grid
        
    }
    
    true = matching(truth) ; true
    
    est = matching(estimate) ; est
    
    grid = true == est ; grid
    
    success = mean(grid, na.rm = TRUE) ; success
    
    attr(success, "Truth") = true
    
    attr(success, "Predicted") = est
    
    attr(success, "Success") = grid
    
    class(success) = c("success", class(success))
    
    return(success)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Cluster random effects
#'
#' Perform clustering using \code{Mclust} on the random effects from a fitted \code{lme} model
#' 
#' Details.
#'
#' @param fit description
#' @param ... description
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

cluster.ranefs = function(fit, ...){
    
    ranefs = ranef(fit)
    
    #     ranefs = ranefs[match(unique(fit$data$subject), rownames(ranefs)), , drop = FALSE] ; head(ranefs)
    
    mc = Mclust(ranefs, ...)
    
    mc$call$data = ranefs
    
    return(mc)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Cluster longitudinal profiles
#'
#' Description.
#' 
#' Details.
#'
#' @param argument description
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

lme.cluster = function(data, max.df = 16, degree = 3, messages = TRUE, range = NULL, criterion = c("BIC","AIC"), try.pdSymm = TRUE, ...){
    
    criterion = match.arg(criterion)
    
    if(messages) message(paste("Using", criterion, "for model selection"))
    
    crit = eval(parse(text = criterion), envir = environment())
    
    #---------------------------------------------------------------------------
    # step 1: fit a series of cubic B-splines with varying degrees of freedom
    
    if(messages){ cat("\n") ;  message(paste("Fitting", max.df - 3, "cubic B-spline models...")) }
    
    range.df = 4:max.df ; range.df
    
    if(is.null(range)) range = range(data$x) ; range
    
    crit.val = sapply(range.df, function(df){ # df=4
        
        if(messages) message(paste("df =", df))
        
        fit = try({
            
            lme.wrapper(data, 
                        mean = set.spline(npars = df, degree = degree, range = range))
            
        }, TRUE) ; fit
        
        if(inherits(fit, "try-error")) return(NA) else return(crit(fit))
        
    }) ; crit.val
    
    delta = crit.val - min(crit.val, na.rm = TRUE) ; delta
    
    i = which(delta < 2)[1] ; i
    
    df = range.df[i] ; df
    
    crit.val.step1 = crit.val[i] ; crit.val.step1
    
    if(messages) message(paste("Best B-spline model: df = ", df, " (", criterion, " = ", round(crit.val.step1,1), ")", sep=""))
    
    #---------------------------------------------------------------------------
    # step 2: try and reduce the dimension of the best step 1 model using FPCs
    
    if(messages){ cat("\n") ;  message(paste("Fitting", df - 1, "FPC models...")) }
    
    range.nfpcs = 1:df ; range.nfpcs
    
    spline.pars = set.spline(npars = df, degree = 3, range = range)
    
    crit.val = sapply(range.nfpcs, function(nfpcs){
        
        if(messages) message(paste("FPCs =", nfpcs))
        
        fit = try({
            
            lme.wrapper(data, 
                        mean    = spline.pars, 
                        nfpcs   = nfpcs)
            
        }, TRUE)
        
        if(inherits(fit, "try-error")) return(NA) else return(crit(fit))
        
    })
        
    delta = crit.val - min(crit.val, na.rm = TRUE) ; delta
    
    i = which(delta < 2)[1] ; i
    
    nfpcs = range.nfpcs[i] ; nfpcs
    
    crit.val.step2 = crit.val[i]
    
    if(messages) message(paste("Best FPC model: FPCs = ", nfpcs, " (", criterion, " = ", round(crit.val.step2, 1), ")", sep=""))
        
    #---------------------------------------------------------------------------
    # step 3: choose the best random effects covariance structure
    
    pdClasses = c("pdIdent","pdDiag","pdCompSymm")
    
    if(try.pdSymm) pdClasses = c(pdClasses, "pdSymm")
    
    if(messages){ cat("\n") ;  message(paste("Fitting", length(pdClasses) - 1, "alternative covariance strucures for the random effects...")) }
    
    crit.val = sapply(pdClasses[-1], function(pdClass){ # pdClass = "pdDiag"
        
        if(messages) message(paste("pdClass =", pdClass)) 
        
        fit = try({
            
            lme.wrapper(data, 
                        mean    = spline.pars,
                        nfpcs   = nfpcs, 
                        pdClass = pdClass)
            
        }, TRUE) ; fit
        
        if(inherits(fit, "try-error")) return(NA) else return(crit(fit))
        
    }) ; crit.val
    
    crit.val = c(crit.val.step2, crit.val) ; crit.val
    
    delta = crit.val - min(crit.val, na.rm = TRUE) ; delta
    
    i = which(delta < 2)[1] ; i
    
    pdClass = pdClasses[i] ; pdClass
    
    crit.val.step3 = crit.val[i] ; crit.val.step3
    
    if(pdClass == "pdIdent"){
        
        if(messages) message("No improvement over previous model (pdIdent)")
        
    }else{
        
        if(messages) message(paste("Best covariance structure for the random effects: ", pdClass, " (BIC = ", round(crit.val.step3, 1), ")", sep=""))
        
    }
    
    #---------------------------------------------------------------------------
    # step 4: choose the best residual covariance structure
    
    varStructs = list("NULL", "varExp(form = ~ x)")
    #     varStructs = list("NULL", "varExp(form = ~ x)", "varPower()")
    
    corStructs = list("NULL", "corAR1(form = ~ 1)", "corCompSymm(form = ~ x)")
    #     corStructs = list("NULL", "corAR1(form = ~ 1)", "corCAR1(form = ~ x)", "corCompSymm(form = ~ x)")
    
    covStructs = expand.grid(var = 1:length(varStructs), cor = 1:length(corStructs)) ; covStructs
    
    covStructs = lapply(1:nrow(covStructs), function(i){
        
        list(var = varStructs[[covStructs$var[i]]],
             cor = corStructs[[covStructs$cor[i]]])
        
    }) ; covStructs
    
    if(messages){ cat("\n") ; message(paste("Fitting", length(covStructs) , "candidate covariance strucures for the residuals...")) }
    
    fit = lapply(1:length(covStructs), function(i){ # i=1
        
        if(messages) message(paste("weights = ", covStructs[[i]]$var, ",", paste(rep(" ", 21 - nchar(covStructs[[i]]$var)), collapse = ""), "correlation = ", covStructs[[i]]$cor, sep="")) 
        
        try({
            
            lme.wrapper(data, 
                        mean        = spline.pars, 
                        nfpcs       = nfpcs, 
                        pdClass     = pdClass, 
                        weights     = eval(parse(text = covStructs[[i]]$var)), 
                        correlation = eval(parse(text = covStructs[[i]]$cor)))
            
        }, TRUE)
        
    }) ; fit[[1]]
    
    crit.val = sapply(fit, function(x){
        
        crit.val = try(crit(x), TRUE)
        
        if(inherits(crit.val, "try-error")) return(NA) else return(crit.val)
        
    }) ; crit.val
    
    delta = crit.val - min(crit.val, na.rm = TRUE) ; delta
    
    i = which(delta < 2) ; i
    
    if(length(i) > 1){
        
        npars = sapply(fit, function(x) attr(logLik(x), "df")) ; npars
        
        i = select.model(crit.val, npars)
        
    }
    
    fit = fit[[i]] ; fit
    
    crit.val.step4 = crit.val[i] ; crit.val.step4
    
    var = covStructs[[i]]$var
    
    cor = covStructs[[i]]$cor
    
    if(messages){
        
        if(i == 1){
            
            message("No improvement over previous model ('weights = NULL' and 'correlation = NULL')")
            
        }else{
            
            message(paste("Best covariance structure for the residuals is 'weights = ", var, "' and 'correlation = ", cor,"' (", criterion, " = ", round(crit.val.step4, 1), ")", sep=""))
            
        }
        
    }
    
    #---------------------------------------------------------------------------
    # step 5: cluster the random effects
        
    if(messages) { cat("\n") ; message("Running Mclust on the random effects from the preferred model...") }
    
    mc = cluster.ranefs(fit, ...) ; mc
    
    if(messages) message(paste("Number of predicted groups =", mc$G))
    
    #---------------------------------------------------------------------------
    # attributes
    
    fit$mclust = mc
    
    fit$data$fitted.group = as.factor(rep(as.numeric(mc$classification), times = as.numeric(table(fit$data$subject))))
    
    fit$spline.pars = spline.pars
    
    fit$nfpcs = nfpcs
    
    fit$pdClass = pdClass
    
    fit$weights = var
    
    fit$correlation = cor
    
    return(fit)    
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' A wrapper for the \code{lme}
#'
#' Allows B-spline models to be specified for the mean and random effects 
#' 
#' Details.
#'
#' @param argument description
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

lme.wrapper = function(data, mean = set.spline(npars = 5, degree = 3), block.mean = FALSE, block.ranefs = FALSE, pdClass = "pdIdent", weights = NULL, correlation = NULL, nfpcs = NULL, opt = "optim", ...){
    
    # make ungrouped X
    if(is.null(nfpcs)){
        
        X = make.X(data, spline.settings = mean)
        
    }else{
        
        temp = make.fpc.matrix(data, spline.settings = mean)
        
        X = temp[, 1:nfpcs, drop = FALSE]
        
        attr(X, "pca.fd") = attr(temp, "pca.fd") # plot(attr(X, "pca.fd"))
        
    } ; head(X) ; dim(X)
    
    # make ungrouped Z
    Z = X ; colnames(Z) = paste("Z", 1:ncol(Z), sep="") ; head(Z) ; dim(Z)
    
    # block the design matrices by group
    if(!is.null(data$group)){
        
        # make blocked X
        if(block.mean) X = make.blocked.X(X, data$group)
        
        # make blocked Z
        if(block.ranefs) Z = make.blocked.X(Z, data$group)
        
    } ; head(X) ; head(Z)
    
    # make groupedData object
    gdata = groupedData(y ~ x | subject, data = cbind(data, X, Z), order.groups = FALSE) ; head(gdata)
    
    #     all(as.numeric(gdata$subject) == as.numeric(data$subject))
    
    # make fixed effects formula    
    fixed = make.fixed.formula(X) ; fixed
    
    # make random effects formula
    random = make.random.formula(Z, pdClass) ; random
    
    # fit lme model
    fit = try({
        
        lme(fixed, gdata, random, correlation, weights, method = "ML", control = list(maxIter = 1000, msMaxIter = 1000, opt = opt, ...))
        
    }, TRUE)
    
    # attributes
    if(!inherits(fit, "try-error")){
        
        fit$X = X
        
        for(i in names(attributes(data))[!names(attributes(data)) %in% names(attributes(fit$data))]) attr(fit$data, i) = attr(data, i)
        
        fit$spline.pars = mean
        
        class(fit$data) = c(class(data)[!class(data) %in% class(fit$data)], class(fit$data))
        
        class(fit) = c("cluster.fit", class(fit))
        
    }
    
    return(fit)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Block a design matrix by group
#'
#' Description
#' 
#' Details.
#'
#' @param X a design matrix
#' @param group a grouping index
#' @return Value. 
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

make.blocked.X = function(X, group){
    
    group = as.factor(group)
    
    ngroups = length(unique(group)) ; ngroups
    
    X.names = colnames(X) ; X.names
    
    X = model.matrix(~ -1 + X:group)
    
    colnames(X) = as.vector(outer(paste(X.names), 
                                  paste("group", 1:ngroups, sep = ""),
                                  function(x,y) paste(x, y, sep="_"))) ; head(X)
    
    return(X)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Make fixed effects formula
#'
#' Description
#' 
#' Details.
#'
#' @param X a design matrix
#' @return Value. 
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

make.fixed.formula = function(X){
    
    form = paste("y ~ -1 ", paste("+", colnames(X), sep=" ", collapse=" "), sep="") ; form
    
    form = eval(parse(text = form)) ; form
    
    return(form)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Make random effects formula
#'
#' Description
#' 
#' Details.
#'
#' @param Z a design matrix
#' @return Value. 
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

make.random.formula = function(Z, pdClass = "pdIdent"){
    
    ngroups = if(all(grepl("group", colnames(Z)))){
        
        length(unique(sapply(strsplit(colnames(Z), "_"), function(x) x[grep("group", x)])))
        
    }else 1 ; ngroups
    
    nbasis = ncol(Z) / ngroups ; nbasis
    
    random = lapply(1:ngroups, function(grp){ # grp=1
        
        i = ((grp-1) * nbasis + 1):(grp * nbasis) ; i
        
        form = paste(pdClass,"(~ -1 + ", paste(colnames(Z)[i], collapse = " + "), ")", sep="") ; form
        
        eval(parse(text = form))
        
    })
    
    random = if(ngroups == 1) random[[1]] else pdBlocked(random)
    
    return(random)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Make a B-spline design matrix
#'
#' Description
#' 
#' Details.
#'
#' @param x values at which the B-spline is to be evaluated
#' @param df total number of basis function (including the intercept, if present)
#' @param degree degree of the B-spline
#' @param intercept if \code{TRUE}, an intercept basis function is included (default is \code{TRUE})
#' @param knots locations of the internal knots (defaults to even spacing)
#' @param range range of the x-axis over which the B-spline is to be defined (defaults to observed range of x)
#' @return Returns a matrix with an \code{fda} basis attribute 
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

make.X = function(data, spline.settings){
    
    ss = spline.settings
    
    range = if(is.null(ss$range)) range(data$x) else ss$range ; range
    
    breaks = if(is.null(ss$knots)) NULL else c(range[1], ss$knots, range[2]) ; breaks
    
    basis = create.bspline.basis(nbasis = ss$df, norder = ss$degree + 1, rangeval = range, breaks = breaks)
    
    X = eval.basis(data$x, basis)
    
    colnames(X) = paste("X", 1:ncol(X), sep = "") ; head(X) ; dim(X)
    
    attr(X, "basis") = basis
    
    return(X)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Make an FPC design matrix
#'
#' Make a design matrix composed of functional principal components from a design matrix of .
#' 
#' Details.
#'
#' @param data data frame containing columns \code{x}, \code{y} and \code{subject}
#' @param x values at which the B-spline is to be evaluated
#' @return Returns a matrix. 
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

make.fpc.matrix = function(data, spline.settings){
    
    # make a temporary data frame with no groups
    temp = data ; temp$group = NULL
    
    # make B-spline design matrix
    X = make.X(temp, spline.settings) ; head(X) ; dim(X)
    
    # make a groupedData object that includes the B-spline basis functions
    gdata = groupedData(y ~ x | subject, data = cbind(data, X), order.groups = FALSE) ; head(gdata)
    
    # make a formula using the B-spline basis functions
    form = formula(paste("y ~ -1 +", paste(colnames(X), collapse=" + "), "| subject")) ; form
    
    # use lmList to fit the B-spline to each subject separately and extract the coeficients
    lmList.coefs = coef(lmList(form, gdata)) ; head(lmList.coefs)
    
    # put the lmList coeficients in the original subject order
    i = match(as.character(unique(gdata$subject)), rownames(lmList.coefs))
    lmList.coefs = lmList.coefs[i,] ; head(lmList.coefs)
    
    # check that the rownames of the matrix of coeficients matches the subject names in the data
    if(!all(rownames(lmList.coefs) == as.character(unique(gdata$subject)))) 
        stop("rownames of lmList coefficients doesn't match the order of subjects in the data.")
    
    # make a functional data object using the lmList coeficients and the original basis
    functional.data.object = fd(coef = t(lmList.coefs), basisobj = attr(X, "basis"))
    
    # make a functional principal components object from the functional data object
    pca.functional.data.object = pca.fd(fdobj = functional.data.object, nharm = spline.settings$df, centerfns = FALSE) # 
    
    # construct a matrix of FPCs by multiplying the original design matrix by the scores for each FPC
    X = sapply(1:spline.settings$df, function(comp) X %*% pca.functional.data.object$harmonics$coefs[,comp])
    
    colnames(X) = paste("X", 1:ncol(X), sep = "") ; head(X) ; dim(X)
    
    attr(X, "pca.fd") = pca.functional.data.object
    
    return(X)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Calculate mean squared prediction error
#'
#' Calculate mean squared prediction error for a \code{cluster.fit} object
#' 
#' Details.
#'
#' @param fit description
#' @param level description
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' @export

mse = function(fit, level = 1){
    
    if(!inherits(fit$data, "lmeSplines.data")) 
        stop("only works if the model has been fitted to lmeSplines.data")
    
    fixed = fixef(fit) ; fixed
    
    ranefs = ranef(fit) ; head(ranefs)
    
    ranefs = ranefs[match(unique(fit$data$subject), rownames(ranefs)), , drop = FALSE] ; head(ranefs)
    
    pred.errors = sapply(1:attr(fit$data, "nsubs"), function(sub){ # sub=1
        
        obs = which(fit$data$subject == unique(fit$data$subject)[sub]) ; obs
        
        beta = as.numeric(fixed + ranefs[sub,]) ; beta
        
        truth = fit$data[obs, paste("Ey", level, sep="")] ; truth
        
        estimate = fit$X[obs,,drop = FALSE] %*% beta ; estimate
        
        return(estimate - truth)
        
    }) ; pred.errors
    
    mse = mean(pred.errors^2) ; mse
    
    return(mse)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' @export

plot.classification = function(fit, dimens = 1:ncol(fit$X), addEllipses = TRUE, col = c("truth","fitted"), labels = TRUE, ...){
    
    col = match.arg(col)
    
    mc = fit$mclust
    
    if(!inherits(mc, "Mclust")) stop("mc not of class \"Mclust\"")
        
    p = length(dimens) ; p

    if(p <= 2) stop("only works for 3 or more dimensions")

    cols = switch(col,
                  "fitted" = mc$classification,
                  "truth" = if(is.null(fit$data$group)){
                      
                      "grey"
                      
                  }else{
                      
                      ggplotCols(length(unique(fit$data$group)))[fit$data$group[!duplicated(fit$data$subject)]]
                      
                  }
    ) ; cols      
    
    ranefs = as.matrix(ranef(fit))[, dimens, drop = FALSE]
    
    oldpar = par(no.readonly = TRUE)
    
    on.exit(par(oldpar))
    
    par(mfrow = c(p, p), mar = rep(c(0.3, 0.3/2), each = 2), oma = c(4, 4, 4, 4))
    
    for(row in seq(p)){ # row = 1
        
        for(col in seq(p)){ # col = 2
            
            if(row == col){ # diagonal panels
                
                plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
                
                text(0, 0, colnames(ranefs)[row], cex = 1.5, adj = 0.5)
                
                box()
                
            }else{
                
                plot(ranefs[,col], ranefs[,row], col = cols, xaxt = "n", yaxt = "n")
                
                if(addEllipses){    
                    
                    for(k in 1:mc$G){ # k = 1
                        
                        label = if(labels) as.character(k) else NULL
                        
                        add.ellipse(mu      = mc$parameters$mean[c(col,row), k],                                    
                                    sigma   = mc$parameters$variance$sigma[c(col,row),c(col,row), k],
                                    label   = label, ...)
                        
#                         mclust:::mvn2plot(mu =  mc$parameters$mean[c(col,row), k], 
#                                           sigma = mc$parameters$variance$sigma[c(col,row),c(col,row), k],
#                                           alone = FALSE)
                        
                    }
                    
                }
                
            }
            
            if(row == 1 && (!(col %% 2))) axis(3)
            if(row == p &&   (col %% 2))  axis(1)
            if(col == 1 && (!(row %% 2))) axis(2)
            if(col == p &&   (row %% 2))  axis(4)
            
        }
        
    }
    
    invisible()
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' @method plot cluster.fit
#' @export

plot.cluster.fit = function(fit, raw = "none", levels = 0:1, cutoff = NULL, ...){
    
    if(inherits(fit, "try-error")) stop("can't plot it - the model didn't converge")
    
    fitted = fitted(fit, level = c(0,1)) ; head(fitted)
            
    temp = fit$data ; head(temp) ; dim(temp)
    
    if(is.null(temp$fitted.group)) temp$fitted.group = temp$group
    
    if(is.null(temp$group)) temp$group = temp$fitted.group
    
    temp$fitted.y0 = fitted[,"fixed"]
    
    temp$fitted.y1 = fitted[,"subject"]
    
    if(!is.null(cutoff)){
        
        if(cutoff > 1 || cutoff <= 0) stop("cutoff must be greater than 0 and less than or equal to 1")
        
        uncertainty = as.numeric(rep(fit$mclust$uncertainty, times = table(fit$data$subject))) ; uncertainty
        
        temp = temp[uncertainty <= cutoff,]
        
    }
    
    if(!inherits(temp, "lmeSplines.data")) class(temp) = c("lmeSplines.data", class(temp)) 
    
    head(temp) ; dim(temp) ; class(temp)
    
    myplot = plot(temp, raw = raw, fitted = TRUE, levels = levels, ...)
    
    return(myplot)    
    
}    

################################################################################
################################################################################
################################################################################
################################################################################

#' @export

plot.fpcs = function (fit, harm = 1:ncol(fit$X), nx = 50, ylim = NULL, col = c(2,4), lty = NULL, lwd = 1, use.symbols = TRUE, xlab = "x"){
    
    if(!(inherits(fit, "cluster.fit"))) stop("Argument 'fit' is not a cluster.fit object.")
    
    pcafd = attr(fit$X, "pca.fd")
    
    harmfd = pcafd[[1]]
    
    basisfd = harmfd$basis
    
    rangex = basisfd$rangeval
    
    argvals = seq(rangex[1], rangex[2], length = nx)
    
    fdmat = eval.fd(argvals, harmfd)
    
    meanmat = eval.fd(argvals, pcafd$meanfd)
    
    #     dimfd = dim(fdmat) ; dimfd
    
    #     nharm = dimfd[2] ; nharm
    
    for(jharm in 1:length(harm)){ # jharm = 1
        
        iharm = harm[jharm] ; iharm
        
        fac = sqrt(pcafd$values[iharm]) ; fac
        
        vecharm = fdmat[, iharm]
        
        pcmat = cbind(meanmat + fac * vecharm, meanmat - fac * vecharm)
        
        percentvar = round(100 * pcafd$varprop[iharm], 1) ; percentvar
        
        plot(argvals, meanmat, type = "l", ylim = ylim, xlab = xlab, ylab = "", main = paste("FPC ", iharm, " (", percentvar, " %)", sep = ""), lwd = lwd, col = "grey")
        
        for(i in 1:2){
            
            lines(argvals, pcmat[, i], col = col[i], lwd = lwd)
            
            if(use.symbols){
                
                pos = seq(1, nx, length = 10)
                
                points(argvals[pos], pcmat[pos, i], col = "white", pch = 19, cex = 1.5)                
                
                points(argvals[pos], pcmat[pos, i], col = col[i], pch = c("+","-")[i])               
                
            }
            
        }
                
    }
    
    invisible(NULL)
}

################################################################################
################################################################################
################################################################################
################################################################################

#' @export

plot.success = function(success, which = 1:3, labels = TRUE, cols = c(2,3)){
    
    n = nrow(attr(success, "Success")) ; n
    
    j = 1:n ; j
    
    for(i in c("Truth", "Predicted", "Success")[which]){ # i = "Truth"
        
        image(j, j, t(attr(success, i)[rev(j),]) * 1, col = cols, zlim = c(0,1), xlab = "", ylab = "", axes = FALSE)
        
        if(i == "Success") title(paste(i, " (", round(100 * as.numeric(success),2), "%)", sep="")) else title(i)
        
        if(labels){
            
            vals = c(n, rev(j[-1]))-0.5 ; vals
            
            segments(x0 = j-0.5, y0 = 0 + 0.5, x1 = j-0.5, y1 = vals)
            
            segments(y0 = j-0.5, y1 = j-0.5, x0 = 0, x1 = vals)
            
            axis(1, j[-n], labels = as.character(j[-n]), tick = FALSE)
            
            axis(2, j[-n], labels = as.character(rev(j[-1])), tick = FALSE, las = 1)
            
        }
        
    }
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' @export

print.success = function(success){
    
    print(as.numeric(success))
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' @method ranef cluster.fit
#' @export

ranef.cluster.fit = function(fit, ...){
    
    class(fit) = "lme"
    
    ranefs = ranef(fit)
    
    ranefs = ranefs[match(unique(fit$data$subject), rownames(ranefs)), , drop = FALSE] ; head(ranefs)
    
    return(ranefs)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' @export

select.model = function(criterion, npars){
    
    delta = criterion - min(criterion, na.rm = TRUE) ; delta
    
    within = which(delta < 2) ; within
    
    best = if(length(within) == 1){
        
        within
        
    }else{        
        
        minpars = which(npars[within] == min(npars[within], na.rm = TRUE)) ; minpars
        
        if(length(minpars) == 1){
            
            within[minpars]
            
        }else{
            
            minscore = which(criterion[within][minpars] == min(criterion[within][minpars], na.rm = TRUE)) ; minscore
            
            within[minpars][minscore]
            
        }
        
    } ; best
    
    return(best)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' Simulate data using model from Luan & Li (2003)
#'
#' Description.
#' 
#' Details.
#'
#' @param nsubs.per.group number of subjects in each of the 4 groups (default is 200, the number used by Luan & Li)
#' @param T a parameter of unspecified value used by Luan & Li (2003) which controls the frequency of the underlying functions - the smaller the number the higher the frequency (the default is 0.5)
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples:
#' 
#' data = sim.luanli.data(10)
#' 
#' head(data)
#' 
#' plot(data)
#' 
#' plot(data, levels = c(0,1), raw = FALSE)
#' 
#' @export

sim.luanli.data = function(nsubs.per.group = 200, T = 0.5){
    
    #---------------------------------------------------------------------------
    # sample size settings 
    
    ngroups = 4
    
    nobs.per.sub = 18
    
    nsubs = ngroups * nsubs.per.group ; nsubs
    
    #---------------------------------------------------------------------------
    # fixed effects parameters
    
    beta = matrix(c(0,  0.5,   0.87,
                    0, -0.81,  0.59,
                    0, -0.81, -0.59,
                    0,  0.31, -0.95), nr = 4, byrow = TRUE) ; beta
    
    #---------------------------------------------------------------------------
    # random effects parameters
    
    sigma = sqrt(0.09)
    
    R = sigma^2 * diag(nobs.per.sub) ; R
    
    Gvar = diag(c(0.065, 0.073, 0.17)) ; Gvar
    
    Gcor = matrix(c(1, 0.7, -0.17, 0.7, 1, -0.62, -0.17, -0.62, 1), 3) ; Gcor
    
    G = sqrt(Gvar) %*% Gcor %*% sqrt(Gvar) ; G
    
    #---------------------------------------------------------------------------
    # data
    
    t = seq(0, 1, length = nobs.per.sub) ; t
    
    data = do.call(rbind, lapply(1:ngroups, function(grp){ # grp=1
        
        do.call(rbind, lapply(1:nsubs.per.group, function(sub){ # sub=1
            
            gamma = as.numeric(rmvnorm(1, sigma = G)) ; gamma
            
            errors = as.numeric(rmvnorm(1, sigma = R)) ; errors
            
            Ey0 = beta[grp,1] + beta[grp,2] * cos(2 * pi * t / T) + beta[grp,3] * sin(2 * pi * t / T)
            
            Ey1 = beta[grp,1] + gamma[1] + 
                
                (beta[grp,2] + gamma[2]) * cos(2 * pi * t / T) +
                
                (beta[grp,3] + gamma[3]) * sin(2 * pi * t / T) 
            
            y = Ey1 + errors
            
            return(data.frame(y = y, x = t, subject = paste(grp, sub, sep="."), group = grp, Ey1 = Ey1, Ey0 = Ey0))
            
        }))
        
    })) ; head(data) ; dim(data)
    
    for(i in c("subject", "group")) data[[i]] = as.factor(data[[i]])
        
    #---------------------------------------------------------------------------
    # attributes
    
    attr(data, "nobs.per.sub") = nobs.per.sub
    
    attr(data, "nsubs.per.group") = nsubs.per.group
    
    attr(data, "ngroups") = ngroups
    
    attr(data, "nsubs") = nsubs
    
    class(data) = c("lmeSplines.data", class(data))
    
    return(data)
    
}

################################################################################
################################################################################
################################################################################
################################################################################
