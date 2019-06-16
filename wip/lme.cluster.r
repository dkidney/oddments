

lme.cluster = function(data, max.df = 16, messages = FALSE, ...){
    
    if(messages) message("Using BIC for model selection")

    #---------------------------------------------------------------------------
    # step 1: fit a series of cubic B-splines with varying degrees of freedom
    
    if(messages){ cat("\n") ;  message(paste("Fitting", max.df - 3, "cubic B-spline models...")) }
    
    range.df = 4:max.df
    
    bic = sapply(range.df, function(df){ # df=4
        
        if(messages) message(paste("df =", df))
        
        fit = try({
            
            lme.wrapper(data, 
                        mean = set.spline(npars = df, degree = 3))
            
        }, TRUE) ; fit
        
        if(inherits(fit, "try-error")) return(NA) else return(BIC(fit))
        
    }) ; bic
    
    delta = bic - min(bic, na.rm = TRUE) ; bic
    
    i = which(delta < 2)[1] ; i
    
    df = range.df[i] ; df
    
    bic.step1 = bic[i] ; bic.step1
    
    if(messages) message(paste("Best B-spline model: df = ", df, " (BIC = ", round(bic.step1,1), ")", sep=""))
    
    #---------------------------------------------------------------------------
    # step 2: try and reduce the dimension of the best step 1 model using FPCs
    
    if(messages){ cat("\n") ;  message(paste("Fitting", df - 1, "FPC models...")) }
    
    range.nfpcs = 1:df
    
    bic = sapply(range.nfpcs[-df], function(nfpcs){
        
        if(messages) message(paste("FPCs =", nfpcs))
        
        fit = try({
            
            lme.wrapper(data, 
                        mean    = set.spline(npars = df, degree = 3), 
                        nfpcs   = nfpcs)
            
        }, TRUE)
        
        if(inherits(fit, "try-error")) return(NA) else return(BIC(fit))
                
    })
    
    bic = c(bic, bic.step1) ; bic
    
    delta = bic - min(bic, na.rm = TRUE) ; delta
    
    i = which(delta < 2)[1] ; i
    
    nfpcs = range.nfpcs[i] ; nfpcs
    
    if(nfpcs == df){
        
        nfpcs = NULL
        
        bic.step2 = bic.step1
        
        if(messages) message("No improvement over best B-spline model")
        
    }else{
        
        bic.step2 = bic[i]
        
        if(messages) message(paste("Best FPC model: FPCs = ", nfpcs, " (BIC = ", round(bic.step2, 1), ")", sep=""))
        
    }
    
    
    #---------------------------------------------------------------------------
    # step 3: choose the best random effects covariance structure
    
    pdClasses = c("pdIdent","pdDiag","pdCompSymm","pdSymm")
    
    if(messages){ cat("\n") ;  message(paste("Fitting", length(pdClasses) - 1, "alternative covariance strucures for the random effects...")) }
    
    bic = sapply(pdClasses[-1], function(pdClass){ # pdClass = "pdDiag"
        
        if(messages) message(paste("pdClass =", pdClass)) 
        
        fit = try({
            
            lme.wrapper(data, 
                        mean    = set.spline(npars = df, degree = 3),
                        nfpcs   = nfpcs, 
                        pdClass = pdClass)
            
        }, TRUE) ; fit
        
        if(inherits(fit, "try-error")) return(NA) else return(BIC(fit))
        
    }) ; bic
    
    bic = c(bic.step2, bic) ; bic
    
    delta = bic - min(bic, na.rm = TRUE) ; delta
    
    i = which(delta < 2)[1] ; i
    
    pdClass = pdClasses[i] ; pdClass
    
    bic.step3 = bic[i] ; bic.step3
    
    if(pdClass == "pdIdent"){
        
        if(messages) message("No improvement over previous model (pdIdent)")
        
    }else{
        
        if(messages) message(paste("Best covariance structure for the random effects: ", pdClass, " (BIC = ", round(bic.step3, 1), ")", sep=""))
        
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
                        mean        = set.spline(npars = df, degree = 3), 
                        nfpcs       = nfpcs, 
                        pdClass     = pdClass, 
                        weights     = eval(parse(text = covStructs[[i]]$var)), 
                        correlation = eval(parse(text = covStructs[[i]]$cor)))
            
        }, TRUE)
                
    }) ; fit[[1]]
    
    bic = sapply(fit, function(x){
        
        bic = try(BIC(x), TRUE)
        
        if(inherits(bic, "try-error")) return(NA) else return(bic)
        
    }) ; bic
    
    delta = bic - min(bic, na.rm = TRUE) ; delta
    
    # need to take account of npars here...
    
    i = which(delta < 2) ; i
    
    if(length(i) > 1){
    
        npars = sapply(fit, function(x) attr(logLik(x), "df")) ; npars
        
        i = select.model(bic, npars)
        
    }
    
    fit = fit[[i]] ; fit
    
    bic.step4 = bic[i] ; bic.step4
    
    if(messages){
        
        if(i == 1){
            
            message("No improvement over previous model ('weights = NULL' and 'correlation = NULL')")
            
        }else{
            
            message(paste("Best covariance structure for the residuals is 'weights = ", covStructs[[i]]$cor, "' and 'correlation = ", covStructs[[i]]$cor,"' (BIC = ", round(bic.step4, 1), ")", sep=""))
            
        }
        
    }
    
    #---------------------------------------------------------------------------
    # step 5: cluster the random effects
    
    mc = cluster.ranefs(fit, ...) ; mc
    
    #---------------------------------------------------------------------------
    # attributes
    
    fit$mclust = mc
    
    return(fit)    
    
}

if(0){
    
    require(clustering)
    require(lmeSplines)
    require(plotting)
    require(grid)
    
    data = sim.luanli.data() ; head(data)
    
    plot(data) + mytheme() + theme(panel.margin = unit(0, "cm"))
    
    start = Sys.time()
    
    fit = lme.cluster(data, max.df = 12, messages = TRUE)
    
    print(Sys.time() - start)
    
    plot(fit) + mytheme() + theme(panel.margin = unit(0, "cm"))
    
}

