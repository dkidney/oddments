#################################################################################################################
#################################################################################################################

ddf.summary <- function(ddf.output, covered.area.sq.km=NULL, study.area.sq.km=NULL){

    ddf.summary <- summary(ddf.output)
    
    a <- covered.area.sq.km
    A <- study.area.sq.km
    
    my.summary <- rbind(

        cluster.abundance.covered   = mini.summary(est = ddf.summary$Nhat, se = ddf.summary$Nhat.se),
        cluster.abundance.study     = mini.summary(est = ddf.summary$Nhat * A/a, se = ddf.summary$Nhat.se * A/a),
        cluster.density.per.sq.km   = mini.summary(est = ddf.summary$Nhat / a, se = ddf.summary$Nhat.se / a),
#        key.scale                   = mini.summary(est = ddf.summary$coeff$key.scale$estimate, se = ddf.summary$coeff$key.scale$se, key = TRUE),
#        key.shape                   = mini.summary(est = ddf.summary$coeff$key.shape$estimate, se = ddf.summary$coeff$key.shape$se, key = TRUE),
        proportion.detected         = mini.summary(est = ddf.summary$average.p , se = ddf.summary$average.p.se)

    )

    return(my.summary)

}

#################################################################################################################
#################################################################################################################