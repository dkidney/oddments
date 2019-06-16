#################################################################################################################
#################################################################################################################

dht.summary <- function(dht.output){
    
    A <- as.numeric(dht.output$individuals$summary$Area)

    my.summary <- rbind(

        individual.density.per.sq.km    = mini.summary(est = as.numeric(dht.output$individuals$N$Estimate) / A, se = as.numeric(dht.output$individuals$N$se) / A),
        cluster.density.per.sq.km       = mini.summary(est = as.numeric(dht.output$clusters$N$Estimate) / A, se = as.numeric(dht.output$clusters$N$se) / A),
        mean.cluster.size               = mini.summary(est = as.numeric(dht.output$individuals$summary$mean.size), se = as.numeric(dht.output$individuals$summary$se.mean)),
        encounter.rate.per.km           = mini.summary(est = as.numeric(dht.output$individuals$summary$ER) * 1000, se = as.numeric(dht.output$individuals$summary$se.ER) * 1000)

    ) ; my.summary

    return(my.summary)

}

#################################################################################################################
#################################################################################################################