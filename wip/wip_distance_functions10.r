#################################################################################################################
#################################################################################################################

plot.transects <- function(transect.table, add=FALSE){

    xlim = range(transect.table[,c("start.utm.x","stop.utm.x")])
    ylim = range(transect.table[,c("start.utm.y","stop.utm.y")])

    if(!add) plot(1,1, type="n", xlim=xlim, ylim=ylim, asp=1, ylab="x (m)", xlab="y (m)")

    for(i in 1:nrow(transect.table)) lines(c(transect.table$start.utm.x[i], transect.table$stop.utm.x[i]), 
                                           c(transect.table$start.utm.y[i], transect.table$stop.utm.y[i]), col=i)

}

#################################################################################################################
#################################################################################################################