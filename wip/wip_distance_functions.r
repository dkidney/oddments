################################################################################
################################################################################

assign.segments <- function(segment.table, obs.table){
    
    new.obs.table <- obs.table ; head(new.obs.table) ; dim(new.obs.table)
    
    new.obs.table$transect.label <- new.obs.table$Sample.Label ; head(new.obs.table) ; dim(new.obs.table)
    
    new.obs.table$segment.number <- new.obs.table$Sample.Label <- NA ; head(new.obs.table) ; dim(new.obs.table)
    
    obs.coords <- as.matrix(new.obs.table[,c("utm.x","utm.y")]) ; head(obs.coords) ; dim(obs.coords)
    
    unique.transects <- unique(as.character(new.obs.table$transect.label)) ; head(unique.transects) ; length(unique.transects)
    
    for(trans in unique.transects){ # trans = unique(as.character(new.obs.table$transect.label))[1] ; trans
        
        segs.in.trans <- which(segment.table$transect.label==trans) ; segs.in.trans
        obs.in.trans  <- which(new.obs.table$transect.label==trans) ; obs.in.trans
        
        if(length(obs.in.trans) > 0){
            
            for(seg in 1:length(segs.in.trans)){ # seg = 2
                
                poly.matrix <- with(segment.table[segs.in.trans,][seg,], {
                    cbind(x = c(corner1.x, corner2.x, corner3.x, corner4.x),
                          y = c(corner1.y, corner2.y, corner3.y, corner4.y))
                }) ; poly.matrix
                
                obs.in.seg <- which(pinpoly(poly.matrix, obs.coords[obs.in.trans,]) != 0) ; obs.in.seg
                
                if(length(obs.in.seg) > 0){
                    
                    new.obs.table$Sample.Label[obs.in.trans][obs.in.seg] <- as.character(segment.table$Sample.Label[segs.in.trans][seg])
                    new.obs.table$segment.number[obs.in.trans][obs.in.seg] <- segment.table$segment.number[segs.in.trans][seg]
                
                }
            }
            
        }
        
    }
    
    return(new.obs.table)
}

################################################################################

# Testing 

if(0){
    
    transect.table <- data.frame(Region.Label = 1,
                                 Sample.Label = 1,
                                 Effort = sqrt(2),
                                 start.utm.x = 0,
                                 start.utm.y = 0,
                                 stop.utm.x = 1,
                                 stop.utm.y = 1
    )
    
    segment.table <- segment.transects(transect.table, w=0.1) ; segment.table
    
    plot.segments(segment.table)
    
    n <- 100
    obs.table <- data.frame(Region.Label = 1,
                            Sample.Label = 1,
                            object = 1:n,
                            observer = 1,
                            detected = 1,
                            size = 1,
                            utm.x = runif(n),
                            utm.y = runif(n)
    )
    
    points(obs.table$utm.x, obs.table$utm.y)
    
    obs.table <- assign.segments(segment.table, obs.table) ; head(obs.table)
    
    points(obs.table$utm.x, obs.table$utm.y, pch=19, col=obs.table$segment.number)
    
    
}

################################################################################
################################################################################