################################################################################
################################################################################

plot.segments <- function(segment.table, add=FALSE, numbers=TRUE){
     
    xlim = range(segment.table[,c("corner1.x","corner2.x","corner3.x","corner4.x")])
    ylim = range(segment.table[,c("corner1.y","corner2.y","corner3.y","corner4.y")])
    
    if(!add) plot(1,1, type="n", xlim=xlim, ylim=ylim, asp=1, ylab="x (m)", xlab="y (m)")
    
    transect.labels <- unique(segment.table$transect.label) ; transect.labels
    
    for(tran in 1:length(transect.labels)){ # tran = 1
        
        segments <- which(segment.table$transect.label == transect.labels[tran]) ; segments
        
        for(seg in segments){ # seg = 1
            
            with(segment.table[seg,], {
                
                polygon(x = c(corner1.x, corner2.x, corner3.x, corner4.x),
                        y = c(corner1.y, corner2.y, corner3.y, corner4.y),
                        border = segment.number)
                
                if(numbers) text(centre.x, centre.y, labels = as.character(segment.number), col = segment.number)
                
            })
            
        }
    }
}

################################################################################
################################################################################

