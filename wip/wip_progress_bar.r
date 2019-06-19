
track_loop = function(loop, nloops, w = 50){
    prop = loop / nloops ; prop
    ndashes = floor(w * prop) ; ndashes
    dashes = rep("-", ndashes) ; dashes
    spaces = rep(" ", w - ndashes) ; spaces
    text = paste0(c("|", dashes, spaces, "| ", round(100 * prop), " %"),
                  collapse = "") ; text
    if(loop > 1) text = paste0("\r", text)
    cat(text)
    if(loop == nloops) cat("\n")
}

if(0){
    nloops = 250
    for(i in 1:nloops){
        Sys.sleep(0.01)
        track_loop(i, nloops)
    }
    for(i in 1:nloops){
        Sys.sleep(0.01)
        track_loop(i, nloops)
    }
}
