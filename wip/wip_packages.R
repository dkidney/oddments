
#' @rdname packages
#' @name pkg_detach
#' @export
pkg_detach = function(pkgs = NULL){
    if(is.null(pkgs)){
        pkgs = names(sessionInfo()$otherPkgs)
    }
    for(pkg in pkgs){
        message("detaching ",  pkg)
        txt = paste0('detach("package:', pkg, '")')
        eval(parse(text = txt))
    }
}



