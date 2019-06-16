
.onAttach = function(library, pkgname){

    linfo = library(help = DMP)$info[[1]]
    
    temp = linfo[grepl("Version", linfo)] ; temp

    version = gsub("[[:alpha:]]|[ ]|[:]", "", temp) ; version

    hello = paste0("DMP version ", version)

    packageStartupMessage(hello)

}
