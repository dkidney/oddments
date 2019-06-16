


search_dir = function(pattern, path, extension = ".r", show.path = FALSE){
    # get all files with the given extension in path
    files = list.files(path, pattern = paste0("\\.", gsub("\\.", "", extension), "$"), all.files = TRUE, full.names = TRUE, recursive = TRUE, ignore.case = TRUE, include.dirs = FALSE)
    # search each file for the pattern
    # make a list giving row numbers containing the pattern for each file
    files = sapply(files, function(file){ # file = files[1]
        lines = which(grepl(pattern, readLines(file, warn = FALSE)))
        if(length(lines) > 0) lines else NULL
    }, simplify = FALSE)
    files = files[!sapply(files, is.null)]
    # modify output
    if(length(files) > 0){
        if(!show.path)
            names(files) = gsub(paste0(path.expand(path), "/"), "", names(files))
        for(i in 1:length(files)) # i=1
            cat(names(files)[i], "\n- lines: ", paste(files[[i]], collapse = ", "), "\n", sep = "")
        # }else{
            # names(files) = basename(names(files))
        # }
    }else{
        cat("pattern not found\n")
        files = NULL
    }
    invisible(files)
}

if(0){

    search_dir("trapcov <- function", "~/dropbox/resources/r/package source code/secr package source code/r")
    search_dir(".rasterImagePlot", "~/dropbox/resources/r/package source code/raster/r")
    search_dir("tkconfigure", "~/dropbox/resources/r/package source code/Rcmdr")
    search_dir("_data", "~/dropbox/packages/gibbonsecr/gibbonsecr_1.0/gibbonsecr/R")
    search_dir("calc.var", "~/dropbox/packages/gibbonsSECR", show.path = TRUE)
    search_dir("addScrollbars", "~/dropbox/resources/r/examples/tcltk/ProgGUIinR examples")
    search_dir("JAVA_HOME", "/usr/local/Cellar/hadoop/2.7.1", extension = ".sh")
    search_dir("platt_scale", "/Volumes/FLASH 64GB/VisualDNA/risk", extension = ".R")
    search_dir("sigmoid", "/Volumes/FLASH 64GB/VisualDNA/risk", extension = ".R")
    search_dir("FAQ", "~/dropbox/resources/r/package source code/mgcv/r", extension = ".R")

}
