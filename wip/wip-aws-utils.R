
check_awscli = function(x){
    path = suppressWarnings(system2("which", "aws", stdout = TRUE, stderr = TRUE))
    ok = is.character(path) && length(path) == 1 && file.exists(path)
    if(!ok){
        message(
            "can't find aws command line tools - try installing via brew:",
            "\n\tbrew install awscli"
        )
    }
    invisible()
}