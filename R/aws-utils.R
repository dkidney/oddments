#' @rdname aws-utils
#' @name aws-utils
#' @title AWS utility functions
#' @description
#' \itemize{
#'     \item \code{check_aws_cli()} - checks that the installation path for \code{awscli}
#'     exists (as returned by \code{Sys.which("aws")})
#'     \item \code{check_aws_creds()} - checks that the relevant enviroemnets variables
#'     for connecting to AWS are set
#' }
NULL

#' @rdname aws-utils
#' @name check_aws_cli
#' @export
check_aws_cli <- function() {
    path <- Sys.which("aws")
    ok <- is.character(path) && length(path) == 1 && file.exists(path)
    if (ok) {
        message("which aws:\n\t", path)
    } else {
        stop(
            "can't find aws command line tools - try installing via brew:",
            "\n\tbrew install awscli"
        )
    }
    invisible()
}

#' @rdname aws-utils
#' @name check_aws_creds
#' @export
check_aws_creds <- function() {
    envvars <- Sys.getenv(c(
        "AWS_USERNAME",
        "AWS_ACCESS_KEY_ID",
        "AWS_SECRET_ACCESS_KEY"
    ))
    ok <- envvars != ""
    if (any(!ok)) {
        stop(
            "some aws environment variables not set:\n\t",
            paste(names(envvars)[!ok], collapse = "\n\t")
        )
    }
}
