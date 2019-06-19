
has_internet <- function() {
  content <- try(suppressWarnings({
    readLines("http://www.r-project.org", n = 1)
  }), TRUE)
  !inherits(content, "try-error")
}
