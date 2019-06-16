\dontrun{

heading("a", " message")
bullet("a", " message")
item("a", " message")
success("a", " message")
concern("a", " message")
panic("a", " message")

# itemize -----

f1 <- function() {
    Sys.sleep(2)
    message("hello")
    message("how are you")
    print(10)
    return(100)
}
f2 <- function() {
    message("hi")
    Sys.sleep(0.001)
    warning("whoops")
    warning("not again...")
    stop("this is bad!")
    print(10)
    return(100)
}
f3 <- function(timer = FALSE) {
    force(timer)
    itemize(f1, .message = "checking something", .timer = timer)()
    itemize(f2, .message = "checking something else", .timer = timer)()
}
f3()
f3(TRUE)
}