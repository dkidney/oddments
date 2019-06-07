\dontrun{

"message" %>% heading()
"message" %>% bullet()
"message" %>% item()
"message" %>% success()
"message" %>% concern()

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