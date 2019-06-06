\dontrun{

"message" %>% heading()
"message" %>% bullet()
"message" %>% item()
"message" %>% success()
"message" %>% concern()

item("item", col = "blue")
item("item", col = "black")
item("item", col = "blue")
item("item", col = "cyan")
item("item", col = "green")
item("item", col = "magenta")
item("item", col = "red")
item("item", col = "white")
item("item", col = "yellow")
item("item", col = "grey")
item("item", col = "silver")

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