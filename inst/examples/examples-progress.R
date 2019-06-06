\dontrun{

heading("heading")
bullet("bullet")
item("item")
success("success")
concern("concern")

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
    message("hello again")
    print(10)
    return(100)
}
f2 <- function() {
    message("hello")
    Sys.sleep(2)
    warning("whoops")
    warning("oh no, not again...")
    print(10)
    return(100)
}
f3 <- function(timer = FALSE){
    force(timer)
    itemize(f1, .message = "checking something", .timer = timer)()
    itemize(f2, .message = "checking something else", .timer = timer)()
}
f3()
f3(FALSE)

}