

f = function(message = "checking something"){
    msg_1 = paste("- ", message, "...")
    nc_1 = nchar(msg_1)
    cat(msg_1, "\r")

    start_time = Sys.time()
    Sys.sleep(2)
    finish_time = Sys.time()
    dt = prettyunits::pretty_dt(difftime(finish_time, start_time))
    dt = str_c("(", dt, ")")

    msg_2 = message
    nc_2 = nchar(msg_2) + nchar(dt) + 3
    msg_2 = cli::col_silver(msg_2)
    msg_2 = paste(msg_2, cli::col_cyan(dt))
    msg_2 = stringr::str_pad(msg_2, width = max(nc_1, nc_2), side = "right", pad = " ")
    cli::cat_bullet(msg_2, bullet = "tick", bullet_col = "green")
}

f()


my_function = function(secs){
    Sys.sleep(secs)
    "hello"
}

timer = function(.f, message = "checking something"){
    function(...){
        msg_1 = paste("- ", message, "...")
        nc_1 = msg_1 %>% str_length
        cat(msg_1, "\r")
        start_time = Sys.time()
        result = .f(...)
        finish_time = Sys.time()
        dt = finish_time %>%
            difftime(start_time) %>%
            prettyunits::pretty_dt() %>%
            str_c("(", ., ")")
        msg_2 = message
        nc_2 = nchar(msg_2) + nchar(dt) + 3
        msg_2 = cli::col_silver(msg_2)
        msg_2 = paste0(" ", msg_2, " ", cli::col_cyan(dt))
        msg_2 = stringr::str_pad(msg_2, width = max(nc_1, nc_2), side = "right", pad = " ")
        cli::cat_bullet(msg_2, bullet = "tick", bullet_col = "green")
        result
    }
}

g = function(){
    x = timer(my_function, "asdfasdf")(3)
    y = timer(my_function, "1212121212")(1)
    list(x, y)
}

g()


