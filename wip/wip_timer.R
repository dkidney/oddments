

f = function(secs = 2){
    message1 = "-  checking something ..."
    nchar1 = nchar(message1)
    cat(message1, "\r")
    
    Sys.sleep(secs)
    
    message2 = " finished :)"
    message2 = cli::col_blue(message2)
    message2 = paste(message2, cli::col_cyan("(time)"))
    nchar2 = nchar(message2)
    message2 = stringr::str_pad(message2, width = max(nchar1, nchar2), side = "right", pad = " ")
    cli::cat_bullet(message2, bullet = "tick", bullet_col = "green")
}

f()

