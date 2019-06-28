
get_query = function(query, con, verbose = TRUE){
    if(verbose) writeLines(query)
    try(dplyr::as_tibble(DBI::dbGetQuery(con, query)))
}

send_query = function(query, con, verbose = TRUE){
    if(verbose) writeLines(query)
    try(DBI::dbSendQuery(con, query))
}

view_table = function(table_name, con){
    try(dplyr::tbl(con, table_name))
}

