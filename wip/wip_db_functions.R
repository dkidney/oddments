
db_get_query = function(query, conn, verbose = FALSE){
    if(verbose) writeLines(query)
    try(dplyr::as_tibble(DBI::dbGetQuery(conn, query)))
}

db_send_query = function(query, conn, verbose = FALSE){
    if(verbose) writeLines(query)
    try(DBI::dbSendQuery(conn, query))
}

