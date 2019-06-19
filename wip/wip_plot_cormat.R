
#' @title Plot a correlation matrix
#' @description Convenience function for plotting correlation matrices using
#'   \link[ggplot2]{geom_tile}.
#' @param x correlation matrix
#' @param type region of matrix to plot
#' @param trunc.names maximum number of characters to use when plotting variable names
#' @param zlim limits of correlation range
#' @param col vector of colours
#' @param cex size of text
#' @example inst/examples/example-plot_cormat.R
#' @export
#' @importFrom forcats fct_inorder fct_rev
#' @importFrom grDevices colorRampPalette
plot_cormat = function(x, type = c("lower", "upper", "full"), trunc.names = 20,
                       zlim = c(-1,1), col = colorRampPalette(c("blue", "white", "red"))(10), cex = 3){
    # check inputs
    if(!(inherits(x, "matrix") && nrow(x) == ncol(x)))
        stop("expecting a square matrix")
    type = match.arg(type)
    # subset matrix
    if(type == "lower") x %<>% lower_tri
    if(type == "upper") x %<>% upper_tri
    # check row/colnames
    if(is.null(rownames(x))) rownames(x) = paste0("V", 1:nrow(x))
    if(is.null(colnames(x))) colnames(x) = paste0("V", 1:ncol(x))
    # check trunc.names doesn't lead to duplicate row/col names
    dups = x %>% rownames %>% str_trunc(trunc.names, "right", ".") %>% duplicated
    if(any(dups))
        stop("duplicated row/col names after truncation - try increasing the value of 'trunc.names'")
    x %>%
        # truncate row/col names
        set_rownames(rownames(.) %>% str_trunc(trunc.names, "right", ".")) %>%
        set_colnames(colnames(.) %>% str_trunc(trunc.names, "right", ".")) %>%
        # convert to long format
        as.data.frame %>%
        mutate(cols = rownames(.)) %>%
        gather(key = "rows", value = "cor", -cols) %>%
        mutate(
            rows = rows %>% factor %>% fct_inorder,
            cols = cols %>% factor %>% fct_inorder %>% fct_rev
        ) %>%
        filter(!is.na(cor)) %>%
        # plot
        ggplot(aes_string(x = 'rows', y = 'cols')) +
        geom_tile(aes_string(fill = 'cor')) +
        geom_text(aes_string(label = "cor %>% exact_dp(2)"), size = cex) +
        coord_fixed() +
        scale_fill_gradientn(colours = col, limits = zlim) +
        theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
              axis.title   = element_blank(),
              legend.title = element_blank())
}

#' @rdname upper_tri
#' @name upper_tri
#' @title Extract the upper or lower triangular part of a Matrix
#' @description Convenience wrappers for \link[base]{lower.tri} and
#'   \link[base]{upper.tri}, but which return a matrix instead of an index.
#' @param x a square matrix
#' @return Returns a matrix with the same dimensions as \code{x} but with the diagonal and
#'   upper/lower elements set to \code{NA}.
#' @export
#' @examples
#' x = matrix(1:9, 3, 3) ; x
#' upper_tri(x)
#' lower_tri(x)
upper_tri = function(x){
    if(!is_square_matrix(x)) stop("expecting a square matrix")
    x[!upper.tri(x)] = NA
    return(x)
}

#' @rdname upper_tri
#' @name lower_tri
#' @export
lower_tri = function(x){
    if(!is_square_matrix(x)) stop("expecting a square matrix")
    x[!lower.tri(x)] = NA
    return(x)
}

is_square_matrix = function(x){
    inherits(x, 'matrix') && nrow(x) == ncol(x)
}





