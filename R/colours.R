#' @title Convert colours to hexadecimal format
#' @description Convert a vector of colours to hexadecimal format.
#' @param x character or integer vector of colour values
#' @returns a character vector of hexadecimal format colours.
#' @export
#' @examples
#' col2hex("blue")
#' col2hex(4)
#' col2hex("4")
#' col2hex(c(4,"blue"))
col2hex = function(x){
  x |> 
    grDevices::col2rgb() |> 
    apply(2, function(x){
      grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255)
    })
}


#' @title \pkg{ggplot2} default hue palette
#' @param n number of colours to return
#' @returns a character vector of hexadecimal format colours.
#' @export
#' @examples
#' ggplot2_colours(2)
#' ggplot2_colours(3)
#' ggplot2_colours(4)
#' scales::show_col(ggplot2_colours(2))
#' scales::show_col(ggplot2_colours(3))
#' scales::show_col(ggplot2_colours(4))
ggplot2_colours = function(n) {
  scales::hue_pal()(n)
}

