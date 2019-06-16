
#' @title Replacement functions for BMP, JPEG, PNG and TIFF graphics devices
#' @description Graphics devices for BMP, JPEG, PNG and TIFF format bitmap files with modified default values.
#' @details These functions work in exactly the same way as the standard functions. The main difference are that the arguments have nicer defaults for making plots for use in reports. See the examples below for tips on how to make sure that the pointsize of titles etc. remain consistent regardless of how many panels there are.
#' @author Darren
#' @seealso \link{Devices}
#' @importFrom tools file_ext
#' @export
#' @examples
#' op = par(no.readonly = TRUE)
#'
#' # single plot
#' jpeg("temp_1")
#' par(mfrow = c(1,1), mar = c(4,4,2,1))
#' plot(1:10, 1:10, main = "A", xlab = "x", ylab = "y")
#' dev.off()
#' system("open temp_1.jpeg")
#'
#' # multipanel plot - titles automatically resized
#' jpeg("temp_2")
#' par(mfrow = c(2,3), mar = c(4,4,2,1))
#' for(i in 1:6) plot(1:10, 1:10, main = LETTERS[i], xlab = "x", ylab = "y")
#' dev.off()
#' system("open temp_2.jpeg")
#'
#' # multipanel plot - maintain pointsize
#' jpeg("temp_3")
#' par(mfrow = c(2,3), mar = c(4,4,2,1), cex = 1)
#' for(i in 1:6) plot(1:10, 1:10, main = LETTERS[i], xlab = "x", ylab = "y")
#' dev.off()
#' system("open temp_3.jpeg")
#'
#' par(op)
#'
#' unlink(c("temp_1.jpeg", "temp_2.jpeg", "temp_3.jpeg"), force = TRUE)

bmp = function(filename, width = 10, height = 8, units = "cm", pointsize = 8, res = 300, family = "Calibri", ...){

    if(family == "Calibri") windowsFonts(Calibri = windowsFont("Calibri"))
    if(file_ext(filename) == "") filename = paste0(filename, ".bmp")

    grDevices:::bmp(filename, width = width, height = height, units = units, pointsize = pointsize, res = res, family = family, ...)

}


#' @export

jpeg = function(filename, width = 10, height = 8, units = "cm", pointsize = 8, res = 300, family = "Calibri", quality = 150, ...){

    if(family == "Calibri") windowsFonts(Calibri = windowsFont("Calibri"))
    if(file_ext(filename) == "") filename = paste0(filename, ".jpeg")

    grDevices:::jpeg(filename, width = width, height = height, units = units, pointsize = pointsize, res = res, family = family, quality = quality, ...)

}

#' @export

png = function(filename, width = 10, height = 8, units = "cm", pointsize = 8, res = 300, family = "Calibri", ...){

    if(family == "Calibri") windowsFonts(Calibri = windowsFont("Calibri"))
    if(file_ext(filename) == "") filename = paste0(filename, ".png")

    grDevices:::png(filename, width = width, height = height, units = units, pointsize = pointsize, res = res, family = family, ...)

}

#' @export

tiff = function(filename, width = 10, height = 8, units = "cm", pointsize = 8, res = 300, family = "Calibri", ...){

    if(family == "Calibri") windowsFonts(Calibri = windowsFont("Calibri"))
    if(file_ext(filename) == "") filename = paste0(filename, ".tiff")

    grDevices:::tiff(filename, width = width, height = height, units = units, pointsize = pointsize, res = res, family = family, ...)

}

#' @export

pdf = function(filename, width = 3.937008, height = 3.149606, pointsize = 8, family = "Calibri", ...){

    if(family == "Calibri") windowsFonts(Calibri = windowsFont("Calibri"))
    if(file_ext(filename) == "") filename = paste0(filename, ".pdf")

    grDevices:::pdf(filename, width = width, height = height, pointsize = pointsize, family = family, ...)

}





