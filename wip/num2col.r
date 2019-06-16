
num2col = function(x, ncol = 10, palette = heat.colors){
    breaks = seq(min(x), max(x), length = ncol + 1)
    i = cut(x, breaks = breaks, include.lowest = TRUE)
    palette(ncol)[i]
}


if(0){
    data = expand.grid(x = seq(-2, 2, length = 20),
                       y = seq(-2, 2, length = 20))
    data$z = dnorm(data$x) * dnorm(data$y)
    plot(data$x, data$y, col = num2col(data$z), pch = 15, cex = 2)
}