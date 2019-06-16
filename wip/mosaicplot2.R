
mosaicplot2 = function(x, y, col = NULL, ...){

    if(is.null(col)) col = "grey"
    tab = df %$% table(x, y, useNA = 'ifany')
    nx = nrow(tab)
    ny = ncol(tab)
    ncol = length(col)
    if(!(ncol == 1 | ncol == ny | ncol == (nx * ny)))
        stop("...adfaf")
    col %<>% matrix(nrow = nx, ncol = ny, byrow = TRUE)
    xs = tab %>% apply(1, sum) %>% cumsum %>% unname %>% c(0, .)
    n = xs %>% last
    gap = n * 0.025
    x0s = xs[1:nx] + 0:(nx - 1) * gap
    x1s = xs[2:(nx+1)] + 0:(nx - 1) * gap

    plot(
        x = c(0, n + gap * (nx - 1)),
        y = c(0, n + gap * (ny - 1)),
        xaxs = "i",
        yaxs = "i",
        ann = FALSE,
        axes = FALSE,
        type = "n",
        ...
    )

    for(i in 1:nx){ # i = 1
        x0 = x0s[i]
        x1 = x1s[i]
        ys = tab[i,] %>% cumsum %>% unname %>% c(0, .)
        ys %<>% divide_by(last(ys)) %>% multiply_by(n)
        y0s = ys[1:ny] + 0:(ny - 1) * gap
        y1s = ys[2:(ny+1)] + 0:(ny - 1) * gap
        for(j in 1:ny){ # j = 1
            y0 = y0s[j]
            y1 = y1s[j]
            rect(x0, y0, x1, y1, col = col[i,j], border = NA)
        }
    }

}

if(0){

    library(magrittr)
    library(dplyr)

    n = 100
    df = data_frame(
        x = sample(1:4, n, replace = TRUE),
        y = sample(1:3, n, replace = TRUE),
        z = x + y
    )

    ncol = 10
    i = df %>%
        group_by(x, y) %>%
        summarise(z = mean(z)) %>%
        extract2("z") %>%
        cut(ncol + 1)
    col = heat.colors(ncol)[i]

    # debugonce(mosaicplot2)
    df %$% mosaicplot2(x, y)
    df %$% mosaicplot2(x, y, "dodgerblue")
    df %$% mosaicplot2(x, y, 1:3)
    df %$% mosaicplot2(x, y, c(2, NA, 4))
    df %$% mosaicplot2(x, y, col)

}





