\dontrun{

library(tidyverse)
library(pips)


# iris ------------------------------------------------------------------------------

# overview

iris$Species %>% overview
iris$Sepal.Length %>% overview
oview = iris %>% overview
oview$dim
oview$classes
oview$stats

# pips

iris %>% pip(Species)
iris %>% pip(Species, Sepal.Length)
iris %>% pip(Species, Sepal.Width)
iris %>% pip(Species, Petal.Length)
iris %>% pip(Species, Petal.Width)
iris %>% pip(Sepal.Length)
iris %>% pip(Sepal.Length, binwidth = 0.5)
iris %>% pip(Sepal.Length, bins = 50)
iris %>% pip(Sepal.Length, breaks = seq(4, 8, 0.25))
iris %>% pip(Sepal.Length, Sepal.Width)
iris %>% pip(Sepal.Length, Sepal.Width, y_method = "glm", y_points = TRUE)
iris %>% pip(Sepal.Length, Sepal.Width, Species, y_method = "glm")


# mtcars ----------------------------------------------------------------------------

# overview

mtcars$mpg %>% overview
oview = mtcars %>% overview
oview$dim
oview$classes
oview$stats

# pips

mtcars %>% pip(mpg)
mtcars %>% pip(disp)
mtcars %>% pip(mpg, disp)
mtcars %>% pip(mpg, disp, y_method = "glm")
mtcars %>% pip(mpg, disp, y_method = "gam")
mtcars %>% pip(mpg, vs)
mtcars %>% pip(mpg, vs, y_method = "glm")
mtcars %>% pip(mpg, vs, y_method = "gam")

mtcars %>% pip(bin_quantile(mpg, 5))
mtcars %>% pip(bin_quantile(mpg, 100))

mtcars %>% pip(bin_pretty(mpg))
mtcars %>% pip(bin_pretty(mpg), vs, y_method = "glm")
mtcars %>% pip(bin_pretty(mpg), am, y_method = "glm")



# chileancredit ---------------------------------------------------------------------

data("chileancredit", package = "smbinning")

# overview

oview = chileancredit %>% overview
oview$dim
oview$classes
oview$stats

# convert low dimension numerics to factor
chileancredit %<>%
    dplyr::mutate(
        cbline = cbline %>% factor(0:5),
        cbtob = cbtob %>% factor(3:8),
        tob = tob %>% factor(0:5)
    )

# pips

chileancredit %>% pip(cbdpd, fgood, y_method = "glm")
chileancredit %>% pip(cbinq, fgood, y_method = "glm")
chileancredit %>% pip(cbline, fgood, y_method = "glm")
chileancredit %>% pip(cblineut, fgood, y_method = "gam")
chileancredit %>% pip(cbnew, fgood, y_method = "glm")
chileancredit %>% pip(cbs1, fgood, y_method = "gam")
chileancredit %>% pip(cbs2, fgood, y_method = "gam")
chileancredit %>% pip(cbs3, fgood, y_method = "gam")
chileancredit %>% pip(cbterm, fgood, y_method = "glm")
chileancredit %>% pip(cbtob, fgood, y_method = "glm")
chileancredit %>% pip(dc, fgood, y_method = "gam")
chileancredit %>% pip(dd, fgood, y_method = "glm")
chileancredit %>% pip(dep, fgood, y_method = "gam")
chileancredit %>% pip(dpd, fgood, y_method = "glm")
chileancredit %>% pip(home, fgood, y_method = "glm")
chileancredit %>% pip(inc, fgood, y_method = "glm")
chileancredit %>% pip(od, fgood, y_method = "glm")
chileancredit %>% pip(online, fgood, y_method = "glm")
chileancredit %>% pip(period, fgood, y_method = "glm")
chileancredit %>% pip(pmt, fgood, y_method = "glm")
chileancredit %>% pip(rnd, fgood, y_method = "gam")
chileancredit %>% pip(tob, fgood, y_method = "glm")
}


