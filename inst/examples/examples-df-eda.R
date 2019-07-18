\dontrun{

library(tidyverse)
theme_set(theme_oddments())

# iris -----

as_tibble(iris)
results <- skim(iris)
print(results)

iris %>% sketch(Sepal.Length)
iris %>% sketch(Sepal.Width)
iris %>% sketch(Petal.Length)
iris %>% sketch(Petal.Length, group = Species)
iris %>% sketch(Petal.Length, Petal.Width)
iris %>% sketch(Petal.Length, Petal.Width, group = Species)

iris %>% sketch(Sepal.Length)

iris %>% sketch(Species)
iris %>% sketch(Species, Sepal.Length) # fix
iris %>% sketch(Species, Sepal.Width) # fix
iris %>% sketch(Species, Petal.Length) # fix
iris %>% sketch(Species, Petal.Width) # fix
iris %>% sketch(Sepal.Length)
iris %>% sketch(Sepal.Length, binwidth = 0.5) # fix
iris %>% sketch(Sepal.Length, bins = 50) # fix
iris %>% sketch(Sepal.Length, breaks = seq(4, 8, 0.25)) # fix
iris %>% sketch(Sepal.Length, Sepal.Width)
iris %>% sketch(Sepal.Length, Sepal.Width, y_method = "glm", y_points = TRUE)
iris %>% sketch(Sepal.Length, Sepal.Width, Species, y_method = "glm")


# mtcars -----

as_tibble(mtcars)
results <- skim(mtcars)
print(results)

mtcars %>% sketch(mpg)
mtcars %>% sketch(disp)
mtcars %>% sketch(mpg, disp)
mtcars %>% sketch(mpg, disp, y_method = "glm")
mtcars %>% sketch(mpg, disp, y_method = "gam")
mtcars %>% sketch(mpg, vs)
mtcars %>% sketch(mpg, vs, y_method = "glm")
mtcars %>% sketch(mpg, vs, y_method = "gam")

mtcars %>% sketch(bin_quantile(mpg, 5))
mtcars %>% sketch(bin_quantile(mpg, 100))

mtcars %>% sketch(bin_pretty(mpg))
mtcars %>% sketch(bin_pretty(mpg), vs, y_method = "glm")
mtcars %>% sketch(bin_pretty(mpg), am, y_method = "glm")




as_tibble(esoph)
results <- skim(esoph)
print(results)

as_tibble(attenu)
results <- skim(attenu)
print(results)

nycflights13::airports
results <- skim(nycflights13::airports)
print(results)

nycflights13::planes
results <- skim(nycflights13::planes)
print(results)

nycflights13::weather
results <- skim(nycflights13::weather)
print(results)

nycflights13::flights
results <- skim(nycflights13::flights)
print(results)

ggplot2::theme_set(theme_oddments())

# discrete x
mtcars %>% sketch(as.integer(cyl))
mtcars %>% sketch(as.character(am))

# continuous x
mtcars %>% sketch(mpg)
mtcars %>% sketch(log(mpg))
mtcars %>% sketch(mpg, x_bins = 3)

# discrete x and continuous y
mtcars %>% sketch(as.integer(gear), qsec)

# discrete x, binary y
mtcars %>% sketch(as.integer(gear), as.integer(vs))

# continuous x, continuous y
mtcars %>% sketch(mpg, disp)
mtcars %>% sketch(mpg, disp, y_method = "glm")
mtcars %>% sketch(mpg, disp, y_method = "glm", y_points = TRUE)

# continuous x, binary y
mtcars %>% sketch(mpg, as.integer(vs))
mtcars %>% sketch(mpg, as.integer(vs), bin = TRUE)

mtcars %>% sketch(cyl)
mtcars %>% sketch(cyl / disp)
mtcars %>% sketch(cyl / disp, vs + am)
}
