\dontrun{

library(tidyverse)
theme_set(theme_oddments())

# iris -----

as_tibble(iris)
results <- skim(iris)
print(results)

iris %>% sketch(Species)
iris %>% sketch(Species, Sepal.Length)
iris %>% sketch(Species, Sepal.Length, y_method = "glm")
iris %>% sketch(Species, Sepal.Length, y_method = "none", y_points = TRUE)
iris %>% sketch(Species, Sepal.Length, y_method = "glm", y_points = TRUE)

iris %>% sketch(Sepal.Length)
iris %>% sketch(Sepal.Length, x_binwidth = 0.5)
iris %>% sketch(Sepal.Length, x_bins = 50)
iris %>% sketch(bin(Sepal.Length, breaks = seq(4, 8, 0.25)))
iris %>% sketch(bin_pretty(Sepal.Length))

iris %>% sketch(Sepal.Length, Sepal.Width)
iris %>% sketch(Sepal.Length, Sepal.Width, y_points = TRUE)
iris %>% sketch(Sepal.Length, Sepal.Width, y_points = TRUE,  y_method = "glm")
iris %>% sketch(Sepal.Length, Sepal.Width, y_points = TRUE,  y_method = "gam")
iris %>% sketch(Sepal.Length, Sepal.Width, y_points = TRUE,  y_method = "none")
iris %>% sketch(Sepal.Length, Sepal.Width, Species)
iris %>% sketch(Sepal.Length, Sepal.Width, Species, y_points = TRUE)
iris %>% sketch(Sepal.Length, Sepal.Width, Species, y_points = TRUE,  y_method = "glm")
iris %>% sketch(Sepal.Length, Sepal.Width, Species, y_points = TRUE,  y_method = "gam")
iris %>% sketch(Sepal.Length, Sepal.Width, Species, y_points = TRUE,  y_method = "none")


# mtcars -----

as_tibble(mtcars)
results <- skim(mtcars)
print(results)

mtcars %>% sketch(mpg)
mtcars %>% sketch(disp)
mtcars %>% sketch(mpg, disp)
mtcars %>% sketch(mpg, disp, y_method = "gam")
mtcars %>% sketch(mpg, vs)
mtcars %>% sketch(mpg, vs, y_method = "glm")
mtcars %>% sketch(mpg, vs, y_method = "gam")

mtcars %>% sketch(bin_quantile(mpg, 5))
mtcars %>% sketch(bin_quantile(mpg, 100))

mtcars %>% sketch(bin_pretty(mpg))
mtcars %>% sketch(bin_pretty(mpg), vs)
mtcars %>% sketch(bin_pretty(mpg), vs, y_method = "glm")


# attenu -----

as_tibble(attenu)
results <- skim(attenu)
print(results)

attenu %>% sketch(dist, accel)
attenu %>% sketch(log(dist), accel)
attenu %>% sketch(log(dist), log(accel))
attenu %>% sketch(log(dist), log(accel), y_method = "glm")
attenu %>% sketch(log(dist), log(accel), y_method = "gam")


# nycflights13::airports -----

nycflights13::airports
results <- skim(nycflights13::airports)
print(results)


# nycflights13::planes -----

nycflights13::planes
results <- skim(nycflights13::planes)
print(results)


# nycflights13::weather -----

nycflights13::weather
results <- skim(nycflights13::weather)
print(results)

nycflights13::weather %>%
    select_if(is.numeric) %>% 
    filter(complete.cases(.)) %>% 
    sample_frac(0.05) %>% 
    pairs()

nycflights13::weather %>% 
    sample_frac(0.1) %>% 
    sketch(month, temp, y_method = "gam", x_bins = 12, x_binwidth = 0.5)

nycflights13::weather %>% 
    sample_frac(0.1) %>% 
    sketch(as.factor(month), temp, y_method = "glm")


# nycflights13::flights -----

nycflights13::flights
results <- skim(nycflights13::flights)
print(results)


mtcars %>% pairs

mtcars %>% sketch(mpg)
mtcars %>% sketch(mpg, disp)
mtcars %>% sketch(mpg, disp, am)
mtcars %>% sketch(mpg, disp, vs)
mtcars %>% sketch(mpg, disp, cyl)

mtcars %>% sketch(as.character(gear), mpg)
mtcars %>% sketch(as.character(gear), qsec)
mtcars %>% sketch(as.character(gear), am)
mtcars %>% sketch(as.character(gear), vs)

mtcars %>% sketch(gear, vs)
mtcars %>% sketch(gear, am)
mtcars %>% sketch(as.integer(gear), am)

mtcars %>% sketch(mpg, disp)
mtcars %>% sketch(mpg, disp, y_method = "glm")
mtcars %>% sketch(mpg, disp, y_method = "gam")

mtcars %>% sketch(mpg, vs)
mtcars %>% sketch(mpg, vs, y_method = "gam")

mtcars %>% sketch(cyl)
mtcars %>% sketch(cyl / disp)
mtcars %>% sketch(cyl / disp, vs + am)
}
