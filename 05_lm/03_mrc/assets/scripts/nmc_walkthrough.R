# - online presentations
# - mid term
# - PA4
# - model selection walkthrough


library(lingStuff)
library(tidyverse)

create_project(name = "nmc")




my_cars <- mtcars %>% 
  select(., mpg, wt, drat)

mod_full <- lm(mpg ~ wt + drat + wt:drat, data = my_cars)
mod_int  <- lm(mpg ~ wt + drat          , data = my_cars)
mod_drat <- lm(mpg ~ wt                 , data = my_cars)
mod_wt   <- lm(mpg ~ drat               , data = my_cars)
mod_null <- lm(mpg ~ 1                  , data = my_cars)

anova(mod_int, mod_full) # keep int
# F(1) = 5.41, p < 0.03

anova(mod_drat, mod_int) # drat not sig.
# F(1) = 0.98, p > 0.05
# F > 1

anova(mod_wt, mod_int) # wt sig.
# F(1) = 36.01, p < 0.001
# There was a main effect of wt ()

library(corrplot)
my_cor <- cor(my_cars)
corrplot(my_cor, type = "upper")


# - papaja
#     - devtools::install_github("crsh/papaja")
#     - install tinytex
#     - Download LaTeX (miktex)


devtools::install_github("crsh/papaja")

library(tinytex)
install_tinytex()


