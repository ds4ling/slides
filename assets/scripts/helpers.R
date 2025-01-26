# Packages --------------------------------------------------------------------

# Custom
library("ds4ling")
library("lingStuff")
library("untidydata")
library("academicWriteR")

# Tidyverse
library("dplyr")
library("tidyr")
library("readr")
library("purrr")
library("here")
library("forcats")
library("stringr")
library("glue")

# Modeling
library("lme4")
library("brms")

# Plotting
library("ggplot2")
library("patchwork")
library("ggridges")
library("ggrepel")
library("viridis")
library("ggfortify")

# Misc
library("knitr")
library("kableExtra")
library("latex2exp")
library("broom")
library("fGarch")
library("equatiomatic")
library("countdown")
library("tiktokrmd")

# set seed for reproducibility
set.seed(12345)


# Plotting functions ----------------------------------------------------------

# TJ Mahr's self documenting function
# https://www.tjmahr.com/self-titled-ggplot2-plots/

self_document <- function(expr) {
  monofont <- ifelse(
    extrafont::choose_font("Consolas") == "", 
    "mono", 
    "Consolas"
  )
  
  p <- rlang::enexpr(expr)
  title <- rlang::expr_text(p) |> 
    grkstyle::grk_style_text() |> 
    paste0(collapse = "\n")
  
  patchwork::wrap_elements(eval(p)) + 
    patchwork::plot_annotation(
      title = title, 
      theme = theme(
        plot.title = element_text(
          family = monofont, hjust = 0, size = rel(.9), 
          margin = margin(0, 0, 5.5, 0, unit = "pt")
        )
      )
    )
}
