library(readxl)
library(extrafont)
library(tidyverse)
library(ggthemes)
library(gganimate)
library(gifski)
library(plotly)

ggplotly(static)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

data_modified <- data_filtered %>%
  accumulate_by(~Year)

custom_colors <- c("black", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2")

plot_ly(data_modified) %>%
  add_lines(
    x = ~Year, y = ~Emissions,
    color = ~Land,
    frame = ~frame,
    colors = custom_colors) %>%
  layout(title = "CO2-Emissionen im Zeitraum von 1990 - 2019",
         yaxis = list(title = "CO2-Emissionen in kt"),
         xaxis = list(title = "Jahr"),
         legend = list(title=list(text='<b> Land </b>')),
         font = list(family = "Sitka Text")) %>% 
  animation_opts(
    frame = 333,
    transition = 0,
    easing = "linear",
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Year"
    )
  )

