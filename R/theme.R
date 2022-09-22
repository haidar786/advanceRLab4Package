library(ggplot2)
library(cowplot)
library(magick)

theme_set(theme_cowplot())
ggdraw() +
  theme(plot.background = element_rect(fill = 'blue', colour = 'blue')) +
  draw_image("https://liu.se/polopoly/KoM/facebookbilder/linkuniv_sigill_facebook_314px.jpg", scale = 0.2, x = -0.4, y = -0.3)
