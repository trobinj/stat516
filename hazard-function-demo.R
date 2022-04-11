library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

options(tibble.print_max = 50)

shape <- 2
scale <- 2

df <- function(x) {
   dgamma(x, shape = shape, scale = scale)    
}
sf <- function(x) {
   1 - pgamma(x, shape = shape, scale = scale)  
}

d <- expand.grid(x = seq(0, 20, by = 0.1), t = seq(0, 10, by = 1))

d <- d %>% mutate(pdx = df(x), pdt = df(t)) %>%
   mutate(hzx = pdx/sf(x), hzt = pdt/sf(t)) %>%
   mutate(trx = ifelse(x >= t, pdx/sf(t), NA))

colors <- c("Probability Density" = "black", 
   "Truncated Probability Density" = "red", "Hazard" = "blue")

p <- ggplot(d, aes(x = x, y = pdx)) + theme_minimal() + 
   geom_line(aes(y = trx, frame = t, color = "Truncated Probability Density")) +
   geom_line(aes(color = "Probability Density")) + 
   geom_line(aes(y = hzx, color = "Hazard")) + 
   geom_segment(aes(x = t, xend = t, y = 0, yend = hzt, frame = t),
      size = 0.1, linetype = 3) +
   geom_point(aes(x = t, y = hzt, frame = t)) + 
   coord_cartesian(xlim = c(0,max(d$t))) + labs(color = NULL) +
   scale_color_manual(values = colors)

fig <- ggplotly(p)
fig <- fig %>% layout(xaxis = list(title = "Time"), yaxis = list(title = "Probability Density"))
fig <- fig %>% layout(legend = list(x = 0.1, y = 0.95, bgcolor = 'rgba(0,0,0,0)')) %>%
   layout(legend = list(title = list(text = "Function")))

fig


