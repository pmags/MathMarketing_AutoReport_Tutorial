
# ======================================================================================= #
# Script Name : Helpers function                                                                                           
# Purpose     : Functions for ploting                                                                     
# Date        : Tue Mar 10 09:39:48 2020   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mathmarketing.eu                                           
# ======================================================================================= #

## Import libraries
library(ggplot2)
library(emojifont)
library(tidyverse)

# ======================================================================================= #
# Create value boxes ----                                         
# ======================================================================================= #

#dim = c("4","6")
#value = c("80%", "80%","80","60")
#info = c("cenas","cenas","cenas","coisas")
# https://stackoverflow.com/questions/47105282/valuebox-like-function-for-static-reports

valueBox <- function(value, label, dim, colorPalette = "Dark2") {
  
  # error msg when dimensions are wrong
  if (length(value) != length(label)) stop("Number of labels is different to values")
  
  df <- data.frame(value, label,
                   x = (rep(seq(2, 3*as.numeric(dim[2]), as.numeric(dim[2]) + 0.25), ceiling(length(value)/3)))[1:length(value)],
                   y = rep(seq(1, as.numeric(dim[1])*ceiling(length(value)/3)+0.5, as.numeric(dim[1])+0.25),each=3)[1:length(value)]) %>% 
    mutate(h = rep(as.numeric(dim[1]),nrow(.)),
           w = rep(as.numeric(dim[2]),nrow(.)),
           color = factor(1:nrow(.)))
  
  ggplot(df, aes(x, y, height = h, width = w, label = label)) +
    geom_tile(aes(fill = color)) +
    geom_text(color = "white", fontface = "bold", size = 10,
              aes(label = value, x = x - 2.9, y = y + 1), hjust = 0) +
    geom_text(color = "white", fontface = "bold",
              aes(label = info, x = x - 2.9, y = y - 1), hjust = 0) +
    coord_fixed() +
    scale_fill_brewer(type = "qual",palette = colorPalette) +
    #geom_text(size = 20, aes(label = shape, family = font_family, x = x + 1.5, y = y + 0.5), alpha = 0.25) +
    theme_void() +
    guides(fill = FALSE)
}

# ======================================================================================= #
# iq spider radar ----                                         
# ======================================================================================= #

iq_global <- data %>% 
  select(nome,contains("iq")) %>% 
  summarise(iq_conteudo = mean(iq_conteudo),
            iq_tecnico = mean(iq_tecnico),
            iq_experiencia = mean(iq_experiencia),
            iq_analytics = mean(iq_analytics),
            iq_ad = mean(iq_ad)) %>% 
  mutate(metric = "global") %>% 
  select(metric,everything()) %>% 
  ggradar(font.radar = "mono", 
          values.radar = c("0","2","5"),
          gridline.min.linetype = "solid",
          gridline.mid.linetype = "solid",
          gridline.max.linetype = "solid",
          gridline.mid.colour = "grey",
          grid.mid = 2,
          grid.max = 5, 
          axis.label.size = 3,
          grid.label.size = 3, 
          legend.text.size = 10,
          group.line.width = 1,
          group.point.size = 3) +
  geom_path(data = funcCircleCoords(c(0, 0), 1 + abs(0 - ((1/9) * (5 - 0))), npoints = 360), 
            aes(x,y),
            lty = "solid", 
            colour = "grey", 
            size = 0.5) +
  geom_path(data = funcCircleCoords(c(0, 0),3 + abs(0 - ((1/9) * (5 - 0))), npoints = 360), 
            aes(x,y),
            lty = "solid", 
            colour = "grey", 
            size = 0.5) +
  geom_path(data = funcCircleCoords(c(0, 0), 4 + abs(0 - ((1/9) * (5 - 0))), npoints = 360), 
            aes(x,y),
            lty = "solid", 
            colour = "grey", 
            size = 0.5)  +
  labs(title = "IQ Digital retalho de moda") +
  theme(plot.title = element_text(size=12))


# ======================================================================================= #
# waffle plots ----                                         
# ======================================================================================= #

# tool waffle

waffle_plots <- list()

for (name in names(data[44:length(names(data))])) {
  
  waffle <- waffle(data %>% 
                     count_(name) %>% 
                     mutate(percent = round(n/nrow(data)*100)) %>%  
                     select(percent) %>% 
                     unlist(), 
                   rows = 10,
                   legend_pos = "botton",
                   flip = TRUE,
                   title = name, 
                   xlab = "1sq == 1%") 
  
  # because waffle objects are ggplot objects too  
  waffle_plots[[name]] <- waffle +  theme(plot.title = element_text(size=12))
}

