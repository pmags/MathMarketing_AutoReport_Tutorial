
# ======================================================================================= #
# Script Name : Helpers function                                                                                           
# Purpose     : Functions for ploting                                                                     
# Date        : Tue Mar 10 09:39:48 2020   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mathmarketing.eu                                           
# ======================================================================================= #

## Import libraries which will be needed for the report
library(tidyverse)
library(gridExtra) # Allows for multiple plots with grid arrangement
library(ggiraph) # Expand on ggplot library
library(ggiraphExtra) # Expand on ggplot library
library(ggradar) # Expands ggplot library with radar plot
library(waffle)
library(scales)

# ======================================================================================= #
# Create value boxes ----                                         
# ======================================================================================= #

# Value: vector with values
# label: vector with labels the same size as value vector
# dim: vector with dimensions of the box
# colorPalette: Color gradientte

valueBox <- function(value, label, dim = c(4,6), colorPalette = "Dark2") {
  
  # error msg when vectors have different sizes
  if (length(value) != length(label)) stop("Number of labels is different to values")
  
  # Converts vectors into dataframe to use on ggplot
  df <- data.frame(value, label,
                   x = (rep(seq(2, 3*as.numeric(dim[2]), as.numeric(dim[2]) + 0.25), ceiling(length(value)/3)))[1:length(value)],
                   y = rep(seq(0, as.numeric(dim[1])*ceiling(length(value)/3)+0.5, as.numeric(dim[1])+0.25),each=3)[1:length(value)]) %>% 
    mutate(h = rep(as.numeric(dim[1]),nrow(.)),
           w = rep(as.numeric(dim[2]),nrow(.)),
           color = factor(1:nrow(.)))
  
  # Uses ggplot to create boxes
  ggplot(df, aes(x, y, height = h, width = w, label = label)) +
    geom_tile(aes(fill = color)) +
    geom_text(color = "white", fontface = "bold", size = 8,
              aes(label = value, x = x - 0.3 , y = y + 0.3), hjust = 0) +
    geom_text(color = "white", fontface = "bold", size = 3,
              aes(label = label, x = x - 0.3 , y = y - 0.3), hjust = 0) +
    coord_fixed() +
    scale_fill_brewer(type = "qual",palette = colorPalette) +
    #geom_text(size = 20, aes(label = shape, family = font_family, x = x + 1.5, y = y + 0.5), alpha = 0.25) +
    theme_void() +
    guides(fill = FALSE)
}

# ======================================================================================= #
# Create spider radar ----                                         
# ======================================================================================= #

# Function to creating IQ plots automatically given results on csv
# data: data.frame to be ploted. Relevant columns have to contain "iq"
# args: selects wich variable to be included. By default is set to include "max", "min" and "mean"
# plotTitle: Title

spiderPlot <- function(data, args = c("max","min","mean"), plotTitle = "Spider Plot") {
  
  # Helper function for new guidelines
  funcCircleCoords <- function(center = c(0, 0), r = 1, npoints = 100) {
    tt <- seq(0, 2 * pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  # Prepares data to be ploted
  dataPlot <- apply(select(data,contains("iq")),2,max) %>% 
    rbind(apply(select(data,contains("iq")),2,min)) %>% 
    rbind(apply(select(data,contains("iq")),2,mean)) %>% 
    data.frame() %>%
    mutate(metric = c("max","min","mean")) %>% 
    select(metric,everything()) %>% 
    filter(metric %in% args)
  
  # Changing ggradar function
  plot <- dataPlot  %>% 
    ggradar(#font.radar = "mono", 
            values.radar = c("0","2","5"),
            gridline.min.linetype = "solid",
            gridline.mid.linetype = "solid",
            gridline.max.linetype = "solid",
            gridline.mid.colour = "grey",
            grid.mid = 2,
            grid.max = 5, 
            axis.label.size = 3,
            grid.label.size = 3, 
            legend.text.size = 8,
            group.line.width = 0.7,
            group.point.size = 2) +
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
    labs(title = plotTitle) +
    theme(plot.title = element_text(size=10),
          legend.position="bottom")

    return(plot)
}

# ======================================================================================= #
# waffle plots ----                                         
# ======================================================================================= #

# Function to create waffle style plots
# data: data frame with tools distribution
# tools: character vector with tools names to be considered. By default it takes all

wafflePlot <- function(data) {
  
  toolsDefault = c("Google_Ad_Services","Doubleclick","Custom_Audiences",
                   "Google_Optimizer", "Google_Analytics", "Google_TagManager", "Hotjar")
  
  # Creates empty list for saving all new plots
  waffle_plots <- list()
  
  # Loop over selected tools
  for (tool in names(data)[names(data) %in% toolsDefault]) {
    
    plotData <- data %>% 
      count_(tool) %>% 
      mutate(percent = round(n/nrow(data)*100)) %>%
      select(percent) %>% 
      unlist()
    
    waffle <- waffle(plotData,
                     rows = 10,
                     size = 1,
                     colors =  ifelse(plotData == 100, pallete[2], pallete), 
                     legend_pos = "botton",
                     flip = TRUE,
                     title = tool,
                     xlab = "1sq == 1%")
    
    # because waffle objects are ggplot objects too  
    waffle_plots[[tool]] <- waffle +  
      theme(plot.title = element_text(size=10, color = DarkPurple, face = "bold"))
  }
  
  # returns results
  return(waffle_plots)
}




