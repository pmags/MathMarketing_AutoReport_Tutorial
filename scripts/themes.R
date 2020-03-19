
# ======================================================================================= #
# Script Name : Themes                                                                                            
# Purpose     : Default themes to be used                                                                     
# Args        : 
# Date        : Mon Mar 16 18:14:50 2020   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ======================================================================================= #

# https://ggplot2.tidyverse.org/reference/theme.html
# https://ggplot2.tidyverse.org/reference/ggtheme.html


# ======================================================================================= #
# Color constants ----                                         
# ======================================================================================= #

DarkPurple <-  "#680c62"
Grey <-  "#bfbfbf"
pallete <- c("#5c2359","#bfbfbf","#c059bb") 

# ======================================================================================= #
# Pie chart theme ----                                         
# ======================================================================================= #

pieTheme <- function(base_size = 10, 
                     base_family = "", 
                     base_line_size = base_size / 170,
                     base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        face = "bold",
        size = 14,
        hjust = 0),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      legend.position="bottom",
      legend.direction = "horizontal",
      
      complete = TRUE
    )
}


# ======================================================================================= #
# Bar chart theme ----                                         
# ======================================================================================= #

barTheme <- function(base_size = 13, 
                     base_family = "", 
                     base_line_size = base_size / 170,
                     base_rect_size = base_size / 170){
  theme_classic(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = DarkPurple, 
        face = "bold",
        size = 10,
        hjust = 0,
        vjust = 3),
      plot.caption = element_text(face = "italic", 
                                  size = 8,
                                  hjust = 1,
                                  vjust = 1),
      panel.grid.major = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted"),
      panel.grid.minor = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted",
        size = rel(4)),
      axis.line = element_line(colour = Grey, 
                               size = 1, 
                               linetype = "solid"),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(70,14,61, maxColorValue = 255),
        size = rel(0.5)),
      complete = TRUE
    )
}

# ======================================================================================= #
# line plot ----                                         
# ======================================================================================= #

lineTheme <- function(base_size = 13, 
                      base_family = "", 
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  theme_classic(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = DarkPurple, 
        face = "bold",
        size = 10,
        hjust = 0,
        vjust = 3),
      plot.caption = element_text(face = "italic", 
                                  size = 8,
                                  hjust = 1,
                                  vjust = 1),
      legend.text = element_text(face = "italic", 
                               size = 8),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-10,-10,-10,-10),
      panel.grid.major = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted"),
      panel.grid.minor = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted",
        size = rel(4)),
      axis.line = element_line(colour = Grey, 
                               size = 1, 
                               linetype = "solid"),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(70,14,61, maxColorValue = 255),
        size = rel(0.5)),
      legend.position="bottom",
      complete = TRUE
    )
}


