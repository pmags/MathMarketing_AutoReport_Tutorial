
# ======================================================================================= #
# Script Name :                                                                                            
# Purpose     :                                                                      
# Args        : 
# Date        : Tue Mar 17 07:37:18 2020   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ======================================================================================= #

source("scripts/helpers.R")
source("scripts/themes.R")

# ======================================================================================= #
# Import data ----                                         
# ======================================================================================= #

gymdata <- read_delim("data/data.csv", ";", 
                      escape_double = FALSE, 
                      locale = locale(decimal_mark = ",", 
                                      grouping_mark = "."), 
                      trim_ws = TRUE)

gymdata$nome <- as.factor(gymdata$nome)


# ======================================================================================= #
# market share ----                                         
# ======================================================================================= #

marketPie <- gymdata %>%  
  select(nome,market_share) %>% 
  arrange(desc(market_share)) %>% 
  mutate(cumsum_marketShare = cumsum(market_share),
         group = as.factor(ifelse(cumsum_marketShare < 0.8, 
                                  yes = as.character(nome), 
                                  no = "others"))) %>% 
  group_by(group) %>% 
  summarise(market_share = sum(market_share)) %>% 
  ggplot(aes(x = "", y = market_share, fill = group)) +
  geom_col() +
  coord_polar("y") +
  pieTheme() +
  scale_fill_manual(values = pallete) +
  theme(axis.text.x = element_blank(), 
        legend.direction = "vertical",
        legend.position = "left") +
  geom_text(aes(label = percent(market_share)),
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(title = "Market Share", 
       caption = "source: publicly available financial statements",
       fill = "") 


# ======================================================================================= #
# Content plots ----                                         
# ======================================================================================= #

backlink_plot <- gymdata %>% 
  ggplot(aes(x = reorder(nome, Backlink), y = Backlink)) + 
  geom_bar(stat = "identity", fill = DarkPurple) +
  geom_text(aes(label = Backlink),size = 3, position = position_stack(vjust = 0.5), colour = "#ffffff") +
  coord_flip() +
  barTheme() +
  theme(axis.title=element_blank()) +
  labs(title = "Total backlinks",
       caption = "source: MathMarketing")

pagerank_plot <- gymdata %>% 
  ggplot(aes(x = reorder(nome, Page_Rank), y = Page_Rank)) + 
  geom_bar(stat = "identity", fill = DarkPurple) +
  geom_text(aes(label = Page_Rank),size = 3, position = position_stack(vjust = 0.5), colour = "#ffffff") +
  coord_flip() +
  barTheme() +
  theme(axis.title=element_blank()) +
  labs(title = "Page Rank index")

pagenumber_plot <- gymdata %>% 
  ggplot(aes(x = reorder(nome, qtd_paginas), y = qtd_paginas)) + 
  geom_bar(stat = "identity", fill = DarkPurple) +
  geom_text(aes(label = qtd_paginas),size = 3, position = position_stack(vjust = 0.5), colour = "#ffffff") +
  coord_flip() +
  barTheme() +
  theme(axis.title=element_blank()) +
  labs(title = "Page Rank index")

# ======================================================================================= #
# speed ----                                         
# ======================================================================================= #

pageSpeedDesktop_mean <- mean(gymdata$PageSpeed_Desktop) %>% percent()
pageSpeedMobile_mean <- mean(gymdata$PageSpeed_Mobile_Speed) %>% percent()

speedPlot <- gymdata %>% 
  select(nome, PageSpeed_Desktop, PageSpeed_Mobile_Speed) %>% 
  gather("source","results", -nome) %>% 
  ggplot(aes(x = nome, y = results, group = source)) + 
  geom_line(aes(color = source)) +
  geom_point(aes(color = source)) +
  scale_colour_manual(values = pallete, labels = c("Desktop","Mobile")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs( x="",y = "", color = "") +
  lineTheme() +
  labs(title = "Page loading speed")

# ======================================================================================= #
# spider plots ----                                         
# ======================================================================================= #

iqPlot <- spiderPlot(gymdata, plotTitle = "")

# ======================================================================================= #
# waffle ----                                         
# ======================================================================================= #


toolWaffle <- wafflePlot(gymdata)

# ======================================================================================= #
# Value boxes ----                                         
# ======================================================================================= #

valueBoxes <- valueBox(value = apply(select(gymdata,contains("iq")),2,mean),
         label = names(apply(select(gymdata,contains("iq")),2,mean)),
         dim = c(1.5,3.5),
         colorPalette = "Spectral")

valueTiles <- list()

data <- data.frame(label = names(apply(select(gymdata,contains("iq")),2,mean)),
           value = round(as.numeric(apply(select(gymdata,contains("iq")),2,mean)),2))

for (tool in data$label) {
  valueTiles[[tool]] <- valueBox(value = filter(data, label == tool)[2] , 
           label = tool, 
           dim = c(1.5,1.5),colorPalette = "PRGn")
}

  

