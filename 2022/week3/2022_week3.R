####### Load packages######
library(sp)
library(rgdal)
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(magrittr)
library(broom)
library(rgeos)

#####Raw data
choc  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')
head(choc)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')



choc.hist<-choc %>% group_by(rating) %>% 
  mutate(mean.perc=mean(cocoa_percent)) %>% 
  separate(cocoa_percent, into = c("Percentage", "Second"), "%") %>% 
  filter(country_of_bean_origin!="Blend") %>% 
 mutate(Percentage=as.numeric(Percentage)) %>% 
  #mutate(p)
mutate(cocoa_percent2= cut(Percentage, breaks=c( 25,50,75,100), 
       labels=c("Low cocoa","Medium cocoa","High cocoa"))) %>% 
  ggplot()+
  geom_violin(aes(rating,cocoa_percent2),color='brown',fill='brown2')+
  geom_boxplot(aes(rating,cocoa_percent2),color='black',alpha=0.6)+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 35, 
                              family = "montserrat",
                              face = "bold"),
    plot.subtitle = element_text(size=12,
                                 family = "montserrat",
                                 margin = margin(t=5,b=20)),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, family = "montserrat", face="bold")
  )+theme_minimal() +
  xlab('Rating')+ylab("Cocoa content")




#Title and subtitle

 
  
#by origin
export<-chocolate %>% 
  count(country_of_bean_origin) %>% 
  filter(country_of_bean_origin!="Blend") %>% 
  arrange(-n) %>% 
  top_n(10) %>%arrange(desc(n)) 


#by manusfacturer countires
imports <- chocolate %>% 
  count(company_location) %>% 
  arrange(-n) %>% 
  top_n(10)%>%arrange(desc(n)) 

choc2<-chocolate %>% 
  filter(company_location %in% imports$company_location) %>% 
  filter(country_of_bean_origin%in%export$country_of_bean_origin ) %>%
  count(country_of_bean_origin, company_location) %>% 
  mutate(origin = "origin") %>% 
  rename(total_number = "n")
brewer.pal(n = 4, name = "Dark2")
## aesthetics
color_palette  <- c("#7570B3" ,"#E7298A", "#66A61E" ,"#E6AB02",
                    "#1B9E77", "#D95F02" ,"#7570B3" ,"#E7298A",
                    "#8B9A46","#A6761D" ,"#666666")

plot<-ggplot(choc2,
       aes(axis1 = country_of_bean_origin,
           axis2 = company_location,
           y = total_number)) +
  geom_alluvium(aes(fill = country_of_bean_origin),
                curve_type = "sigmoid",
                width = 1/12,
                knot.pos = 0.4,
                alpha= 0.7)+
  geom_stratum(aes(fill = country_of_bean_origin),
               width = 1/6, color = "black", alpha=1, fill ="white",
               linetype="dotted") +
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  scale_x_continuous(breaks = 1:2, 
                     labels = c("Origin", "Destination country"),
                     position = "top")+
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            color = "black",
            family="montserrat",
            size = 2) +
  theme_minimal() +
  labs(y = "",
       x="")+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 35, 
                              family = "montserrat",
                              face = "bold"),
    plot.subtitle = element_text(size=12,
                                 family = "montserrat",
                                 margin = margin(t=5,b=20)),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, family = "montserrat", face="bold")
  )


  

#Title and subtitle
header<-ggdraw() +
  draw_text("Cocoa pathways from origin to destination", x= 0.49, y = 0.80, size = 24, family = "bebas") +
  draw_text("Chocolate rating relative to the cocoa content", 
            0.01, y = 0.35, size = 12, family = "montserrat", hjust = 0,lineheight = 2.1)  +

  draw_text("Chocolate's path from the raw cocoa \nbean to the chocolate bar from top exporter countries. \n The stroke width suggests the total chocolates \nexported and where it goes.", 
           x= 1.3, y = 0.35, size = 12, family = "montserrat", hjust = 0,lineheight = 1.1)  

#grid of images
#caption
caption<-ggdraw() +
  draw_text("Cacao pathways | Visualization: Vratika Chaudhary",
            size = 10, x = 0.07, y = 0.01, hjust = 0,
            color = "black", family = "montserrat") 


final_plot <- plot_grid(
  
 
  
header,
choc.hist,
  plot,caption,
rel_widths = c(4,4,3,1),
rel_heights = c(2.5,7.5,7.5,1),
nrow = 4)


