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



# Load US hexagon file
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
#Tidy names
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

#####Raw data
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

colony <- colony %>% filter(year != "6/")
colonies_2021 <- colony %>%
  group_by(state) %>% 
  filter(year == max(year) & months == "April-June")

colonies_2021 <- colonies_2021 %>% 
  mutate(colony_added_pct = colony_added/colony_max*100) %>% 
  mutate(net_colony_change = colony_added_pct - colony_lost_pct)

plot_df <- colonies_2021 %>% 
  select(state, net_colony_change)

spdf_fortified <- spdf_fortified %>%
  left_join(. , plot_df, by=c("id"="state")) 


bee_change <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = net_colony_change/100, x = long, y = lat, group = group) , size=0, alpha=0.9, color = "gray20") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="black", size=3, alpha=0.6) +
  theme_void() +
  ggtitle( "Net bee colony change (April-June 2021)" ) +
  scale_fill_gradient2(high = "springgreen4", labels= percent) +
  guides(fill = guide_colorbar( barwidth = unit(20, "lines"), 
                                barheight = unit(.3, "lines"))) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

ggsave("bee_change.png")