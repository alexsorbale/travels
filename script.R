#Общая за июль-август

flights <- read.csv("C:/Users/alexs/Desktop/Путешествия Р/flights.csv", sep=';', stringsAsFactors = FALSE)

library(maps)
library(ggplot2)
library(ggrepel)

russia<- map_data('world', 'Russia') 
Russia_summer<- ggplot() + geom_polygon(data = russia, aes(x=long, y = lat, group = group), fill='#efede1')+
  coord_fixed(2)+
  geom_point(data = flights, aes(x = lon, y = lat), col = "#970027") +
  geom_text_repel(data=flights, aes(x = lon, y = lat, label = airport), col = "black", size = 3, segment.color = NA) +
  theme(panel.background = element_rect(fill="white"), axis.line = element_blank(),
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank()
)+
geom_curve(data = flights, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y), col = "#b29e7d", size = 0.5) + 
  theme_void()
Russia_summer

ggsave("flights_map.png", dpi = 300)


#Камчатка:2-11 июля
 
library(rgdal) 
library(maptools)
library(rgeos)
library(mapproj)
library(sp)
library(ggrepel)

Russia<-readRDS("C:/Users/alexs/Desktop/Путешествия Р/gadm36_RUS_3_sp.rds") 
Kamchatka <- subset(Russia, NL_NAME_1 == "Камчатская край") 

Kamchatka_df <- fortify(Kamchatka, region = "NAME_2")
ggplot() + geom_map(data = Kamchatka_df, 
                    aes(map_id = id), fill="#efede1",  
                    map = Kamchatka_df, col = "#970027")+
  expand_limits(x = Kamchatka_df$long, y = Kamchatka_df$lat)+
  coord_map("polyconic")+
  theme(panel.background = element_rect(fill="white"), axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

Kamchatka@data$Days <- c('не был', 'не был', '2-9 июля', 'не был', 'не был', 'не был', 'не был', 'не был', 'не был', 'не был', '10 июля') 
Kamchatka_to_map <- as.data.frame(Kamchatka@data) 

ggplot() + geom_map(data = Kamchatka_to_map,
                    aes(map_id = NAME_2, fill = Days), 
                    map = Kamchatka_df) + 
  expand_limits(x = Kamchatka_df$long, y = Kamchatka_df$lat) +
  coord_map("polyconic") +
  theme(panel.background = element_rect(fill="white"), axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

library(scales) 
centroids_df <- as.data.frame(coordinates(Kamchatka))
names(centroids_df) <- c("Longitude", "Latitude") 
Kamchatka_to_map$Longitude <- centroids_df$Longitude
Kamchatka_to_map$Latitude <- centroids_df$Latitude 
Kamchatka_to_map[5, 19] = 55.10
Kamchatka_to_map[10, 18] = 156.70
Kamchatka_to_map[10, 19] = 51.33852
Kamchatka_to_map[11, 19] = 56.77066
Kamchatka_to_map[2, 19] = 55.90307
library(RColorBrewer)
library(dplyr)
groups <- Kamchatka_to_map %>% 
  pull(NL_NAME_2) %>% 
  unique() %>% 
  length()
colors2 <- rep(c("aquamarine1", "slategray1", "#efede1"), length.out = groups)
Kamchatka_to_map$Days <- factor(Kamchatka_to_map$Days, levels = c("2-9 июля", "10 июля", "не был"))

Kamchatka_final<-ggplot() + geom_map(data = Kamchatka_to_map,
                    aes(map_id = NAME_2, fill = Days),
                    colour = "gray",
                    map = Kamchatka_df) + 
  expand_limits(x = Kamchatka_df$long, y = Kamchatka_df$lat) +
  geom_text(data = Kamchatka_to_map, aes(label = NL_NAME_2, x = Longitude, y = Latitude), size = 3, inherit.aes = FALSE) +
  coord_map("polyconic") +
  theme (
    axis.line = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )+
  scale_fill_manual(name = "Дни", values = colors2)

Kamchatka_final

kamchatka_places <- read.csv("C:/Users/alexs/Desktop/Путешествия Р/Kamchatka.csv", sep=';', stringsAsFactors = FALSE)
Kamchatka_to_map[4, 2] = 160.00

attach(kamchatka_places)

 
group = as.factor(c(2, 2, 2, 1, 3, 3, 2, 3, 1, 2))
kamchatka_places_1 = cbind(kamchatka_places,group)

Kamchatka_points<-Kamchatka_final+geom_point(data=kamchatka_places_1, aes(x = lon, y = lat, shape=group, color=group))+
geom_text_repel(data=kamchatka_places, aes(x = lon, y = lat, label = place), col = "#211906", size = 2.3, segment.color = NA)+
scale_shape_manual(name = "Обозначения", values = c(16, 17, 8), labels = c("Населённые пункты", "Вулканы", "Заповедники и парки"))+
scale_color_manual(name = "Обозначения", values = c('red', '#970027', 'darkgreen'), labels = c("Населённые пункты", "Вулканы", "Заповедники и парки"))
Kamchatka_points

ggsave("Kamchatka_map.png", width = 9, height = 8, dpi = 350, units = "in", device='png')


