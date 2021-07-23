Russia<-readRDS("C:/Users/alexs/Desktop/Ïóòåøåñòâèÿ Ð/gadm36_RUS_3_sp.rds") 
Kaliningrad <- subset(Russia, NL_NAME_1 == "Êàëèíèíãðàäñêàÿ îáëàñòü") 

Kaliningrad_df <- fortify(Kaliningrad, region = "NAME_2")
ggplot() + geom_map(data = Kaliningrad_df, 
                    aes(map_id = id), fill="#efede1",  
                    map = Kaliningrad_df, col = "#970027")+
  expand_limits(x = Kaliningrad_df$long, y = Kaliningrad_df$lat)+
  coord_map("polyconic")+
  theme(panel.background = element_rect(fill="white"), axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

Kaliningrad@data$Days <- c('íå áûë', 'íå áûë', 'íå áûë', '16-21 èþëÿ', 'íå áûë', 'íå áûë', 'íå áûë', 'íå áûë', 'íå áûë', 'íå áûë', 'íå áûë', 'íå áûë', 'íå áûë', 'íå áûë', 'íå áûë','íå áûë','18-19 èþëÿ') 
Kaliningrad_to_map <- as.data.frame(Kaliningrad@data) 

ggplot() + geom_map(data = Kaliningrad_to_map,
                    aes(map_id = NAME_2, fill = Days), 
                    map = Kaliningrad_df) + 
  expand_limits(x = Kaliningrad_df$long, y = Kaliningrad_df$lat) +
  coord_map("polyconic") +
  theme(panel.background = element_rect(fill="white"), axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

library(scales) 
centroids_df <- as.data.frame(coordinates(Kaliningrad))
names(centroids_df) <- c("Longitude", "Latitude") 
Kaliningrad_to_map$Longitude <- centroids_df$Longitude
Kaliningrad_to_map$Latitude <- centroids_df$Latitude 
Kaliningrad_to_map[4, 19] = 54.78389
Kaliningrad_to_map[4, 18] = 20.67834

library(RColorBrewer)
library(dplyr)
groups <- Kaliningrad_to_map %>% 
  pull(NL_NAME_2) %>% 
  unique() %>% 
  length()
colors2 <- rep(c("aquamarine1", "slategray1", "#efede1"), length.out = groups)
Kaliningrad_to_map$Days <- factor(Kaliningrad_to_map$Days, levels = c("16-21 èþëÿ", "18-19 èþëÿ", "íå áûë"))

Kaliningrad_final<-ggplot() + geom_map(data = Kaliningrad_to_map,
                                       aes(map_id = NAME_2, fill = Days),
                                       colour = "gray",
                                       map = Kaliningrad_df) +
  expand_limits(x = Kaliningrad_df$long, y = Kaliningrad_df$lat) +
  geom_text(data = Kaliningrad_to_map, aes(label = NL_NAME_2, x = Longitude, y = Latitude), size = 3, inherit.aes = FALSE) +
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
  scale_fill_manual(name = "Äíè", values = colors2)


Kaliningrad_final

kaliningrad_places <- read.csv("C:/Users/alexs/Desktop/Ïóòåøåñòâèÿ Ð/Kaliningrad.csv", sep=';', stringsAsFactors = FALSE)

attach(kaliningrad_places)


group = as.factor(c(2, 2, 2, 2, 2, 1, 3))
kaliningrad_places_1 = cbind(kaliningrad_places,group)

Kaliningrad_points<-Kaliningrad_final+geom_point(data=kaliningrad_places_1, aes(x = lon, y = lat, shape=group, color=group))+
  geom_text_repel(data=kaliningrad_places, aes(x = lon, y = lat, label = place), col = "#211906", size = 2.3, segment.color = NA)+
  scale_shape_manual(name = "Îáîçíà÷åíèÿ", values = c(8, 16, 10), labels = c("Çàïîâåäíèêè è ïàðêè","Íàñåë¸ííûå ïóíêòû", "Óêðåïëåíèÿ"))+
  scale_color_manual(name = "Îáîçíà÷åíèÿ", values = c('darkgreen', '#970027', 'black'), labels = c("Çàïîâåäíèêè è ïàðêè","Íàñåë¸ííûå ïóíêòû", "Óêðåïëåíèÿ"))
Kaliningrad_points

ggsave("Kaliningrad_map.png", width = 9, height = 8, dpi = 350, units = "in", device='png')
