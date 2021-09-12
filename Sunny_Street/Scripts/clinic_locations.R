library(tidyverse)
library(cowplot)
library(readxl)
library(ggdist)
library(extrafont)
library(patchwork)
library(zoo)
library(sf)

#read in data
detailed_stats <- tibble(read_excel("Data/detailed_stats.xlsx"))
AUS_shp <- read_sf("./Shapefiles/LGA11aAust.shp")

#plot map
clinic_locations <- ggplot() +
  geom_sf(data=AUS_shp, colour="#FEF7D2", fill="#ed0c6e") +
  geom_point(data=filter(detailed_stats,Program != "COVID19 Respiratory Clinics"), 
             mapping=aes(x=Longitude,y=Latitude, colour=Program), size=1) +
  scale_color_manual("Program", values=c("Fraser Coast"="#21958C", "Brisbane"="black", "Gold Coast"="#fddb22", "Sunshine Coast"="#BEBEBE")) +
  coord_sf(xlim = c(151.5, 154), ylim = c(-28,-25), expand=F) +
  theme(plot.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        panel.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        legend.background = element_rect(fill = "#FEF7D2"),
        legend.key = element_rect(fill = "#FEF7D2", colour="#FEF7D2"), 
        legend.key.size = unit(0.2, 'cm'),
        legend.text =  element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        legend.position="right",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(0.1, 2, 0.1, 0.1), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
clinic_locations

#save plot as pdf
setwd("../Viz")
ggsave(clinic_locations, filename = "clinic_locations.pdf",  device=cairo_pdf, bg = "transparent", height=75, width=100, unit="mm")

