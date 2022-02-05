library(tidyverse)
library(readxl)
library(sf)
library(cowplot)
library(magick)
library(rcartocolor)
library(usefunc)
library(glue)

# read in data
map_data <- tibble(read_excel("Build_Up_Nepal/Data/nepal_data.xlsx"))
nepal_sf <- read_sf("Build_Up_Nepal/Data/nepal_shp/npl_admbnda_adm1_nd_20201117.shp")

# read images
img <- image_read("Build_Up_Nepal/Images/bg.jpg")
logo <- image_read("Build_Up_Nepal/Images/logo.png")
vfsg_logo <- image_read("vfsg_logo_dark.png")

# data for table
sum_data <- map_data %>%
  summarise(across(c("CO2 saved", "Houses built", "Total jobs", "Schools"), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(num_proj = nrow(map_data))

# plot
p <- ggplot() +
  geom_sf(data=nepal_sf, colour="black", fill="black", alpha=0.4, size = 0.2) +
  geom_point(data=filter(map_data, Longitude != 0), 
             mapping=aes(x=Longitude, y=Latitude), 
             size = 1, colour = "white") +
  scale_color_carto_d(palette = "Safe") +
  labs(title = "Build Up Nepal", 
       subtitle = str_wrap_break("Build up Nepal is a social business dedicated to building resilient communities and fighting poverty in rural areas of Nepal.\n\nN. Rennie | Data: Build Up Nepal", 80)) +
  theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.background = element_rect(fill = "transparent", colour="transparent"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="mono"),
        plot.title = element_text(colour = "black", size=15, face="bold", hjust = 0, vjust = 3, family="mono"),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.5, "lines"))
p

q <- ggdraw() + 
  draw_image(img) + 
  draw_plot(p, 0, 0, 1, 1) +
  draw_image(vfsg_logo, x = -0.35, y = -0.43, scale = 0.3) +
  draw_image(logo, x = 0.41, y = -0.43, scale = 0.15) +
  draw_label(x = 0.6, y = 0.6, fontfamily = "mono", size = 12, colour = "black", hjust = 0, lineheight = 1.2,
             label = glue("{sum_data$num_proj} Projects\n{sum_data$`Houses built`} Houses built\n{sum_data$Schools} Schools\n{sum_data$`Total jobs`} Jobs created\n{round(sum_data$`CO2 saved`)} Tonnes of CO2 saved")) +
  theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.background = element_rect(fill = "transparent", colour="transparent"))
q






