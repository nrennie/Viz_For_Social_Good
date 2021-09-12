library(tidyverse)
library(cowplot)
library(readxl)
library(extrafont)
library(ggalluvial)

#read in data
detailed_stats <- tibble(read_excel("Data/detailed_stats.xlsx"))

#prep data
sankey_data <- detailed_stats[,c(2,13,14,15,17,26)] %>%
  filter(Program != "COVID19 Respiratory Clinics") %>%
  group_by(Program) %>%
  summarise("Medical"=sum(`Medical Consults`), 
            "Nurse Practitioner"=sum(`Nurse Practitioner Consults`),
            "Nursing/Paramedic"=sum(`Nursing/Paramedic Consults`), 
            "Allied Health"=sum(`Allied Health`), 
            "Telehealth"=sum(`Telehealth consults that happened at Clinic`)) %>%
  pivot_longer(names_to="consult", values_to = "number", cols=2:6)
sankey_data

#sankey chart
p <- ggplot(as.data.frame(sankey_data),
            aes(y = number, axis1 = Program, axis2 = consult)) +
  geom_stratum(width = 1/12, fill = "transparent", color = "black", alpha = 0.6, lwd=0.4) +
  geom_alluvium(aes(fill = Program), width = 1/12) +
  scale_fill_manual("", values=c("Fraser Coast"="#21958C", "Brisbane"="#ed0c6e", "Gold Coast"="#fddb22", "Sunshine Coast"="#BEBEBE")) +
  labs(x="", y="") +
  guides(fill=guide_legend(ncol=4)) +
  theme(plot.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        panel.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        legend.background = element_rect(fill = "#FEF7D2"),
        legend.key = element_rect(fill = "#FEF7D2", colour="#FEF7D2"), 
        legend.key.size = unit(0.2, 'cm'),
        legend.text =  element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(0.1, 1, 0.1, 0.1), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "#ed0c6e", size=12, hjust = 1, family="Gill Sans MT Condensed"),
        plot.subtitle = element_text(colour = "#ed0c6e", size=12, hjust = 0.5, family="Gill Sans MT Condensed"))
p

#save plot as pdf
setwd("../Viz")
ggsave(p, filename = "consultation_types.pdf",  device=cairo_pdf, bg = "transparent", height=80, width=96, unit="mm")

