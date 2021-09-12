library(tidyverse)
library(cowplot)
library(readxl)
library(ggdist)
library(extrafont)
library(patchwork)
library(zoo)

#read in data
patient_data <- tibble(read_excel("Data/patient_data.xlsx"))

#age
age_plot_data <- patient_data[,c(1,5)] %>% mutate(age = Year - `Year of birth`) %>% filter(`Year of birth` >= 1901) %>%
  mutate(bin = cut(age, breaks = c(-Inf,18,40,65,Inf), labels=c("< 18", "18-40", "40-65", "> 65"))) %>%
  group_by(bin, Year) %>%
  summarise(n=n())
age_plot <- ggplot(data=age_plot_data, mapping=aes(x=factor(Year),y=n, fill=bin)) + 
  guides(fill=guide_legend(ncol=2)) +
  geom_bar(position="fill", stat="identity") +
  labs(x="", y="Percentage of patients\n") +
  scale_fill_manual("", values=c("< 18"="#21958C", "18-40"="#ed0c6e", "40-65"="#fddb22", "> 65"="#BEBEBE"), 
                    breaks=c("< 18", "18-40", "40-65", "> 65")) +
  theme(plot.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        panel.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        legend.background = element_rect(fill = "#FEF7D2"),
        legend.key = element_rect(fill = "#FEF7D2", colour="#FEF7D2"), 
        legend.text =  element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        legend.position="top",
        legend.key.size = unit(0.2, 'cm'),
        axis.text = element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        axis.title = element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
age_plot 
ggsave(age_plot, filename = "age.pdf",  device=cairo_pdf, bg = "transparent", height=52, width=52, unit="mm")

#gender
gender_plot_data <- patient_data
gender_plot_data$Gender <- replace_na(gender_plot_data$Gender, "Unknown")
gender_plot_data %<>% group_by(factor(Year), Gender) %>% summarise(n=n())
gender_plot <- ggplot(data=gender_plot_data, mapping=aes(x=`factor(Year)`,y=n, fill=Gender)) + 
  guides(fill=guide_legend(ncol=2)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual("", values=c("Male"="#ed0c6e", "Female"="#21958C", Other="#fddb22", "Unknown"="#BEBEBE"), breaks=c("Female", "Male", "Other", "Unknown")) +
  labs(x="", y="Percentage of patients\n") +
  theme(plot.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        panel.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        legend.background = element_rect(fill = "#FEF7D2"),
        legend.key = element_rect(fill = "#FEF7D2", colour="#FEF7D2"), 
        legend.text =  element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        legend.position="top",
        legend.key.size = unit(0.2, 'cm'),
        axis.text = element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        axis.title = element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
gender_plot
ggsave(gender_plot, filename = "gender.pdf",  device=cairo_pdf, bg = "transparent", height=52, width=52, unit="mm")

#ethnicity 
ethnicity_plot_data <- patient_data
ethnicity_plot_data$Ethnicity <- replace_na(ethnicity_plot_data$Ethnicity, "Unknown")
ethnicity_plot_data$Ethnicity <- recode(ethnicity_plot_data$Ethnicity, 
                                        "Not provided"="Unknown", "Torres Strait Islander"="Aboriginal/Torres Strait Islander", "Aboriginal"="Aboriginal/Torres Strait Islander")
ethnicity_plot_data$Ethnicity[which(ethnicity_plot_data$Ethnicity %notin% c("Unknown", "Other", "Aboriginal/Torres Strait Islander", "Australian"))] <- "Other"
ethnicity_plot_data %<>% group_by(factor(Year), Ethnicity) %>% summarise(n=n())
ethnicity_plot <- ggplot(data=ethnicity_plot_data, mapping=aes(x=`factor(Year)`,y=n, fill=Ethnicity)) + 
  guides(fill=guide_legend(ncol=2)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual("", values=c("Aboriginal/Torres Strait Islander"="#21958C", "Australian"="#ed0c6e", Other="#fddb22", "Unknown"="#BEBEBE"), breaks=c("Aboriginal/Torres Strait Islander", "Australian", "Other", "Unknown")) +
  labs(x="", y="Percentage of patients\n") +
  theme(plot.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        panel.background = element_rect(fill = "#FEF7D2", colour="#FEF7D2"),
        legend.background = element_rect(fill = "#FEF7D2"),
        legend.key = element_rect(fill = "#FEF7D2", colour="#FEF7D2"), 
        legend.text =  element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        legend.position="top",
        legend.key.size = unit(0.2, 'cm'),
        axis.text = element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        axis.title = element_text(colour = "#ed0c6e", size=8, family="Californian FB"),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ethnicity_plot
ggsave(ethnicity_plot, filename = "ethnicity.pdf",  device=cairo_pdf, bg = "transparent", height=52, width=52, unit="mm")
