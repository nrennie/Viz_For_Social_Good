library(tidyverse)
library(cowplot)
library(readxl)
library(waffle)
library(patchwork)
library(rcartocolor)
library(packcircles)
library(showtext)
library(ggwordcloud)

#add fonts
font_add_google(name = "Libre Franklin", family = "libre") 
showtext_auto()

#read in data
responses <- tibble(read_excel("Visualize_Our_Community/Data/Form Responses 2021 Pivoted Excel.xlsx"))

#### WORD CLOUD ####
word_data <- responses %>%
  filter(Questions == "What do you like about volunteering with VFSG?", 
         !is.na(Answers)) %>%
  pull(Answers) %>%
  str_split(" ") %>%
  unlist()

#process text data
word_corpus = word_data %>% 
  tm::VectorSource()%>% 
  tm::Corpus()

word_corpus_clean =  word_corpus %>% 
  tm_map(FUN = content_transformer(tolower)) %>% # Convert the text to lower case
  tm_map(FUN = removeNumbers) %>% # Remove numbers
  tm_map(removeWords, stopwords("english")) %>% # Remove english common stopwords
  tm_map(removeWords, c("will", "let", "ring", "looking")) %>%   # Remove words 
  tm_map(removePunctuation) %>%   # Remove punctuations
  tm_map(stripWhitespace) 

word_corpus_clean_tb =  word_corpus_clean %>% 
  tm::TermDocumentMatrix() %>% 
  as.matrix() %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  dplyr::rename(word = 1) %>%
  pivot_longer(cols="1":"861") %>%
  filter(value != 0) %>%
  group_by(word) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

p <- ggplot(data = word_corpus_clean_tb, 
       aes(label = word, size = n, col = n)) + 
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      family="libre")+
  scale_colour_carto_c(name = "", palette = "Teal", direction = 1) +
  scale_size_area(max_size = 14)+
  labs(
    x="",
    y="", 
    title="What do you like about volunteering with VFSG?\n\n", 
    subtitle="Viz For Social Good volunteers like helping others and developing their data\nvisualization skills at the same time.") +
  theme(panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.background = element_rect(fill = "grey90", colour="grey90"),
        legend.background = element_rect(fill = "grey90"),
        legend.key = element_rect(fill = "grey90", colour="grey90"), 
        legend.text =  element_text(colour = "grey30", size=10, hjust = 0, family="libre"),
        plot.subtitle = element_text(colour = "grey30", size=10, hjust = 0, family="libre"),
        plot.title = element_text(colour = "grey30", size=12, face="bold", hjust = 0, family="libre"),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.5, "lines"))
p
ggsave(p, filename="Visualize_Our_Community/Viz/word.pdf", width=7, height=6, unit="in", device=cairo_pdf)

#### VOLUNTEERS ####

#education - waffle
education_data <- responses %>%
  filter(Questions == "What is the highest level of education you have completed?") %>%
  group_by(Answers) %>%
  summarise(n = n()) %>%
  mutate(education = factor(Answers, 
                      levels=c("High school diploma", "Some college", "Bachelor's degree (undergraduate)", "Master's degree (Graduate)", "Ph.D", "Other"), 
                      labels=c("High school", "College", "Bachelor's", "Master's", "Ph.D", "Other"))) %>%
  arrange(education)
p_education <- ggplot(education_data, aes(fill = education, values = n)) +
  geom_waffle(color = "grey90", size = .1, n_rows = 16, flip = TRUE) +
  scale_fill_manual("", values=c("#2f4656", 
                                 "#4a6e87", 
                                 "#4f90a6", 
                                 "#85c4c9", 
                                 "#d3eae7", 
                                 "grey80")) +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 16, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  labs(
    x="",
    y="", 
    title="What is the highest level of education\nyou have completed?\n", 
    subtitle="Most of the Viz For Social Good community is\neducated to Master's degree level.") +
  theme(panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.background = element_rect(fill = "grey90", colour="grey90"),
        legend.background = element_rect(fill = "grey90"),
        legend.key = element_rect(fill = "grey90", colour="grey90"), 
        legend.text =  element_text(colour = "grey30", size=10, hjust = 0, family="libre"),
        plot.subtitle = element_text(colour = "grey30", size=10, hjust = 0, family="libre"),
        plot.title = element_text(colour = "grey30", size=12, face="bold", hjust = 0, family="libre"),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.5, "lines"))
p_education
ggsave(p_education, filename="Visualize_Our_Community/Viz/education.pdf", width=3.5, height=4.5, unit="in", device=cairo_pdf)


#age - waffle
age_data <- responses %>%
  filter(Questions == "What is your age?") %>%
  group_by(Answers) %>%
  summarise(n = n()) %>%
  mutate(age = factor(Answers, 
         levels=c("18 - 25 years old", "26 - 35 years old", "36 - 50 years old", ">50 years old", "Prefer not to answer"), 
         labels=c("18 - 25", "26 - 35", "36 - 50", "> 50", "Prefer not\nto answer"))) %>%
  arrange(age)
p_age <- ggplot(age_data, aes(fill = age, values = n)) +
  geom_waffle(color = "grey90", size = .1, n_rows = 16, flip = TRUE) +
  scale_fill_manual("", values=c("18 - 25"="#4a6e87", 
                                 "26 - 35"="#4f90a6", 
                                 "36 - 50"="#85c4c9", 
                                 "> 50"="#d3eae7", 
                                 "Prefer not\nto answer"="grey80")) +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 16, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  labs(
    x="",
    y="", 
    title="What is your age?\n\n", 
    subtitle="The most common age bracket in the Viz\nFor Social Good community is 26-35 years.") +
  guides(fill=guide_legend(nrow=2)) +
  theme(panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.background = element_rect(fill = "grey90", colour="grey90"),
        legend.background = element_rect(fill = "grey90"),
        legend.key = element_rect(fill = "grey90", colour="grey90"), 
        legend.text =  element_text(colour = "grey30", size=10, hjust = 0, family="libre"),
        plot.subtitle = element_text(colour = "grey30", size=10, hjust = 0, family="libre"),
        plot.title = element_text(colour = "grey30", size=12, face="bold", hjust = 0, family="libre"),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.5, "lines"))
p_age
ggsave(p_age, filename="Visualize_Our_Community/Viz/age.pdf", width=3.5, height=4.5, unit="in", device=cairo_pdf)

#country - map
country_data <- responses %>%
  filter(Questions == "What country do you live in?", 
         !is.na(Answers)) %>%
  group_by(Answers) %>%
  summarise(n = n())
country_data[37,1] <- "USA"
country_data[36,1] <- "UK"
country_data[16,1] <- "South Korea"
colnames(country_data)[1] <- "region"
world <- map_data("world")
plot_data <- left_join(world, country_data, by="region") %>%
  filter(region != "Antarctica")
p_country <- ggplot(plot_data) +
  geom_map(data = plot_data, map = plot_data,
           aes(long, lat, map_id = region, fill=n),
           color = NA, size = 0.1) +
  scale_fill_carto_c(name = "", palette = "Teal", direction = 1, na.value="grey80", limits=c(0,100)) +
  labs(title = "What country do you live in?\n\n", 
       subtitle = "Respondents come from 38 different countries,\nwith most (39%) from the USA.\n\n") +
  theme(panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.background = element_rect(fill = "grey90", colour="grey90"),
        legend.background = element_rect(fill = "grey90"),
        legend.key = element_rect(fill = "grey90", colour="grey90"), 
        legend.text =  element_text(colour = "grey30", size=10, hjust = 0.5, family="libre"),
        plot.subtitle = element_text(colour = "grey30", size=10, hjust = 0, family="libre"),
        plot.title = element_text(colour = "grey30", size=12, face="bold", hjust = 0, family="libre"),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.3, 0.8, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.5, "lines"))
p_country
ggsave(p_country, filename="Visualize_Our_Community/Viz/country.pdf", width=3.5, height=4.5, unit="in", device=cairo_pdf)


#industry - bubble
industry_data <- responses %>%
  filter(Questions == "What industry do you work in?", 
         !is.na(Answers)) %>%
  mutate(answers = recode(Answers, 
                          "Trades and Personal Services" = "Other", 
                          "Legal Services" = "Other", 
                          "Pharmaceuticals" = "Other",
                          "Government and Public Administration" = "Government",
                          "Retired or unemployed" = "Retired/ unemployed"
                          )) %>%
  group_by(answers) %>%
  summarise(n = n()) 
packing <- circleProgressiveLayout(industry_data$n, sizetype='area')
data <- cbind(industry_data, packing)
plot_data_i <- circleLayoutVertices(packing, npoints=50)
plot_data_i$n <- rep(industry_data$n, each=51)

p_industry <- ggplot() +
  geom_polygon(data = plot_data_i, aes(x, y, group = id, fill=n), colour = "black", alpha = 0.6) +
  scale_fill_carto_c(palette="Teal") +
  labs(title = "What industry do you work in?\n", 
       subtitle = "Most people work in IT and Software industries,\nwith consulting coming second.") +
  geom_text(data = data, aes(x, y, size=n, label = str_wrap(answers, 3)), family="libre") +
  scale_size_continuous(range = c(1.2,4)) +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.background = element_rect(fill = "grey90", colour="grey90"),
        legend.background = element_rect(fill = "grey90"),
        legend.key = element_rect(fill = "grey90", colour="grey90"), 
        legend.text =  element_text(colour = "grey30", size=10, hjust = 0.5, family="libre"),
        plot.subtitle = element_text(colour = "grey30", size=10, hjust = 0, family="libre"),
        plot.title = element_text(colour = "grey30", size=12, face="bold", hjust = 0, family="libre"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.5, "lines"))
p_industry
ggsave(p_industry, filename="Visualize_Our_Community/Viz/industry.pdf", width=3.5, height=4.5, unit="in", device=cairo_pdf)


