library(tidyverse)
library(readxl)
library(usefunc)
library(purrr)
library(patchwork)
library(cowplot)
library(emojifont)
library(magick)

# read in data
health <- read_xlsx("TEWWY/Data/JUNE-OCT Patient Health Questionnaire-9 (PHQ-9) Data Pivoted.xlsx")

# prep data
input_data <- health %>%
  filter(!is.na(Gender), 
         str_detect(Questions, 
                    "Over the last 2 weeks, how often have you been bothered by the following problems?"), 
         str_detect(Answers, 
                    " /")) %>% 
  select(Gender, Questions, Answers) %>% 
  mutate(score = as.numeric(str_sub(Answers, 1, 1))) %>% 
  filter(!is.na(score)) %>% 
  mutate(YN = score > 0) %>% 
  select(Gender, Questions, YN) %>% 
  mutate(Questions = recode(Questions, 
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Feeling bad about yourself - or that you’ 1" = "Feeling bad about yourself?", 
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Feeling down, depressed or hopeless) (Score" = "Feeling down, depressed, or hopeless?",
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Feeling tired or having little energy) (Sco" = "Feeling tired or having little energy?", 
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Little interest or pleasure in doing thin 1" = "Little interest or pleasure in doing things?", 
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Moving or speaking so slowly that other p 1" = "Moving or speaking so slowly that other people could have noticed?", 
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Poor appetite or overeating) (Score)" = "Poor appetite or overeating?", 
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Thoughts that you would be better off dea 1" = "Thoughts that you would be better off dead?", 
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Trouble concentrating on things, such as  1" = "Trouble concentrating on things, such as reading the newspaper or watching television?", 
                                "Over the last 2 weeks, how often have you been bothered by the following problems? (Trouble falling asleep, staying asleep, o 1" = "Trouble falling asleep, staying asleep, or sleeping too much?"))

plot_data <- input_data %>% 
  group_by(Questions, Gender) %>% 
  mutate(n = n(), 
         Yes = sum(YN)) %>% 
  filter(YN == TRUE) %>% 
  distinct() %>% 
  mutate(Yes = round(100*Yes/n), 
         Gender = paste0(Gender, " (", Yes, "%)")) %>% 
  select(Gender, Questions, Yes) %>% 
  mutate(No = 100 - Yes) %>% 
  pivot_longer(3:4)

# plotting function
plot_question <- function(question){
  # filter data
  p_data <- plot_data %>% filter(Questions == question) 
  # waffle plot data
  waffle_data <- rep_df(expand.grid(x = rep(1:10), y = rep(1:10)), length(unique(p_data$Gender))) %>%
    mutate(gender = rep(unique(p_data$Gender), each = 100),
           label = fontawesome('fa-frown-o'),
           type = rep(p_data$name, times = p_data$value))
  # plot
  ggplot() +
    geom_text(data = waffle_data,
              mapping = aes(x = x,
                            y = y,
                            label = label,
                            colour = type),
              family='fontawesome-webfont', size = 6) +
    facet_wrap(~gender, nrow = 1) +
    scale_colour_manual("", values = c("#bababa", "#2b909c")) +
    labs(x = "",
         y = "") +
    ggtitle(str_wrap_break(question, 60)) +
    theme_minimal() +
    scale_y_reverse() +
    coord_fixed() +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.background = element_rect(fill = "#e7e7e7", colour = "#e7e7e7"),
          plot.background = element_rect(fill = "#e7e7e7", colour = "#e7e7e7"),
          legend.position="none",
          strip.background =element_rect(fill="#e7e7e7", colour ="#e7e7e7"),
          strip.text = element_text(colour = '#404040', family="sans", size=22),
          plot.title = element_text(colour = "#2b909c", size=22, hjust = 0, vjust = -1, face = "bold", family="sans", lineheight = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank())
}

# map over questions
questions <- plot_data %>% 
  pull(Questions) %>% 
  unique()
all_plots <- map(.x = questions, .f = ~plot_question(.x))

# define subtitle 
st <- str_wrap_break("Tap Elderly Women's Wisdom for Youth (TEWWY) is an organisation which taps the wisdom of grandmothers to serve the mental health needs of vulnerable populations . TEWWY’s Mental Health Intervention Programs (MHIPs) have reached 17,975 people through its initiatives in Dar-es-Salaam’s 5 municipalities and in Kilwa, Lindi rural community. \n\nNearly 1 in 10 people have a mental health disorder, but only 1% of the global health workforce provides mental health care. 1,397 patients answered questions about their mental health, giving scores from 0 to 3 on how often, over the last 2 weeks, they have been bothered by different problems. A score of 0 indicates that the patient answered `No`, whilst a score above 0 indicates the patient answered `Yes`. The inforgraphics below indicate the percentage of people answering `Yes` to each questions, split by male and female patients.\n\nN. Rennie | Data: Tap Elderly Women's Wisdom for Youth\n\n\n", 140)

# read image
vfsg_logo <- image_read("vfsg_logo_dark.png")

# plot
p <- wrap_plots(all_plots, ncol = 3, nrow = 3) + 
  plot_annotation(
    title = "\n\n",
    subtitle = st
  ) &
  theme(plot.background = element_rect(fill = "#e7e7e7", colour = "#e7e7e7"),
        panel.background = element_rect(fill = "#e7e7e7", colour = "#e7e7e7"), 
        plot.subtitle = element_text(colour = "#e7e7e7", size=22, hjust = 0, vjust = 0, 
                                     family="sans", lineheight = 0.5, margin = margin(20, 20, 10, 20)))

# add logo and title
q <- ggdraw() + 
  draw_plot(p, 0, 0, 1, 1) +
  draw_image(vfsg_logo, x = 0.335, y = 0.45, scale = 0.3) +
  draw_label(x = 0.03, y = 0.95, fontfamily = "sans", fontface = "bold", size = 70, 
             colour = "#2b909c", hjust = 0, 
             label = "Tap Elderly Women's Wisdom for Youth") +
  draw_label(x = 0.03, y = 0.83, fontfamily = "sans", size = 22, 
             colour = "black", hjust = 0, lineheight = 0.5,
             label = st) 
  
ggsave(q, filename = "TEWWY/viz.png", unit = "in", width = 10, height = 11)

