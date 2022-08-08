#### Intergenerational Conversation Webinar ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-08-07 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)


#### Load data ####
intergen <- read_csv(here("Data", "intergenerational_convo.csv"))


#### View data ####
view(intergen)


#### Data analysis ####
intergen_clean <- intergen %>%
  select(participants, overall_rating, speaker_rating, advertisement, event_timing, audience_learning) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices)
view(intergen_clean)

advertisement_data <- intergen_clean %>%
  count(advertisement_choices)
view(advertisement_data)

data <- intergen_clean %>%
  mutate(overall_rating = factor(overall_rating,
                               levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much")))
view(data)

overall_rating <- data %>%
  count(overall_rating)

speaker_rating <- data %>%
  count(speaker_rating)

event_timing <- data %>%
  count(event_timing)

audience_data <- data %>%
  count(audience_learning)


#### plot ####
colorpal<- c("#F2D0A4", "#5DD39E", "#348AA7", "#525174", "#513B56", "#595A9F", "#9091AA")

advertisement_data %>%
  ggplot(aes(x = reorder(advertisement_choices, -n), y = n, fill = advertisement_choices)) +
  geom_col() +
  labs(title = "How did respondants learn about the webinar on intergenerational conversation on activism?",
       x = "Responses",
       y = "Counts") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 10,
                                  face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "#000000"),
        axis.text.y = element_text(color = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = colorpal) +
  guides(fill = FALSE)
ggsave(here("Output", "2021-2022", "intergenerational", "advertisement.png"), width = 8, height = 5)


overall_rating %>%
  ggplot(aes(x = reorder(overall_rating, -n), y = n, fill = overall_rating)) +
  geom_col() +
  labs(title = "How did respondants rate the event overall?",
       x = "Responses",
       y = "Counts") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 10,
                                  face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "#000000"),
        axis.text.y = element_text(color = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#595A9F", "#525174", "#513B56")) +
  guides(fill = FALSE)
ggsave(here("Output","2021-2022", "intergenerational", "event_rating.png"), width = 6, height = 5)

speaker_rating %>%
  ggplot(aes(x = reorder(speaker_rating, -n), y = n, fill = speaker_rating)) +
  geom_col() +
  labs(title = "How did respondants rate the speakers?",
       x = "Responses",
       y = "Counts") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 10,
                                  face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "#000000"),
        axis.text.y = element_text(color = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#595A9F", "#525174", "#513B56")) +
  guides(fill = FALSE)
ggsave(here("Output","2021-2022", "intergenerational", "speaker_rating.png"), width = 6, height = 5)

event_timing %>%
  ggplot(aes(x = reorder(event_timing, n), y = n, fill = event_timing)) +
  geom_col() +
  labs(title = "How convenient was the event for the respondents?",
       x = "Responses",
       y = "Counts") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 10,
                                  face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "#000000"),
        axis.text.y = element_text(color = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#595A9F", "#525174", "#513B56")) +
  guides(fill = FALSE)
ggsave(here("Output", "2021-2022", "intergenerational", "convenience_of_event.png"), width = 6, height = 5)

audience_data %>%
  ggplot(aes(x = reorder(audience_learning, -n), y = n, fill = audience_learning)) +
  geom_col() +
  labs(title = "Did the respondents learn from the event?",
       x = "Responses",
       y = "Counts") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 10,
                                  face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "#000000"),
        axis.text.y = element_text(color = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#595A9F", "#525174", "#513B56")) +
  guides(fill = FALSE)
ggsave(here("Output","2021-2022", "intergenerational", "audience_learning.png"), width = 6, height = 5)
