#### Teach-in on Asian Hate ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-08-07 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)

#### Load data ####
teach_in <-read_csv(here("Data", "anti_asian_teach_in.csv"))

#### View data ####
view(teach_in)

#### Data analysis ####
teach_in_clean <- teach_in %>%
  select(participants, event_rating, speaker_rating, advertisement, event_timing, audience_learning) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices)
view(teach_in_clean)

teach_data <- teach_in_clean %>%
  mutate(event_rating = factor(event_rating,
                               levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much")))
view(teach_data)

advertisement_data <- teach_in_clean %>%
  count(advertisement_choices)
view(advertisement_data)


event_rating <- teach_data %>%
  count(event_rating)
event_rating

speaker_rating <- teach_data %>%
  count(speaker_rating)
speaker_rating

timing_data <- teach_data %>%
  count(event_timing)
timing_data

learning_data <- teach_data %>%
  count(audience_learning)
learning_data


#### plot ####
colorpal<- c("#F2D0A4", "#5DD39E", "#348AA7", "#525174", "#513B56", "#595A9F", "#9091AA")

advertisement_data %>%
  ggplot(aes(x = reorder(advertisement_choices, -n), y = n, fill = advertisement_choices)) +
  geom_col() +
  labs(title = "How did respondants learn about the teach-in on Anti-Asian Hate?",
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
ggsave(here("Output", "2020-2021", "teach_in", "advertisement.png"), width = 6, height = 5)

event_rating %>%
  ggplot(aes(x = reorder(event_rating, -n), y = n, fill = event_rating)) +
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
ggsave(here("Output", "2020-2021", "teach_in", "event_rating.png"), width = 6, height = 5)

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
ggsave(here("output", "2020-2021", "teach_in", "speakers_rating.png"), width = 6, height = 5)

timing_data %>%
  ggplot(aes(x = reorder(event_timing, -n), y = n, fill = event_timing)) +
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
ggsave(here("Output", "2020-2021", "teach_in", "event_timing.png"), width = 6, height = 5)

learning_data %>%
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
ggsave(here("Output", "2020-2021", "teach_in", "audience_learning.png"), width = 6, height = 5)
