#### COVID-19 and Vulnerable Communities ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-08-07 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)

#### Load data ####
vulnerable <- read_csv(here("Data", "vulnerable_communities_webinar.csv"))

#### View data ####
view(vulnerable)
glimpse(vulnerable)

#### data analysis ####
vulnerable_clean <- vulnerable %>%
  select(participants, overall_rating, speaker_rating, advertisement, event_timing, audience_learning) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices)
view(vulnerable_clean)

advertisement_data <- vulnerable_clean %>%
  count(advertisement_choices)
view(advertisement_data)

data <- vulnerable_clean %>%
  mutate(overall_rating = factor(overall_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much")))
view(data)          

overall_rate <- data %>%
  count(overall_rating)
overall_rate

speaker_rate <- data %>%
  count(speaker_rating)
speaker_rate

timing_data <- data %>%
  count(event_timing)

learning_data <- data %>%
  count(audience_learning)


#### plotting data ####
colorpal<- c("#BCE784", "#5DD39E", "#348AA7", "#525174", "#513B56", "#595A9F", "#9091AA")

advertisement_data %>%
  ggplot(aes(x = reorder(advertisement_choices, -n), y = n, fill = advertisement_choices)) +
  geom_col() +
  labs(title = "How did respondants learn from the CDSC COVID-19 and Vulnerable Communities Webinar?",
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
ggsave(here("Output", "2021-2022", "vulnerable_communities", "advertisement.png"), width = 8, height = 5)

overall_rate %>%
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
  scale_fill_manual(values = colorpal) +
  guides(fill = FALSE)
ggsave(here("Output", "2021-2022", "vulnerable_communities", "event_rating.png"), width = 6, height = 5)

speaker_rate %>%
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
  scale_fill_manual(values = colorpal) +
  guides(fill = FALSE)
ggsave(here("Output", "2021-2022", "vulnerable_communities", "speaker_rating.png"), width = 6, height = 5)


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
  scale_fill_manual(values = c("#E88D72", "#543855")) +
  guides(fill = FALSE)
ggsave(here("Output", "2021-2022", "vulnerable_communities", "convenience_of_event.png"), width = 6, height = 5)


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
  scale_fill_manual(values = colorpal) +
  guides(fill = FALSE)
ggsave(here("Output", "2021-2022", "vulnerable_communities", "audience_learning.png"), width = 6, height = 5)
