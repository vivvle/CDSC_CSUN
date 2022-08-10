#### 2020 Election Webinar ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-08-09 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)

#### Load Data ####
detention <- read_csv(here("Data", "migrant_detention.csv"))

#### View Data ####
view(detention)
glimpse(detention)

#### Data Analysis ####
detention_clean <- detention %>%
  select(participants, event_rating, speaker_rating, advertisement, event_timing, audience_learning) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices)
view(detention_clean)


detention_data <- detention_clean %>%
  mutate(event_rating = factor(event_rating,
                               levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much")))
view(detention_data)


advertisement_data <- detention_data %>%
  count(advertisement_choices)
advertisement_data

event_data <- detention_data %>%
  count(event_rating)
event_data

speaker_data <- detention_data %>%
  count(speaker_rating)
speaker_data

timing_data <- detention_data %>%
  count(event_timing)
timing_data

audience_data <- detention_data %>%
  count(audience_learning)
audience_data


#### Plot ####
colorpal <- c("#34403A", "#22577A", "#38A3A5", "#57CC99", "#80ED99", "#C7F9CC")
colorgrad <- c("#57CC99", "#38A3A5","#22577A")

advertisement_data %>%
  ggplot(aes(x= (reorder(advertisement_choices, -n)), y = n, fill = advertisement_choices)) +
  geom_col() +
  labs(title = "How did the respondants learn about the webinar on activism on migrant detention and deportation?",
       x = "Responses",
       y = "Count") +
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
ggsave(here("Output", "2020-2021", "migrant_detention", "advertisement.png"), width = 8, height = 6)

event_data %>%
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
  scale_fill_manual(values = colorgrad) +
  guides(fill = FALSE)
ggsave(here("Output", "2020-2021", "migrant_detention", "event_rating.png"), width = 6, height = 5 )

speaker_data %>%
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
  scale_fill_manual(values = colorgrad) +
  guides(fill = FALSE)
ggsave(here("Output", "2020-2021", "migrant_detention", "speaker_ratings.png"), width = 6, height = 5)

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
  scale_fill_manual(values = colorgrad) +
  guides(fill = FALSE)
ggsave(here("Output", "2020-2021", "migrant_detention", "event_timing.png"), width = 6, height = 5)

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
  scale_fill_manual(values = colorgrad) +
  guides(fill = FALSE)
ggsave(here("Output", "2020-2021", "migrant_detention", "audience_learning.png"), width = 6, height = 5)
