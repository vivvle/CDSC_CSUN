#### Webinar with Roxanne Dunbar-Ortiz ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-05-17 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)

#### Load data ####
film <- read_csv(here("Data", "dunbar_ortiz_film.csv"))


#### view data ####
view(film)
glimpse(film)


#### data analysis ####
film_clean <- film %>%
  select(participants, event_rating, speaker_rating, advertisement, event_timing, audience_learning) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices)
view(film_clean)

advertisement_data <- film_clean %>%
  count(advertisement_choices)
view(advertisement_data)


data <- film_clean %>%
  mutate(event_rating = factor(event_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much")))
view(data)


event_rating <- data %>%
  count(event_rating)
event_rating


speaker_rating <- data %>%
  count(speaker_rating)
speaker_rating

timing_data <- data %>%
  count(event_timing)

learning_data <- data %>%
  count(audience_learning)


#### plot 
advertisement_data %>%
  ggplot(aes(x = reorder(advertisement_choices, -n), y = n, fill = advertisement_choices)) +
  geom_col() +
  labs(title = "How did respondants learn from the film screening with Roxanne Dunbar-Ortiz?",
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
  scale_fill_viridis_d() +
  guides(fill = FALSE)
ggsave(here("Output", "dunbar_ortiz_film", "advertisement.png"), width = 8, height = 5)

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
  scale_fill_viridis_d() +
  guides(fill = FALSE)
ggsave(here("Output", "dunbar_ortiz_film", "event_rating.png"), width = 6, height = 5)

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
  scale_fill_viridis_d() +
  guides(fill = FALSE)
ggsave(here("Output", "dunbar_ortiz_film", "speaker_rating.png"), width = 6, height = 5)

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
  scale_fill_viridis_d() +
  guides(fill = FALSE)
ggsave(here("Output", "dunbar_ortiz_film", "convenience_of_event.png"), width = 6, height = 5)
