#### Southeast Asian American Journeys: 45 Years of Resilience, Advocacy, and Organizing with SEARAC ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-08-09 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)

#### Load Data ####
sea_asam <- read_csv(here("Data", "searac_event.csv"))

#### View Data ####
view(sea_asam)
glimpse(sea_asam)


#### Data Analysis ####
sea_asam_clean <- sea_asam %>%
  select(participants, event_rating, speaker_rating, advertisement, event_timing, audience_learning) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices)
view(sea_asam_clean)

sea_asam_data <- sea_asam_clean %>%
  mutate(event_rating = factor(event_rating,
                               levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much")))
view(sea_asam_data)


advertisement_data <- sea_asam_data %>%
  count(advertisement_choices)
advertisement_data

event_data <- sea_asam_data %>%
  count(event_rating)
event_data

speaker_data <- sea_asam_data %>%
  count(speaker_rating)
speaker_data

timing_data <- sea_asam_data %>%
  count(event_timing)
timing_data

audience_data <- sea_asam_data %>%
  count(audience_learning)
audience_data

#### Plot ####
colorpal <- c("#E8871E", "#F0803C", "#A7C957", "#6A994E", "#262A10")
colorgrad <- c("#A7C957", "#6A994E", "#262A10")

advertisement_data %>%
  ggplot(aes(x=(reorder(advertisement_choices, -n)), y = n, fill = advertisement_choices)) +
  geom_col() +
  labs(title = "How did the respondants learn about the webinar on Southeast Asian Americans' journey?",
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
ggsave(here("Output", "2020-2021", "sea_asam_event", "advertisement.png"), width = 8, height = 6)

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
ggsave(here("Output", "2020-2021", "sea_asam_event", "event_rating.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "sea_asam_event", "speaker_rating.png"), width = 6, height = 5)

