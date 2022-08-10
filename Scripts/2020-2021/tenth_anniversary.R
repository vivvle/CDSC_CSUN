#### 2020 Election Webinar ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-08-09 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)

#### Load Data ####
anniversary <- read_csv(here("Data", "tenth_anniversary.csv"))

#### View Data ####
view(anniversary)
glimpse(anniversary)


#### Data analysis ####
anniversary_clean <- anniversary %>%
  select(participants, event_rating, speaker_rating, advertisement, event_timing, audience_learning) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices)
view(anniversary_clean)

anniversary_data <- anniversary_clean %>%
  mutate(event_rating = factor(event_rating,
                               levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much")))
view(anniversary_data)

advertisement_data <- anniversary_data %>%
  count(advertisement_choices)
advertisement_data

event_data <- anniversary_data %>%
  count(event_rating)
event_data

speaker_data <- anniversary_data %>%
  count(speaker_rating)
speaker_data

timing_data <- anniversary_data %>%
  count(event_timing)
timing_data

audience_data <- anniversary_data %>%
  count(audience_learning)
audience_data

#### Plot ####
colorpal <- c("#FFE5AA", "#FABE90", "#E15634", "#CF2F3C", "#94172D", "#FCEADE", "#FF8A5B")
colorgrad <- c("#F78764", "#E15634", "#E03730")

advertisement_data %>%
  ggplot(aes(x = (reorder(advertisement_choices, -n)), y = n, fill = advertisement_choices))  +
  geom_col() +
  labs(title = "How did the respondants learn about CDSC's 10th Anniversary Webinar?",
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
ggsave(here("Output", "2020-2021", "tenth_anniversary", "advertisement_data.png"), width = 8, height = 6)

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
  scale_fill_manual(values = colorgrad) +
  guides(fill = FALSE)
ggsave(here("Output", "2020-2021", "tenth_anniversary", "event_rating.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "tenth_anniversary", "speaker_rating.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "tenth_anniversary", "event_timing.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "tenth_anniversary", "audience_learning.png"), width = 6, height = 5)
