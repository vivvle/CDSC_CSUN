#### 2020 Election Webinar ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-08-08 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)

#### Load Data ####
election <- read_csv(here("Data", "election_2020.csv"))

#### View Data ####
view(election)
glimpse(election)

#### Data analysis ####
election_clean <- election %>%
  select(participants, event_rating, speaker_rating, advertisement, event_timing, audience_learning, voting_in_election) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices)
view(election_clean)

election_data <- election_clean %>%
  mutate(event_rating = factor(event_rating,
                               levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much"))) %>%
  mutate(voting_in_election = factor(voting_in_election,
                                     levels = c("Very much", "Somewhat", "Not much")))
view(election_data)


advertisement_data <- election_data %>%
  count(advertisement_choices)
view(advertisement_data)

event_rating <- election_data %>%
  count(event_rating)
event_rating

speaker_rating <- election_data %>%
  count(speaker_rating)
speaker_rating

timing_data <- election_data %>%
  count(event_timing)
timing_data

learning_data <- election_data %>%
  count(audience_learning)
learning_data

voting_data <- election_data %>%
  count(voting_in_election)
voting_data


#### Plot ####
colorpal<- c("#F2D0A4", "#5DD39E", "#348AA7", "#525174", "#513B56", "#595A9F", "#9091AA")

advertisement_data %>%
  ggplot(aes(x = reorder(advertisement_choices, -n), y = n, fill = advertisement_choices)) +
  geom_col() +
  labs(title = "How did respondants learn about the Election 2020 Informational Webinar?",
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
ggsave(here("Output", "2020-2021", "election_2020_webinar", "advertisement.png"), width = 8, height = 6)

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
ggsave(here("Output", "2020-2021", "election_2020_webinar", "event_rating.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "election_2020_webinar", "speaker_rating.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "election_2020_webinar", "event_timing.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "election_2020_webinar", "audience_learning.png"), width = 6, height = 5)

voting_data %>%
  ggplot(aes(x = reorder(voting_in_election, -n), y = n, fill = voting_in_election)) +
  geom_col() +
  labs(title = "How likely the respondents going to vote in the 2020 Election?",
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
ggsave(here("Output", "2020-2021", "election_2020_webinar", "voting_likely.png"), width = 6, height = 5)
