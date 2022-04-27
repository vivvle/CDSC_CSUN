#### COVID-19 and Vulerable Communities ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-04-27 ####

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

event_rate <- vulnerable_clean %>%
  mutate(overall_rating = factor(overall_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  count(overall_rating)

speaker_data <- vulnerable_clean %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  count(speaker_rating)

advertisment_data <- vulnerable_clean %>%
  count(advertisement_choices)