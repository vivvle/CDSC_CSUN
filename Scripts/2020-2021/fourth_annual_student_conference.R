#### 4th Annual Social Justice Student Reserach Conference ####
#### Created by: Vivian Vy Le ####
#### Updated on: 2022-08-11 ####

#### Load library ####
library(tidyverse)
library(here)
library(ggplot2)

#### Load Data ####
fourth_con <- read_csv(here("Data", "fourth_annual_student_conference.csv"))

#### View Data ####
view(fourth_con)
glimpse(fourth_con)

#### Data Analysis ####
fourth_data <- fourth_con %>%
  select(participants, panel_attendence, event_rating, speaker_rating, advertisement, event_timing, audience_learning) %>%
  rowwise() %>%
  mutate(advertisement_choices = str_split(advertisement, pattern = ";")) %>%
  mutate(panel_attendence = str_split(panel_attendence, pattern = ";")) %>%
  ungroup() %>%
  unnest(advertisement_choices) %>%
  unnest(panel_attendence)
view(fourth_data)


fourth_clean <- fourth_data %>%
  mutate(event_rating = factor(event_rating,
                               levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(speaker_rating = factor(speaker_rating,
                                 levels = c("Excellent", "Fairly Good", "Not Good at All"))) %>%
  mutate(event_timing = factor(event_timing,
                               levels = c("Extremely convenient", "Somewhat convenient", "Not at all convenient"))) %>%
  mutate(audience_learning = factor(audience_learning,
                                    levels = c("Very much", "Somewhat", "Not much")))
view(fourth_clean)

advertisement_data <-fourth_clean %>%
  count(advertisement_choices)
advertisement_data

event_data <- fourth_clean %>%
  count(event_rating)
event_data

speaker_data <- fourth_clean %>%
  count(speaker_rating)
speaker_data

timing_data <- fourth_clean %>%
  count(event_timing)
timing_data

audience_data <- fourth_clean %>%
  count(audience_learning)
audience_data

panel_data <- fourth_clean %>%
  count(panel_attendence)
panel_data


#### Plot ####
colorpal <- c("#FFE5AA", "#FABE90", "#E15634", "#CF2F3C", "#94172D", "#FCEADE", "#FF8A5B")
colorgrad <- c("#F78764", "#E15634", "#E03730")


advertisement_data %>%
  ggplot(aes(x= reorder(advertisement_choices, -n), y = n, fill = advertisement_choices)) +
  geom_col() +
  labs(title = "How did respondants learn about the 4th Annual Social Justice Student Research Conference?",
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
ggsave(here("Output", "2020-2021", "fourth_annual_student_conference", "advertisement.png"), width = 8, height = 6)

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
ggsave(here("Output", "2020-2021", "fourth_annual_student_conference", "event_rating.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "fourth_annual_student_conference", "speaker_rating.png"), width = 6, height = 5)

timing_data %>%
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
  scale_fill_manual(values = colorgrad) +
  guides(fill = FALSE)
ggsave(here("Output", "2020-2021", "fourth_annual_student_conference", "event_timing.png"), width = 6, height = 5)

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
ggsave(here("Output", "2020-2021", "fourth_annual_student_conference", "audience_learning.png"), width = 6, height = 5)

panel_data %>%
  ggplot(aes(x = reorder(panel_attendence, n), y = n, fill = panel_attendence)) +
  geom_col() +
  labs(title = "Which panels did the participants attend?",
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
  coord_flip() +
  guides(fill = FALSE)
ggsave(here("Output", "2020-2021", "fourth_annual_student_conference", "panel_attendence.png"), width = 8, height = 5)
