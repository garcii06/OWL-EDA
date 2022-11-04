# OWL Season 2022 EDA

library(lubridate)
library(tidyverse)

# Importing and tidying the data ----
# From an initial import, start and end time were wrong as they got imported  as
# char instead of datetime.
owl_data <- read_csv("Data_all/match_map_stats.csv",
                     col_types = cols(
                       round_start_time = col_datetime("%m/%d/%y %H:%M"),
                       round_end_time = col_datetime("%m/%d/%y %H:%M")))

# Spec was used to know the data type of the columns.
spec(owl_data)

owl_data %>% 
  head(10) %>% 
  view()

# EDA ----
# The first match of the 2022 season started on May 05.
# Mutate/add some columns:
# Total match duration.
# Type of match as stage_class: (Postseason, Qualifiers, Tournament).
# Type of map as map_class: (Control, Hybrid, Escort, Push).
owl_data_2022 <- owl_data %>% 
  filter(year(round_start_time) >= 2022)

# Map lists ----
# Create list for every type of map.
map_escort <- list("Circuit royal", "Dorado", "Junkertown", "Route 66", "Watchpoint: Gibraltar")
map_hybrid <- list("Eichenwalde", "Hollywood", "King's Row", "Midtown", "Paraíso")
map_control <- list("Busan", "Ilios", "Lijiang Tower", "Nepal", "Oasis")
map_push <- list("Colosseo", "Esperança", "New Queen Street")

# Get the distinct name of maps available to get categorised. 
owl_data_2022 %>%
  distinct(map_name) %>% 
  arrange(map_name)

owl_data_2022 %>% 
  view()

# Tidy dataset ----
owl_data_2022 <- owl_data_2022 %>%
  rename(team_home = team_one_name, team_away = team_two_name) %>% 
  group_by(match_id) %>%
  mutate(total_time_match = max(round_end_time) - min(round_start_time),
         total_time_match = as.duration(total_time_match),
         match_rounds = row_number(),
         stage_class = case_when(str_detect(stage, "Postseason$") ~ "Postseason",
                                 str_detect(stage, "Qualifiers$") ~ "Qualifiers",
                                 str_detect(stage, "Tournament$") ~ "Tournament"),
         map_class = case_when(map_name %in% map_escort ~ "Escort",
                               map_name %in% map_hybrid ~ "Hybrid",
                               map_name %in% map_control ~ "Control",
                               map_name %in% map_push ~ "Push")) %>% 
  ungroup()

# For some reason the match with id 39156 is wrong in the game_number column.
# The value associated with that match will be removed.
owl_data_2022 %>% 
  filter(match_id == 39156) %>% 
  view()

# Overwatch data season 2022 with scores, although these steps can be implemented
# in owl_data_2022.
owl_data_2022_scores <- owl_data_2022 %>%
  filter(match_id != 39156) %>% 
  group_by(match_id, game_number) %>% 
  mutate(num_row = row_number()) %>%
  ungroup() %>% 
  group_by(match_id) %>% 
  mutate(score_winner = ifelse(num_row == 1,str_count(map_winner, match_winner), 0),
         score_winner = sum(score_winner),
         score_loser = max(game_number) - score_winner) %>%
  ungroup()

# Distribution of the matches duration in Hours ----
owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  filter(game_number == max(game_number), match_rounds == max(match_rounds), stage_class != "Postseason") %>%
  ggplot(aes(x = as.numeric(total_time_match, "hour"))) +
  geom_histogram() +
  facet_grid(stage_class ~ ., scales = "free_y") + 
  labs(x = "Total time match (Hours)",
       y = "Total matches",
       title = "OWL Season 2022 Match duration")

# Box Plot distribution of rounds. ----
owl_data_2022_scores %>%  
  group_by(match_id) %>% 
  filter(game_number == max(game_number), match_rounds == max(match_rounds), stage_class != "Postseason") %>%
  ggplot(aes(x = as.numeric(total_time_match, "hour"), y = match_rounds)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(stage_class ~ ., scales = "free_y") +
  labs(x = "Total time match (Hours)",
       y = "Total rounds of the match",
       title = "OWL Season 2022 Match duration")

# Type of maps played during the season. ----
# The steps for this are:
# Group by a unique identifier and the map identifier, as map class can be repeated
# on a match it is not unique, but the game_number is unique from 1 map to 4 or more.
# Assign a number for each row using the previous criteria, and get only the first 
# to get no duplicates.
owl_data_2022_scores %>%
  select(match_id, game_number, match_winner, map_name, total_time_match, stage_class, map_class) %>% 
  group_by(match_id, game_number) %>% 
  mutate(rnum = row_number()) %>%
  filter(rnum == 1, stage_class != "Postseason") %>% 
  ungroup() %>% 
  ggplot(aes(x = map_class)) +
  geom_bar() +
  facet_grid(stage_class ~ .)

owl_data_2022_scores %>%
  select(match_id, game_number, match_winner, map_name, total_time_match, stage_class, map_class) %>% 
  group_by(match_id, game_number) %>% 
  mutate(rnum = row_number()) %>%
  filter(rnum == 1, stage_class != "Postseason") %>% 
  ungroup() %>% 
  ggplot(aes(x = map_class)) +
  geom_bar() +
  coord_flip() +
  facet_grid(stage_class ~ .) +
  labs(y = "Number of maps played",
       x = "Type of map",
       title = "Maps played in the season 2022")

# Common scores ----
# TODO: improve visualization
owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  filter(game_number == max(game_number), num_row == 1) %>%
  mutate(score_str = str_c(score_winner, score_loser, sep = "-")) %>%
  ungroup() %>% 
  ggplot(aes(x = score_str)) +
  geom_bar()

owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  filter(game_number == max(game_number), num_row == 1, stage_class != "Postseason") %>%
  mutate(score_str = str_c(score_winner, score_loser, sep = "-")) %>%
  ungroup() %>% 
  ggplot(aes(x = score_str)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(. ~ stage_class, nrow = 2)

# Total maps taken ----
# Total score/points of winners against losers score.
# First group by each unique identifier, then filter by just one/first/max entry
# for each map.
owl_data_2022_scores %>%
  group_by(match_id) %>% 
  filter(game_number == min(game_number), num_row == 1) %>%
  ungroup() %>% 
  summarise(score_winner = sum(score_winner),
            score_loser = sum(score_loser))

# Score by map type ----
# TODO: improve visualization
# The total points for losers is 268, but the sum here is 267.
owl_data_2022_scores %>%
  group_by(match_id, game_number) %>% 
  filter(num_row == 1) %>%
  ungroup() %>% 
  group_by(map_class) %>% 
  summarise(wins = ifelse((map_winner == match_winner), 1 , 0)) %>%
  count(wins) %>% 
  mutate(wins = ifelse(wins == 1,"Winner", "Loser")) %>% 
  ggplot(aes(x = map_class, y = n)) +
  geom_bar(aes(fill = wins), stat = "identity", position = "dodge")

owl_data_2022_scores %>%
  group_by(match_id, game_number) %>% 
  filter(num_row == 1) %>%
  ungroup() %>% 
  group_by(map_class, stage_class) %>% 
  filter(stage_class != "Postseason") %>% 
  summarise(wins = ifelse((map_winner == match_winner), 1 , 0)) %>%
  count(wins) %>% 
  mutate(wins = ifelse(wins == 1,"Winner", "Loser")) %>% 
  ggplot(aes(x = map_class, y = n)) +
  geom_bar(aes(fill = wins), stat = "identity", position = "dodge") +
  coord_flip() + 
  facet_wrap(. ~ stage_class, nrow = 2)

# Reverse sweep ----
# Get the first two maps and check if the they were won by the same team but the
# match winner is different.
owl_data_2022_scores %>%
  group_by(match_id, game_number) %>% 
  filter(num_row == 1, stage_class != "Postseason") %>%
  ungroup() %>% 
  group_by(match_id) %>% 
  filter(game_number <= 2) %>%
  mutate(winner_second_map = lag(map_winner)) %>% 
  filter(game_number == 2) %>% 
  mutate(reversed_possible = ifelse(map_winner == winner_second_map, 1, 0),
         reversed = ifelse(reversed_possible == 1 & match_winner != map_winner,
                           "Reversed", "No")) %>% 
  ungroup() %>% 
  count(reversed)

# TODO ----
# Improve data visualizations
# Look into the maps.
# Comparison regular season vs. tournaments
# First map result into win?
# Fastest teams
# Slowest teams
# Win rate by team and map
