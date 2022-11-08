# OWL Season 2022 EDA
# Last update of the csv: 10/26/2022
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

# Team Regions ----
# East or APAC region.
region_APAC <- list("Seoul Dynasty", "Shanghai Dragons", "Philadelphia Fusion",
                    "Hangzhou Spark", "Guangzhou Charge", "Chengdu Hunters",
                    "Los Angeles Valiant")

# West or NA region.
region_NA <- list("Atlanta Reign", "Boston Uprising", "Dallas Fuel", "Florida Mayhem",
                  "Houston Outlaws", "London Spitfire", "Los Angeles Gladiators",
                  "New York Excelsior", "Paris Eternal", "San Francisco Shock",
                  "Toronto Defiant", "Vancouver Titans", "Washington Justice")

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
                               map_name %in% map_push ~ "Push"),
         team_region = case_when(map_winner %in% region_APAC ~ "APAC",
                                 map_winner %in% region_NA ~ "NA")) %>% 
  ungroup()

# For some reason the match with id 39156 is wrong in the map_name and map_round column.
# The values for this match were updated with the overview information of the page.
owl_data_2022 %>% 
  filter(match_id == 39156) %>% 
  view()

owl_data_2022$map_name[owl_data_2022$match_id == 39156][3] <- "King's Row"
owl_data_2022$map_round[owl_data_2022$match_id == 39156][4] <- 1
owl_data_2022$game_number[owl_data_2022$match_id == 39156][3] <- 2

owl_data_2022_scores %>% 
  filter(match_id == 39156) %>% 
  view()

# Overwatch data season 2022 with scores, although these steps can be implemented
# in owl_data_2022.
owl_data_2022_scores <- owl_data_2022 %>%
  group_by(match_id, game_number) %>% 
  mutate(num_row = row_number()) %>%
  ungroup() %>% 
  group_by(match_id) %>% 
  mutate(score_winner = ifelse(num_row == 1,str_count(map_winner, match_winner), 0),
         score_winner = sum(score_winner),
         score_loser = max(game_number) - score_winner) %>%
  ungroup()

write_csv(owl_data_2022_scores, "owl_data_2022_scores.csv")

# Distribution of the matches duration in Hours ----
owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  filter(game_number == max(game_number), match_rounds == max(match_rounds), stage_class != "Postseason") %>%
  ggplot(aes(x = as.numeric(total_time_match, "hour"))) +
  geom_histogram() +
  facet_grid(stage_class ~ ., scales = "free_y") + 
  labs(x = "Total match time (Hours)",
       y = "Total matches",
       title = "OWL Season 2022 Match duration",
       caption = "Source: https://overwatchleague.com/en-us/statslab")

# Box Plot distribution of rounds. ----
owl_data_2022_scores %>%  
  group_by(match_id) %>% 
  filter(game_number == max(game_number), match_rounds == max(match_rounds), stage_class != "Postseason") %>%
  ggplot(aes(x = as.numeric(total_time_match, "hour"), y = match_rounds)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(stage_class ~ ., scales = "free_y") +
  labs(x = "Total match time (Hours)",
       y = "Total rounds per match",
       title = "OWL Season 2022 Match duration",
       caption = "Source: https://overwatchleague.com/en-us/statslab")

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
  facet_grid(stage_class ~ .) +
  labs(x = NULL,
       y = "Total times played",
       title = "OWL Season 2022",
       subtitle = "Maps played during this season",
       caption = "Source: https://overwatchleague.com/en-us/statslab")

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
  labs(x = NULL,
       y = "Number of maps played",
       title = "Maps played in the season 2022",
       caption = "Source: https://overwatchleague.com/en-us/statslab")

# Common scores ----
# TODO: improve visualization
owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  filter(game_number == max(game_number), num_row == 1) %>%
  mutate(score_str = str_c(score_winner, score_loser, sep = "-")) %>%
  ungroup() %>% 
  ggplot(aes(x = score_str)) +
  geom_bar() +
  labs(x = NULL,
       y = "Total matches",
       title = "Common scores during season 2022",
       caption = "Source: https://overwatchleague.com/en-us/statslab")

owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  filter(game_number == max(game_number), num_row == 1, stage_class != "Postseason") %>%
  mutate(score_str = str_c(score_winner, score_loser, sep = "-")) %>%
  ungroup() %>% 
  ggplot(aes(x = score_str)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(. ~ stage_class, nrow = 2) +
  labs(x = NULL,
       y = "Total matches",
       title = "Common scores during season 2022",
       caption = "Source: https://overwatchleague.com/en-us/statslab")

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
# Change legend
owl_data_2022_scores %>%
  group_by(match_id, game_number) %>% 
  filter(num_row == 1) %>%
  ungroup() %>% 
  group_by(map_class) %>% 
  summarise(wins = ifelse((map_winner == match_winner), 1 , 0)) %>%
  count(wins) %>% 
  mutate(wins = ifelse(wins == 1,"Winner", "Loser")) %>% 
  ggplot(aes(x = map_class, y = n)) +
  geom_bar(aes(fill = wins), stat = "identity", position = "dodge") +
  labs(x = NULL,
       y = "Total maps",
       title = "Maps taken by the team",
       caption = "Source: https://overwatchleague.com/en-us/statslab")

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
  facet_wrap(. ~ stage_class, nrow = 2) +
  labs(x = NULL,
       y = "Total maps",
       title = "Maps taken by the team",
       caption = "Source: https://overwatchleague.com/en-us/statslab")

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

# Winning/losing times by teams in Qualifiers and win rate ----
# Each team in Qualifiers played a total of 24 games in NA region
# Each team played 12 times as home team (except for Dallas with 11, London 13)
# and 12 times as away team (except for Dallas with 13, London 11).
# Fastest is defined by the sum of match time.
owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  mutate(row_match = row_number()) %>% 
  ungroup() %>% 
  filter(row_match == 1, stage_class %in% "Qualifiers") %>% 
  group_by(team_home) %>% 
  summarise(total = sum(game_number))

# Average time per win
# Although some times have a higher win rate, they are not the fastest.
owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  mutate(row_match = row_number()) %>% 
  ungroup() %>% 
  filter(row_match == 1, stage_class %in% "Qualifiers") %>% 
  group_by(team_region, match_winner) %>% 
  summarise(total_time = sum(total_time_match),
            total_wins = sum(game_number),
            wins_time = total_time / total_wins) %>% 
  arrange(team_region, wins_time, total_wins, total_time)

# Win rate
owl_data_2022_scores %>% 
  group_by(match_id) %>% 
  mutate(row_match = row_number()) %>% 
  ungroup() %>% 
  filter(row_match == 1, stage_class %in% "Qualifiers") %>% 
  group_by(team_region, match_winner) %>% 
  summarise(total_wins = sum(game_number),
            win_rate = 100 * total_wins/24) %>% 
  arrange(team_region, desc(total_wins))

# First map result into win? ----
# Global
owl_data_2022_scores %>%
  group_by(match_id) %>% 
  mutate(row_match = row_number()) %>% 
  ungroup() %>% 
  filter(row_match == 1) %>% 
  mutate(first_win = ifelse(match_winner == map_winner, 1, 0)) %>% 
  summarise(total_matches = sum(game_number),
            total_first = sum(first_win),
            total_percent = total_first/total_matches)

# By type of stage
owl_data_2022_scores %>%
  group_by(match_id) %>% 
  mutate(row_match = row_number()) %>% 
  ungroup() %>% 
  filter(row_match == 1) %>% 
  mutate(first_win = ifelse(match_winner == map_winner, 1, 0)) %>% 
  group_by(stage_class) %>% 
  summarise(total_matches = sum(game_number),
            total_first = sum(first_win),
            total_percent = total_first/total_matches)

# TODO ----
# Improve data visualizations
# Look into the maps.
# Comparison regular season vs. tournaments
# Win rate by team and map
