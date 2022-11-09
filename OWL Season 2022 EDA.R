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

## Map lists ----
# Create list for every type of map.
map_escort <- list("Circuit royal", "Dorado", "Junkertown", "Route 66", "Watchpoint: Gibraltar")
map_hybrid <- list("Eichenwalde", "Hollywood", "King's Row", "Midtown", "Paraíso")
map_control <- list("Busan", "Ilios", "Lijiang Tower", "Nepal", "Oasis")
map_push <- list("Colosseo", "Esperança", "New Queen Street")

## Team Regions ----
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

## Data fixing ----
# For some reason the match with id 39156 is wrong in the map_name and map_round column.
# The values for this match were updated with the overview information of the page.
owl_data_2022 %>% 
  filter(match_id == 39156) %>% 
  view()

owl_data_2022$map_name[owl_data_2022$match_id == 39156][3] <- "King's Row"
owl_data_2022$map_round[owl_data_2022$match_id == 39156][4] <- 1
owl_data_2022$game_number[owl_data_2022$match_id == 39156][3] <- 2

# Also, the match with id 38981 has for Circuit royal a duplicate entry
owl_data_2022 %>% 
  filter(match_id == 38981) %>% 
  view()

owl_data_2022$game_number[owl_data_2022$match_id == 38981][8] <- 3
owl_data_2022$game_number[owl_data_2022$match_id == 38981][9] <- 4

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
  ungroup() %>% 
  select(-num_row)

owl_data_2022_scores %>% 
  filter(match_id == 39156) %>% 
  view()

owl_data_2022_scores %>% 
  filter(match_id == 38981) %>% 
  view()

write_csv(owl_data_2022_scores, "owl_data_2022_scores.csv")

# Distribution of the matches duration in Hours ----
owl_data_2022_scores %>% 
  select(match_id, game_number, match_rounds, stage_class, total_time_match) %>% 
  group_by(match_id) %>% 
  filter(row_number() == 1, stage_class != "Postseason") %>%
  ggplot(aes(x = as.numeric(total_time_match, "hour"))) +
  geom_histogram() +
  facet_grid(stage_class ~ ., scales = "free_y") + 
  labs(x = "Total match time (Hours)",
       y = "Total matches",
       title = "OWL Season 2022 Match duration",
       caption = "Source: https://overwatchleague.com")

# Box Plot distribution of rounds. ----
owl_data_2022_scores %>%  
  select(match_id, game_number, match_rounds, stage_class, total_time_match) %>% 
  group_by(match_id) %>% 
  filter(game_number == max(game_number), match_rounds == max(match_rounds), stage_class != "Postseason") %>%
  ggplot(aes(x = as.numeric(total_time_match, "hour"), y = match_rounds)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(stage_class ~ ., scales = "free_y") +
  labs(x = "Total match time (Hours)",
       y = "Total rounds per match",
       title = "OWL Season 2022 Match duration",
       caption = "Source: https://overwatchleague.com")

## Type of maps played during the season. ----
owl_data_2022_scores %>%
  select(match_id, game_number, match_winner, map_name, total_time_match, stage_class, map_class) %>% 
  group_by(match_id, game_number) %>% 
  filter(row_number() == 1, stage_class != "Postseason") %>% 
  ggplot(aes(x = map_class)) +
  geom_bar() +
  coord_flip() +
  facet_grid(stage_class ~ .) +
  labs(x = NULL,
       y = "Total times played",
       title = "OWL Season 2022",
       subtitle = "Maps played during this season",
       caption = "Source: https://overwatchleague.com")

# Common scores ----
# TODO: improve visualization
owl_data_2022_scores_common <- owl_data_2022_scores %>% 
  select(match_id, score_winner, score_loser, stage_class) %>% 
  group_by(match_id) %>% 
  filter(row_number() == 1) %>%
  mutate(score_str = str_c(score_winner, score_loser, sep = "-"))

owl_data_2022_scores_common %>%
  ggplot(aes(x = score_str)) +
  geom_bar() +
  labs(x = NULL,
       y = "Total matches",
       title = "Common scores during all season 2022",
       caption = "Source: https://overwatchleague.com")

owl_data_2022_scores_common %>%
  filter(stage_class != "Postseason") %>%
  ggplot(aes(x = score_str)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(. ~ stage_class, nrow = 2) +
  labs(x = NULL,
       y = "Total matches",
       title = "Common scores during season 2022",
       caption = "Source: https://overwatchleague.com")

# Total maps taken ----
# Total score/points of winners against losers score.
# First group by each unique identifier, then filter by just one/first/max entry
# for each map.
owl_data_2022_scores %>%
  select(match_id, score_winner, score_loser) %>% 
  group_by(match_id) %>% 
  filter(row_number() == 1) %>%
  ungroup() %>% 
  summarise(score_winner = sum(score_winner),
            score_loser = sum(score_loser))

# Score by map type ----
# TODO: improve visualization
# Change legend
owl_data_2022_scores_maps_taken <- owl_data_2022_scores %>% 
  select(match_id, game_number, map_class, map_winner, match_winner, stage_class) %>% 
  group_by(match_id, game_number) %>% 
  filter(row_number() == 1)

owl_data_2022_scores_maps_taken %>% 
  group_by(map_class) %>% 
  summarise(wins = ifelse((map_winner == match_winner), 1 , 0)) %>%
  mutate(wins = ifelse(wins == 1,"Winner", "Loser")) %>% 
  count(wins) %>% 
  ggplot(aes(x = map_class, y = n)) +
  geom_bar(aes(fill = wins), stat = "identity", position = "dodge") +
  labs(x = NULL,
       y = "Total maps",
       title = "Maps taken by the team",
       caption = "Source: https://overwatchleague.com")

owl_data_2022_scores_maps_taken %>% 
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
       caption = "Source: https://overwatchleague.com")
  
# Reverse sweep ----
# Get the first two maps and check if the they were won by the same team but the
# match winner is different.
owl_data_2022_scores %>%
  select(match_id, game_number, stage_class, map_winner, match_winner) %>% 
  group_by(match_id, game_number) %>% 
  filter(row_number() == 1, stage_class != "Postseason") %>%
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
# Each team played 12 times as home team (except for Dallas with 11, Houston Outlaws 13)
# and 12 times as away team (except for Dallas with 13, Houston Outlaws 11).
# Fastest is defined by the sum of match time.
owl_data_2022_scores_wins <- owl_data_2022_scores %>% 
  select(match_id, game_number, stage_class, map_winner, map_loser, team_region,
         team_home, total_time_match, match_winner) %>% 
  group_by(match_id) %>% 
  filter(row_number() == 1, stage_class %in% "Qualifiers")

owl_data_2022_scores_wins %>% 
  group_by(team_home) %>% 
  summarise(total = sum(game_number))
## Average time per win ----
# Although some times have a higher win rate, they are not the fastest.
owl_data_2022_scores_wins %>% 
  ungroup() %>% 
  summarise(total_time = sum(total_time_match),
            total_wins = sum(game_number),
            time_to_win = total_time / total_wins)

owl_data_2022_scores_wins %>% 
  group_by(team_region, match_winner) %>% 
  summarise(total_time = sum(total_time_match),
            total_wins = sum(game_number),
            time_to_win = total_time / total_wins) %>% 
  arrange(team_region, time_to_win, total_wins, total_time)

# Win rate ----
owl_data_2022_scores_wins %>% 
  group_by(team_region, match_winner) %>% 
  summarise(total_wins = sum(game_number),
            win_rate = 100 * total_wins/24) %>% 
  arrange(team_region, desc(total_wins))
  
## Win rate in Qualifiers by team ----
owl_data_2022_scores_rate <- owl_data_2022_scores %>%
  select(match_id, game_number, stage_class, map_winner, map_loser, team_region, map_class) %>% 
  group_by(match_id, game_number) %>% 
  filter(row_number() == 1, stage_class %in% "Qualifiers") %>% 
  pivot_longer(c("map_winner", "map_loser"), names_to = "result", values_to = "team")

owl_data_2022_scores_rate %>% 
  group_by(team) %>% 
  mutate(total_maps_played = n()) %>% 
  group_by(team, result) %>% 
  summarise(team_region, total_maps_played, total_maps_win = n(),win_rate = n()/total_maps_played) %>% 
  filter(row_number() == 1, result == "map_winner") %>% 
  ungroup() %>% 
  select(team_region, team, win_rate, total_maps_win, total_maps_played, -result) %>% 
  arrange(team_region, desc(win_rate))

## Win rate in Qualifiers by team and map type ----
owl_data_2022_scores_rate %>%
  group_by(team, map_class) %>% 
  mutate(total_maps_played = n()) %>% 
  group_by(team, result, map_class) %>% 
  summarise(team_region, total_maps_played, total_maps_win = n(),win_rate = n()/total_maps_played) %>% 
  filter(row_number() == 1, result == "map_winner") %>% 
  ungroup() %>% 
  select(team_region, team, map_class, win_rate, total_maps_win, total_maps_played,-result) %>% 
  arrange(team_region, team, map_class, desc(win_rate))

# First map result into win? ----
# Global
owl_data_2022_scores_first <- owl_data_2022_scores %>% 
  select(match_id, match_winner, map_winner, game_number, stage_class) %>% 
  group_by(match_id) %>% 
  filter(row_number() == 1) %>% 
  mutate(first_map = ifelse(match_winner == map_winner, 1, 0))

owl_data_2022_scores_first %>% 
  ungroup() %>% 
  summarise(total_matches = sum(game_number),
            total_first = sum(first_map),
            total_percent = total_first/total_matches)

# By type of stage
owl_data_2022_scores_first %>% 
  group_by(stage_class) %>% 
  summarise(total_matches = sum(game_number),
            total_first = sum(first_map),
            total_percent = total_first/total_matches)
# TODO ----
# Improve data visualizations
# Look into the maps, distances, progress.
# Comparison regular season vs. tournaments
# Comparison team vs average/team