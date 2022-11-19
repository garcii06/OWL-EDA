# OWL Season 2022 EDA
# Last update of the csv: 10/26/2022
library(ggimage)
library(lubridate)
library(tidyverse)

# Importing and tidying the data ----
# From an initial import, start and end time were wrong as they got imported  as
# char instead of datetime.
owl_data_2022_scores <- read_rds("Data_all\\owl_data_2022_scores.rds")
owl_images <- read_rds("Data_all\\owl_logos.rds")

# Distribution of the matches duration in Hours ----
owl_data_2022_scores %>% 
  select(match_id, game_number, stage_class, total_time_match) %>% 
  group_by(match_id) %>% 
  filter(row_number() == 1, stage_class != "Postseason") %>%
  ggplot(aes(x = as.numeric(as.duration(total_time_match), "hour"))) +
  geom_histogram(binwidth = 0.1) +
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
  ggplot(aes(x = as.numeric(as.duration(total_time_match), "hour"), y = match_rounds)) +
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
  ggplot(aes(x = map_class, fill = map_class)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", hjust = -0.1) +
  coord_flip() +
  facet_grid(stage_class ~ .) +
  labs(x = NULL,
       y = "Total times played",
       title = "OWL Season 2022",
       subtitle = "Maps played during this season",
       caption = "Source: https://overwatchleague.com",
       fill = "Map type") +
  paletteer::scale_fill_paletteer_d("rtist::vangogh")

# Common scores ----
# Qualifiers is played as a "first to 3" game.
# In Qualifiers, the maximum is 3-2 as in case of tie, 2-2, the last map is 
# Push, where it is "impossible" to draw again.
# Tournaments are "first to 4".
owl_data_2022_scores_common <- owl_data_2022_scores %>% 
  select(match_id, score_winner, score_loser, stage_class) %>% 
  group_by(match_id) %>% 
  filter(row_number() == 1) %>%
  mutate(score_str = str_c(score_winner, score_loser, sep = "-"))

owl_data_2022_scores_common %>%
  ggplot(aes(x = score_str, fill = score_str)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.25) +
  labs(x = NULL,
       y = "Total matches",
       title = "Common scores during all season 2022",
       caption = "Source: https://overwatchleague.com",
       fill = "Score") +
  paletteer::scale_fill_paletteer_d("rtist::vangogh")

owl_data_2022_scores_common %>%
  filter(stage_class != "Postseason") %>%
  ggplot(aes(x = score_str, fill = score_str)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(. ~ stage_class, nrow = 2) +
  labs(x = NULL,
       y = "Total matches",
       title = "Common scores during season 2022",
       caption = "Source: https://overwatchleague.com",
       fill = "Final Score") +
  paletteer::scale_fill_paletteer_d("rtist::vangogh")

# Total maps taken ----
# Some maps have a defender and attacker round for both teams, others are king
# of the hill format. So, each team has a opportunity to win maps.
owl_data_2022_scores %>%
  select(match_id, score_winner, score_loser) %>% 
  group_by(match_id) %>% 
  filter(row_number() == 1) %>%
  ungroup() %>% 
  summarise(`Maps taken by winning team` = sum(score_winner),
            `Maps taken by loser team` = sum(score_loser))

# Score by map type ----
# TODO: improve visualization
owl_data_2022_scores_maps_taken <- owl_data_2022_scores %>% 
  select(match_id, game_number, map_class, map_winner, match_winner, stage_class) %>% 
  group_by(match_id, game_number) %>% 
  filter(row_number() == 1)

owl_data_2022_scores_maps_taken %>% 
  group_by(map_class) %>% 
  summarise(wins = ifelse((map_winner == match_winner), 1 , 0)) %>%
  mutate(wins = ifelse(wins == 1,"Winner", "Loser")) %>% 
  count(wins) %>% 
  ggplot(aes(x = map_class, y = n, fill = wins)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = NULL,
       y = "Total maps",
       title = "Maps taken by winner/loser match team",
       caption = "Source: https://overwatchleague.com",
       fill = "Maps taken by:") +
  paletteer::scale_fill_paletteer_d("rtist::vangogh")


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
       caption = "Source: https://overwatchleague.com",
       fill = "Map taken by:") +
  paletteer::scale_fill_paletteer_d("rtist::vangogh")
  
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

## Which teams have reversed their matches? ----
owl_reversed <- owl_data_2022_scores %>%
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
  filter(reversed == "Reversed") %>% 
  select(-reversed_possible, -reversed, -stage_class, -game_number, -map_winner, -match_id) %>% 
  rename("match_loser" = winner_second_map)

### Who accomplished a reverse sweep? ----
owl_reversed %>% 
  group_by(match_winner) %>% 
  count() %>%
  left_join(owl_images, c("match_winner" = "team_name")) %>% 
  ggplot(aes(x = n, y = reorder(match_winner, n))) +
  geom_image(aes(image = team_image), size = 0.05) +
  labs(y = NULL,
       x = "Number of reverse sweep accomplished",
       title = "Who accomplished a reverse sweep?",
       caption = "Source: https://overwatchleague.com") +
  scale_x_continuous(labels = c(0:4),
                     breaks = c(0:4),
                     expand = expansion(add = 1))

### Who accomplished a reverse sweep? Against who? How many times? ----
owl_reversed %>% 
  group_by(match_winner) %>% 
  mutate(num_reverse = n()) %>% 
  group_by(match_winner, match_loser) %>%
  mutate(num_match = row_number(),
         num_match = max(num_match)) %>% 
  left_join(owl_images, c("match_loser" = "team_name")) %>% 
  ggplot(aes(x = match_loser, y = reorder(match_winner, num_reverse))) +
  geom_image(aes(image = team_image), size = 0.05) +
  geom_text(aes(label = num_match), nudge_x = 0.5) +
  labs(y = "Winner team",
       x = "Number of reverse sweep accomplished",
       title = "Who accomplished a reverse sweep?",
       subtitle = "Against who? How many times?",
       caption = "Source: https://overwatchleague.com") +
  scale_x_discrete(labels = NULL)

# Commenting both, as the previous queries/charts have the same information
# in different format.
## Who got reverse? ----
# owl_reversed %>% 
#   group_by(map_winner) %>% 
#   count() %>%
#   left_join(owl_images, c("map_winner" = "team_name")) %>% 
#   ggplot(aes(x = n, y = reorder(map_winner, n))) +
#   geom_image(aes(image = team_image), size = 0.05) + 
#   labs(y = NULL,
#        x = "Number of matches with reverse sweep",
#        title = "Who got reverse sweep?",
#        caption = "Source: https://overwatchleague.com") +
#   scale_x_continuous(labels = c(0:10),
#                      breaks = c(0:10),
#                      expand = expansion(add = 1))

## Reversed by which team? ----
# owl_reversed %>% 
#   group_by(map_winner) %>% 
#   count(match_winner) %>%
#   left_join(owl_images, c("map_winner" = "team_name")) %>%
#   ggplot(aes(x = n, y = reorder(map_winner, n))) +
#   geom_image(aes(image = team_image), size = 0.05) + 
#   labs(y = NULL,
#        x = "Number of matches with reverse sweep",
#        title = "Who got reverse sweep?",
#        caption = "Source: https://overwatchleague.com") +
#   scale_x_continuous(labels = c(0:10),
#                      breaks = c(0:10),
#                      expand = expansion(add = 1))

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
            time_to_win = total_time / total_wins,
            time_to_win = as.numeric(as.duration(time_to_win), "hour"))

owl_data_2022_scores_wins %>% 
  group_by(team_region, match_winner) %>% 
  summarise(total_time = sum(total_time_match),
            total_wins = sum(game_number),
            time_to_win = total_time / total_wins,
            time_to_win = as.numeric(as.duration(time_to_win), "hour"),
            time_to_win = round(time_to_win, 2)) %>%
  select(team_region, match_winner, time_to_win, total_wins) %>% 
  arrange(team_region, match_winner, time_to_win, total_wins) %>% 
  rename("team region" = team_region,
         "team" = match_winner,
         "average time to get a win" = time_to_win,
         "total match wins" = total_wins)

owl_data_2022_scores_wins %>% 
  group_by(team_region, match_winner) %>% 
  summarise(total_time = sum(total_time_match),
            total_wins = sum(game_number),
            time_to_win = total_time / total_wins,
            time_to_win = as.numeric(as.duration(time_to_win), "hour")) %>% 
  arrange(team_region, time_to_win, total_wins, total_time) %>% 
  ggplot(aes(x = time_to_win,
             y = reorder(match_winner, desc(time_to_win)))) +
  geom_point() +
  geom_vline(xintercept = 1.23, colour = "red") +
  geom_text(aes(x = 1.3, y = 20,label = "League average", family = "serif"),
            colour = "red") +
  labs(x = "Average time of the match (hours)",
       y = NULL,
       title = "Average time to achive a win.",
       caption = "Source: https://overwatchleague.com")

owl_data_2022_scores_wins %>% 
  group_by(team_region, match_winner) %>% 
  summarise(total_time = sum(total_time_match),
            total_wins = sum(game_number),
            time_to_win = total_time / total_wins,
            time_to_win = as.numeric(as.duration(time_to_win), "hour")) %>% 
  arrange(team_region, time_to_win, total_wins, total_time) %>% 
  left_join(owl_images, c("match_winner" = "team_name")) %>% 
  ggplot(aes(x = time_to_win,
             y = reorder(match_winner, desc(time_to_win)))) +
  geom_image(aes(image = team_image)) +
  geom_vline(xintercept = 1.23, colour = "red") +
  geom_text(aes(x = 1.3, y = 20,label = "League average", family = "serif"),
            colour = "red") +
  labs(x = "Average time of the match (hours)",
       y = NULL,
       title = "Average time to achive a win.",
       caption = "Source: https://overwatchleague.com") +
  scale_x_continuous(expand = expansion(0.5))

# Correlation between the time to win a match and the wins.
win_base <- owl_data_2022_scores_wins %>% 
  group_by(team_region, match_winner) %>% 
  summarise(total_time = sum(total_time_match),
            total_wins = sum(game_number),
            time_to_win = total_time / total_wins,
            time_to_win = as.numeric(as.duration(time_to_win), "hour")) %>% 
  arrange(team_region, time_to_win, total_wins, total_time) 

win_base %>% 
  left_join(owl_images, c("match_winner" = "team_name")) %>% 
  ggplot(aes(x = total_wins,y = time_to_win)) +
  geom_image(aes(image = team_image)) +
  #annotate(geom = "text", x = 17, y = 1.13, label = "Seoul Dynasty") + 
  labs(x = "Matches wins",
       y = "Average time of the match (hours)",
       colour = "Number of wins",
       title = "Correlation between wins and the time to win.",
       caption = "Source: https://overwatchleague.com") 

# Win rate ----
# Global win rate of each team.
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

# Look to the wins each week. ----
owl_data_2022_scores %>% 
  select(match_id, match_winner, team_home, team_away, stage_class) %>%
  group_by(match_id) %>% 
  filter(row_number() == 1, stage_class == "Qualifiers") %>% 
  pivot_longer(c(team_home, team_away), names_to = "team_place", values_to = "team_name") %>% 
  mutate(match_wins = case_when(team_name == match_winner ~ 1,
                                   team_name != match_winner ~ 0)) %>% 
  group_by(team_name) %>% 
  arrange(team_name, match_id) %>% 
  mutate(schedule = c(0:23),
         season_points = cumsum(match_wins)) %>%
  ggplot(aes(x = schedule, y = season_points)) +
  geom_line(aes(colour = team_name)) +
  labs(x = "Schedule",
       y = "Wins",
       colour = "Team",
       title = "Roadmap of the regular season",
       caption = "Source: https://overwatchleague.com") 

# Reordered version in mini plots. 
owl_data_2022_scores %>% 
  select(match_id, match_winner, team_home, team_away, stage_class, team_region) %>%
  group_by(match_id) %>% 
  filter(row_number() == 1, stage_class == "Qualifiers") %>% 
  pivot_longer(c(team_home, team_away), names_to = "team_place", values_to = "team_name") %>% 
  mutate(match_wins = case_when(team_name == match_winner ~ 1,
                                team_name != match_winner ~ 0)) %>% 
  group_by(team_name) %>% 
  arrange(team_name, match_id) %>% 
  mutate(schedule = c(0:23),
         season_points = cumsum(match_wins)) %>%
  ggplot(aes(x = schedule, y = season_points)) +
  geom_line() +
  facet_wrap(~ reorder(team_name, season_points)) +
  labs(x = "Schedule",
       y = "Wins",
       title = "Roadmap of the regular season",
       caption = "Source: https://overwatchleague.com") +
  theme(legend.position = "none")

# TODO ----
# Improve data visualizations
# Look into the maps, distances, progress.
# Comparison regular season vs. tournaments
# Comparison team vs average/team