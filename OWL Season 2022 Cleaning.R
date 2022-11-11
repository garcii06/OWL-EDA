# OWL Season 2022 Cleaning
# Last update of the csv: 10/26/2022
# As in any project the steps of importing, cleaning and mutating additional
# information is a fundamental step for the analysis. 
library(lubridate)
library(tidyverse)

# Importing and parsing the original data source ----
# From an initial import, start and end time were wrong as they got imported  as
# char instead of datetime.
owl_data <- read_csv("Data_all/match_map_stats.csv",
                     col_types = cols(
                       round_start_time = col_datetime("%m/%d/%y %H:%M"),
                       round_end_time = col_datetime("%m/%d/%y %H:%M")))

# Getting the data types by using specs function.
spec(owl_data)

# Preview of imported data.
owl_data %>% 
  head(10) %>% 
  view()

# Tidying and preprocessing ----
# To extend the analysis there is a need to add some columns, between them are:
# The total match duration.
# The type of match: (Postseason, Qualifiers, Tournament).
# The type of map played: (Control, Hybrid, Escort, Push).
# Also, filter only the matches that are from interest, in this case, just the
# season 2022 of the OWL.
owl_data_2022 <- owl_data %>% 
  filter(year(round_start_time) == 2022)

## Map lists ----
# Creating a list for every type of map will allow to extend the information of 
# the matches in the map pool for the season.
# The reason of why a list instead of vector or factor is simple, maps are just
# from one type of data, strings, and do not have an specific order.
map_escort <- list("Circuit royal", "Dorado", "Junkertown", "Route 66", "Watchpoint: Gibraltar")
map_hybrid <- list("Eichenwalde", "Hollywood", "King's Row", "Midtown", "Paraíso")
map_control <- list("Busan", "Ilios", "Lijiang Tower", "Nepal", "Oasis")
map_push <- list("Colosseo", "Esperança", "New Queen Street")

## Team Regions ----
# Also, every team is associated with a region, teams of one region play only
# between them except for some tournaments and the final where all the teams
# that qualified can play against the other region.
# East or APAC region.
region_APAC <- c("Seoul Dynasty", "Shanghai Dragons", "Philadelphia Fusion",
                    "Hangzhou Spark", "Guangzhou Charge", "Chengdu Hunters",
                    "Los Angeles Valiant")

# West or NA region.
region_NA <- c("Atlanta Reign", "Boston Uprising", "Dallas Fuel", "Florida Mayhem",
                  "Houston Outlaws", "London Spitfire", "Los Angeles Gladiators",
                  "New York Excelsior", "Paris Eternal", "San Francisco Shock",
                  "Toronto Defiant", "Vancouver Titans", "Washington Justice")
# Tidy dataset ----
# Now, it is time to arrange and enrich the original data.
owl_data_2022 <- owl_data_2022 %>%
  rename(team_home = team_one_name, team_away = team_two_name) %>% 
  group_by(match_id) %>%
  mutate(total_time_match = max(round_end_time) - min(round_start_time),
         total_time_match = total_time_match,
         match_rounds = row_number(),
         stage_class = case_when(str_detect(stage, "Postseason$") ~ "Postseason",
                                 str_detect(stage, "Qualifiers$") ~ "Qualifiers",
                                 str_detect(stage, "Tournament$") ~ "Tournament"),
         map_class = case_when(map_name %in% map_escort ~ "Escort",
                               map_name %in% map_hybrid ~ "Hybrid",
                               map_name %in% map_control ~ "Control",
                               map_name %in% map_push ~ "Push"),
         team_region = case_when(map_winner %in% region_APAC ~ "APAC region",
                                 map_winner %in% region_NA ~ "NA region")) %>% 
  ungroup()

## Data fixing ----
# Although the original data is "semi" clean, human/entry errors occurred in some
# matches. The quick way could be to remove them, but the OWL page has the 
# sufficient data to fix them.
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
# in owl_data_2022 I preferred to add them in an additional step for readability
# and to avoid monolithic code.
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

owl_data_2022_scores %>% view()

# File writing ----
# The last step is to write the data into a new file. Depending on the type of
# analysis, we can modify the structure of the file, but it is a good way to
# work from this existing file that has most of the things prepared to work.
write_rds(owl_data_2022_scores, "Data_all\\owl_data_2022_scores.rds")
write_csv(owl_data_2022_scores, "Data_all\\owl_data_2022_scores.csv")