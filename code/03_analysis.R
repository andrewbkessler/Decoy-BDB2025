library(tidyverse)
library(here)
library(caret)

### Load Data
games <- read_csv(here("data", "games.csv"))
players <- read_csv(here("data", "players.csv"))
plays <- read_csv(here("data", "plays.csv")) |> 
  mutate(unique_play_id = paste(gameId, playId, sep = "-"))
decoy_projs <- read_csv("data/decoy_projs.csv")
test_projs <- read_csv("data/test_projs.csv")


################### Model Analysis ###################

### Confusion Matrix
conf_data <- test_projs |> 
  mutate(decoy_odds = ifelse(decoy_odds > .5, 1, 0))
actual <- factor(conf_data$decoy)
predicted <- factor(conf_data$decoy_odds)
conf_matrix <- confusionMatrix(actual, predicted)
print(conf_matrix)

### Accuracy by bracket
bracket_accuracy <- test_projs |> 
  mutate(odds_bracket = ifelse(decoy_odds <= (1/3), 'low',
                          ifelse(decoy_odds > (.5), 'high', 'medium')),
         bracket_correct = case_when(
           odds_bracket == 'low' & decoy == 1 ~ 0,
           odds_bracket == 'low' & decoy == 0 ~ 1,
           odds_bracket == 'high' & decoy == 1 ~ 1,
           odds_bracket == 'high' & decoy == 0 ~ 0)
         ) |> 
  filter(odds_bracket != 'medium') |> 
  group_by(odds_bracket, bracket_correct) |> 
  summarise(n = n()) |> 
  group_by(odds_bracket) |> 
  mutate(accuracy = n/sum(n))


################### Team Analysis ###################

team_decoy_projs <- decoy_projs |> 
  left_join(select(plays, unique_play_id, possessionTeam),by='unique_play_id')

decoy_rate <- team_decoy_projs |> 
  group_by(possessionTeam, decoy) |> 
  summarise(n = n()) |> 
  group_by(possessionTeam) |> 
  mutate(rate = n/sum(n)) |> 
  filter(decoy==1)

avg_model_error <- team_decoy_projs |> 
  mutate(error = abs(decoy-decoy_odds)) |> 
  group_by(possessionTeam) |> 
  summarise(n = n(), total_error = sum(error), avg_error = total_error/n)
### Do an accuracy test too (just 50/50 cutoff)










