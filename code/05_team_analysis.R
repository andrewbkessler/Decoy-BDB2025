library(tidyverse)
library(here)

### General Data
games <- read_csv(here("data", "games.csv"))
players <- read_csv(here("data", "players.csv"))
plays <- read_csv(here("data", "plays.csv")) |> 
  mutate(unique_play_id = paste(gameId, playId, sep = "-"))
decoy_projs <- read_csv("data/decoy_projs.csv")

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








