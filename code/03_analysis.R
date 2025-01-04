library(tidyverse)
library(here)
library(caret)
library(nflfastR)
library(gt)
library(gtExtras)

### Load Data
games <- read_csv(here("data", "games.csv"))
players <- read_csv(here("data", "players.csv"))
plays <- read_csv(here("data", "plays.csv")) |> 
  mutate(unique_play_id = paste(gameId, playId, sep = "-"))
decoy_projs <- read_csv("data/decoy_projs.csv")
test_projs <- read_csv("data/test_projs.csv")
team_info <- teams_colors_logos


################### Model Analysis ###################

### Confusion Matrix
conf_data <- test_projs |> 
  mutate(decoy_odds = ifelse(decoy_odds > .5, 1, 0))
actual <- factor(conf_data$decoy)
predicted <- factor(conf_data$decoy_odds)
conf_matrix <- confusionMatrix(actual, predicted)
print(conf_matrix)

## Recall & Precision
recall <- conf_data |> 
  filter(decoy==1) |> 
  group_by(decoy_odds) |> 
  summarise(n=n()) |>
  mutate(recall=n/sum(n))
precision <- conf_data |> 
  filter(decoy_odds==1) |> 
  group_by(decoy) |> 
  summarise(n=n()) |>
  mutate(recall=n/sum(n))

### Accuracy by bracket (for testing different thresholds)
bracket_accuracy <- test_projs |> 
  mutate(odds_bracket = ifelse(decoy_odds <= (.5), 'low', 'high'),
         bracket_correct = case_when(
           odds_bracket == 'low' & decoy == 1 ~ 0,
           odds_bracket == 'low' & decoy == 0 ~ 1,
           odds_bracket == 'high' & decoy == 1 ~ 1,
           odds_bracket == 'high' & decoy == 0 ~ 0)
         ) |> 
  group_by(odds_bracket, bracket_correct) |> 
  summarise(n = n()) |> 
  group_by(odds_bracket) |> 
  mutate(accuracy = n/sum(n))


################### Team Analysis ###################

team_decoy_projs <- decoy_projs |> 
  left_join(select(plays, unique_play_id, possessionTeam),by='unique_play_id')

### Decoy Rate
decoy_rate <- team_decoy_projs |> 
  group_by(possessionTeam, decoy) |> 
  summarise(n = n()) |> 
  group_by(possessionTeam) |> 
  mutate(tot_plays = sum(n), rate = n/sum(n)) |> 
  filter(decoy==1)

### Model Error by team
team_avg_model_error <- team_decoy_projs |> 
  mutate(error = abs(decoy-decoy_odds)) |> 
  group_by(possessionTeam) |> 
  summarise(n = n(), total_error = sum(error), avg_error = total_error/n)

### Accuracy by team
team_split_acc <- team_decoy_projs |> 
  mutate(correct = case_when(
    decoy_odds >= .5 & decoy == 1 ~ 1,
    decoy_odds >= .5 & decoy == 0 ~ 0,
    decoy_odds < .5 & decoy == 1 ~ 0,
    decoy_odds < .5 & decoy == 0 ~ 1)) |> 
  group_by(possessionTeam, correct) |> 
  summarise(n = n()) |> 
  group_by(possessionTeam) |> 
  mutate(rate = n/sum(n)) |> 
  filter(correct==1)

### Merging metrics by team
team_proj_data <- decoy_rate |> 
  ungroup() |> 
  rename(decoy_rate = rate) |> 
  left_join(select(team_avg_model_error, possessionTeam, avg_error),by='possessionTeam') |> 
  left_join(select(team_split_acc, possessionTeam, rate),by='possessionTeam') |> 
  left_join(select(team_info, team_abbr, team_logo_espn),by=c('possessionTeam'='team_abbr')) |> 
  rename(accuracy = rate) |> 
  select(-possessionTeam, -decoy, -n) |> 
  relocate(team_logo_espn) |> 
  arrange(-tot_plays)

### Team tendencies chart
team_chart <- team_proj_data |> 
  gt() |>
  gt_img_rows(team_logo_espn) |>
  data_color(columns = avg_error, palette = c("green", "red")) |>
  data_color(columns = accuracy, palette = c("red", "green")) |>
  fmt_percent(columns = decoy_rate, decimals = 1) |> 
  fmt_number(columns = c(avg_error, accuracy), decimals = 3) |> 
  cols_label(team_logo_espn = "Team", tot_plays = "Play Count", decoy_rate = "Decoy Rate",
             avg_error = "Avg Error", accuracy = "Model Accuracy") |> 
  opt_row_striping() |>
  tab_header(title = "Team Decoy Tendencies") |>
  opt_align_table_header(align = "center") |> cols_align("center")
team_chart
