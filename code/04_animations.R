library(tidyverse)
library(here)
library(gridExtra)
library(gganimate)
library(sportyR)
library(nflfastR)
# library(magick)

### General Data
games <- read_csv(here("data", "games.csv"))
players <- read_csv(here("data", "players.csv"))
plays <- read_csv(here("data", "plays.csv")) |> 
  mutate(unique_play_id = paste(gameId, playId, sep = "-"))
decoy_data_join <- read_csv("data/model_data_noOL_w_ball.csv") |> 
  mutate(decoy = ifelse(decoy=='Y',1,0)) |> 
  rename(label = decoy) |> 
  select(-down, -M_pos,
         -D1_x, -D2_x, -D3_x, -D4_x, -D5_x, -D6_x, -D7_x, -D8_x, -D9_x, -D10_x, -D11_x,
         -D1_y, -D2_y, -D3_y, -D4_y, -D5_y, -D6_y, -D7_y, -D8_y, -D9_y, -D10_y, -D11_y,
         -O1_x, -O2_x, -O3_x, -O4_x, -O5_x)
decoy_projs <- read_csv("data/decoy_projs.csv")
motion_tracking <- read_csv("data/motion_tracking.csv")
team_colors <- teams_colors_logos
browns_color <- teams_colors_logos |> filter(team_abbr=='CLE') |>  pull(team_color)
falcons_color <- teams_colors_logos |> filter(team_abbr=='ATL') |>  pull(team_color2)
bengals_color <- teams_colors_logos |> filter(team_abbr=='CIN') |>  pull(team_color2)

### Snapshots of plays
play_1_tracking <- motion_tracking |> 
  filter(unique_play_id=='2022091803-1890' & event=='ball_snap') |> 
  left_join(select(jax_plays, unique_play_id, motionNflId),by='unique_play_id') |> 
  mutate(club = ifelse(nflId==motionNflId, 'MOT',club))
play_2_tracking <- motion_tracking |>
  filter(unique_play_id=='2022092509-2417' & event=='ball_snap') |>
  left_join(select(jax_plays, unique_play_id, motionNflId),by='unique_play_id') |>
  mutate(club = ifelse(nflId==motionNflId, 'MOT',club))
play_1_info <- decoy_data_join |> 
  filter(unique_play_id=='2022091803-1890')
play_2_info <- decoy_data_join |>
  filter(unique_play_id=='2022092509-2417')
play_infos <- rbind(play_1_info, play_2_info)
play_1 <- ggplot(data=play_1_tracking, mapping = aes(x=x,y=y,color=club)) +
  geom_point(size=2) +
  xlim(0, 120) + ylim(0, 53.3)+
  ggtitle("Play 1 - Decoy",
          subtitle = paste0("Play: ", play_1_info$unique_play_id)) 
play_2 <- ggplot(data=play_2_tracking, mapping = aes(x=x,y=y,color=club)) +
  geom_point(size=2) +
  xlim(0, 120) + ylim(0, 53.3)+
  ggtitle("Play 2 - NonDecoy",
          subtitle = paste0("Play: ", play_2_info$unique_play_id))
grid.arrange(play_1, play_2, nrow=2)
# play_1


##################### Find similar plays ####################
all_plays <- read_csv(here("data", "decoy_identifier.csv")) |> 
  left_join(select(plays, unique_play_id, possessionTeam, offenseFormation, receiverAlignment),by=c('unique_play_id')) |> 
  filter(!is.na(decoy)) |> 
  left_join(select(decoy_projs, unique_play_id, decoy_odds),by='unique_play_id') |> 
  mutate(is_decoy = ifelse(decoy=='Y',1,0),
         error = abs(decoy_odds-is_decoy),
         motionNflId = nflId) |> 
  arrange(error) |> 
  filter(error < .3)

# similar_plays <- all_plays |> 
#   group_by(possessionTeam, motion_type) |> 
#   filter(n_distinct(decoy) > 1) |> 
#   # mutate(cum_error = sum(error)) |> 
#   ungroup()

find_pairs <- function(df) {
  # Group by posessionTeam and motion_type
  grouped_df <- df %>% 
    group_by(possessionTeam, motion_type) %>% 
    filter(n_distinct(decoy) > 1) %>% 
    arrange(possessionTeam, motion_type) 
  
# Find pairs of rows with different decoy values
pairs <- grouped_df %>% 
  do({
    df_sub <- .
    df_combinations <- expand.grid(seq(nrow(df_sub)), seq(nrow(df_sub)))
    df_pairs <- df_combinations %>%
      filter(Var1 < Var2) %>%
      mutate(error_sum = df_sub$error[Var1] + df_sub$error[Var2])
    
    # Find the smallest sum of error values
    min_error_pair <- df_pairs[which.min(df_pairs$error_sum), ]
    df_sub[min_error_pair$Var1, ]%>%
      bind_rows(df_sub[min_error_pair$Var2, ])
    }) %>%
  ungroup()

return(pairs)
}

# Apply the function to the all_plays dataframe
similar_plays <- find_pairs(all_plays) |> 
    left_join(select(games, gameId, week),by='gameId') |>
    left_join(select(plays, unique_play_id, quarter, down, yardsToGo, gameClock),by=c('unique_play_id')) |> 
  select(-motionNflId)
play_1_info <- decoy_data_join |> 
  filter(unique_play_id=='2022100201-855')
play_2_info <- decoy_data_join |>
  filter(unique_play_id=='2022103100-1150')
play_infos <- rbind(play_1_info, play_2_info)


### Play Description
play_desc <-plays |> 
  filter(gameId == 2022100201, playId == 855) |> 
  pull(playDescription) |> 
  str_replace("\\)\\.", "\\)")

#### Play Animations
animation_tracking <- motion_tracking |>
  filter(unique_play_id=='2022100201-855')

# Frame & players where you want to pause
pause_frame <- animation_tracking |> 
  filter(event=='ball_snap') |> distinct(frameId) |> pull(frameId)

# Number of times to duplicate the pause frame (5 seconds * fps)
fps <- 10 # frames per second
pause_length <- fps * 5

# Duplicate the pause frame and create increasing frameIds
pause_data <- animation_tracking |> filter(frameId == pause_frame) |> 
  left_join(select(players, nflId, position),by='nflId') |> 
  filter(position %in% c('QB','RB','WR','TE', 'FB') | club=='football')
# Create an empty dataframe to store the replicated rows
pause_data_replicated <- data.frame()
# Loop through each row of pause_data
for (i in 1:nrow(pause_data)) {
  # Get the current row
  current_row <- pause_data[i, ]
  # Create a new dataframe with 50 replicated rows of the current row
  replicated_rows <- current_row[rep(1, 50), ]
  # Increment the frameId for each replicated row
  replicated_rows$frameId <- current_row$frameId + 0:49
  # Append the replicated rows to the expanded dataframe
  pause_data_replicated <- rbind(pause_data_replicated, replicated_rows)
}
rm(current_row, i, replicated_rows)

# Adjust frameId for subsequent frames in original data
animation_tracking <- animation_tracking |> 
  mutate(frameId = ifelse(frameId > pause_frame, frameId + pause_length-1, frameId))

# Combine the original and duplicated frames
animation_tracking <- bind_rows(animation_tracking, pause_data_replicated)

# Sort the data by frameId
animation_tracking <- animation_tracking |> arrange(frameId)

# Add characteristics to data for animation
animation_data <- animation_tracking |> 
  mutate(
    pt_color = case_when(
      event == 'ball_snap' & nflId != 44898 ~ "grey",
      event == 'ball_snap' & nflId == 44898 ~ "green",
      club == "ATL" & (event != 'ball_snap' | is.na(event)) ~ falcons_color,
      club == "CLE" & nflId != 44898 & (event != 'ball_snap' | is.na(event)) ~ browns_color,
      club == "CLE" & nflId == 44898 & (event != 'ball_snap' | is.na(event)) ~ "green",
      club == "football" ~ "white"
    ),
    pt_size = case_when(
      club == "ATL" ~ 2.8,
      club == "CLE" ~ 2.8,
      club == "football" ~ 1.4
    ),
    line_x = ifelse(between(frameId, 126, 175) & nflId == 44898, 36.47, NA),
    line_xend = ifelse(between(frameId, 126, 175) & nflId == 44898, 36.3, NA),
    line_y = ifelse(between(frameId, 126, 175) & nflId == 44898, 25.85333, NA),
    line_yend = ifelse(between(frameId, 126, 175) & nflId == 44898, 30, NA),
    s_text = ifelse(between(frameId, 126, 175) & nflId == 44898, "s = 6.81", NA)
  )

# Build animation w/ NFL field
anim <- ggplot()  +
  annotate("text", 
           x = seq(30, 60, 10),
           y = 10,
           color = "#bebebe",
           # family = "Chivo",
           label = 10 * c(2:5)) +
  annotate("text", 
           x = seq(30, 60, 10),
           y = 40,
           color = "#bebebe",
           # family = "Chivo",
           label = 10 * c(2:5),
           angle = 180) +
  annotate("text", 
           x = setdiff(seq(25, 65, 1), seq(25, 65, 5)),
           y = 0,
           color = "#bebebe",
           label = "—",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(25, 65, 1), seq(25, 65, 5)),
           y = 160 / 3,
           color = "#bebebe",
           label = "—",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(25, 65, 1), seq(25, 65, 5)),
           y = 23.36667,
           color = "#bebebe",
           label = "–",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(25, 65, 1), seq(25, 65, 5)),
           y = 29.96667,
           color = "#bebebe",
           label = "–",
           angle = 90) +
  annotate("segment", 
           x = 15,
           xend = 65,
           y = c(-Inf, Inf),
           yend = c(-Inf, Inf),
           color = "#bebebe") +
  geom_vline(xintercept = seq(25, 65, 5), color = "#bebebe") +
  geom_point(data = animation_data, shape = 21,
             aes(x, y, size = pt_size, fill = pt_color)) +
  geom_segment(data = animation_data,
               aes(x=line_x, xend = line_xend, y=line_y, yend=line_yend),
               arrow = arrow(length = unit(.2, "cm"))) +
  geom_text(data = animation_data,
            aes(x = 34, y = 32, label = s_text, hjust=0)) +
  scale_size_identity() +
  scale_fill_identity() +
  transition_time(frameId) +
  ease_aes('linear') +
  coord_cartesian(xlim = c(25, 65), ylim = c(0, 160 / 3), expand = FALSE) +
  theme_minimal() +
  labs(title = "**DECOY**: <span style = 'color:#FF3C00;'>**Cleveland Browns**</span> vs. <span style = 'color:#000000;'>**Atlanta Falcons**</span>, 2022 NFL Week 4",
       subtitle = str_c("Q1: ", play_desc)) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        plot.title = ggtext::element_markdown(hjust = 0.5, size = 10),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

a1 <- animate(
  anim,
  width = 520,
  height = 320,
  # duration = 6.3,
  fps = 10,
  end_pause = 4,
  res = 105,
)

a1

anim_save("animations/anim1.gif", a1)



