library(tidyverse)
library(here)
library(gridExtra)
library(gganimate)
library(sportyR)
library(nflfastR)

### Load Data
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


#################### Play 1 animation ##################
### Play Info & Description
play_info <- decoy_data_join |> 
  filter(unique_play_id=='2022100201-855')
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

### Run animation
a1 <- animate(
  anim,
  width = 520,
  height = 320,
  fps = 10,
  end_pause = 4,
  res = 105,
)

a1

### Uncomment row below to save animation
# anim_save("animations/anim1.gif", a1)


#################### Play 2 animation ##################
### Play Info & Description
play_info <- decoy_data_join |>
  filter(unique_play_id=='2022103100-1150')
play_desc <-plays |> 
  filter(gameId == 2022103100, playId == 1150) |> 
  pull(playDescription) |> 
  str_replace("\\)\\.", "\\)")

#### Play Animations
animation_tracking <- motion_tracking |>
  filter(unique_play_id=='2022103100-1150')

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
      club == "CIN" & (event != 'ball_snap' | is.na(event)) ~ bengals_color,
      club == "CLE" & nflId != 44898 & (event != 'ball_snap' | is.na(event)) ~ browns_color,
      club == "CLE" & nflId == 44898 & (event != 'ball_snap' | is.na(event)) ~ "green",
      club == "football" ~ "white"
    ),
    pt_size = case_when(
      club == "CIN" ~ 2.8,
      club == "CLE" ~ 2.8,
      club == "football" ~ 1.4
    ),
    line_x = ifelse(between(frameId, 122, 171) & nflId == 44898, 31.35, NA),
    line_xend = ifelse(between(frameId, 122, 171) & nflId == 44898, 31.2, NA),
    line_y = ifelse(between(frameId, 122, 171) & nflId == 44898, 18.04, NA),
    line_yend = ifelse(between(frameId, 122, 171) & nflId == 44898, 23, NA),
    s_text = ifelse(between(frameId, 122, 171) & nflId == 44898, "s = 5.38", NA)
  )

# Build animation w/ NFL field
anim <- ggplot()  +
  annotate("text", 
           x = seq(20, 50, 10),
           y = 10,
           color = "#bebebe",
           # family = "Chivo",
           label = 10 * c(2:5)) +
  annotate("text", 
           x = seq(20, 50, 10),
           y = 40,
           color = "#bebebe",
           # family = "Chivo",
           label = 10 * c(2:5),
           angle = 180) +
  annotate("text", 
           x = setdiff(seq(15, 55, 1), seq(15, 55, 5)),
           y = 0,
           color = "#bebebe",
           label = "—",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(15, 55, 1), seq(15, 55, 5)),
           y = 160 / 3,
           color = "#bebebe",
           label = "—",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(15, 55, 1), seq(15, 55, 5)),
           y = 23.36667,
           color = "#bebebe",
           label = "–",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(15, 55, 1), seq(15, 55, 5)),
           y = 29.96667,
           color = "#bebebe",
           label = "–",
           angle = 90) +
  annotate("segment", 
           x = 15,
           xend = 55,
           y = c(-Inf, Inf),
           yend = c(-Inf, Inf),
           color = "#bebebe") +
  geom_vline(xintercept = seq(15, 55, 5), color = "#bebebe") +
  geom_point(data = animation_data, shape = 21,
             aes(x, y, size = pt_size, fill = pt_color)) +
  geom_segment(data = animation_data,
               aes(x=line_x, xend = line_xend, y=line_y, yend=line_yend),
               arrow = arrow(length = unit(.2, "cm"))) +
  geom_text(data = animation_data,
            aes(x = 27, y = 25, label = s_text, hjust=0)) +
  scale_size_identity() +
  scale_fill_identity() +
  transition_time(frameId) +
  ease_aes('linear') +
  coord_cartesian(xlim = c(15, 55), ylim = c(0, 160 / 3), expand = FALSE) +
  theme_minimal() +
  labs(title = "**NON-DECOY**: <span style = 'color:#FF3C00;'>**Cleveland Browns**</span> vs. <span style = 'color:#000000;'>**Cincinnati Bengals**</span>, 2022 NFL Week 8",
       subtitle = str_c("Q2: ", play_desc)) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        plot.title = ggtext::element_markdown(hjust = 0.5, size = 10),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

### Run animation
a2 <- animate(
  anim,
  width = 520,
  height = 320,
  fps = 10,
  end_pause = 4,
  res = 105,
)

a2

### Uncomment row below to save animation
# anim_save("animations/anim2.gif", a2)
