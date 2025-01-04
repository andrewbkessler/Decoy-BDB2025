library(progress)
library(tidyverse)
library(here)

### Load Data
games <- read_csv(here("data", "games.csv"))
plays <- read_csv(here("data", "plays.csv"))
players <- read_csv(here("data", "players.csv"))
player_plays <- read_csv(here("data", "player_play.csv"))
plays_mod <- read_csv(here("data", "plays_mod.csv"))
tracking <- here("data") |>
  list.files() |>
  str_subset("tracking_week_") |>
  str_c("data/", `...` = _) |>
  map(read_csv) |>
  list_rbind()

### Make all plays go left to right (flip player direction/orientation if needed)
tracking <- tracking |>
  mutate(
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )
play_directions <- tracking |> 
  filter(!is.na(playDirection)) |> 
  distinct(gameId, playId, playDirection)
plays <- plays |> 
  left_join(select(play_directions, gameId, playId, playDirection),by=c('gameId','playId')) |> 
  mutate(targetX = ifelse(playDirection == "left", 120 - targetX, targetX),
         targetY = ifelse(playDirection == "left", 160 / 3 - targetY, targetY))

### Manually determine if player in motion at snap
### Make a blank dataframe to place data
motion_check <- data.frame(gameId = integer(), playId = integer(), nflId = integer(), displayName = character(), max_s = numeric(), tot_dis = numeric(), motion_check = logical())
### loop through tracking data, calculate inMotionAtBallSnap
for (i in 1:9) {
  tracking_fix <- read_csv(here("data", paste0("tracking_week_",i,".csv")))
  snap_frames <- tracking_fix |>
    filter(frameType=='SNAP') |>
    distinct(gameId,playId,frameId) |> rename(snapFrameId = frameId)
  temp_tracking <- tracking_fix |>
    left_join(snap_frames,by=c('gameId','playId')) |>
    filter(snapFrameId-frameId<5 & snapFrameId>=frameId)
  motion_check_temp <- temp_tracking |>
    group_by(gameId, playId, nflId, displayName) |>
    summarise(max_s = max(s), tot_dis = sum(dis)) |>
    mutate(motion_check = ifelse(max_s>.62 & tot_dis>=1.2, TRUE, FALSE))
  motion_check <- rbind(motion_check, motion_check_temp)
}
player_plays_correct <- player_plays |>
  left_join(select(motion_check, gameId, playId, nflId, motion_check), by=c('gameId','playId','nflId')) |>
  mutate(motion_check = ifelse(is.na(inMotionAtBallSnap),NA,motion_check))
rm(tracking_fix, snap_frames, temp_tracking, motion_check_temp, motion_check)

### Get only motion plays
motion_plays <- player_plays_correct |> 
  filter(motion_check==TRUE) |> 
  distinct(gameId, playId) |>
  mutate(is_motion=TRUE,
         unique_play_id = paste(gameId, playId, sep = "-"))

### Get Motion Player Plays
motion_player_plays <- player_plays_correct |>
  filter(motion_check==TRUE) |>
  mutate(unique_play_id = paste(gameId, playId, sep = "-"),
         player_play_id = paste(gameId, playId, nflId, sep = '-'))

### Find QB scrambles/sacks to filter out
qb_scrambles_sacks <- plays |> 
  filter(passResult %in% c('R','S')) |> 
  mutate(unique_play_id = paste(gameId, playId, sep = "-"))

### Find Double Motion Plays to filter out
double_motion_plays <- motion_player_plays |> 
  group_by(gameId, playId) |> tally(sort = TRUE) |> 
  filter(n>1) |> 
  mutate(unique_play_id = paste(gameId, playId, sep = "-"))

### Find Aborted Plays to filter out
aborted_plays <- plays |> 
  filter(grepl('(Aborted)', playDescription)>0) |> 
  mutate(unique_play_id = paste(gameId, playId, sep = "-"))

### Filter out scrambles, sacks, and double motion plays from plays & player_plays
motion_plays <- motion_plays |> 
  filter(!unique_play_id %in% qb_scrambles_sacks$unique_play_id &
           !unique_play_id %in% double_motion_plays$unique_play_id &
           !unique_play_id %in% aborted_plays$unique_play_id)
motion_player_plays <- motion_player_plays |> 
  filter(!unique_play_id %in% qb_scrambles_sacks$unique_play_id &
           !unique_play_id %in% double_motion_plays$unique_play_id &
           !unique_play_id %in% aborted_plays$unique_play_id)

### Get a tracking subset for motion plays
motion_tracking <- tracking |> 
  mutate(unique_play_id = paste(gameId, playId, sep = "-")) |> 
  filter(unique_play_id %in% motion_plays$unique_play_id)
rm(tracking)
### Uncomment row below to save tracking data (to be used in different script)
# write_csv(motion_tracking, "data/motion_tracking.csv")

### Label Tackle Box
tackle_tracking_at_snap <- motion_tracking |>
  mutate(player_play_id = paste(gameId, playId, nflId, sep = '-')) |>
  filter(event=='ball_snap') |>
  group_by(gameId, playId) |>
  mutate(ball_y = ifelse(club=='football',y,NA)) |>
  fill(ball_y, .direction = "updown") |>
  left_join(select(players, nflId, position),by='nflId') |>
  filter(position %in% c('C','G','T')) |>
  mutate(dist_to_ball = abs(y-ball_y),
         dist_rank = rank(dist_to_ball, ties.method = "min")) |> ungroup()
tacklebox_tracking <- tackle_tracking_at_snap |>
  filter(dist_rank <= 5) |>
  group_by(unique_play_id) |>
  summarise(left_tackle_y = max(y),
            right_tackle_y = min(y))

### Grab motions at snap
motion_at_snap <- motion_tracking |> 
  mutate(player_play_id = paste(gameId, playId, nflId, sep = '-')) |>
  filter(player_play_id %in% motion_player_plays$player_play_id) |> 
  filter(event=='ball_snap') |>
  left_join(select(players, nflId, position),by='nflId') |> 
  arrange(unique_play_id) |> 
  mutate(player_label = 'M')
### Identify motion type
motion_types <- motion_at_snap |> 
  left_join(tacklebox_tracking, by='unique_play_id') |> 
  mutate(motion_side = ifelse(between(y, right_tackle_y, left_tackle_y), 'Inside',
                              ifelse(y>=left_tackle_y, 'Left', 'Right')),
         motion_direction = ifelse(motion_side=='Inside' & between(dir, 90, 270), 'Right',
                                   ifelse(motion_side=='Inside' & !between(dir, 90, 270), 'Left',
                                          ifelse(motion_side=='Left' & between(dir, 90, 270), 'Inside',
                                                 ifelse(motion_side=='Left' & !between(dir, 90, 270), 'Outside',
                                                        ifelse(motion_side=='Right' & between(dir, 90, 270), 'Outside',
                                                               ifelse(motion_side=='Right' & !between(dir, 90, 270), 'Inside', NA)))))),
         is_jet = ifelse((dir<30 | between(dir, 150, 220) | dir > 320) & 
                           s >= 4 & between(y, right_tackle_y - 5, left_tackle_y + 5) & motion_direction != 'Outside', 'Y', 'N'),
         jet_side = ifelse(is_jet=='Y' & (dir < 90 | dir > 270), 'Left', ifelse(is_jet=='Y' & between(dir, 90, 270), 'Right', NA)),
         motion_type = ifelse(is_jet=='N',paste(motion_side, motion_direction, sep = '-'),paste0('Jet-',jet_side)))

### Get all non-motion players positions at snap & label left-to-right
### Filter out offensive lineman
non_motion_at_snap <-  motion_tracking |> 
  mutate(player_play_id = paste(gameId, playId, nflId, sep = '-')) |>
  filter(!player_play_id %in% motion_player_plays$player_play_id) |> 
  filter(event=='ball_snap') |> 
  left_join(select(tackle_tracking_at_snap, player_play_id, dist_rank),by='player_play_id') |> 
  filter(!dist_rank %in% c(1,2,3,4,5)) |>
  group_by(gameId, playId, club) |> 
  mutate(player_label_num = rank(-y, ties.method = "random")) |> 
  ungroup() |> 
  left_join(select(plays_mod, gameId, playId, possessionTeam),by=c('gameId','playId')) |>
  mutate(off_def = ifelse(displayName == 'football','fb',
                          ifelse(club==possessionTeam, 'O', 'D')),
         player_label = paste0(off_def, player_label_num)) |> 
  arrange(unique_play_id, club, -y)

### Label motion player as decoy or not
decoy_guide <- read_csv("data/decoy_guide.csv")
decoy_identifier <- motion_player_plays |> 
  select(gameId, playId, nflId, hadRushAttempt, wasTargettedReceiver) |> 
  left_join(select(plays_mod, gameId, playId, targetY, rushLocationType),by=c('gameId','playId')) |>
  left_join(select(motion_types, gameId, playId, nflId, left_tackle_y, right_tackle_y, motion_type),by=c('gameId','playId','nflId')) |> 
  mutate(pass_loc = ifelse(targetY>(2*53.3/3), 'left',
                           ifelse(between(targetY, 53.3/3, 2*53.3/3), 'middle',
                                  ifelse(targetY<53.3/3, 'right', NA)))) |> 
  mutate(receiver = ifelse(hadRushAttempt==1 | wasTargettedReceiver==1, 1, 0)) |> 
  left_join(decoy_guide, by=c('motion_type'='motionType', 'rushLocationType'='ballLocation')) |> rename('rush_decoy'='decoy') |> 
  left_join(decoy_guide, by=c('motion_type'='motionType', 'pass_loc'='ballLocation')) |> rename('pass_decoy'='decoy') |> 
  mutate(decoy = ifelse(receiver==1, 'N', coalesce(rush_decoy, pass_decoy)),
         unique_play_id = paste(gameId, playId, sep = "-"))

### Combine all data into final dataframe
motion_player <- motion_at_snap |> 
  select(unique_play_id, x, y, dir, s, o, position) |> 
  rename(M_x=x, M_y=y, M_dir=dir, M_s=s, M_o=o, M_pos=position)
O_1 <- non_motion_at_snap |> filter(player_label=='O1') |> select(unique_play_id, x, y) |> rename(O1_x=x, O1_y=y)
O_2 <- non_motion_at_snap |> filter(player_label=='O2') |> select(unique_play_id, x, y) |> rename(O2_x=x, O2_y=y)
O_3 <- non_motion_at_snap |> filter(player_label=='O3') |> select(unique_play_id, x, y) |> rename(O3_x=x, O3_y=y)
O_4 <- non_motion_at_snap |> filter(player_label=='O4') |> select(unique_play_id, x, y) |> rename(O4_x=x, O4_y=y)
O_5 <- non_motion_at_snap |> filter(player_label=='O5') |> select(unique_play_id, x, y) |> rename(O5_x=x, O5_y=y)
D_1 <- non_motion_at_snap |> filter(player_label=='D1') |> select(unique_play_id, x, y) |> rename(D1_x=x, D1_y=y)
D_2 <- non_motion_at_snap |> filter(player_label=='D2') |> select(unique_play_id, x, y) |> rename(D2_x=x, D2_y=y)
D_3 <- non_motion_at_snap |> filter(player_label=='D3') |> select(unique_play_id, x, y) |> rename(D3_x=x, D3_y=y)
D_4 <- non_motion_at_snap |> filter(player_label=='D4') |> select(unique_play_id, x, y) |> rename(D4_x=x, D4_y=y)
D_5 <- non_motion_at_snap |> filter(player_label=='D5') |> select(unique_play_id, x, y) |> rename(D5_x=x, D5_y=y)
D_6 <- non_motion_at_snap |> filter(player_label=='D6') |> select(unique_play_id, x, y) |> rename(D6_x=x, D6_y=y)
D_7 <- non_motion_at_snap |> filter(player_label=='D7') |> select(unique_play_id, x, y) |> rename(D7_x=x, D7_y=y)
D_8 <- non_motion_at_snap |> filter(player_label=='D8') |> select(unique_play_id, x, y) |> rename(D8_x=x, D8_y=y)
D_9 <- non_motion_at_snap |> filter(player_label=='D9') |> select(unique_play_id, x, y) |> rename(D9_x=x, D9_y=y)
D_10 <- non_motion_at_snap |> filter(player_label=='D10') |> select(unique_play_id, x, y) |> rename(D10_x=x, D10_y=y)
D_11 <- non_motion_at_snap |> filter(player_label=='D11') |> select(unique_play_id, x, y) |> rename(D11_x=x, D11_y=y)
fb <- non_motion_at_snap |> filter(displayName=='football') |> select(unique_play_id, x, y) |> rename(fb_x=x, fb_y=y)
model_data <- motion_player |> 
  left_join(O_1,by='unique_play_id') |> left_join(O_2,by='unique_play_id') |> left_join(O_3,by='unique_play_id') |> 
  left_join(O_4,by='unique_play_id') |> left_join(O_5,by='unique_play_id') |>
  left_join(D_1,by='unique_play_id') |> left_join(D_2,by='unique_play_id') |> 
  left_join(D_3,by='unique_play_id') |> left_join(D_4,by='unique_play_id') |> left_join(D_5,by='unique_play_id') |> 
  left_join(D_6,by='unique_play_id') |> left_join(D_7,by='unique_play_id') |> left_join(D_8,by='unique_play_id') |> 
  left_join(D_9,by='unique_play_id') |> left_join(D_10,by='unique_play_id') |> left_join(D_11,by='unique_play_id') |> 
  left_join(fb, by='unique_play_id') |> 
  left_join(select(decoy_identifier, unique_play_id, decoy),by='unique_play_id') |> filter(!is.na(decoy))
rm(O_1, O_2, O_3, O_4, O_5, O_6, O_7, D_1, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9, D_10, D_11, fb)
### Uncomment row below to save model data (to be used in different script)
# write_csv(model_data, "data/model_data_noOL_w_ball.csv")

