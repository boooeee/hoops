# Step 2: build one row per "trailing team doesn't have the ball" situation.
#
# Situation A, "kickoff":
#   4th quarter. Team T has just scored and is kicking off, and still trails
#   by 6, 7, 8 or 9 points after the extra point / 2-point try.
#
# Situation B, "opp_ball":
#   4th quarter. T trails by 6-9 and the leading team L has a 1st down
#   between its own 20 and own 35 (yardline_100 from 65 to 80). This is about
#   where a normal kickoff leaves L. One row per L drive: the first snap that
#   qualifies. Drives that began with a Situation A kickoff are dropped,
#   because the kickoff row already covers them.
#
# In both situations nflfastR has L as posteam (on kickoffs posteam is the
# RECEIVING team), T as defteam, and score_differential equals T's deficit.
#
# Output: output/situations.csv, one row per situation. Columns are described
# in README.md.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# =====================================================================
# SETTINGS
# =====================================================================
proj_dir   <- "."   # folder holding these scripts: setwd() there, or put the full path here
cache_file <- file.path(proj_dir, "data", "pbp_q3_to_ot.rds")
out_file   <- file.path(proj_dir, "output", "situations.csv")
deficits   <- 6:9
own_20     <- 80    # own 20 as yardline_100 (yards from the opponent's end zone)
own_35     <- 65    # own 35
snap_types <- c("run", "pass", "no_play", "qb_kneel", "qb_spike")
kick_regex <- "\\bkicks\\b.{0,20}?\\b-?\\d+ yards? from\\b"   # also catches re-kicks logged as no_play

# =====================================================================
# helpers
# =====================================================================
kick_era <- function(season) case_when(
  season <= 2010 ~ "1999-2010 kick from 30",
  season <= 2017 ~ "2011-2017 kick from 35",
  season <= 2023 ~ "2018-2023 new kick formation",
  TRUE           ~ "2024+ dynamic kickoff"
)

ot_rule <- function(season, season_type) case_when(
  season_type == "POST" & season <= 2009 ~ "sudden death",
  season_type == "POST" & season >= 2022 ~ "both teams possess",
  season_type == "POST"                  ~ "modified sudden death",
  season <= 2011                         ~ "sudden death",
  season >= 2025                         ~ "both teams possess (10 min)",
  season <= 2016                         ~ "modified sudden death (15 min)",
  TRUE                                   ~ "modified sudden death (10 min)"
)

points_label <- function(p) case_when(
  is.na(p) ~ "unknown",
  p == 3   ~ "FG",
  p == 6   ~ "TD, no try",
  p == 7   ~ "TD + XP",
  p == 8   ~ "TD + 2pt",
  p == 0   ~ "none (after safety?)",
  TRUE     ~ "other"
)

score_label <- function(d) case_when(d >= 6 ~ "TD", d == 3 ~ "FG", d == 2 ~ "safety", TRUE ~ "other")

# =====================================================================
# load
# =====================================================================
# file order is the true play order (order_sequence is missing in 1999-2000)
pbp <- readRDS(cache_file) %>%
  arrange(season, row_in_season) %>%
  mutate(
    pos          = row_number(),
    desc         = coalesce(desc, ""),
    is_scrimmage = !is.na(down) & !is.na(posteam),
    is_kick      = !is.na(posteam) &
                   (coalesce(kickoff_attempt == 1, FALSE) | str_detect(desc, kick_regex))
  )

# =====================================================================
# Situation A: trailing team kicking off
# =====================================================================
# A re-kick after a penalty is a second row at the same score. Keep the first
# row for the game state, and remember the last one (the kick that counted).
A <- pbp %>%
  filter(qtr == 4, is_kick) %>%
  group_by(game_id, posteam, posteam_score, defteam_score) %>%
  mutate(live_pos = last(pos), n_kicks = n()) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  filter(score_differential %in% deficits) %>%
  mutate(source = "kickoff")

# =====================================================================
# Situation B: leading team has a 1st down at its own 20-35
# =====================================================================
drive_first <- pbp %>%
  filter(is_scrimmage) %>%
  distinct(game_id, fixed_drive, .keep_all = TRUE) %>%
  select(game_id, fixed_drive, drive_first_pos = pos)

B <- pbp %>%
  filter(qtr == 4, is_scrimmage, down == 1, play_type %in% snap_types,
         score_differential %in% deficits,
         between(yardline_100, own_35, own_20)) %>%
  distinct(game_id, fixed_drive, .keep_all = TRUE) %>%   # first qualifying snap per drive
  left_join(drive_first, by = c("game_id", "fixed_drive")) %>%
  mutate(source = "opp_ball")

# =====================================================================
# outcomes, looking forward from each situation
# =====================================================================
sits <- bind_rows(A, B) %>%
  mutate(sit_id = row_number(),
         t_team = defteam, l_team = posteam, t_home = defteam == home_team,
         t_pre  = defteam_score, l_pre = posteam_score)

later_plays <- pbp %>%
  select(game_id, pos2 = pos, qtr, posteam2 = posteam, fixed_drive2 = fixed_drive,
         is_scrimmage, yl2 = yardline_100, total_home_score, total_away_score,
         secs2 = game_seconds_remaining, drive_result2 = fixed_drive_result)

# every play from the situation to the end of its game (a non-equi join)
fwd <- sits %>%
  select(sit_id, game_id, pos, t_team, t_home, t_pre, l_pre) %>%
  inner_join(later_plays, by = join_by(game_id, pos <= pos2)) %>%
  arrange(sit_id, pos2) %>%
  mutate(t_after = if_else(t_home, total_home_score, total_away_score),
         l_after = if_else(t_home, total_away_score, total_home_score),
         in_reg  = qtr == 4)

outcomes <- fwd %>%
  group_by(sit_id) %>%
  summarise(
    # next score in regulation
    k = which(in_reg & coalesce(t_after != t_pre | l_after != l_pre, FALSE))[1],
    next_score_team      = case_when(is.na(k) ~ "none",
                                     t_after[k] > t_pre[1] ~ "trailing",
                                     TRUE ~ "leading"),
    next_score_type      = if_else(is.na(k), "none",
                                   score_label(pmax(t_after[k] - t_pre[1], l_after[k] - l_pre[1]))),
    next_score_secs_left = secs2[k],
    # did T tie or lead at any later point in regulation?
    t_tied_or_led_later_reg = as.integer(any(in_reg & t_after >= l_after, na.rm = TRUE)),
    # score at the end of regulation
    reg_end_margin = last(t_after[in_reg]) - last(l_after[in_reg]),
    # T possessions after this play (regulation only)
    t_possessions_after = n_distinct(fixed_drive2[pos2 > pos[1] & in_reg & is_scrimmage &
                                                  coalesce(posteam2 == t_team[1], FALSE)]),
    # first scrimmage play at or after the situation
    first_scrim        = pos2[is_scrimmage][1],
    first_scrim_team   = posteam2[is_scrimmage][1],
    first_scrim_yl     = yl2[is_scrimmage][1],
    first_scrim_drive  = fixed_drive2[is_scrimmage][1],
    first_scrim_result = drive_result2[is_scrimmage][1],
    # no score between the situation and that first scrimmage play?
    same_score = !is.na(first_scrim) &
                 all(t_after[pos2 < first_scrim] == t_pre[1]) &
                 all(l_after[pos2 < first_scrim] == l_pre[1]),
    .groups = "drop"
  ) %>%
  select(-k)

live_kick <- pbp %>%
  select(live_pos = pos, live_td = touchdown, live_td_team = td_team, ko_desc = desc,
         ko_kick_distance = kick_distance, ko_touchback = touchback, ko_return_yards = return_yards)

sits <- sits %>%
  left_join(outcomes, by = "sit_id") %>%
  left_join(live_kick, by = "live_pos") %>%
  mutate(
    kick_td_against = coalesce(live_td == 1 & live_td_team == l_team, FALSE),
    kick_td_for     = coalesce(live_td == 1 & live_td_team == t_team, FALSE),
    got_ball        = kick_td_for | (same_score & coalesce(first_scrim_team == t_team, FALSE)),
    lead_gets_ball  = !got_ball & !kick_td_against & same_score,
    lead_start_own_yardline = case_when(
      source == "opp_ball" ~ 100 - yardline_100,
      lead_gets_ball       ~ 100 - first_scrim_yl
    ),
    lead_drive_result = case_when(
      source == "opp_ball" ~ fixed_drive_result,
      lead_gets_ball       ~ first_scrim_result,
      kick_td_against      ~ "Touchdown (kick return)"
    ),
    lead_drive_key = if_else(source == "kickoff", first_scrim_drive, NA_real_)
  )

# drop B drives already covered by a Situation A kickoff (the receiving drive)
a_drives <- sits %>%
  filter(source == "kickoff", !is.na(lead_drive_key)) %>%
  distinct(game_id, fixed_drive = lead_drive_key)
covered <- sits %>%
  filter(source == "opp_ball") %>%
  semi_join(a_drives, by = c("game_id", "fixed_drive"))
message("B: dropping ", nrow(covered), " drives that follow a Situation A kickoff")
sits <- sits %>% filter(!sit_id %in% covered$sit_id)

# what T just scored before kicking off: compare T's score now with T's
# score before the last scrimmage play (T on offense or defense)
just_scored <- sits %>%
  filter(source == "kickoff") %>%
  select(sit_id, game_id, pos, t_team) %>%
  inner_join(pbp %>% filter(is_scrimmage) %>%
               select(game_id, pos2 = pos, posteam2 = posteam, ps = posteam_score, ds = defteam_score),
             by = join_by(game_id, closest(pos > pos2))) %>%
  transmute(sit_id, t_before = if_else(posteam2 == t_team, ps, ds))

# =====================================================================
# final columns
# =====================================================================
S <- sits %>%
  left_join(just_scored, by = "sit_id") %>%
  mutate(
    trailing_team = t_team,
    leading_team  = l_team,
    deficit       = as.integer(score_differential),
    secs_left     = as.integer(game_seconds_remaining),
    mins_left     = round(secs_left / 60, 2),
    t_is_home     = as.integer(t_home),
    t_score       = as.integer(defteam_score),
    l_score       = as.integer(posteam_score),
    t_timeouts    = defteam_timeouts_remaining,
    l_timeouts    = posteam_timeouts_remaining,
    # pregame closing line from T's side: positive = T was favored
    t_pregame_spread = if_else(t_home, spread_line, -spread_line),
    # nflfastR model probabilities, flipped to T's side (posteam is L here)
    nflfastr_wp_t       = 1 - wp,
    nflfastr_vegas_wp_t = 1 - vegas_wp,
    # Situation A only
    t_points_just_scored      = if_else(source == "kickoff", defteam_score - t_before, NA_real_),
    t_score_type              = if_else(source == "kickoff", points_label(t_points_just_scored), NA_character_),
    ko_onside                 = if_else(source == "kickoff",
                                        as.integer(str_detect(ko_desc, regex("onside", ignore_case = TRUE)) |
                                                   coalesce(ko_kick_distance <= 20, FALSE)),
                                        NA_integer_),
    ko_kicking_team_recovered = if_else(source == "kickoff", as.integer(got_ball), NA_integer_),
    ko_touchback              = if_else(source == "kickoff", as.integer(coalesce(ko_touchback, 0)), NA_integer_),
    ko_return_td_against      = if_else(source == "kickoff", as.integer(kick_td_against), NA_integer_),
    across(c(ko_kick_distance, ko_return_yards), ~ if_else(source == "kickoff", .x, NA_real_)),
    ko_desc                   = if_else(source == "kickoff", ko_desc, NA_character_),
    # Situation B only (older seasons spell transitions "Punt", newer "PUNT")
    b_first_snap_of_drive = if_else(source == "opp_ball", as.integer(pos == drive_first_pos), NA_integer_),
    b_ydstogo             = if_else(source == "opp_ball", ydstogo, NA_real_),
    b_how_lead_got_ball   = if_else(source == "opp_ball",
                                    str_replace_all(toupper(drive_start_transition), " ", "_"),
                                    NA_character_),
    # final outcome for T
    t_final_margin = if_else(t_home, result, -result),
    t_win  = as.integer(t_final_margin > 0),
    t_tie  = as.integer(t_final_margin == 0),
    t_loss = as.integer(t_final_margin < 0),
    went_ot                = as.integer(reg_end_margin == 0),
    t_won_in_reg           = as.integer(reg_end_margin > 0),
    t_won_in_ot            = as.integer(reg_end_margin == 0 & t_final_margin > 0),
    t_forced_ot_or_won_reg = as.integer(reg_end_margin >= 0),
    # eras
    kick_era    = kick_era(season),
    pat_from_15 = as.integer(season >= 2015),
    ot_rule     = ot_rule(season, season_type),
    # free kicks after a safety: T didn't just score, so a different situation
    exclude_free_kick_after_safety = as.integer(source == "kickoff" & coalesce(t_points_just_scored <= 0, FALSE))
  ) %>%
  select(
    # who / when
    source, season, season_type, week, game_date, game_id, play_id,
    home_team, away_team, trailing_team, leading_team, t_is_home,
    home_coach, away_coach, roof,
    # state at the moment
    deficit, t_score, l_score, secs_left, mins_left, t_timeouts, l_timeouts,
    lead_start_own_yardline, t_pregame_spread, total_line, nflfastr_wp_t, nflfastr_vegas_wp_t,
    # Situation A only
    t_points_just_scored, t_score_type, n_kicks, ko_onside, ko_kicking_team_recovered,
    ko_touchback, ko_kick_distance, ko_return_yards, ko_return_td_against, ko_desc,
    # Situation B only
    b_first_snap_of_drive, b_ydstogo, b_how_lead_got_ball,
    # what happened next
    lead_drive_result, next_score_team, next_score_type, next_score_secs_left,
    t_possessions_after, t_tied_or_led_later_reg,
    # how it ended
    reg_end_margin, went_ot, t_final_margin, t_win, t_tie, t_loss,
    t_won_in_reg, t_won_in_ot, t_forced_ot_or_won_reg,
    # eras and flags
    kick_era, pat_from_15, ot_rule, exclude_free_kick_after_safety
  ) %>%
  arrange(season, game_id, desc(secs_left))

dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
write_csv(S, out_file, na = "")

# =====================================================================
# quick report
# =====================================================================
message("wrote ", out_file, ": ", format(nrow(S), big.mark = ","), " rows")
print(addmargins(table(S$source, S$deficit)))
message("free kicks after a safety (flagged): ", sum(S$exclude_free_kick_after_safety))
S %>%
  filter(exclude_free_kick_after_safety == 0) %>%
  group_by(source, deficit) %>%
  summarise(t_win = round(mean(t_win), 3), .groups = "drop") %>%
  pivot_wider(names_from = deficit, values_from = t_win) %>%
  print()
