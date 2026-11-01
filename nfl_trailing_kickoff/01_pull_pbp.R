# Step 1: pull nflverse play-by-play for every season and cache a slim copy.
#
# Keeps 3rd-quarter through overtime plays. Situations are always in the 4th
# quarter; the 3rd quarter is only there to look back (a drive or a score that
# began just before the 4th started). Everything else the analysis needs
# (final score, Vegas line, nflfastR win probability) is already on each play.

library(nflreadr)
library(dplyr)
library(purrr)

# =====================================================================
# SETTINGS
# =====================================================================
proj_dir   <- "."   # folder holding these scripts: setwd() there, or put the full path here
seasons    <- 1999:nflreadr::most_recent_season()
cache_file <- file.path(proj_dir, "data", "pbp_q3_to_ot.rds")

# Columns kept. Each one is used in 02_build_situations.R.
keep <- c(
  # game identifiers
  "game_id", "play_id", "order_sequence", "season", "season_type", "week",
  "game_date", "home_team", "away_team", "home_coach", "away_coach", "roof",
  # play state
  "qtr", "game_seconds_remaining", "quarter_seconds_remaining",
  "posteam", "defteam", "down", "ydstogo", "yardline_100",
  "play_type", "play_type_nfl", "desc", "penalty", "penalty_team",
  "timeout", "timeout_team",
  # score state (posteam_score etc. are BEFORE the play,
  # total_home_score / total_away_score are AFTER the play)
  "posteam_score", "defteam_score", "score_differential",
  "total_home_score", "total_away_score",
  "posteam_timeouts_remaining", "defteam_timeouts_remaining",
  # kickoff details
  "kickoff_attempt", "own_kickoff_recovery", "touchback", "kick_distance",
  "return_yards", "return_touchdown", "kickoff_inside_twenty",
  "kickoff_in_endzone", "kickoff_out_of_bounds", "kickoff_fair_catch",
  "fumble_lost",
  # scoring flags
  "touchdown", "td_team", "field_goal_result", "safety",
  "extra_point_result", "two_point_conv_result", "defensive_two_point_conv",
  # drive info
  "fixed_drive", "fixed_drive_result", "drive_start_transition",
  # models and Vegas (wp / vegas_wp are from posteam's point of view)
  "wp", "vegas_wp", "ep", "spread_line", "total_line",
  # final outcome (result = home_score - away_score, includes OT)
  "home_score", "away_score", "result"
)

pull_season <- function(s) {
  pbp <- nflreadr::load_pbp(s)
  out <- pbp %>%
    mutate(row_in_season = row_number()) %>%   # the file's own play order
    filter(qtr >= 3) %>%
    select(row_in_season, any_of(keep))
  message(s, ": ", format(nrow(out), big.mark = ","), " Q3-OT plays")
  out
}

pbp <- map(seasons, possibly(pull_season, otherwise = NULL)) %>%
  bind_rows() %>%
  as_tibble()

dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
saveRDS(pbp, cache_file)
message("wrote ", cache_file, ": ", format(nrow(pbp), big.mark = ","), " rows, ",
        n_distinct(pbp$season), " seasons")
