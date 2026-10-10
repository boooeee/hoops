rm(list = ls(all = TRUE))

library(XML)
library(RJSONIO)
library(httr)
library(RMySQL)
library(ggplot2)
library(data.table)
library(urltools)
library(RCurl)
library(lubridate)
library(stringr)
library(tidyverse)

# =====================================================================
# SETTINGS - check these first
# =====================================================================
db_name     <- "wnba"              # MySQL database holding WNBA gms / wpg / box / player
out_dir     <- "~/Dropbox/Nba/"    # outputs are prefixed "wnba_" so NBA files aren't overwritten
yr_offset   <- 0                   # WNBA ssn should be the calendar year (2025 Finals -> ssn 2025).
                                   # If your WNBA ssn is stored like the NBA (start year), set to 1.
po_prefix   <- "104"               # WNBA playoff gid prefix (excludes Commissioner's Cup etc.)
incomplete_yr <- 2026              # season whose playoffs are still in progress (unfinished series allowed); NA once the Finals end
n_shots     <- 10                  # rows kept in bigshots
min_minutes <- 300                 # top_shooters plot cutoff (WNBA playoff careers are shorter)

sqlf<-function(x) {
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, username="root", password="", dbname=db_name, host="localhost", port=3306)
  out<-dbGetQuery(con,x)
  dbDisconnect(con)
  return(out)
}

# =====================================================================
# 1. Series probabilities for every format the WNBA has used (Bo1/3/5/7)
#    (replaces series_win.RData, which only covered NBA formats)
#    Perspective = "top seed" = home team in Game 1 of the series. With
#    every game 50/50 the label doesn't matter, only the perspective.
# =====================================================================
series_prob <- function(w, o, target = 4) {           # P(win series | score), 50/50 games
  if (w >= target) return(1)
  if (o >= target) return(0)
  0.5 * series_prob(w + 1, o, target) + 0.5 * series_prob(w, o + 1, target)
}

series_win <- expand_grid(best_of = c(1, 3, 5, 7), wins = 0:3, opp_wins = 0:3) %>%
  mutate(target = (best_of + 1) / 2) %>%
  filter(wins < target, opp_wins < target) %>%
  rowwise() %>%
  mutate(series_prob_win  = series_prob(wins + 1, opp_wins, target),   # if top seed wins this game
         series_prob_loss = series_prob(wins, opp_wins + 1, target)) %>% # if top seed loses this game
  ungroup() %>%
  select(-target)

# =====================================================================
# 2. Playoff games -> series -> round and format
#    The format comes from the league's rules for each season (po_rules), not
#    from how many games gms has, so a series with games missing still gets
#    the right format. A series' round comes from the bracket: 1 + the round
#    of the previous series either team played that season.
# =====================================================================
po_rules <- tribble(
  ~from, ~to, ~round, ~best_of, ~round_lab,
  1997, 1997, 1, 1, "SF",          # 4 teams, single games
  1997, 1997, 2, 1, "Finals",
  1998, 1998, 1, 3, "SF",
  1998, 1998, 2, 3, "Finals",
  1999, 1999, 1, 1, "R1",          # conference #1 seeds had byes
  1999, 1999, 2, 3, "CF",
  1999, 1999, 3, 3, "Finals",
  2000, 2004, 1, 3, "CSF",
  2000, 2004, 2, 3, "CF",
  2000, 2004, 3, 3, "Finals",
  2005, 2015, 1, 3, "CSF",
  2005, 2015, 2, 3, "CF",
  2005, 2015, 3, 5, "Finals",
  2016, 2021, 1, 1, "R1",          # single elimination; top four seeds had byes
  2016, 2021, 2, 1, "R2",
  2016, 2021, 3, 5, "SF",
  2016, 2021, 4, 5, "Finals",
  2022, 2024, 1, 3, "R1",
  2022, 2024, 2, 5, "SF",
  2022, 2024, 3, 5, "Finals",
  2025, Inf,  1, 3, "R1",          # set `to` and add rows if the format changes again
  2025, Inf,  2, 5, "SF",
  2025, Inf,  3, 7, "Finals"
) %>%
  group_by(from) %>%
  mutate(rounds_after = as.integer(max(round) - round)) %>%
  ungroup()

# Games gms is missing. Each result sets the series score for every later game in its
# series, so they're added here; a row is skipped once gms has that game.
missing_games <- tribble(
  ~yr,  ~dt,          ~htm,  ~atm,  ~hscr, ~ascr,
  2004, "2004-09-29", "CON", "WAS",  76,    56,     # East semis G3
  2006, "2006-08-26", "LAS", "SAC",  58,    72,     # West finals G2
  2021, "2021-10-10", "PHO", "CHI",  77,    91,     # Finals G1
  2022, "2022-08-24", "DAL", "CON",  58,    73,     # First round G3
  2022, "2022-08-28", "CHI", "CON",  63,    68      # Semis G1
) %>%
  mutate(ssn = as.integer(yr - yr_offset), dt = as_date(dt), gid = paste("manual", dt, htm, sep = "_")) %>%
  select(-yr)

po <- sqlf("SELECT * FROM gms WHERE po=1") %>%
  filter(substr(gid,1,3)==po_prefix) %>%
  mutate(dt = as_date(dt))
po <- bind_rows(po, anti_join(missing_games, po, join_by(ssn, dt, htm, atm)))

po_series <- po %>%
  select(ssn, gid, dt, htm, atm, hscr, ascr) %>%
  mutate(series_teams = paste(ssn,ifelse(htm<atm,htm,atm),ifelse(atm<htm,htm,atm),sep="_")) %>%
  arrange(ssn,series_teams,dt) %>%
  group_by(series_teams) %>%
  mutate(top_seed=first(htm),start_date=as_date(first(dt)),end_date=as_date(last(dt))) %>%
  mutate(top_seed_won=ifelse((hscr>ascr & top_seed==htm) | (hscr<ascr & top_seed==atm),1,0)) %>%
  mutate(bottom_seed_won=1-top_seed_won) %>%
  mutate(top_seed_wins=cumsum(top_seed_won),bottom_seed_wins=cumsum(bottom_seed_won)) %>%
  ungroup()

series <- po_series %>%
  group_by(ssn, series_teams) %>%
  summarise(top_seed = first(top_seed), bottom_seed = first(atm),
            start_date = first(start_date), end_date = first(end_date), games = n(),
            top_seed_wins = last(top_seed_wins), bottom_seed_wins = last(bottom_seed_wins), .groups = "drop")

# each team's previous series that season, if it had one
team_series <- series %>%
  pivot_longer(c(top_seed, bottom_seed), values_to = "team") %>%
  select(ssn, series_teams, team, start_date, end_date)
prev_series <- team_series %>%
  inner_join(select(team_series, ssn, team, prev_teams = series_teams, prev_end = end_date),
             join_by(ssn, team, start_date > prev_end)) %>%
  group_by(series_teams, team) %>%
  slice_max(prev_end, n = 1) %>%
  ungroup() %>%
  select(series_teams, prev_teams)

# round = 1 + the round of either team's previous series. Not a count of series played:
# teams with byes (1999 Finals, 2016-2021 SF/Finals) would come out a round too early.
series_round <- distinct(series, series_teams) %>% mutate(round = 1L)
for (i in 1:4) {   # 4 passes covers the deepest (4-round) bracket
  series_round <- series_round %>%
    left_join(prev_series, join_by(series_teams)) %>%
    left_join(select(series_round, prev_teams = series_teams, prev_round = round), join_by(prev_teams)) %>%
    group_by(series_teams) %>%
    summarise(round = 1L + max(coalesce(prev_round, 0L)))
}

series_outcome <- series %>%
  left_join(series_round, join_by(series_teams)) %>%
  mutate(yr = ssn + yr_offset) %>%
  left_join(po_rules, join_by(between(yr, from, to), round)) %>%
  select(-from, -to) %>%
  mutate(wins_needed   = (best_of + 1) / 2,
         series_winner = case_when(top_seed_wins == wins_needed ~ top_seed,
                                   bottom_seed_wins == wins_needed ~ bottom_seed))
print(count(series_outcome, yr, round_lab, best_of), n = 200)

# A finished series needs a winner with exactly wins_needed wins. Fewer means games are still
# missing, and every later game in that series would get the wrong series score (2021 Finals
# G1 missing put G4 on the chart as "G3 (1-1)"). More means a game is in gms twice.
format_check <- series_outcome %>%
  mutate(most_wins = pmax(top_seed_wins, bottom_seed_wins)) %>%
  filter(is.na(best_of) | most_wins > wins_needed | (most_wins < wins_needed & !yr %in% incomplete_yr))
if (nrow(format_check) > 0) {
  print(select(format_check, ssn, series_teams, round_lab, start_date, end_date, games,
               top_seed, top_seed_wins, bottom_seed_wins, best_of), width = Inf)
  stop("Series above don't fit the playoff rules - add the games gms is missing to missing_games ",
       "(or remove duplicates), or update po_rules if the format is wrong.")
}

po_situation <- po_series %>%
  inner_join(select(series_outcome,series_teams,best_of,rounds_after,round_lab),join_by(series_teams)) %>%
  arrange(ssn,series_teams,dt) %>%
  group_by(ssn,series_teams) %>%
  mutate(top_seed_sitch=lag(top_seed_wins,default = 0),bottom_seed_sitch=lag(bottom_seed_wins,default = 0)) %>%
  ungroup() %>%
  select(ssn,dt,gid,series_teams,rounds_after,round_lab,best_of,htm,atm,top_seed,top_seed_sitch,bottom_seed_sitch) %>%
  left_join(series_win,join_by(best_of,top_seed_sitch==wins,bottom_seed_sitch==opp_wins)) %>%
  # add championship probability: win this series, then 50/50 in each remaining round #
  mutate(champ_prob_win  = series_prob_win  * (1/2)^rounds_after,
         champ_prob_loss = series_prob_loss * (1/2)^rounds_after,
         series_diff = series_prob_win - series_prob_loss,
         champ_diff  = champ_prob_win  - champ_prob_loss)

bx <- sqlf("SELECT ssn, box.* FROM box INNER JOIN gms ON gms.gid=box.gid WHERE po=1")
wp <- sqlf("SELECT ssn, wpg.* FROM wpg INNER JOIN gms ON gms.gid=wpg.gid WHERE po=1")

player <- sqlf("SELECT * from player")

# add series WPA and championship WPA
bxa <- bx %>%
  filter(substr(gid,1,3)==po_prefix) %>%
  left_join(select(po_situation,gid,series_diff,champ_diff),join_by(gid)) %>%
  left_join(select(player,pid,ssn,fnm=fn,lnm=ln),join_by(pid,ssn)) %>%
  mutate(across(c(WPA,shWPA,dfWPA,ftWPA), ~ .x * series_diff, .names="{.col}_series")) %>%
  mutate(across(c(WPA,shWPA,dfWPA,ftWPA), ~ .x * champ_diff, .names="{.col}_champ"))

# top players #
top_players <- bxa %>%
  group_by(pid,fnm,lnm) %>%
  summarise(across(c(WPA,WPA_series,WPA_champ),sum)) %>%
  ungroup() %>%
  mutate(across(c(WPA,WPA_series,WPA_champ), ~ rank(- .x), .names="{.col}_rank"))

top_shooters <- bxa %>%
  group_by(pid,fnm,lnm) %>%
  mutate(fg_pts=pts-ftm) %>%
  summarise(minutes=sum(min),fg_pts=sum(fg_pts),across(c(shWPA,shWPA_series,shWPA_champ),sum)) %>%
  ungroup() %>%
  mutate(name=paste(substr(fnm,1,1),lnm)) %>%
  mutate(across(c(fg_pts,shWPA,shWPA_series,shWPA_champ), ~ rank(- .x), .names="{.col}_rank"))

library(ggrepel)
ggplot(subset(top_shooters,minutes>=min_minutes),aes(x=fg_pts,y=shWPA_champ,label=name)) +
  geom_point() +
  geom_text_repel(max.overlaps = 10) +
  theme_minimal() +
  geom_smooth(method="lm")

df <- bxa %>%
  group_by(ssn) %>%
  summarise(dfWPA=sum(dfWPA))

shots <- wp %>%
  filter(substr(gid,1,3)==po_prefix) %>%
  filter(pt1 %in% c("MK2","MK3")) %>%
  left_join(select(po_situation,dt,htm,atm,top_seed,round_lab,rounds_after,best_of,top_seed_sitch,bottom_seed_sitch,gid,series_prob_win,series_prob_loss),join_by(gid)) %>%
  left_join(select(player,pid,ssn,fnm=fn,lnm=ln),join_by(pid1==pid,ssn)) %>%
  mutate(shooter_top=(di=="h" & htm==top_seed) | (di=="a" & atm==top_seed)) %>%
  mutate(series_prob_win_shooter=ifelse(shooter_top,series_prob_win,1-series_prob_loss)) %>%
  mutate(series_prob_loss_shooter=ifelse(shooter_top,series_prob_loss,1-series_prob_win)) %>%
  mutate(champ_prob_win_shooter=series_prob_win_shooter*(1/2)^rounds_after) %>%
  mutate(champ_prob_loss_shooter=series_prob_loss_shooter*(1/2)^rounds_after) %>%
  mutate(win_prob_post=ifelse(di=="h",wp,1-wp),
         win_prob_pre=ifelse(di=="h",wp-wpa,1-wp+wpa)) %>%
  mutate(series_prob_win_post=win_prob_post*series_prob_win_shooter+(1-win_prob_post)*series_prob_loss_shooter,
         series_prob_win_pre=win_prob_pre*series_prob_win_shooter+(1-win_prob_pre)*series_prob_loss_shooter) %>%
  mutate(champ_prob_win_post=win_prob_post*champ_prob_win_shooter+(1-win_prob_post)*champ_prob_loss_shooter,
         champ_prob_win_pre=win_prob_pre*champ_prob_win_shooter+(1-win_prob_pre)*champ_prob_loss_shooter) %>%
  mutate(champ_prob_change=champ_prob_win_post-champ_prob_win_pre) %>%
  filter(champ_prob_win_pre>0,champ_prob_win_pre<1) %>%
  # clamp so a title-clinching shot (post = 100%) doesn't produce NaN
  mutate(pre_c=pmin(pmax(champ_prob_win_pre,1e-9),1-1e-9),
         post_c=pmin(pmax(champ_prob_win_post,1e-9),1-1e-9)) %>%
  mutate(champ_prob_kl= post_c*log(post_c/pre_c,base=2) + (1-post_c)*log((1-post_c)/(1-pre_c),base=2)) %>%
  select(-pre_c,-post_c) %>%
  # shooter's side of the series record (before this game) and of the score (after this shot)
  mutate(shooter_wins=ifelse(shooter_top,top_seed_sitch,bottom_seed_sitch),
         opp_wins=ifelse(shooter_top,bottom_seed_sitch,top_seed_sitch),
         shooter_pts=ifelse(di=="h",h,a),
         opp_pts=ifelse(di=="h",a,h),
         shot_pts=ifelse(pt1=="MK3",3,2)) %>%
  select(ssn, dt, gid, htm, atm, q, min, sec, wp, wpa, m, ma, h, a, d, pid1, fnm, lnm,
         round_lab, rounds_after, best_of, top_seed, top_seed_sitch, bottom_seed_sitch,
         shooter_wins, opp_wins, shooter_pts, opp_pts, shot_pts,
         starts_with("series_prob"), starts_with("champ_prob"), win_prob_pre, win_prob_post)

# sanity check: every made shot should help the shooter's title odds
summary(shots$champ_prob_change)

# rank separately, so the KL chart is the top n by KL, not the top n by change re-sorted
bigshots    <- shots %>% arrange(desc(champ_prob_change)) %>% slice_head(n=n_shots)
bigshots_kl <- shots %>% arrange(desc(champ_prob_kl)) %>% slice_head(n=n_shots)

library(ggplot2)
library(dplyr)
library(scales)

# =====================================================================
# Leverage grid - current WNBA format (2025+: Bo3 / Bo5 / Bo7)
# =====================================================================
cur_fmt <- tibble(round = 1:3,
                  round_name = c("First Round (Bo3)", "Semifinals (Bo5)", "WNBA Finals (Bo7)"),
                  best_of = c(3, 5, 7),
                  rounds_after = c(2, 1, 0))

leverage <- cur_fmt %>%
  inner_join(series_win, join_by(best_of)) %>%
  mutate(champ_prob_win  = series_prob_win  * 0.5^rounds_after,
         champ_prob_loss = series_prob_loss * 0.5^rounds_after,
         champ_swing     = champ_prob_win - champ_prob_loss)

leverage |>
  mutate(round_name = factor(round_name, levels = cur_fmt$round_name)) |>
  ggplot(aes(x = wins, y = opp_wins, fill = champ_swing)) +
  geom_tile(color = "white", linewidth = 1.1) +
  geom_text(aes(label = percent(champ_swing, accuracy = 0.1),
                color = champ_swing > 0.5),
            size = 3.1, fontface = "bold") +
  facet_wrap(~ round_name, nrow = 1, scales = "free") +
  scale_fill_gradientn(
    colours = c("#fff5eb", "#fdbe85", "#fd8d3c", "#e6550d", "#a63603"),
    labels = percent, limits = c(0, 1), name = "Title-odds\nswing"
  ) +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"), guide = "none") +
  scale_x_continuous(breaks = 0:3, expand = c(0, 0)) +
  scale_y_continuous(breaks = 0:3, expand = c(0, 0)) +
  labs(
    title = "How much does one game move your WNBA title odds?",
    subtitle = "Swing in championship probability from winning vs. losing a single game, by round and series score (all games 50/50)",
    x = "Your wins in the series", y = "Opponent wins",
    caption = "A winner-take-all game doubles in value each round: 25% → 50% → 100%"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    aspect.ratio  = 1,
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10, margin = margin(b = 8)),
    plot.caption  = element_text(color = "grey45", hjust = 0),
    panel.grid    = element_blank(),
    strip.text    = element_text(face = "bold", size = 11),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(paste0(out_dir, "wnba_championship_leverage.png"), width = 12, height = 4.2, dpi = 300)

library(gt)

tbl <- leverage |>
  mutate(
    game  = wins + opp_wins + 1,
    score = paste0(wins, "–", opp_wins),
    round_name = factor(round_name, levels = cur_fmt$round_name)
  ) |>
  arrange(desc(champ_swing), desc(as.integer(round_name)), desc(game)) |>
  mutate(rank = row_number()) |>
  select(rank, round = round_name, game, score, champ_prob_win, champ_prob_loss, champ_swing) %>%
  filter(rank<=25)

tbl_gt <- gt(tbl) |>
  tab_header(
    title = md("**WNBA games ranked by championship leverage**"),
    subtitle = "How much winning a single game moves your title odds — current format, all games assumed 50/50"
  ) |>
  cols_label(
    rank = "#", round = "Round", game = "Game", score = "Series",
    champ_prob_win  = "Title odds if WIN",
    champ_prob_loss = "Title odds if LOSE",
    champ_swing     = "Swing"
  ) |>
  fmt_percent(c(champ_prob_win, champ_prob_loss, champ_swing), decimals = 1) |>
  data_color(
    columns = champ_swing,
    fn = scales::col_numeric(c("#fff5eb", "#fd8d3c", "#a63603"), domain = c(0, 1))
  ) |>
  tab_style(style = cell_text(color = "#1a7a1a"), locations = cells_body(champ_prob_win)) |>
  tab_style(style = cell_text(color = "#b22222"), locations = cells_body(champ_prob_loss)) |>
  tab_style(style = cell_text(weight = "bold"),  locations = cells_body(c(rank, champ_swing))) |>
  cols_align("center", c(rank, game, score, champ_prob_win, champ_prob_loss, champ_swing)) |>
  cols_align("left", round) |>
  tab_source_note(md("*Swing = P(champion | win this game) − P(champion | lose this game)*")) |>
  opt_row_striping() |>
  tab_options(
    table.font.size = px(13),
    data_row.padding = px(4),
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold"
  )

gtsave(tbl_gt, paste0(out_dir, "wnba_championship_leverage_table.png"))     # needs webshot2 + Chrome

# =====================================================================
# Biggest shots chart (simple version, ranked by KL)
# =====================================================================
bar_col <- "#e6701a"

# Row labels, all from the shooter's side: series record going into the game, score before -> after.
# The WNBA played two 20-minute halves through 2005, but wpg numbers those periods like quarters
# (1999/2003 game-enders came out as "Q4"), so label q 1-2 / 3-4 as H1 / H2 and add back the
# half's first 10 minutes for q 1 and 3. Under a minute show tenths like the scoreboard, rather
# than truncating (2.9 s used to print as 0:02).
shot_labels <- function(d) d |>
  mutate(
    yr     = ssn + yr_offset,
    halves = yr <= 2005 & q <= 4,
    period = case_when(q > 4  ~ paste0("OT", q - 4),
                       halves ~ paste0("H", ceiling(q / 2)),
                       TRUE   ~ paste0("Q", q)),
    secs   = min * 60 + sec + ifelse(halves & q %% 2 == 1, 600, 0),
    clock  = ifelse(secs < 60, sprintf("0:%04.1f", secs),
                    sprintf("%d:%02d", as.integer(secs %/% 60), as.integer(floor(secs %% 60)))),
    game   = top_seed_sitch + bottom_seed_sitch + 1,
    series = paste0(shooter_wins, "–", opp_wins),
    score  = paste0(shooter_pts - shot_pts, "–", opp_pts, " → ", shooter_pts, "–", opp_pts),
    l1 = paste0(rank, ".  ", fnm, " ", lnm, "   ·   ", yr),
    l2 = paste0(round_lab, " G", game, " (", series, ")  ·  ", period, " ", clock, "  ·  ", score),
    yn = rev(rank)
  )

d <- bigshots_kl |>
  arrange(desc(champ_prob_kl)) |>
  mutate(rank = row_number()) |>
  shot_labels()
n <- nrow(d)

ggplot(d) +
  geom_segment(aes(x = champ_prob_win_pre, xend = champ_prob_win_post, y = yn, yend = yn),
               color = bar_col, linewidth = 1.8, lineend = "round") +
  geom_point(aes(x = champ_prob_win_pre,  y = yn), color = "grey70", size = 3) +
  geom_point(aes(x = champ_prob_win_post, y = yn), color = bar_col, size = 4.6) +
  geom_text(aes(x = champ_prob_win_pre - 0.012, y = yn, label = percent(champ_prob_win_pre, 0.1)),
            hjust = 1, size = 3.1, color = "grey55") +
  geom_text(aes(x = champ_prob_win_post + 0.012, y = yn, label = percent(champ_prob_win_post, 0.1)),
            hjust = 0, size = 3.5, fontface = "bold", color = "grey20") +
  geom_text(aes(x = -1.15, y = yn + 0.20, label = l1), hjust = 0, size = 4.0, fontface = "bold") +
  geom_text(aes(x = -1.15, y = yn - 0.22, label = l2), hjust = 0, size = 3.3, color = "grey45") +
  geom_text(aes(x = 1.18, y = yn, label = sprintf("%.3f", champ_prob_kl)),
            hjust = 1, size = 3.8, fontface = "bold", color = "#7a2602") +
  annotate("text", x = 1.18, y = n + 0.8, label = "KL", hjust = 1, fontface = "bold", size = 3.9) +
  annotate("text", x = 0.42, y = n + 0.8, label = "Championship odds:  before → after",
           hjust = 0.5, fontface = "bold", size = 3.9, color = "grey30") +
  scale_x_continuous(breaks = c(0, .25, .5, .75, 1), labels = percent, limits = c(-1.18, 1.22)) +
  scale_y_continuous(expand = expansion(add = c(0.6, 1.5))) +
  labs(
    title = "The biggest shots in WNBA history, by championship leverage",
    subtitle = "Ranked by KL divergence between title odds before and after the shot • all games modeled 50/50",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "grey35", size = 12, margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(14, 16, 12, 16)
  )

ggsave(paste0(out_dir, "wnba_biggest_shots_kl.png"), width = 10, height = 1.2 + 0.62 * nrow(bigshots_kl), dpi = 300)

# =====================================================================
# Biggest shots chart with headshots (ranked by title-odds change)
# =====================================================================
library(ggpath)

track_col <- "#ececec"
band_col  <- "#f7f7f7"
xL <- -1.72; xR <- 2.02
kl0 <- 1.24; kl1 <- 1.58

# Players with no usable headshot on the WNBA CDN: add image URL or local path keyed by pid1, e.g.
# headshot_overrides <- c("100720" = "https://...")
headshot_overrides <- character(0)

d <- bigshots |>
  arrange(desc(champ_prob_change)) |>
  mutate(
    rank = row_number(),
    headshot_url = paste0("https://cdn.wnba.com/headshots/wnba/latest/1040x760/", pid1, ".png"),
    headshot_url = coalesce(unname(headshot_overrides[as.character(pid1)]), headshot_url),
    chg_frac = champ_prob_change / max(champ_prob_change)
  ) |>
  shot_labels()
n <- nrow(d)

ggplot(d) +
  geom_rect(data = filter(d, rank %% 2 == 0),
            aes(xmin = xL, xmax = xR, ymin = yn - 0.5, ymax = yn + 0.5), fill = band_col) +
  geom_from_path(aes(x = -1.55, y = yn, path = headshot_url), width = 0.085) +
  geom_text(aes(x = -1.31, y = yn + 0.22, label = l1), hjust = 0, size = 5.9, fontface = "bold") +
  geom_text(aes(x = -1.31, y = yn - 0.25, label = l2), hjust = 0, size = 4.5, color = "grey45") +
  geom_segment(aes(x = champ_prob_win_pre, xend = champ_prob_win_post, y = yn, yend = yn),
               color = bar_col, linewidth = 2.1, lineend = "round") +
  geom_point(aes(x = champ_prob_win_pre,  y = yn), color = "grey70", size = 3.8) +
  geom_point(aes(x = champ_prob_win_post, y = yn), color = bar_col, size = 5.6) +
  geom_text(aes(x = champ_prob_win_pre - 0.030, y = yn, label = percent(champ_prob_win_pre, 0.1)),
            hjust = 1, size = 4.7, color = "grey55") +
  geom_text(aes(x = champ_prob_win_post + 0.034, y = yn, label = percent(champ_prob_win_post, 0.1)),
            hjust = 0, size = 5.2, fontface = "bold", color = "grey20") +
  geom_segment(aes(x = kl0, xend = kl1, y = yn, yend = yn),
               color = track_col, linewidth = 6.6, lineend = "round") +
  geom_segment(aes(x = kl0, xend = kl0 + chg_frac * (kl1 - kl0), y = yn, yend = yn),
               color = "#a63603", linewidth = 6.6, lineend = "round") +
  geom_text(aes(x = kl1 + 0.05, y = yn, label = sprintf("%+.1f pts", 100 * champ_prob_change)),
            hjust = 0, size = 5.2, fontface = "bold", color = "#7a2602") +
  annotate("text", x = 0.42, y = n + 0.95, label = "Championship odds:  before → after",
           hjust = 0.5, fontface = "bold", size = 5.4, color = "grey30") +
  annotate("text", x = (kl0 + kl1) / 2, y = n + 0.95, label = "Title-odds swing",
           hjust = 0.5, fontface = "bold", size = 5.4, color = "grey30") +
  scale_x_continuous(breaks = c(0, .25, .5, .75, 1), labels = percent, limits = c(xL, xR)) +
  scale_y_continuous(expand = expansion(add = c(0.55, 0.75))) +
  labs(
    title = "The biggest shots in WNBA history, by championship impact",
    subtitle = "Ranked by swing in title odds before and after the shot • all games modeled 50/50",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(color = "grey35", size = 15.5, margin = margin(b = 16)),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 17, color = "grey45"),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 20, 14, 20)
  )

ggsave(paste0(out_dir, "wnba_biggest_shots.png"), width = 13, height = 1.6 + 0.95 * nrow(bigshots), dpi = 300)
