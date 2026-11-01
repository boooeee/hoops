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
out_dir     <- "~/Dropbox/wnba/"   # where the PNGs and CSV are written (plus fonts/ and headshot_cache/)
yr_offset   <- 0                   # WNBA ssn should be the calendar year (2025 Finals -> ssn 2025).
                                   # If your WNBA ssn is stored like the NBA (start year), set to 1.
po_prefix   <- "104"               # WNBA playoff gid prefix (excludes Commissioner's Cup etc.)
incomplete_yr <- 2026              # season whose playoffs are still in progress (unfinished series allowed); NA once the Finals end
n_shots     <- 10                  # rows kept in bigshots
n_shots_long <- 25                 # rows on the long board (one line per shot)
board_text_scale <- 1.2            # type size on the biggest-shots boards (1 = compact, 1.5 = large);
                                   # the layout re-fits around it
min_minutes <- 300                 # top_shooters plot cutoff (WNBA playoff careers are shorter)
headshot_files <- paste0(out_dir, "wnba_headshots/")  # your own headshots: a folder, or image files, or
                                   # both; each named by pid1 ("100639.png") or player name ("Deanna Nolan.jpg").
                                   # These beat every other source.

# MySQL login comes from ~/.Renviron, same variables as the WNBA win-probability scripts:
#   WNBA_DB_USER=root   (optional, defaults to root)
#   WNBA_DB_PASS=...
# Re-read here so an edit to ~/.Renviron is picked up without restarting R.
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")
if (!nzchar(Sys.getenv("WNBA_DB_PASS")))
  message("WNBA_DB_PASS is not set in ~/.Renviron - connecting to MySQL with an empty password")

sqlf<-function(x) {
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, username=Sys.getenv("WNBA_DB_USER", "root"), password=Sys.getenv("WNBA_DB_PASS"),
                   dbname=db_name, host="localhost", port=3306)
  out<-dbGetQuery(con,x)
  dbDisconnect(con)
  return(out)
}

# =====================================================================
# House style - the evollve graphics tokens, set in Open Sans
# =====================================================================
INK      <- "#22211e"   # primary text
INK_SOFT <- "#6f6b64"   # secondary text
RULE     <- "#d9d6d0"
STRIPE   <- "#f4f3f0"
PAPER    <- "#ffffff"
ACCENT   <- "#3b6ea5"

# Open Sans is a webfont, so if this machine doesn't have it, fetch it once into a cache
# beside the graphics and register it (SIL Open Font License). A family R can't find
# falls back to the device default silently, so say so.
FONT_FAMILY <- "Open Sans"
FONT_URLS   <- c(plain = "https://raw.githubusercontent.com/googlefonts/opensans/main/fonts/ttf/OpenSans-Regular.ttf",
                 bold  = "https://raw.githubusercontent.com/googlefonts/opensans/main/fonts/ttf/OpenSans-Bold.ttf")
font_found <- function(fam) tolower(fam) %in% tolower(c(systemfonts::system_fonts()$family,
                                                        systemfonts::registry_fonts()$family))
if (!font_found(FONT_FAMILY)) {
  font_files <- file.path(path.expand(out_dir), "fonts", basename(FONT_URLS))
  dir.create(dirname(font_files[1]), showWarnings = FALSE, recursive = TRUE)
  for (i in seq_along(FONT_URLS)) if (!file.exists(font_files[i]))
    try(download.file(FONT_URLS[i], font_files[i], mode = "wb", quiet = TRUE), silent = TRUE)
  if (all(file.exists(font_files) & file.size(font_files) > 50000))   # not an error page
    systemfonts::register_font(FONT_FAMILY, plain = font_files[1], bold = font_files[2])
}
FONT <- if (font_found(FONT_FAMILY)) FONT_FAMILY else ""
if (!nzchar(FONT)) message("Open Sans not found or fetched - graphics use the device default font")
update_geom_defaults("text", list(family = FONT))   # geom_text doesn't inherit the theme font

# A plot pane can refuse a registered font ("invalid font type") that the PNG device
# renders fine. The preview is a convenience and the .png is the deliverable.
preview <- function(plot) tryCatch(print(plot), error = function(e)
  message("Preview skipped (", conditionMessage(e), ") - the .png is unaffected"))

# ragg sees registered fonts; the default png device may not
save_png <- function(plot, file, width, height, dpi = 300)
  ggsave(paste0(out_dir, file), plot, width = width, height = height, dpi = dpi, bg = PAPER,
         device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png")

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

# A team's score before a play: the highest score it had in that game below its score after
# the play. Scores only go up, so this doesn't depend on the order wpg's rows come back in.
# The shot's value comes from this rather than pt1, which has some made 3s coded MK2 (2026
# Finals G7, Gabby Williams, 57-57 -> 60-57 was labelled 58-57 -> 60-57).
score_before <- function(x) {
  s <- sort(unique(c(0, x)))
  s[pmax(findInterval(x, s, left.open = TRUE), 1)]
}

shots <- wp %>%
  filter(substr(gid,1,3)==po_prefix) %>%
  group_by(gid) %>%
  mutate(h_pre=score_before(h), a_pre=score_before(a)) %>%
  ungroup() %>%
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
         shot_pts_coded=ifelse(pt1=="MK3",3,2),
         shot_pts=shooter_pts-ifelse(di=="h",h_pre,a_pre),
         # a jump that isn't 2 or 3 is a score glitch, not a shot value - fall back to pt1
         shot_pts=ifelse(shot_pts %in% 2:3,shot_pts,shot_pts_coded)) %>%
  select(ssn, dt, gid, htm, atm, q, min, sec, wp, wpa, m, ma, h, a, d, pid1, pt1, fnm, lnm,
         round_lab, rounds_after, best_of, top_seed, top_seed_sitch, bottom_seed_sitch,
         shooter_wins, opp_wins, shooter_pts, opp_pts, shot_pts, shot_pts_coded,
         starts_with("series_prob"), starts_with("champ_prob"), win_prob_pre, win_prob_post)

# sanity check: every made shot should help the shooter's title odds
summary(shots$champ_prob_change)

# made shots whose pt1 code disagrees with the score change (the labels use the score change)
shots %>% filter(shot_pts!=shot_pts_coded) %>% count(ssn, pt1, shot_pts) %>% print(n=Inf)

# rank separately, so the KL chart is the top n by KL, not the top n by change re-sorted
bigshots    <- shots %>% arrange(desc(champ_prob_change)) %>% slice_head(n=n_shots)
bigshots_kl <- shots %>% arrange(desc(champ_prob_kl)) %>% slice_head(n=n_shots)

library(scales)
library(magick)

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

LEVERAGE_RAMP <- c("#f1f5fa", "#c6d6ea", "#8eaed3", ACCENT, "#22446b")   # one hue, light -> dark

leverage_grid <- leverage |>
  mutate(round_name = factor(round_name, levels = cur_fmt$round_name)) |>
  ggplot(aes(x = wins, y = opp_wins, fill = champ_swing)) +
  geom_tile(color = PAPER, linewidth = 1.1) +
  geom_text(aes(label = percent(champ_swing, accuracy = 0.1),
                color = champ_swing > 0.5),
            size = 9 / .pt, fontface = "bold") +
  facet_wrap(~ round_name, nrow = 1, scales = "free") +
  scale_fill_gradientn(colours = LEVERAGE_RAMP, labels = percent, limits = c(0, 1),
                       name = "Title-odds\nswing") +
  scale_color_manual(values = c(`TRUE` = PAPER, `FALSE` = INK), guide = "none") +
  scale_x_continuous(breaks = 0:3, expand = c(0, 0)) +
  scale_y_continuous(breaks = 0:3, expand = c(0, 0)) +
  labs(
    title = "How much does one game move your WNBA title odds?",
    subtitle = "Swing in championship probability from winning vs. losing a single game, by round and series score (all games 50/50)",
    x = "Your wins in the series", y = "Opponent wins",
    caption = "A winner-take-all game doubles in value each round: 25% → 50% → 100%"
  ) +
  theme_minimal(base_size = 12, base_family = FONT) +
  theme(
    aspect.ratio  = 1,
    text          = element_text(colour = INK),
    axis.text     = element_text(colour = INK_SOFT),
    axis.title    = element_text(colour = INK_SOFT, size = 10),
    legend.text   = element_text(colour = INK_SOFT, size = 9),
    legend.title  = element_text(colour = INK_SOFT, size = 9, margin = margin(b = 8)),
    plot.title    = element_text(face = "bold", size = 17, margin = margin(b = 4)),
    plot.subtitle = element_text(colour = INK_SOFT, size = 10.5, margin = margin(b = 10)),
    plot.caption  = element_text(colour = INK_SOFT, size = 9, hjust = 0, margin = margin(t = 10)),
    plot.title.position = "plot", plot.caption.position = "plot",
    panel.grid    = element_blank(),
    strip.text    = element_text(face = "bold", size = 11, colour = INK),
    plot.background = element_rect(fill = PAPER, colour = NA),
    plot.margin   = margin(18, 18, 12, 18)
  )
preview(leverage_grid)
save_png(leverage_grid, "wnba_championship_leverage.png", width = 11.2, height = 4.4)

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
  data_color(columns = champ_swing, fn = scales::col_numeric(LEVERAGE_RAMP, domain = c(0, 1))) |>
  tab_style(style = cell_text(color = INK_SOFT), locations = cells_body(c(rank, champ_prob_loss))) |>
  tab_style(style = cell_text(weight = "bold"),  locations = cells_body(c(rank, champ_swing))) |>
  tab_style(style = cell_text(color = INK_SOFT), locations = cells_column_labels()) |>
  tab_style(style = cell_text(color = INK_SOFT), locations = cells_title("subtitle")) |>
  cols_align("center", c(rank, game, score, champ_prob_win, champ_prob_loss, champ_swing)) |>
  cols_align("left", round) |>
  tab_source_note(md("*Swing = P(champion | win this game) − P(champion | lose this game)*")) |>
  opt_row_striping() |>
  opt_table_font(font = list(google_font(FONT_FAMILY), default_fonts())) |>
  tab_options(
    table.font.color = INK,
    table.font.size = px(13),
    data_row.padding = px(4),
    heading.align = "left",
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold",
    row.striping.background_color = STRIPE,
    table.border.top.color = PAPER,
    heading.border.bottom.color = RULE,
    column_labels.border.bottom.color = RULE,
    table_body.hlines.color = PAPER,
    table_body.border.bottom.color = RULE
  )

gtsave(tbl_gt, paste0(out_dir, "wnba_championship_leverage_table.png"))     # needs webshot2 + Chrome

# =====================================================================
# Biggest-shots boards
#   Drawn entirely inside one panel measured in inches (the evollve board
#   approach), with the text column and the gutters either side of the 0-100%
#   track sized from the measured width of the actual labels - so no label can
#   run into another, however long a name or score line is.
# =====================================================================
ROUND_TXT <- c(Finals = "Finals", SF = "Semifinals", CF = "Conf. Finals", CSF = "Conf. Semis",
               R1 = "Round 1", R2 = "Round 2")

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
    series = case_when(shooter_wins >  opp_wins ~ paste0("up ",   shooter_wins, "–", opp_wins),
                       shooter_wins <  opp_wins ~ paste0("down ", shooter_wins, "–", opp_wins),
                       TRUE                     ~ paste0("tied ", shooter_wins, "–", opp_wins)),
    stage  = case_when(best_of == 1 ~ ROUND_TXT[round_lab],
                       game == 1    ~ paste0(ROUND_TXT[round_lab], " G1"),
                       TRUE         ~ paste0(ROUND_TXT[round_lab], " G", game, ", ", series)),
    score  = paste0(shooter_pts - shot_pts, "–", opp_pts, " → ", shooter_pts, "–", opp_pts),
    name   = paste(fnm, lnm),
    stage_txt = paste(yr, stage),
    play_txt  = paste0(period, " ", clock, " · ", score),
    detail    = paste0(stage_txt, " · ", play_txt)
  )

# ---- Headshots --------------------------------------------------------------
# Resolution order, best first:
#   1. your own image in headshot_files, named by pid1 or player name ("100639.png",
#      "Deanna Nolan.jpg" - case, spaces and punctuation are ignored);
#   2. headshot_overrides: an image URL or file for a pid1;
#   3. the WNBA CDN by pid1. The CDN answers a player it has no photo for with a
#      generic silhouette, so a download identical to the one it serves for a
#      nonexistent id counts as missing;
#   4. an initials tile.
# Every headshot is cropped to a circle on AVATAR_BG, so cut-outs and photos with their
# own backgrounds look alike. Downloads are cached in out_dir/headshot_cache (delete a
# file to re-fetch it). A missing headshot is cosmetic, so nothing here stops the script.
HEADSHOT_URL <- "https://cdn.wnba.com/headshots/wnba/latest/1040x760/%s.png"
headshot_overrides <- character(0)   # e.g. c("100639" = "https://.../nolan.png")
AVATAR_BG <- "#e9e6e1"

name_key    <- function(x) gsub("[^a-z0-9]", "", tolower(x))
IMAGE_EXT   <- "\\.(png|jpe?g|webp|gif)$"
local_heads <- unlist(lapply(path.expand(headshot_files), function(p)
  if (dir.exists(p)) list.files(p, pattern = IMAGE_EXT, ignore.case = TRUE, full.names = TRUE)
  else if (file.exists(p)) p))
local_keys  <- name_key(tools::file_path_sans_ext(basename(local_heads)))
gone <- headshot_files[!file.exists(path.expand(headshot_files))]
if (length(gone)) message("headshot_files not found: ", paste(gone, collapse = ", "))
cat("Headshots:", length(local_heads), "local file(s):", paste(basename(local_heads), collapse = ", "), "\n")

cache_dir <- file.path(path.expand(out_dir), "headshot_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
fetch_image <- function(src, dest) {
  if (file.exists(path.expand(src))) return(path.expand(src))       # a local file
  if (!file.exists(dest)) try(download.file(src, dest, mode = "wb", quiet = TRUE), silent = TRUE)
  if (file.exists(dest) && file.size(dest) > 1000) return(dest)     # smaller = an error page
  unlink(dest)
  NA_character_
}
cdn_silhouette <- fetch_image(sprintf(HEADSHOT_URL, "0"), file.path(cache_dir, "cdn_silhouette.png"))
headshot_path <- function(pid, name) {
  pid <- as.character(pid)
  hit <- local_heads[local_keys %in% name_key(c(pid, name))]
  if (length(hit)) return(hit[1])
  over <- unname(headshot_overrides[pid])
  if (!is.na(over)) {
    path <- fetch_image(over, file.path(cache_dir, paste0(pid, "_override.", tools::file_ext(over))))
    if (!is.na(path)) return(path)
  }
  path <- fetch_image(sprintf(HEADSHOT_URL, pid), file.path(cache_dir, paste0(pid, ".png")))
  if (!is.na(path) && !is.na(cdn_silhouette) &&
      unname(tools::md5sum(path)) == unname(tools::md5sum(cdn_silhouette))) return(NA_character_)
  path
}
# Square crop from the top (faces sit high in a head-and-shoulders shot), laid on an
# AVATAR_BG disc and clipped to it.
read_headshot <- function(path, px = 500) {
  if (is.na(path)) return(NULL)
  tryCatch({
    img  <- image_trim(image_read(path)[1])
    info <- image_info(img)
    side <- min(info$width, info$height)
    img  <- image_crop(img, sprintf("%dx%d+%d+0", side, side, (info$width - side) %/% 2)) |>
      image_scale(sprintf("%dx%d!", px, px))
    disc <- image_draw(image_blank(px, px, "none"))                  # AVATAR_BG disc, clear outside
    symbols(px / 2, px / 2, circles = px / 2, inches = FALSE, add = TRUE, bg = AVATAR_BG, fg = NA)
    dev.off()
    as.raster(image_composite(disc, img, operator = "Atop"))         # photo only where the disc is
  }, error = function(e) NULL)
}

# ---- Board ------------------------------------------------------------------
# width in inches of each string as drawn - systemfonts measures the actual font
text_in <- function(x, pt, bold = FALSE)
  systemfonts::string_width(x, family = FONT, size = pt, bold = bold, res = 72) / 72
pt <- function(x) x / .pt                                            # font points -> geom_text size

# widest line of each (possibly multi-line) label
lines_in <- function(x, pt, bold = FALSE) max(text_in(unlist(strsplit(x, "\n")), pt, bold))
# word-wrap to a width in inches, measured in the actual font (keeps existing line breaks)
wrap_in <- function(x, pt, width, bold = FALSE) {
  out <- character(0)
  for (para in strsplit(x, "\n")[[1]]) {
    line <- ""
    for (w in strsplit(para, " ")[[1]]) {
      cand <- if (nzchar(line)) paste(line, w) else w
      if (nzchar(line) && text_in(cand, pt, bold) > width) { out <- c(out, line); line <- w } else line <- cand
    }
    out <- c(out, line)
  }
  paste(out, collapse = "\n")
}

# d: one row per shot, ranked, with shot_labels() columns plus value_label.
# Each row is [rank | headshot | name over one detail line | before -> after track | swing]:
# the track has a column of its own, clear of the text, with full-height quarter
# gridlines. compact = TRUE (for long lists) puts each shot on one line instead, with
# Player / Game / Shot columns under headers. Returns list(plot, width, height) in inches,
# so the PNG is drawn at exactly that size.
shot_board <- function(d, value_head, title, subtitle, caption, headshots = TRUE, compact = FALSE) {
  n <- nrow(d)
  k  <- board_text_scale
  sz <- lapply(if (compact)
                 list(rank = 22, name = 19, detail = 15, pre = 14, post = 15.5, value = 19,
                      head = 15, axis = 15, title = 38, sub = 18.5, cap = 15)
               else
                 list(rank = 32, name = 24, detail = 17, pre = 16, post = 18, value = 25,
                      head = 16, axis = 15.5, title = 38, sub = 18.5, cap = 15), `*`, k)
  LH <- function(p) p / 72 * 1.3                                      # line height, inches
  name_h <- sz$name * 1.22 / 72; det_h <- sz$detail * 1.3 / 72
  ROW_H  <- if (compact) name_h + 0.26 else name_h + det_h + 0.18     # one or two text lines + padding
  AV     <- ROW_H - if (compact) 0.1 else 0.16                         # headshot circle diameter
  mk     <- k * if (compact) 0.8 else 1                               # dot and line sizes
  M <- 0.5; TRACK_W <- 4.8; GAP_V <- 0.6; GAP_C <- 0.45                # GAP_V: post label -> values;
  d <- d |> mutate(detail_txt = paste0(stage_txt, " \u00b7 ", play_txt))   # GAP_C: between columns

  x_rank <- M + max(text_in(as.character(d$rank), sz$rank, TRUE))     # rank numbers right-aligned here
  x_av   <- x_rank + 0.26 + AV / 2
  x_text <- if (headshots) x_av + AV / 2 + 0.26 else x_rank + 0.32
  if (compact) {                                                      # Player | Game | Shot columns
    hdr    <- c(name = "Player", game = "Game", shot = "Shot")
    x_game <- x_text + max(text_in(c(d$name, hdr[["name"]]), sz$name, TRUE)) + GAP_C
    x_shot <- x_game + max(text_in(d$stage_txt, sz$detail), text_in(hdr[["game"]], sz$head, TRUE)) + GAP_C
    text_w <- x_shot + max(text_in(d$play_txt, sz$detail), text_in(hdr[["shot"]], sz$head, TRUE)) - x_text
  } else {
    text_w <- max(text_in(d$name, sz$name, TRUE), text_in(d$detail_txt, sz$detail))
  }
  x0     <- x_text + text_w + text_in("100.0%", sz$pre) + 0.35 + 0.14 * k   # 0% sits past the widest pre label
  x1     <- x0 + TRACK_W                                              # 100%
  val_w  <- max(text_in(d$value_label, sz$value, TRUE), lines_in(value_head, sz$head, TRUE))
  W      <- x1 + 0.16 * k + text_in("100.0%", sz$post, TRUE) + GAP_V + val_w + M
  x_val  <- W - M                                                     # values right-aligned here
  title    <- wrap_in(title, sz$title, W - 2 * M, TRUE)               # the rows set the width and
  subtitle <- wrap_in(subtitle, sz$sub, W - 2 * M)                    # the words wrap to it
  caption  <- wrap_in(caption, sz$cap, W - 2 * M)
  n_lines  <- function(x) length(strsplit(x, "\n")[[1]])

  y_sub  <- 0.5 + n_lines(title) * LH(sz$title) + 0.1                 # from the top edge
  TOP    <- y_sub + n_lines(subtitle) * LH(sz$sub) + 2 * LH(sz$head) + 0.35   # + column headers
  BOTTOM <- 0.2 + LH(sz$axis) + 0.25 + n_lines(caption) * LH(sz$cap) + 0.4
  H      <- TOP + n * ROW_H + BOTTOM

  d <- d |> mutate(y = H - TOP - (rank - 0.5) * ROW_H, row_bg = ifelse(rank %% 2 == 0, STRIPE, PAPER),
                   x_pre = x0 + champ_prob_win_pre * TRACK_W, x_post = x0 + champ_prob_win_post * TRACK_W)
  rows_top <- H - TOP; rows_bot <- H - TOP - n * ROW_H
  # percentages sit on a patch of their row's colour, so gridlines stop at the text
  pct_label <- function(...) geom_label(..., family = FONT, label.size = 0, label.r = unit(0, "pt"),
                                        label.padding = unit(1.5, "pt"))
  grid <- tibble(p = seq(0, 1, 0.25), x = x0 + p * TRACK_W)

  g <- ggplot(d) +
    geom_rect(data = filter(d, rank %% 2 == 0),
              aes(xmin = M - 0.2, xmax = W - M + 0.2, ymin = y - ROW_H / 2, ymax = y + ROW_H / 2),
              fill = STRIPE) +
    geom_segment(data = grid, aes(x = x, xend = x, y = rows_bot, yend = rows_top),
                 colour = RULE, linewidth = 0.45) +
    geom_text(data = grid, aes(x = x, y = rows_bot - 0.2, label = percent(p)),   # vjust 1: hangs below
              size = pt(sz$axis), colour = INK_SOFT, vjust = 1) +
    geom_text(aes(x = x_rank, y = y, label = rank), hjust = 1, size = pt(sz$rank),
              fontface = "bold", colour = INK_SOFT) +
    geom_segment(aes(x = x_pre, xend = x_post, y = y, yend = y),
                 colour = ACCENT, linewidth = 3 * mk, lineend = "round") +
    geom_point(aes(x = x_pre, y = y), shape = 21, size = 4.8 * mk, stroke = 1.8 * mk, fill = PAPER, colour = ACCENT) +
    geom_point(aes(x = x_post, y = y), shape = 21, size = 6.6 * mk, stroke = 1.3 * mk, fill = ACCENT, colour = PAPER) +
    pct_label(aes(x = x_pre - 0.14 * k, y = y, label = percent(champ_prob_win_pre, 0.1), fill = I(row_bg)),
              hjust = 1, size = pt(sz$pre), colour = INK_SOFT) +
    pct_label(aes(x = x_post + 0.16 * k, y = y, label = percent(champ_prob_win_post, 0.1), fill = I(row_bg)),
              hjust = 0, size = pt(sz$post), fontface = "bold", colour = INK) +
    geom_text(aes(x = x_val, y = y, label = value_label), hjust = 1, size = pt(sz$value),
              fontface = "bold", colour = INK) +
    annotate("text", x = (x0 + x1) / 2, y = rows_top + 0.16, label = "Title odds:  before \u2192 after",
             vjust = 0, size = pt(sz$head), fontface = "bold", colour = INK_SOFT) +
    annotate("text", x = x_val, y = rows_top + 0.16, label = value_head, hjust = 1, vjust = 0,
             size = pt(sz$head), fontface = "bold", colour = INK_SOFT, lineheight = 1.1) +
    annotate("text", x = M, y = H - 0.5, label = title, hjust = 0, vjust = 1,
             size = pt(sz$title), fontface = "bold", colour = INK, lineheight = 1.05) +
    annotate("text", x = M, y = H - y_sub, label = subtitle, hjust = 0, vjust = 1,
             size = pt(sz$sub), colour = INK_SOFT, lineheight = 1.15) +
    annotate("text", x = M, y = 0.4, label = caption, hjust = 0, vjust = 0,
             size = pt(sz$cap), colour = INK_SOFT, lineheight = 1.3) +
    coord_fixed(xlim = c(0, W), ylim = c(0, H), expand = FALSE) +
    theme_void(base_family = FONT) +
    theme(plot.background = element_rect(fill = PAPER, colour = NA), plot.margin = margin(0))

  g <- g + if (compact) list(
    geom_text(aes(x = x_text, y = y, label = name), hjust = 0, size = pt(sz$name),
              fontface = "bold", colour = INK),
    geom_text(aes(x = x_game, y = y, label = stage_txt), hjust = 0, size = pt(sz$detail), colour = INK_SOFT),
    geom_text(aes(x = x_shot, y = y, label = play_txt), hjust = 0, size = pt(sz$detail), colour = INK_SOFT),
    annotate("text", x = c(x_text, x_game, x_shot), y = rows_top + 0.16, label = unname(hdr),
             hjust = 0, vjust = 0, size = pt(sz$head), fontface = "bold", colour = INK_SOFT)
  ) else list(
    geom_text(aes(x = x_text, y = y + det_h / 2, label = name), hjust = 0, size = pt(sz$name),
              fontface = "bold", colour = INK),
    geom_text(aes(x = x_text, y = y - name_h / 2, label = detail_txt), hjust = 0, size = pt(sz$detail),
              colour = INK_SOFT)
  )

  if (headshots) {
    paths <- vapply(seq_len(n), function(i) headshot_path(d$pid1[i], d$name[i]), character(1))
    imgs  <- lapply(paths, read_headshot)
    for (i in which(!vapply(imgs, is.null, logical(1))))
      g <- g + annotation_raster(imgs[[i]], xmin = x_av - AV / 2, xmax = x_av + AV / 2,
                                 ymin = d$y[i] - AV / 2, ymax = d$y[i] + AV / 2)
    missing <- vapply(imgs, is.null, logical(1))
    if (any(missing)) {
      tiles <- d[missing, ] |>
        mutate(initials = paste0(substr(fnm, 1, 1), substr(lnm, 1, 1))) |>
        cross_join(tibble(theta = seq(0, 2 * pi, length.out = 73))) |>
        mutate(cx = x_av + AV / 2 * cos(theta), cy = y + AV / 2 * sin(theta))
      g <- g +
        geom_polygon(data = tiles, aes(x = cx, y = cy, group = rank), fill = AVATAR_BG, colour = NA) +
        geom_text(data = distinct(tiles, rank, y, initials), aes(x = x_av, y = y, label = initials),
                  size = pt(AV * 72 * 0.36), fontface = "bold", colour = INK_SOFT)
      cat("No headshot (initials tile) for:", paste0(d$name[missing], " [pid ", d$pid1[missing], "]", collapse = ", "),
          "\n  -> add an image named by pid or name to headshot_files, e.g.",
          paste0(d$pid1[missing][1], ".png or '", d$name[missing][1], ".jpg'\n"))
    }
  }
  list(plot = g, width = W, height = H)
}

yrs <- range(shots$ssn + yr_offset)
board_caption <- paste0(
  "Title odds = chance of winning the championship, from the series score and the in-game win probability, ",
  "with every game a coin flip. WNBA playoffs ", yrs[1], "–", yrs[2], ", made field goals only. ",
  "Series and game scores are from the shooter's side.")

# ---- Simple version, ranked by KL -------------------------------------------
d <- bigshots_kl |>
  arrange(desc(champ_prob_kl)) |>
  mutate(rank = row_number(), value_label = sprintf("%.3f", champ_prob_kl)) |>
  shot_labels()
board <- shot_board(d, value_head = "KL\n(bits)", headshots = FALSE,
                    title = "The biggest shots in WNBA history, by leverage",
                    subtitle = "Ranked by how far each shot moved title odds (KL divergence)",
                    caption = board_caption)
preview(board$plot)
save_png(board$plot, "wnba_biggest_shots_kl.png", width = board$width, height = board$height)

# ---- With headshots, ranked by title-odds change ----------------------------
d <- bigshots |>
  arrange(desc(champ_prob_change)) |>
  mutate(rank = row_number(), value_label = sprintf("%+.1f", 100 * champ_prob_change)) |>
  shot_labels()
board <- shot_board(d, value_head = "Swing\n(pct. pts)",
                    title = "The biggest shots in WNBA history",
                    subtitle = "Made shots ranked by how much they raised their team's title odds",
                    caption = board_caption)
preview(board$plot)
save_png(board$plot, "wnba_biggest_shots.png", width = board$width, height = board$height)

# The rows as drawn, for checking each one against the record
print(as_tibble(select(d, rank, name, detail, champ_prob_win_pre, champ_prob_win_post, value_label, gid)),
      n = Inf, width = Inf)
write.csv(select(d, rank, name, detail, ssn, dt, gid, pid1, q, min, sec, wp, wpa,
                 champ_prob_win_pre, champ_prob_win_post, champ_prob_change),
          paste0(out_dir, "wnba_biggest_shots.csv"), row.names = FALSE)

# ---- Top n_shots_long, one line per shot ------------------------------------
d_long <- shots |>
  arrange(desc(champ_prob_change)) |>
  slice_head(n = n_shots_long) |>
  mutate(rank = row_number(), value_label = sprintf("%+.1f", 100 * champ_prob_change)) |>
  shot_labels()
board_long <- shot_board(d_long, value_head = "Swing\n(pct. pts)", compact = TRUE,
                         title = paste("The", n_shots_long, "biggest shots in WNBA history"),
                         subtitle = "Made shots ranked by how much they raised their team's title odds",
                         caption = board_caption)
preview(board_long$plot)
save_png(board_long$plot, sprintf("wnba_biggest_shots_top%d.png", n_shots_long),
         width = board_long$width, height = board_long$height)
write.csv(select(d_long, rank, name, detail, ssn, dt, gid, pid1, q, min, sec, wp, wpa,
                 champ_prob_win_pre, champ_prob_win_post, champ_prob_change),
          paste0(out_dir, sprintf("wnba_biggest_shots_top%d.csv", n_shots_long)), row.names = FALSE)
