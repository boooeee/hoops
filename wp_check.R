# =====================================================================
# wp_check.R - test the win-probability model against what actually happened
#
# Pick a game situation - a range of scoring margins and a stretch of game clock -
# and wp_check() finds every game where a team was in it, then compares how often
# those teams went on to win with what the model (wpg.wp) said at the time.
#
#   res <- wp_check(margin = c(-27, -23), time = c("Q2 10:00", "Q2 7:00"))
#   res                    # summary table + the upsets
#   res$summary            # one row per season group (+ "All" when split)
#   res$games              # one row per team-game in the sample
#   plot_wp_check(res, "wp_check.png")
#
# margin   the team's score minus the opponent's, both ends included. c(-27, -23) is
#          "down 23 to 27"; positive is leading. One number = exactly that margin.
# time     two game-clock points, either order, both ends included: "Q2 10:00",
#          "Q4 1:30", "OT1 2:00" ("OT" alone is OT1) - or minutes left in regulation,
#          c(30, 27). Before 2006 the WNBA played 20-minute halves, which wpg numbers
#          like quarters, so "Q2 7:00" is 13 minutes into the game in every season.
#          A window inside overtime uses only overtime plays (the regulation buzzer is
#          the same moment as "OT1 5:00"), and a window inside regulation only regulation.
# seasons  NULL for every season, or e.g. 2015:2025
# by       how to split the seasons:
#            NULL                   one pooled result
#            "season"               every season
#            5                      blocks of 5 seasons
#            c(1997, 2006, 2016)    eras starting in those seasons
# games    "all", "regular" or "playoffs"
# side     "any", "home" or "away"
#
# Each game counts once per team: at the first play inside the time window where the
# team's margin is in range - the moment it first found itself in that spot. Counting
# every play instead would count a game once per possession it spent there, and the
# sample would look far bigger (and the error bars far tighter) than it really is.
#
# Columns in the summary:
#   win_pct      how often those teams won;  ci_lo/ci_hi its 95% interval (Wilson)
#   model_pct    the model's average win probability for them at that moment
#   exp_wins     wins the model expected (the sum of those probabilities)
#   gap          win_pct - model_pct
#   p_value      chance of a gap at least this big if the model were right. Exact
#                (Poisson-binomial, so each game keeps its own probability); a normal
#                approximation above 20,000 games.
#
# wp_check.py does the same thing in Python, if you want to compare the two.
# =====================================================================

library(DBI)
library(RMySQL)
library(tidyverse)

# =====================================================================
# SETTINGS - check these first
# =====================================================================
db_name      <- "wnba"            # MySQL database holding gms and wpg
qtr_secs     <- 600               # quarter length in seconds: 600 for the WNBA, 720 for the NBA
ot_secs      <- 300               # overtime length in seconds
gid_prefixes <- c("102", "104")   # first 3 characters of gid to keep: regular season and playoffs
                                  # (leaves out preseason, All-Star, Commissioner's Cup final).
                                  # NULL keeps every game in gms.
play_order   <- "r"               # wpg column numbering the plays within a game. The win-probability
                                  # scripts write r after putting plays in clock order. NULL if wpg
                                  # has none: plays on the same game clock (free throws) are then
                                  # taken in the order the database returns them.
out_dir      <- "~/Dropbox/wnba/" # where plot_wp_check() writes its PNG

# MySQL login from ~/.Renviron, same as the other scripts:
#   WNBA_DB_USER=root   (optional, defaults to root)
#   WNBA_DB_PASS=...
#   WNBA_DB_HOST=...    (optional, defaults to localhost)
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")
if (!nzchar(Sys.getenv("WNBA_DB_PASS")))
  message("WNBA_DB_PASS is not set in ~/.Renviron - connecting to MySQL with an empty password")

sqlf <- function(x) {
  con <- dbConnect(MySQL(), username = Sys.getenv("WNBA_DB_USER", "root"),
                   password = Sys.getenv("WNBA_DB_PASS"), dbname = db_name,
                   host = Sys.getenv("WNBA_DB_HOST", "localhost"), port = 3306)
  on.exit(dbDisconnect(con))
  withCallingHandlers(dbGetQuery(con, x), warning = function(w)      # UNSIGNED / DECIMAL columns
    if (grepl("imported as numeric", conditionMessage(w))) invokeRestart("muffleWarning"))
}

# =====================================================================
# Game clock <-> seconds left in regulation (negative in overtime)
# =====================================================================
clock_secs <- function(x) {
  if (is.numeric(x)) return(x * 60)                         # minutes left in regulation
  p <- str_match(toupper(str_squish(x)), "^(?:Q([1-4])|OT([0-9]*)) ?([0-9]+)(?::([0-9]+(?:\\.[0-9]+)?))?$")
  bad <- is.na(p[, 1])
  if (any(bad)) stop('Can\'t read time "', x[bad][1], '" - use "Q2 10:00", "OT1 2:00" or minutes left in regulation')
  q     <- as.numeric(p[, 2])
  ot    <- ifelse(is.na(p[, 3]) | p[, 3] == "", 1, as.numeric(p[, 3]))
  clock <- as.numeric(p[, 4]) * 60 + coalesce(as.numeric(p[, 5]), 0)
  if (any(clock > ifelse(is.na(q), ot_secs, qtr_secs))) stop("Clock longer than the period in ", paste(x, collapse = ", "))
  ifelse(!is.na(q), (4 - q) * qtr_secs + clock, clock - ot * ot_secs)
}

# A moment on a period boundary is the start of the next period when it opens a window
# (end = FALSE) and the end of the period before when it closes one (end = TRUE).
secs_clock <- function(s, end = FALSE) {
  ot   <- if (end) s < 0 else s <= 0
  per  <- if (end) ifelse(ot, ceiling(-s / ot_secs), pmax(1, 4 - floor(s / qtr_secs)))
          else     ifelse(ot, floor(-s / ot_secs) + 1, pmax(1, 5 - ceiling(s / qtr_secs)))
  left <- ifelse(ot, s + per * ot_secs, s - (4 - per) * qtr_secs)
  sprintf("%s%d %d:%02d", ifelse(ot, "OT", "Q"), as.integer(per), as.integer(left %/% 60), as.integer(floor(left %% 60)))
}

# =====================================================================
# The query: plays inside the window where either team's margin is in range.
# Margin and time are worked out in SQL so only those plays come back. Columns are
# cast to SIGNED first: if h, a or q are UNSIGNED, MySQL errors on a negative difference.
# =====================================================================
situation_sql <- function(margin, secs, seasons, games, side) {
  num <- function(x) format(x, scientific = FALSE, trim = TRUE)
  clock <- "CAST(w.`min` AS SIGNED) * 60 + CAST(w.sec AS DECIMAL(8,3))"
  t_left <- sprintf("CASE WHEN CAST(w.q AS SIGNED) <= 4 THEN (4 - CAST(w.q AS SIGNED)) * %s + %s
                          ELSE %s - (CAST(w.q AS SIGNED) - 4) * %s END",
                    num(qtr_secs), clock, clock, num(ot_secs))
  inner <- c("w.wp IS NOT NULL", "g.hscr <> g.ascr")       # skip games without a result
  if (!is.null(seasons))      inner <- c(inner, sprintf("g.ssn IN (%s)", paste(as.integer(seasons), collapse = ", ")))
  if (!is.null(gid_prefixes)) inner <- c(inner, sprintf("LEFT(g.gid, 3) IN (%s)", paste0("'", gid_prefixes, "'", collapse = ", ")))
  if (games == "playoffs")    inner <- c(inner, "g.po = 1")
  if (games == "regular")     inner <- c(inner, "(g.po IS NULL OR g.po <> 1)")
  home_ok <- sprintf("x.home_margin BETWEEN %s AND %s", num(margin[1]), num(margin[2]))
  away_ok <- sprintf("x.home_margin BETWEEN %s AND %s", num(-margin[2]), num(-margin[1]))
  outer <- c(sprintf("x.t_left BETWEEN %s AND %s", num(secs[1]), num(secs[2])),
             switch(side, any = paste0("(", home_ok, " OR ", away_ok, ")"), home = home_ok, away = away_ok),
             # the regulation buzzer and "OT1 5:00" are the same moment: an overtime window
             # uses only overtime plays, a regulation window only regulation plays
             if (secs[2] <= 0 && secs[1] < 0) "x.q > 4" else if (secs[1] >= 0) "x.q <= 4")
  paste0(
    "SELECT * FROM (
       SELECT g.ssn, g.dt, g.gid, g.po, g.htm, g.atm, g.hscr, g.ascr,
              w.q, w.`min`, w.sec, w.h, w.a, w.wp,", if (!is.null(play_order)) sprintf(" w.`%s` AS play_seq,", play_order), "
              ", t_left, " AS t_left,
              CAST(w.h AS SIGNED) - CAST(w.a AS SIGNED) AS home_margin
       FROM wpg w INNER JOIN gms g ON g.gid = w.gid
       WHERE ", paste(inner, collapse = "\n         AND "), "
     ) x
     WHERE ", paste(outer, collapse = "\n       AND "))
}

# =====================================================================
# Stats
# =====================================================================
wilson <- function(x, n, z = qnorm(0.975)) {                # 95% interval for a win rate
  p <- x / n
  mid  <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(mid - half, mid + half)
}

# Two-sided p-value for k wins when game i is won with probability p[i] (a
# Poisson-binomial count): 2 x the smaller tail, built up one game at a time.
poisbinom_p <- function(k, p) {
  if (length(p) > 20000) {                                  # normal approximation
    mu <- sum(p); s <- sqrt(sum(p * (1 - p)))
    return(min(1, 2 * min(pnorm((k + 0.5 - mu) / s), 1 - pnorm((k - 0.5 - mu) / s))))
  }
  dist <- 1                                                 # dist[j + 1] = P(j wins so far)
  for (pr in p) dist <- c(dist * (1 - pr), 0) + c(0, dist * pr)
  min(1, 2 * min(sum(dist[1:(k + 1)]), sum(dist[(k + 1):length(dist)])))
}

tally <- function(d) {
  ci <- wilson(sum(d$won), nrow(d))
  tibble(n = nrow(d), wins = sum(d$won), win_pct = wins / n, ci_lo = ci[1], ci_hi = ci[2],
         model_pct = mean(d$team_wp), exp_wins = sum(d$team_wp), gap = win_pct - model_pct,
         p_value = poisbinom_p(wins, d$team_wp), avg_margin = mean(d$margin))
}

season_group <- function(ssn, by) {
  if (is.null(by)) return(rep("All", length(ssn)))
  if (identical(by, "season")) return(as.character(ssn))
  if (!is.numeric(by))
    stop('by must be NULL, "season", a block length like 5, or era start seasons like c(1997, 2006, 2016)')
  first  <- min(ssn)
  starts <- if (length(by) == 1) seq(first, max(ssn), by = by) else sort(unique(c(first, by[by > first])))
  ends   <- c(starts[-1] - 1, max(ssn))
  labels <- ifelse(starts == ends, starts, paste0(starts, "-", ends))
  labels[findInterval(ssn, starts)]
}

# =====================================================================
# wp_check()
# =====================================================================
wp_check <- function(margin, time, seasons = NULL, by = NULL,
                     games = c("all", "regular", "playoffs"), side = c("any", "home", "away")) {
  games <- match.arg(games)
  want  <- match.arg(side)
  rng_m <- sort(rep_len(margin, 2))
  rng_t <- sort(clock_secs(time))
  if (length(margin) > 2 || length(rng_t) != 2) stop("margin takes one or two values, time takes two")

  rows <- tryCatch(sqlf(situation_sql(rng_m, rng_t, seasons, games, want)), error = function(e) {
    if (!is.null(play_order) && grepl("Unknown column", conditionMessage(e)))
      stop("wpg has no column `", play_order, "` - set play_order <- NULL at the top of wp_check.R", call. = FALSE)
    stop(e)
  })
  if (nrow(rows) == 0) stop("No game was ever in that situation - widen margin or time")
  rows <- rows |>
    mutate(across(c(ssn, po, hscr, ascr, q, min, sec, h, a, wp, t_left, home_margin), as.numeric),
           seq = if (is.null(play_order)) row_number() else as.numeric(play_seq))

  # one row per team per play, from that team's side
  both <- bind_rows(
    home = transmute(rows, ssn, dt, gid, po, q, min, sec, t_left, seq, team = htm, opp = atm,
                     margin = home_margin, team_wp = wp, pts = hscr, opp_pts = ascr),
    away = transmute(rows, ssn, dt, gid, po, q, min, sec, t_left, seq, team = atm, opp = htm,
                     margin = -home_margin, team_wp = 1 - wp, pts = ascr, opp_pts = hscr),
    .id = "side")

  sample <- both |>
    filter(between(margin, rng_m[1], rng_m[2]), want == "any" | side == want) |>
    arrange(gid, side, desc(t_left), seq) |>
    distinct(gid, side, .keep_all = TRUE) |>                # the first qualifying play
    mutate(won = pts > opp_pts, group = season_group(ssn, by))

  summary <- sample |> group_by(group) |> group_modify(~ tally(.x)) |> ungroup()
  if (!is.null(by)) summary <- bind_rows(summary, mutate(tally(sample), group = "All"))

  margin_txt <- if (rng_m[2] < 0) paste("down", paste(unique(-rev(rng_m)), collapse = "–"))
                else if (rng_m[1] > 0) paste("up", paste(unique(rng_m), collapse = "–"))
                else if (all(rng_m == 0)) "tied"
                else sprintf("with a margin of %+g to %+g", rng_m[1], rng_m[2])
  situation <- paste0(c(any = "Teams", home = "Home teams", away = "Road teams")[want], " ", margin_txt, ", ",
                      secs_clock(rng_t[2]), " to ", secs_clock(rng_t[1], end = TRUE))
  filters <- paste0(c(all = "All games", regular = "Regular season", playoffs = "Playoffs")[games], ", ",
                    paste(unique(range(sample$ssn)), collapse = "–"))

  games_tbl <- sample |>
    transmute(ssn, dt, gid, side, team, opp,
              clock = sprintf("%s %d:%02d", ifelse(q > 4, paste0("OT", q - 4), paste0("Q", q)), min, floor(sec)),
              margin, team_wp, won, final = paste0(pts, "-", opp_pts), group) |>
    arrange(dt, gid, side)

  structure(list(summary = summary, games = games_tbl, situation = situation, filters = filters, by = by),
            class = "wp_check")
}

pct <- function(x) sprintf("%.1f%%", 100 * x)

print.wp_check <- function(x, ...) {
  s <- x$summary
  tbl <- data.frame(Seasons = s$group, Games = s$n, Won = s$wins, `Win %` = pct(s$win_pct),
                    `95% CI` = paste0(pct(s$ci_lo), "-", pct(s$ci_hi)), `Model %` = pct(s$model_pct),
                    `Exp. wins` = sprintf("%.1f", s$exp_wins), `Gap (pts)` = sprintf("%+.1f", 100 * s$gap),
                    p = ifelse(s$p_value < 0.001, "<0.001", sprintf("%.3f", s$p_value)), check.names = FALSE)
  if (is.null(x$by)) tbl$Seasons <- NULL
  cat("\n", x$situation, " · ", x$filters, "\n\n", sep = "")
  print(tbl, row.names = FALSE, right = TRUE)
  cat("p = chance of a gap at least this big if the model were right\n")

  upsets <- x$games |>
    mutate(odds = ifelse(won, team_wp, 1 - team_wp)) |>
    filter(odds < 0.5) |>
    arrange(odds) |>
    slice_head(n = 10)
  if (nrow(upsets) == 0) {
    cat("\nNo upsets: every game went the way the model favored.\n")
  } else {
    cat("\nUpsets - results the model called less likely, least likely first:\n")
    print(data.frame(Date = upsets$dt, Game = paste(upsets$team, ifelse(upsets$side == "home", "vs", "@"), upsets$opp),
                     When = upsets$clock, Margin = sprintf("%+g", upsets$margin), Model = pct(upsets$team_wp),
                     Result = paste(ifelse(upsets$won, "W", "L"), upsets$final), gid = upsets$gid),
          row.names = FALSE, right = FALSE)
  }
  invisible(x)
}

# =====================================================================
# Chart: actual win rate (dot + 95% interval) vs. the model (diamond), per group
# =====================================================================
INK       <- "#22211e"
INK_SOFT  <- "#6f6b64"
RULE      <- "#d9d6d0"
PAPER     <- "#ffffff"
ACCENT    <- "#3b6ea5"   # actual
MODEL_COL <- "#d0703a"   # model
FONT <- if ("open sans" %in% tolower(c(systemfonts::system_fonts()$family, systemfonts::registry_fonts()$family)))
  "Open Sans" else ""

plot_wp_check <- function(res, file = NULL) {
  s <- res$summary
  if (n_distinct(s$group) > 1) s <- filter(s, group != "All")
  s <- mutate(s, group = factor(group, levels = unique(group)))
  pts <- bind_rows(tibble(group = s$group, series = "Actual", value = s$win_pct),
                   tibble(group = s$group, series = "Model",  value = s$model_pct))
  key <- c(Actual = "Actual win rate, with 95% interval", Model = "Model's average win probability")

  g <- ggplot(pts, aes(x = group, y = value)) +
    geom_linerange(data = s, aes(x = group, ymin = ci_lo, ymax = ci_hi), inherit.aes = FALSE,
                   colour = ACCENT, linewidth = 0.9) +
    geom_point(aes(shape = series, fill = series, colour = series), size = 3.4, stroke = 1.3) +
    scale_shape_manual(values = c(Actual = 21, Model = 23), labels = key, name = NULL) +
    scale_fill_manual(values = c(Actual = ACCENT, Model = PAPER), labels = key, name = NULL) +
    scale_colour_manual(values = c(Actual = PAPER, Model = MODEL_COL), labels = key, name = NULL) +
    scale_x_discrete(labels = paste0(s$group, "\n", s$n)) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.02, 0.08))) +
    expand_limits(y = 0) +
    labs(title = res$situation,
         subtitle = paste0(res$filters, ". How often they won, against what the model gave them at the time."),
         x = NULL, y = NULL,
         caption = paste0("Number under each label = games in the sample. Each team-game counts once, at its\n",
                          "first play in the time window with the margin in range.")) +
    theme_minimal(base_size = 12, base_family = FONT) +
    theme(text = element_text(colour = INK),
          axis.text = element_text(colour = INK_SOFT, size = 9.5),
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = RULE, linewidth = 0.4),
          legend.position = "top", legend.justification = "left", legend.text = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(colour = INK_SOFT, size = 10.5),
          plot.caption = element_text(colour = INK_SOFT, size = 8.5, hjust = 0),
          plot.title.position = "plot", plot.caption.position = "plot",
          plot.background = element_rect(fill = PAPER, colour = NA),
          plot.margin = margin(14, 16, 10, 14))
  if (!is.null(file))
    ggsave(file.path(path.expand(out_dir), file), g, width = max(7, 1.5 + 0.5 * nrow(s)), height = 4.8,
           dpi = 300, bg = PAPER, device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png")
  g
}

# =====================================================================
# Examples - edit these, or call wp_check() from the console
# =====================================================================
# Down about 25 early in the 2nd quarter (first 3 minutes of it)
res <- wp_check(margin = c(-27, -23), time = c("Q2 10:00", "Q2 7:00"))
print(res)

# Same spot, split into 5-season blocks, regular season only
res_blocks <- wp_check(margin = c(-27, -23), time = c("Q2 10:00", "Q2 7:00"), by = 5, games = "regular")
print(res_blocks)
plot_wp_check(res_blocks, "wp_check_down25_q2.png")

# More: one row per season since 2015; down 10-15 with 3-6 minutes left; the playoffs only
# print(wp_check(margin = c(-27, -23), time = c("Q2 10:00", "Q2 7:00"), seasons = 2015:2025, by = "season"))
# print(wp_check(margin = c(-15, -10), time = c("Q4 6:00", "Q4 3:00")))
# print(wp_check(margin = c(-15, -10), time = c(6, 3), games = "playoffs", by = c(1997, 2006, 2016)))
