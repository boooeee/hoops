# =====================================================================
# kalshiNBA_rtask.R - Kalshi's NBA game prices, minute by minute
#
# Kalshi keeps 1-minute candlesticks for every market, settled ones included, and
# serves them without a login. For each finished NBA game this pulls three kinds
# of market:
#
#   game    KXNBAGAME    "<team> wins"                     one market per team
#   spread  KXNBASPREAD  "<team> wins by over X.5 points"  a ladder of strikes
#   total   KXNBATOTAL   "Over X.5 points scored"          a ladder of strikes
#
# and writes, under out_dir:
#
#   minutes/<date>_<AWYHOM>.csv  one row per market per minute: bid, ask, mid, last
#                                trade, volume, open interest
#   markets.csv                  one row per market: kind, team, strike, result, volume
#   game_minutes.csv             one row per game per minute: the home team's win
#                                probability, and the margin and total lines the
#                                ladders imply
#
# Each game runs from an hour before the first play in wpg to the minute its markets
# close. Kalshi only sends a candle for a minute when something changed, and it sends
# one at least every 10 minutes. The quote after a gap is the quote before it, so
# missing minutes are filled forward (filled = 1, volume 0).
#
# Columns:
#   t, ts      end of the minute: Unix seconds (UTC), and the same as text. bid and ask
#              are the quotes at that moment, in dollars (= probability).
#   kind       game, spread or total
#   x          the threshold the market is about, from the home team's side:
#                game, home team       yes if home margin > 0      x = 0,   yes_over = 1
#                game, away team       yes if home margin < 0      x = 0,   yes_over = 0
#                spread, home by X.5   yes if home margin > X.5    x = X.5, yes_over = 1
#                spread, away by X.5   yes if home margin < -X.5   x = -X.5, yes_over = 0
#                total, over X.5       yes if total > X.5          x = X.5, yes_over = 1
#              So P(home margin > x), or P(total > x), is mid when yes_over = 1 and
#              1 - mid when yes_over = 0.
#   home_wp      average of the home market's mid and 1 - the away market's mid
#   margin_line  home margin m where the market says P(margin > m) = 50%: the game
#                markets and spread ladder, made monotone, interpolated. NA when the
#                ladder doesn't straddle 50% (common late in blowouts).
#   total_line   the same for the total
#   Only strikes with a two-sided quote (bid > 0, ask < 1) go into the lines.
#
# gid comes from gms (date, away, home), so the files join to wpg. wpg.ts is the NBA's
# timeActual, also UTC; each minute goes with the last play at or before it:
#
#   km <- fread("~/Dropbox/Nba/kalshi/game_minutes.csv", colClasses = list(character = "gid"))
#   wp <- as.data.table(dbGetQuery(con, "SELECT gid, r, ts, wp, a, h FROM wpg WHERE gid IN (...)"))
#   wp[, t := as.numeric(as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))]
#   setorder(wp, gid, t, r)
#   both <- wp[km, on = .(gid, t), roll = TRUE]   # last play with t <= the minute
#
# Markets settled more than about two months ago move to Kalshi's archive (the
# /historical endpoints): same data, but one market per request, so 2025-26 takes
# about 4 hours (10 seconds a game); recent games come 60 markets a request and take
# a few seconds. The script reads the live endpoints first and falls back.
# Games already in minutes/ are skipped, so a stopped run picks up where it left off.
#
# From a terminal:  Rscript kalshiNBA_rtask.R                        (settings below)
#                   Rscript kalshiNBA_rtask.R 2025-10-21 2026-06-30  (date range)
# =====================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(data.table)
  library(DBI)
  library(RMySQL)
})

# =====================================================================
# SETTINGS - check these first
# =====================================================================
out_dir           <- "~/Dropbox/Nba/kalshi/"
date_from         <- as.Date("2025-04-01")   # game dates (US Eastern), both ends included.
date_to           <- Sys.Date() - 1          # Kalshi has game markets from April 2025, spread
                                             # and total markets from October 2025.
series            <- c(game = "KXNBAGAME", spread = "KXNBASPREAD", total = "KXNBATOTAL")
pre_tip_mins      <- 60      # minutes of pre-game to keep before the first play in wpg
no_tip_mins       <- 300     # a game not in wpg: keep this many minutes before the game
                             # markets close. They usually close ~15 minutes after the
                             # final buzzer, sometimes 2+ hours after; 300 covers the game
                             # in 99.9% of 2025-26 games.
redo              <- FALSE   # TRUE pulls games already in minutes/ again
use_db            <- TRUE    # look up gid in gms and tip time in wpg; FALSE leaves gid NA
                             # and always uses no_tip_mins
wait_days         <- 3       # a game this recent that isn't in gms yet waits for a later run
                             # (the WP task hasn't loaded it); older ones (preseason,
                             # exhibitions, postponed) are pulled without a gid
reqs_per_sec      <- 4       # without a login Kalshi answers 429 to bursts (retried after
                             # a pause); 4 a second, evenly spaced, rarely hits it

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) date_from <- as.Date(args[1])
if (length(args) >= 2) date_to   <- as.Date(args[2])

kalshi_api <- "https://api.elections.kalshi.com/trade-api/v2"
out_dir    <- path.expand(out_dir)

`%||%` <- function(x, y) if (is.null(x)) y else x


# =====================================================================
# Kalshi requests. Market data needs no login. 429s and server errors are
# retried with backoff; a 404 comes back as NULL.
# =====================================================================
last_req <- new.env()
last_req$t <- 0

kget <- function(path, query = list()) {
  wait <- last_req$t + 1 / reqs_per_sec - as.numeric(Sys.time())   # evenly spaced, no bursts
  if (wait > 0) Sys.sleep(wait)
  last_req$t <- as.numeric(Sys.time())
  resp <- request(kalshi_api) |>
    req_url_path_append(path) |>
    req_url_query(!!!query) |>
    req_user_agent("hoops-kalshi-nba") |>
    req_retry(max_tries = 6, retry_on_failure = TRUE,
              is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
              backoff = \(i) 2^i) |>
    req_timeout(60) |>
    req_error(is_error = \(r) resp_status(r) >= 400 && resp_status(r) != 404) |>
    req_perform()
  if (resp_status(resp) == 404) return(NULL)
  fromJSON(resp_body_string(resp), flatten = TRUE)
}

num <- function(x) suppressWarnings(as.numeric(x %||% NA))

utc_secs <- function(x) {
  as.numeric(as.POSIXct(substr(x, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}

utc_text <- function(t) format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# Live and archived candles name their fields differently: price.close_dollars and
# volume_fp live, price.close and volume in the archive.
tidy_names <- function(d) {
  setnames(d, sub("_(dollars|fp)$", "", names(d)))
  d
}


# =====================================================================
# Games: every event in the three series, keyed by the code in its ticker,
# e.g. KXNBAGAME-26NOV02TORLAC -> 26NOV02TORLAC = Nov 2 2026, TOR at LAC.
# The date is the US Eastern game date, away team first - the same as gms.
# =====================================================================
list_events <- function(series_ticker) {
  out <- list()
  cursor <- NULL
  repeat {
    j <- kget("events", list(series_ticker = series_ticker, limit = 200, cursor = cursor))
    if (length(j$events) == 0) break
    out[[length(out) + 1]] <- as.data.table(j$events)[, .(event_ticker, sub_title)]
    cursor <- j$cursor
    if (is.null(cursor) || cursor == "") break
  }
  rbindlist(out)
}

game_codes <- function() {
  ev <- rbindlist(lapply(names(series), function(k) {
    e <- list_events(series[[k]])
    if (nrow(e) > 0) e[, kind := k]
    e
  }))
  ev[, game := sub("^[^-]+-", "", event_ticker)]
  ev <- ev[grepl("^[0-9]{2}[A-Z]{3}[0-9]{2}[A-Z]{6}$", game)]
  mon <- match(substr(ev$game, 3, 5), toupper(month.abb))
  ev[, date := as.Date(sprintf("20%s-%02d-%s", substr(game, 1, 2), mon, substr(game, 6, 7)))]
  ev[, `:=`(away = substr(game, 8, 10), home = substr(game, 11, 13))]
  ev
}

gid_lookup <- function(dates) {
  if (!use_db) return(NULL)
  if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")
  tryCatch({
    con <- dbConnect(RMySQL::MySQL(),
      username = Sys.getenv("NBA_DB_USER", Sys.getenv("WNBA_DB_USER", "root")),
      password = Sys.getenv("NBA_DB_PASS", Sys.getenv("WNBA_DB_PASS")),
      dbname = "nba", host = "localhost", port = 3306L)
    on.exit(dbDisconnect(con))
    g <- dbGetQuery(con, sprintf(paste(
      "SELECT g.gid, g.dt, g.atm, g.htm, MIN(w.ts) AS tip FROM gms g",
      "LEFT JOIN wpg w ON w.gid = g.gid AND w.ts LIKE '20%%'",
      "WHERE g.dt BETWEEN '%s' AND '%s' GROUP BY g.gid, g.dt, g.atm, g.htm"),
      min(dates), max(dates)))
    data.table(gid = g$gid, date = as.Date(g$dt), away = g$atm, home = g$htm,
               tip = utc_secs(g$tip))
  }, error = function(e) {
    message("No gid lookup (", conditionMessage(e), "); gid left NA")
    NULL
  })
}


# =====================================================================
# One game
# =====================================================================
event_markets <- function(event_ticker) {
  j <- kget("markets", list(event_ticker = event_ticker, limit = 1000))
  src <- "live"
  if (length(j$markets) == 0) {
    j <- kget("historical/markets", list(event_ticker = event_ticker, limit = 1000))
    src <- "historical"
  }
  if (length(j$markets) == 0) return(NULL)
  m <- as.data.table(j$markets)
  data.table(ticker = m$ticker, event_ticker = m$event_ticker, status = m$status,
             result = m$result, floor_strike = num(m$floor_strike),
             volume = num(m$volume_fp %||% m$volume),
             open_t = utc_secs(m$open_time), close_t = utc_secs(m$close_time),
             yes_sub_title = m$yes_sub_title, source = src)
}

# Every market of one game, with kind, team, x and yes_over (see the top of the file)
game_markets <- function(gm) {
  mk <- rbindlist(lapply(seq_len(nrow(gm)), function(i) {
    m <- event_markets(gm$event_ticker[i])
    if (!is.null(m)) m[, kind := gm$kind[i]]
    m
  }))
  if (nrow(mk) == 0) return(mk)
  home <- gm$home[1]
  mk[, suffix := sub(".*-", "", ticker)]
  mk[, team := ifelse(kind == "total", NA_character_, sub("[0-9.]+$", "", suffix))]
  mk[, strike := ifelse(kind == "game", 0, floor_strike)]
  mk[, x := fcase(kind == "total", strike,
                  team == home, strike,
                  default = -strike)]
  mk[, yes_over := as.integer(kind == "total" | team == home)]
  mk[, suffix := NULL]
  mk[]
}

candles_live <- function(tickers, t_from, t_to) {
  per_req <- max(1, min(100, floor(9000 / ((t_to - t_from) / 60 + 1))))  # <= 10,000 candles a request
  chunks <- split(tickers, ceiling(seq_along(tickers) / per_req))
  rbindlist(lapply(chunks, function(tk) {
    j <- kget("markets/candlesticks", list(market_tickers = paste(tk, collapse = ","),
              start_ts = t_from, end_ts = t_to, period_interval = 1))
    if (is.null(j) || length(j$markets) == 0) return(NULL)
    rbindlist(lapply(seq_len(nrow(j$markets)), function(i) {
      cs <- j$markets$candlesticks[[i]]
      if (length(cs) == 0 || nrow(cs) == 0) return(NULL)
      cs <- tidy_names(as.data.table(jsonlite::flatten(cs)))
      cs[, ticker := j$markets$market_ticker[i]]
    }), fill = TRUE)
  }), fill = TRUE)
}

candles_historical <- function(tickers, t_from, t_to) {
  rbindlist(lapply(tickers, function(tk) {
    j <- kget(paste0("historical/markets/", tk, "/candlesticks"),
              list(start_ts = t_from, end_ts = t_to, period_interval = 1))
    if (is.null(j) || length(j$candlesticks) == 0 || nrow(j$candlesticks) == 0) return(NULL)
    cs <- tidy_names(as.data.table(j$candlesticks))
    cs[, ticker := tk]
  }), fill = TRUE)
}

# One row per market per minute, from its first candle to the minute it closed
fill_minutes <- function(cs, mk, t_keep, t_last) {
  col <- function(nm) if (nm %in% names(cs)) num(cs[[nm]]) else NA_real_
  raw <- data.table(ticker = cs$ticker, t = as.numeric(cs$end_period_ts),
                    bid = col("yes_bid.close"), ask = col("yes_ask.close"),
                    last = fcoalesce(col("price.close"), col("price.previous")),
                    vol = col("volume"), oi = col("open_interest"))
  raw <- unique(raw, by = c("ticker", "t"))
  ends <- mk[, .(ticker, t_end = pmin(ceiling(close_t / 60) * 60, t_last))]
  grid <- raw[, .(t0 = min(t)), by = ticker][ends, on = "ticker", nomatch = NULL]
  grid <- grid[t_end >= t0, .(t = seq(t0, t_end, by = 60)), by = ticker]
  d <- raw[grid, on = .(ticker, t)]
  d[, filled := as.integer(is.na(bid) & is.na(ask) & is.na(vol))]
  setorder(d, ticker, t)
  d[, c("bid", "ask", "last", "oi") := lapply(.SD, nafill, type = "locf"),
    by = ticker, .SDcols = c("bid", "ask", "last", "oi")]
  d[is.na(vol), vol := 0]
  d[, mid := (bid + ask) / 2]
  d[t >= t_keep]
}

# The x where P(quantity > x) crosses 50%: average repeats, force the curve to fall as x
# rises (isotonic regression), then interpolate between the strikes either side of 50%
implied_line <- function(x, p) {
  ok <- !is.na(x) & !is.na(p)
  if (sum(ok) < 2) return(NA_real_)
  a <- data.table(x = x[ok], p = p[ok])[, .(p = mean(p)), keyby = x]
  if (nrow(a) < 2) return(NA_real_)
  s <- -isoreg(a$x, -a$p)$yf
  i <- which(s[-length(s)] >= 0.5 & s[-1] < 0.5)
  if (length(i) == 0) return(NA_real_)
  i <- i[1]
  if (s[i] == s[i + 1]) return(a$x[i])
  a$x[i] + (s[i] - 0.5) / (s[i] - s[i + 1]) * (a$x[i + 1] - a$x[i])
}

game_summary <- function(d) {
  d <- copy(d)
  d[, two_sided := !is.na(bid) & !is.na(ask) & bid > 0 & ask < 1]
  d[, p_over := fifelse(yes_over == 1, mid, 1 - mid)]
  d[, .(
    home_bid    = bid[kind == "game" & yes_over == 1][1],
    home_ask    = ask[kind == "game" & yes_over == 1][1],
    home_mid    = mid[kind == "game" & yes_over == 1][1],
    away_mid    = mid[kind == "game" & yes_over == 0][1],
    home_wp     = mean(p_over[kind == "game"], na.rm = TRUE),
    margin_line = implied_line(x[kind != "total" & two_sided], p_over[kind != "total" & two_sided]),
    total_line  = implied_line(x[kind == "total" & two_sided], p_over[kind == "total" & two_sided]),
    n_spread    = sum(kind == "spread" & two_sided),
    n_total     = sum(kind == "total" & two_sided),
    game_vol    = sum(vol[kind == "game"]),
    spread_vol  = sum(vol[kind == "spread"]),
    total_vol   = sum(vol[kind == "total"])
  ), by = t][, `:=`(home_wp     = fifelse(is.nan(home_wp), NA_real_, round(home_wp, 4)),
                    margin_line = round(margin_line, 2),
                    total_line  = round(total_line, 2))][]
}

pull_game <- function(gm, gid, tip) {
  mk <- game_markets(gm)
  ml <- mk[kind == "game"]
  if (nrow(ml) == 0) return("no game markets")
  if (any(!ml$result %in% c("yes", "no")))
    return(paste0("not settled (result: ", paste(unique(ml$result), collapse = "/"), ")"))

  # From an hour before tip, or failing that no_tip_mins before the game markets
  # close, to the last market's close (at most 7 hours on)
  t_close <- floor(min(ml$close_t) / 60) * 60
  tip_ok  <- !is.na(tip) && (t_close - tip) >= 90 * 60 && (t_close - tip) <= 6 * 3600
  t_keep  <- if (tip_ok) floor(tip / 60) * 60 - pre_tip_mins * 60 else t_close - no_tip_mins * 60
  t_last  <- min(ceiling(max(mk$close_t) / 60) * 60, t_keep + 7 * 3600)
  t_from  <- t_keep - 15 * 60      # a little earlier, so every market has a candle by t_keep

  cs <- rbindlist(list(
    if (any(mk$source == "live"))
      candles_live(mk[source == "live", ticker], t_from, t_last),
    if (any(mk$source == "historical"))
      candles_historical(mk[source == "historical", ticker], t_from, t_last)
  ), fill = TRUE)
  if (nrow(cs) == 0) return("no candles")

  d <- fill_minutes(cs, mk, t_keep, t_last)
  d <- mk[, .(ticker, kind, x, yes_over)][d, on = "ticker"]
  d[, `:=`(gid = gid, game = gm$game[1], ts = utc_text(t))]
  setcolorder(d, c("gid", "game", "ticker", "kind", "x", "yes_over", "t", "ts",
                   "bid", "ask", "mid", "last", "vol", "oi", "filled"))
  setorder(d, kind, x, ticker, t)

  sm <- game_summary(d)
  sm[, `:=`(gid = gid, game = gm$game[1], date = gm$date[1], away = gm$away[1],
            home = gm$home[1], ts = utc_text(t))]
  setcolorder(sm, c("gid", "game", "date", "away", "home", "t", "ts"))

  mko <- mk[, .(gid = gid, game = gm$game[1], date = gm$date[1], away = gm$away[1],
                home = gm$home[1], ticker, kind, team, strike, x, yes_over, result,
                volume, open_ts = utc_text(open_t), close_ts = utc_text(close_t),
                source, title = yes_sub_title)]

  # the minutes file goes last: it marks the game as done
  fwrite(mko, file.path(out_dir, "markets.csv"), append = TRUE)
  fwrite(sm, file.path(out_dir, "game_minutes.csv"), append = TRUE)
  fwrite(d, game_file(gm))
  sprintf("%d markets, %d minutes, window from %s", nrow(mk), nrow(sm),
          if (tip_ok) "tip" else "close")
}

game_file <- function(gm) {
  file.path(out_dir, "minutes", sprintf("%s_%s%s.csv", gm$date[1], gm$away[1], gm$home[1]))
}

# A run that stopped between files can leave a game in markets.csv or game_minutes.csv
# twice; keep its last copy
dedupe <- function(file, keys) {
  if (!file.exists(file)) return(invisible())
  d <- fread(file, colClasses = list(character = "gid"))
  n <- nrow(d)
  d <- unique(d, by = keys, fromLast = TRUE)
  if (nrow(d) < n) fwrite(d, file)
}


# =====================================================================
# Run
# =====================================================================
kalshi_pull <- function() {
  dir.create(file.path(out_dir, "minutes"), recursive = TRUE, showWarnings = FALSE)

  ev <- game_codes()[date >= date_from & date <= date_to]
  games <- unique(ev[kind == "game", .(game, date, away, home)])
  setorder(games, date, game)
  message(nrow(games), " games from ", date_from, " to ", date_to)
  if (nrow(games) == 0) return(invisible())

  ids <- gid_lookup(games$date)
  if (!is.null(ids)) {
    games <- ids[games, on = .(date, away, home)]
    wait <- is.na(games$gid) & games$date >= Sys.Date() - wait_days
    if (any(wait)) {
      message(sum(wait), " recent games not in gms yet, left for a later run: ",
              paste(games[wait, game], collapse = ", "))
      games <- games[!wait]
    }
    if (any(is.na(games$gid)))
      message(sum(is.na(games$gid)), " games not in gms, pulled without a gid: ",
              paste(head(games[is.na(gid), game], 10), collapse = ", "))
  } else {
    games[, `:=`(gid = NA_character_, tip = NA_real_)]
  }

  done <- file.exists(vapply(seq_len(nrow(games)), \(i) game_file(games[i]), ""))
  if (!redo && any(done)) {
    message(sum(done), " already pulled")
    games <- games[!done]
  }

  for (i in seq_len(nrow(games))) {
    g <- games[i]
    res <- tryCatch(pull_game(ev[game == g$game], g$gid, g$tip),
                    error = function(e) paste("failed:", conditionMessage(e)))
    message(sprintf("%d of %d  %s  %s", i, nrow(games), g$game, res))
  }

  dedupe(file.path(out_dir, "markets.csv"), c("game", "ticker"))
  dedupe(file.path(out_dir, "game_minutes.csv"), c("game", "t"))
  invisible()
}

kalshi_pull()
