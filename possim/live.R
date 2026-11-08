# possim/live.R - live betting core: from a moment in a game to Kelly-sized Kalshi orders
#
#   su  <- live_setup("0022600180")          # once a game: team numbers, anchored to the line and total
#   act <- game_actions(su)                  # the NBA play-by-play (pbpr2: the live feed's own format)
#   st  <- state_at(su, act, k)              # the model's state at action k, as weighted possession starts
#   d   <- score_dist(su, st)                # simulated final scores and their probabilities
#   Y   <- market_matrix(d, markets)         # which final scores pay out on each Kalshi contract
#   kelly_orders(d$p, Y, quotes, positions, bankroll = 1000)
#
# The model only knows possession starts, so a stoppage is turned into one:
#   quarter break                  the next period's first possession (overtime: either team, 50/50)
#   after a make / rebound / steal / dead-ball turnover, clock not yet moving
#                                  that possession start, exactly
#   free throws still to come      every make/miss sequence, weighted by ft_pct; after the last one
#                                  the other team has the ball (MFT if made, DRB if missed)
#   anything else (a timeout mid-possession, a non-shooting foul, a violation)
#                                  an inbound (DTO) for the team with the ball, clock as it stands;
#                                  the shot clock is ignored
#
# Kelly: maximize expected utility of wealth over the simulated final scores. gamma = 1 is
# log utility (full Kelly); gamma = 2 or 4 is the CRRA version of half or quarter Kelly.
# Existing positions count, so buying the other side of a held contract trims it. Costs
# include Kalshi's taker fee, fee x P x (1 - P) per contract (0.07 for the NBA series).

here <- local({   # this script's folder, whether run with Rscript or source()d
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "chart.R"))   # the engine, game_states() and team_numbers()
suppressMessages(library(parallel))
RNGkind("L'Ecuyer-CMRG")            # independent random streams in each forked simulation

kalshi_fee <- 0.07
ft_pct     <- 0.78                  # league free-throw rate, for free throws still to come

# ── Once a game ──────────────────────────────────────────────────────────────
# gm: one row with gid, ssn, date, htm, atm, line, total. tip: 0 if the home team won the
# opening tip (NA until it's known; the live loop fills it in from the feed).
setup_game <- function(gm, tip = NA_integer_) {
  eng <- chart_engine()
  tn  <- team_numbers(gm)
  g   <- game_inputs(eng, gm$line, gm$total, home = tn$home, away = tn$away)
  list(gid = gm$gid, game = gm, eng = eng, g = g, tt = team_terms(eng, g), tip = tip, ratings = tn$source)
}

# a game already in the database (gms, pos, wpg): the backtest's way in
live_setup <- function(gid) {
  gd <- game_states(gid)
  setup_game(gd$game, gd$tip)
}

# ── Today's games, from the NBA's scoreboard file ────────────────────────────
nba_scoreboard <- c("https://nba-prod-us-east-1-mediaops-stats.s3.amazonaws.com/NBA/liveData/scoreboard/todaysScoreboard_00.json",
                    "https://cdn.nba.com/static/json/liveData/scoreboard/todaysScoreboard_00.json")
# the NBA's live files, with a short timeout: a hung read would hold up every game in the loop
nba_json <- function(u, timeout = 8) {
  resp <- httr2::request(u) |> httr2::req_user_agent("hoops-possim-live") |> httr2::req_timeout(timeout) |>
    httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
}

todays_games <- function() {
  for (u in nba_scoreboard) {
    j <- tryCatch(nba_json(u)$scoreboard, error = function(e) NULL)
    if (!is.null(j)) break
  }
  if (is.null(j) || length(j$games) == 0) return(data.table())
  g <- as.data.table(j$games)
  g[, .(gid = gameId, status = gameStatus, status_text = gameStatusText,
        tip_t = as.numeric(as.POSIXct(substr(gameTimeUTC, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")),
        date = as.Date(j$gameDate), atm = fix_team(awayTeam.teamTricode), htm = fix_team(homeTeam.teamTricode))]
}

# ── Kalshi (public market data, no login) ────────────────────────────────────
kalshi_api <- "https://api.elections.kalshi.com/trade-api/v2"
kalshi_series <- c(game = "KXNBAGAME", spread = "KXNBASPREAD", total = "KXNBATOTAL")
kget <- function(path, query = list()) {
  resp <- httr2::request(kalshi_api) |> httr2::req_url_path_append(path) |> httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("hoops-possim-live") |> httr2::req_timeout(15) |>
    # jittered, so games being set up side by side don't all retry at the same moment
    httr2::req_retry(max_tries = 4, is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504),
                     backoff = \(i) runif(1, 1, 2) * 2^(i - 1)) |>
    httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
}

# Kalshi's code for a game: YYMONDD + away + home, the US Eastern date
kalshi_code <- function(gm) paste0(toupper(format(gm$date, "%y%b%d")), gm$atm, gm$htm)

# Every win, spread and total market for the game, with the best prices and the size
# at each: yes_ask / yes_ask_size to buy yes, no_ask / no_ask_size to buy no.
# kind, x and yes_over as in kalshiNBA_rtask.R (see market_matrix).
kalshi_quotes <- function(gm) {
  code <- kalshi_code(gm)
  mk <- rbindlist(lapply(names(kalshi_series), function(k) {
    j <- kget("markets", list(event_ticker = paste0(kalshi_series[[k]], "-", code), limit = 200))
    if (length(j$markets) == 0) return(NULL)
    m <- as.data.table(j$markets)
    # a field Kalshi leaves out (win markets have no floor_strike) is NA, not a zero-row table
    num <- function(v) if (is.null(v)) rep(NA_real_, nrow(m)) else suppressWarnings(as.numeric(v))
    data.table(ticker = m$ticker, kind = k, status = m$status, floor_strike = num(m$floor_strike),
               yes_bid = num(m$yes_bid_dollars), yes_ask = num(m$yes_ask_dollars),
               no_ask = num(m$no_ask_dollars), yes_ask_size = num(m$yes_ask_size_fp),
               no_ask_size = num(m$yes_bid_size_fp), shard = num(m$exchange_index))
  }))
  if (nrow(mk) == 0) return(mk)
  mk[, team := ifelse(kind == "total", NA_character_, sub("[0-9.]+$", "", sub(".*-", "", ticker)))]
  mk[, x := fcase(kind == "total", floor_strike, kind == "game", 0,
                  team == gm$htm, floor_strike, default = -floor_strike)]
  mk[, yes_over := as.integer(kind == "total" | team == gm$htm)]
  mk[, mid := (yes_bid + yes_ask) / 2]
  mk[status == "active"]
}

# The x where P(quantity > x) crosses 50%, from a ladder (isotonic, then interpolated)
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

# The home line (negative = home favoured) and total Kalshi's prices imply, from the mids
# at time t (Unix seconds; default now). Used to anchor a game that has no line in gms.
kalshi_lines <- function(gm, t = NULL) {
  mk <- kalshi_quotes(gm)
  if (nrow(mk) == 0) return(c(line = NA_real_, total = NA_real_))
  if (!is.null(t)) {    # the mids in the minute before t, from Kalshi's 1-minute candles
    j <- kget("markets/candlesticks", list(market_tickers = paste(mk$ticker, collapse = ","),
                                           start_ts = t - 900, end_ts = t, period_interval = 1))
    if (length(j$markets)) {
      last <- rbindlist(lapply(seq_len(nrow(j$markets)), function(i) {
        cs <- j$markets$candlesticks[[i]]
        if (length(cs) == 0 || nrow(cs) == 0) return(NULL)
        cs <- as.data.table(jsonlite::flatten(cs))
        b <- suppressWarnings(as.numeric(cs$yes_bid.close_dollars %||% cs$yes_bid.close))
        a <- suppressWarnings(as.numeric(cs$yes_ask.close_dollars %||% cs$yes_ask.close))
        data.table(ticker = j$markets$market_ticker[i], b2 = tail(b, 1), a2 = tail(a, 1))
      }))
      # the quotes as they stood then, not now: mid-game, many strikes have gone one-sided
      if (nrow(last)) mk <- merge(mk, last, by = "ticker")[, `:=`(yes_bid = b2, yes_ask = a2, mid = (b2 + a2) / 2)]
      else mk <- mk[0]
    }
  }
  two <- mk[yes_bid > 0 & yes_ask < 1]
  two[, p_over := fifelse(yes_over == 1, mid, 1 - mid)]
  c(line = -implied_line(two[kind != "total", x], two[kind != "total", p_over]),
    total = implied_line(two[kind == "total", x], two[kind == "total", p_over]))
}

# ── Play-by-play ─────────────────────────────────────────────────────────────
# pbpr2 rows are the NBA live feed's actions, so the live loop can hand over the
# feed's actions in the same shape.
game_actions <- function(su) {
  a <- sqlq(sprintf(paste("SELECT orderNumber, actionNumber, clock, timeActual, period, actionType, subType,",
                          "descriptor, possession, scoreHome, scoreAway, teamId, teamTricode, shotResult, description",
                          "FROM pbpr2 WHERE gid = '%s'"), su$gid))
  tidy_actions(a, su$game)
}

# The same actions straight from the NBA's live play-by-play file: the S3 bucket behind
# cdn.nba.com first, as wpNBA_rtask.R reads it, then cdn.nba.com.
nba_feed <- c("https://nba-prod-us-east-1-mediaops-stats.s3.amazonaws.com/NBA/liveData/playbyplay/playbyplay_%s.json",
              "https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_%s.json")
feed_actions <- function(su) {
  j <- NULL
  for (u in nba_feed) {
    j <- tryCatch(nba_json(sprintf(u, su$gid)), error = function(e) NULL)
    if (!is.null(j)) break
  }
  if (is.null(j)) stop("couldn't read the NBA play-by-play for ", su$gid)
  feed_table(j$game$actions, su$game)
}
# the live file's actions, parsed, in the form game_actions() gives
feed_table <- function(a, gm) {
  a <- as.data.table(a)
  for (cn in c("descriptor", "possession", "teamId", "teamTricode", "shotResult", "subType", "description"))
    if (!cn %in% names(a)) a[, (cn) := NA]
  a <- a[, .(orderNumber, actionNumber, clock, timeActual, period, actionType, subType, descriptor,
             possession, scoreHome = as.numeric(scoreHome), scoreAway = as.numeric(scoreAway),
             teamId, teamTricode, shotResult, description)]
  tidy_actions(a, gm)
}

tidy_actions <- function(a, gm) {
  a <- as.data.table(a)
  setorder(a, orderNumber)
  a[, tr := clock_secs(clock)]
  a[, t := as.numeric(as.POSIXct(substr(timeActual, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))]
  a[, tm := fix_team(teamTricode)]
  a[possession == 0, possession := NA]
  a[, possession := nafill(possession, "locf")]   # who has the ball, carried over actions that don't say
  ids <- unique(a[!is.na(teamId) & !is.na(tm), .(teamId, tm)])
  a[, `:=`(home_id = ids[tm == gm$htm, teamId][1], away_id = ids[tm == gm$atm, teamId][1])]
  a[]
}

clock_secs <- function(x) {
  m <- suppressWarnings(as.numeric(sub("^PT([0-9]+)M.*$", "\\1", x)))
  s <- suppressWarnings(as.numeric(sub("^PT[0-9]+M([0-9.]+)S$", "\\1", x)))
  round(m * 60 + s)
}

# actions that change the game state; the rest (timeouts, substitutions, reviews) don't
state_types <- c("2pt", "3pt", "freethrow", "rebound", "turnover", "steal", "foul",
                 "violation", "jumpball", "period", "heave")
admin_types <- c("timeout", "substitution", "instantreplay", "stoppage", "ejection", "memo")

made <- function(a) {
  r <- a$shotResult
  ifelse(!is.na(r) & nzchar(r), r == "Made", !grepl("^MISS", a$description))
}

# ── The model's state at action k ────────────────────────────────────────────
# Returns weighted possession starts: q, tr, h, a (scores at that start), m, side (0 =
# home ball), st, w, and what kind of moment it is.
state_at <- function(su, act, k) {
  x <- act[k]
  home_id <- x$home_id; tip <- su$tip
  one <- function(q, tr, h, a, side, st, w = 1)
    data.table(q = q, tr = tr, h = h, a = a, m = h - a, side = side, st = st, w = w)
  side_of <- function(team_id) as.integer(team_id != home_id)
  h <- x$scoreHome; a <- x$scoreAway

  prev <- act[seq_len(k)][actionType %in% state_types]
  if (nrow(prev) == 0) return(cbind(one(1, 720, 0, 0, tip, "PST"), kind = "tip"))
  L <- prev[.N]

  # quarter break: the next period's first possession
  if (L$actionType == "period" && L$subType == "end") {
    if (L$period >= 4 && h != a) return(cbind(one(L$period, 0, h, a, 0L, "PST"), kind = "game over"))
    q <- L$period + 1
    if (q <= 4) return(cbind(one(q, 720, h, a, if (q %in% 2:3) 1L - tip else tip, "PST"), kind = "period break"))
    return(cbind(rbind(one(q, 300, h, a, 0L, "PST", 0.5), one(q, 300, h, a, 1L, "PST", 0.5)), kind = "before overtime"))
  }
  if (L$actionType == "period" && L$subType == "start") {
    q <- L$period
    side <- if (q <= 4) (if (q %in% 2:3) 1L - tip else tip) else side_of(x$possession)
    return(cbind(one(q, L$tr, h, a, side, "PST"), kind = "period start"))
  }

  # free throws still to come
  pend <- 0L; shooter <- NA; flagrant <- FALSE
  if (L$actionType == "freethrow") {
    jn <- suppressWarnings(as.integer(sub("^[^0-9]*([0-9]+) of [0-9]+.*$", "\\1", L$subType)))
    n  <- suppressWarnings(as.integer(sub("^[^0-9]*[0-9]+ of ([0-9]+).*$", "\\1", L$subType)))
    if (!is.na(jn) && !is.na(n) && jn < n && !grepl("echnical", L$subType)) { pend <- n - jn; shooter <- L$teamId }
    flagrant <- grepl("lagrant", L$subType)
  } else if (L$actionType == "foul" && !grepl("echnical", L$subType)) {
    n <- suppressWarnings(as.integer(sub(".*\\([^()]* ([0-9]) FT\\).*", "\\1", L$description)))
    if (!is.na(n) && grepl("[0-9] FT\\)", L$description)) {
      pend <- n; shooter <- if (L$teamId == home_id) x$away_id else home_id
      flagrant <- grepl("lagrant", L$subType)
    }
  }
  if (pend > 0) {
    s_side <- side_of(shooter)
    seqs <- as.matrix(expand.grid(rep(list(0:1), pend)))
    w <- apply(seqs, 1, function(r) prod(ifelse(r == 1, ft_pct, 1 - ft_pct)))
    out <- rbindlist(lapply(seq_len(nrow(seqs)), function(i) {
      pts <- sum(seqs[i, ]); last_made <- seqs[i, pend] == 1
      hh <- h + if (s_side == 0) pts else 0; aa <- a + if (s_side == 1) pts else 0
      if (flagrant) one(x$period, x$tr, hh, aa, s_side, "DTO", w[i])          # fouled team keeps the ball
      else one(x$period, x$tr, hh, aa, 1L - s_side, if (last_made) "MFT" else "DRB", w[i])
    }))
    out <- out[, .(w = sum(w)), by = .(q, tr, h, a, m, side, st)]
    return(cbind(out, kind = sprintf("%d free throw%s to come", pend, if (pend > 1) "s" else "")))
  }

  # the clock has run since the last state change: mid-possession
  q <- x$period; tr <- x$tr
  if (!is.na(L$tr) && L$period == q && L$tr - tr > 1)
    return(cbind(one(q, tr, h, a, side_of(x$possession), "DTO"), kind = "mid-possession"))

  other <- function(team_id) 1L - side_of(team_id)
  st <- switch(L$actionType,
    "2pt" =, "3pt" = if (made(L)) list(other(L$teamId), "MFG") else NULL,
    "freethrow" = list(other(L$teamId), if (made(L)) "MFT" else "DRB"),
    "rebound" = if (L$subType == "defensive") list(side_of(L$teamId), "DRB") else NULL,
    "steal" = list(side_of(L$teamId), "STL"),
    "turnover" = list(other(L$teamId), "DTO"),
    NULL)
  if (!is.null(st)) return(cbind(one(q, tr, h, a, st[[1]], st[[2]]), kind = paste("after", L$actionType)))
  cbind(one(q, tr, h, a, side_of(x$possession), "DTO"), kind = "inbound")
}

# ── Final scores ─────────────────────────────────────────────────────────────
# nsim games simulated from the states (in proportion to their weights), split over
# cores. Technical free throws still to come are added at their per-game rate.
score_dist <- function(su, states, nsim = 1e5, cores = detectCores()) {
  eng <- su$eng; tt <- su$tt
  n <- as.integer(round(states$w / sum(states$w) * nsim))
  jobs <- rbindlist(lapply(seq_len(nrow(states)), function(i) {
    if (n[i] == 0) return(NULL)
    k <- max(1L, min(cores, n[i] %/% 2000L))
    data.table(i = i, n = diff(round(seq(0, n[i], length.out = k + 1))))
  }))
  sims <- mclapply(seq_len(nrow(jobs)), function(j) {
    s <- states[jobs$i[j]]
    x <- sim_game(eng$P0o, eng$P0d, eng$pts, eng$nxt, eng$pend, tt$ltH, tt$ltA, tt$lamH, tt$lamA,
                  eng$fw, max_dur, as.integer(min(s$q, 5)), as.integer(s$tr), as.integer(s$m),
                  as.integer(s$side), st_index(s$st), as.integer(su$tip), as.integer(jobs$n[j]))
    left <- if (s$q <= 4) (4 - s$q) * 720 + s$tr else 0
    nt <- rpois(nrow(x), eng$tech_pts * left / 2880); th <- rbinom(nrow(x), nt, 0.5)
    data.table(hf = s$h + x[, 1] + th, af = s$a + x[, 2] + nt - th, ot = x[, 3] > 0)
  }, mc.cores = cores)
  d <- rbindlist(sims)
  out <- d[, .(p = .N / nrow(d), p_ot = mean(ot)), keyby = .(hf, af)]
  attr(out, "nsim") <- nrow(d)
  out[]
}

# ── Contracts ────────────────────────────────────────────────────────────────
# mk: one row per market with kind (game, spread, total), x and yes_over, as in
# kalshiNBA_rtask.R: game/spread yes if home margin > x (yes_over 1) or < x (0);
# total yes if the total > x. Overtime counts.
market_matrix <- function(d, mk) {
  mg <- d$hf - d$af; tot <- d$hf + d$af
  Y <- vapply(seq_len(nrow(mk)), function(j) {
    v <- if (mk$kind[j] == "total") tot else mg
    as.numeric(if (mk$yes_over[j] == 1) v > mk$x[j] else v < mk$x[j])
  }, numeric(length(mg)))
  if (is.null(dim(Y))) Y <- matrix(Y, nrow = 1)
  colnames(Y) <- mk$ticker
  Y
}

# A side of a contract as a sportsbook bet: "OKC -7.5" pays if OKC wins by 8 or more, "HOU +7.5"
# if HOU wins or loses by 7 or fewer, "Over 225.5" on 226 points or more.
bet_label <- function(kind, x, yes_over, side, home, away) {
  over <- (yes_over == 1) == (side == "yes")      # pays when the margin (home minus away) or total is above x
  fifelse(kind == "total", sprintf("%s %.1f", fifelse(over, "Over", "Under"), x),
  fifelse(kind == "game", sprintf("%s to win", fifelse(over, home, away)),
          fifelse(over, sprintf("%s %+.1f", home, -x), sprintf("%s %+.1f", away, x))))
}

kfee <- function(price, contracts = 1, fee = kalshi_fee) ceiling(100 * fee * contracts * price * (1 - price) - 1e-9) / 100

# ── Kelly ────────────────────────────────────────────────────────────────────
# p: outcome probabilities; Y: outcomes x markets (1 = yes pays). quotes: ticker,
# yes_ask, no_ask (dollars), and optionally yes_depth / no_depth (contracts on offer
# at that price). positions: ticker and net yes contracts (negative = no contracts).
# bankroll: dollars to Kelly against, not counting this game's positions.
# max_cost: most to spend on any one market at a time, as a share of bankroll.
# min_edge: smallest model probability minus fee-inclusive price worth buying (a side that
# trims a held position is always allowed). price_range: only buy at prices in this range.
# hedge: also let sides below min_edge be bought, valued as if their edge were at most zero
# (the model's small edges aren't trusted), so they're bought only where they lower the risk
# of the rest of the book - say, the other team to win against held spread contracts.
kelly_orders <- function(p, Y, quotes, positions = NULL, bankroll, gamma = 1, fee = kalshi_fee,
                         max_cost = 0.25, min_edge = 0, price_range = c(0.005, 0.995), hedge = FALSE) {
  q <- as.data.table(quotes)[match(colnames(Y), ticker)]
  if (is.null(q$yes_depth)) q[, yes_depth := Inf]
  if (is.null(q$no_depth))  q[, no_depth := Inf]
  pos <- if (is.null(positions) || nrow(positions) == 0) rep(0, ncol(Y)) else {
    v <- positions$position[match(colnames(Y), positions$ticker)]; v[is.na(v)] <- 0; v }
  E <- as.vector(Y %*% pmax(pos, 0) + (1 - Y) %*% pmax(-pos, 0)) / bankroll   # what held contracts pay

  # the candidate bets: buy yes or buy no on each market with a live quote
  cand <- rbind(q[, .(j = .I, ticker, side = "yes", price = yes_ask, depth = yes_depth)],
                q[, .(j = .I, ticker, side = "no",  price = no_ask,  depth = no_depth)])
  cand <- cand[!is.na(price) & price >= price_range[1] & price <= price_range[2]]
  cand[, cost := price + fee * price * (1 - price)]                           # per contract, with the fee
  py <- as.vector(crossprod(Y, p))
  cand[, p_win := ifelse(side == "yes", py[j], 1 - py[j])]
  cand[, edge := p_win - cost]
  cand[, below := edge <= min_edge]
  if (!hedge) cand <- cand[!below | (side == "yes" & pos[j] < 0) | (side == "no" & pos[j] > 0)]
  cand[, cost_opt := fifelse(below & hedge, pmax(cost, p_win), cost)]   # what the optimizer is told it pays
  empty <- list(orders = data.table(), growth = 0, ev = 0, candidates = cand)
  if (nrow(cand) == 0) return(empty)

  R <- sapply(seq_len(nrow(cand)), function(i) {
    y <- Y[, cand$j[i]]; (if (cand$side[i] == "yes") y else 1 - y) - cand$cost_opt[i]
  })
  if (is.null(dim(R))) R <- matrix(R, nrow = length(p))
  U  <- if (gamma == 1) function(w) log(w) else function(w) (w^(1 - gamma) - 1) / (1 - gamma)
  dU <- function(w) w^(-gamma)
  floor_w <- 1e-6
  f <- function(c) {
    w <- 1 + E + as.vector(R %*% c)
    if (any(w <= floor_w)) return(1e10)
    -sum(p * U(w))
  }
  gr <- function(c) {
    w <- pmax(1 + E + as.vector(R %*% c), floor_w)
    -as.vector(crossprod(R, p * dU(w)))
  }
  upper <- pmin(cand$depth / bankroll, max_cost / cand$cost)
  # a side let in only because it trims a held position can sell that position, not go past flat
  if (!hedge) upper[cand$below] <- pmin(upper[cand$below], abs(pos[cand$j[cand$below]]) / bankroll)
  op <- optim(rep(0, nrow(cand)), f, gr, method = "L-BFGS-B", lower = 0, upper = upper,
              control = list(factr = 1e7, maxit = 500))
  cand[, contracts := floor(op$par * bankroll + 1e-9)]
  o <- cand[contracts > 0]
  if (nrow(o) == 0) return(empty)
  o[, `:=`(fee = kfee(price, contracts, fee), cost_total = round(price * contracts, 2) + kfee(price, contracts, fee))]
  c_int <- numeric(nrow(cand)); c_int[cand$contracts > 0] <- cand$contracts[cand$contracts > 0] / bankroll
  w0 <- 1 + E; w1 <- 1 + E + as.vector(R %*% c_int)
  list(orders = o[, .(ticker, side, contracts, price, fee, cost = cost_total, p_win = round(p_win, 4),
                      edge = round(edge, 4))],
       growth = sum(p * U(pmax(w1, floor_w))) - sum(p * U(w0)),
       ev = sum(o$contracts * o$edge),   # at the model's own probabilities, hedges included
       p_lose = sum(p[w1 < w0]),
       candidates = cand)
}
