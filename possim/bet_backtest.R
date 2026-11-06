# possim/bet_backtest.R - replay held-out games against Kalshi's quotes at stoppages
#
# For each game with Kalshi minute quotes (from kalshiNBA_rtask.R) and NBA play-by-play
# (pbpr2), at each stoppage - a timeout, a quarter break, a foul with free throws to come,
# a review:
#   - the model's final-score distribution from that moment (live.R)
#   - the quote Kalshi showed during the break: the first minute ending at least 20 seconds
#     after the stoppage and before play resumed. Breaks too short to hold such a minute,
#     like most free-throw breaks, are skipped.
#   - Kelly orders under each strategy below, buying at the ask, with cash and positions
#     carried through the game and settled on Kalshi's result. Each game starts with $1,000.
# Minute quotes carry no depth, so every order is assumed to fill at the ask, up to
# max_order dollars per market per stoppage.
#
# Writes backtest.rds to data_dir: every contract quote at every decision with the model's
# probability (for calibration against the market), and every order (for profit and loss).
#
#   Rscript bet_backtest.R                         every game with quotes and play-by-play
#   Rscript bet_backtest.R 2026-10-20 2026-11-04   games in a date range

here <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "live.R"))

kalshi_dir <- path.expand(Sys.getenv("KALSHI_DIR", "~/Dropbox/Nba/kalshi"))
bankroll   <- 1000        # each game starts with this much cash; bets are sized on cash left plus positions
max_order  <- 100         # most to spend on one market at one stoppage: a stand-in for order-book depth
nsim       <- 30000
quote_lag  <- 20          # seconds after the stoppage before a quote counts
strategies <- list(
  full_kelly    = list(gamma = 1, min_edge = 0,    range = c(0.005, 0.995), kinds = c("game", "spread", "total")),
  quarter_kelly = list(gamma = 4, min_edge = 0,    range = c(0.005, 0.995), kinds = c("game", "spread", "total")),
  quarter_2c    = list(gamma = 4, min_edge = 0.02, range = c(0.05, 0.95),   kinds = c("game", "spread", "total")),
  quarter_2c_win = list(gamma = 4, min_edge = 0.02, range = c(0.05, 0.95),  kinds = "game"))

args <- commandArgs(trailingOnly = TRUE)
date_from <- if (length(args) >= 1) as.Date(args[1]) else as.Date("2025-10-01")
date_to   <- if (length(args) >= 2) as.Date(args[2]) else Sys.Date()

# ── Games ────────────────────────────────────────────────────────────────────
mkts <- fread(file.path(kalshi_dir, "markets.csv"), colClasses = list(character = "gid"))
mkts <- mkts[!is.na(gid) & nzchar(gid) & substr(gid, 1, 3) %in% gid_types]
games <- unique(mkts[, .(gid, game, date = as.Date(date), away, home)])
games <- games[date >= date_from & date <= date_to]
games[, file := file.path(kalshi_dir, "minutes", sprintf("%s_%s%s.csv", date, away, home))]
games <- games[file.exists(file)]
have_pbp <- sqlq(paste0("SELECT DISTINCT gid FROM pbpr2 WHERE gid IN ", in_list(games$gid)))$gid
games <- games[gid %in% have_pbp]
setorder(games, date, gid)
message(nrow(games), " games from ", min(games$date), " to ", max(games$date))

# ── One game ─────────────────────────────────────────────────────────────────
is_stop <- function(a) a$actionType %in% c("timeout", "instantreplay") |
  (a$actionType == "period" & a$subType == "end" & !(a$period >= 4 & a$scoreHome != a$scoreAway)) |
  (a$actionType == "foul" & grepl("[0-9] FT\\)", a$description))

one_game <- function(gid) {
  this <- gid
  gi <- games[gid == this]
  su <- live_setup(gid)
  act <- game_actions(su)
  if (nrow(act) == 0) return(NULL)
  qm <- fread(gi$file)
  setorder(qm, ticker, t)
  qm[, last_real := nafill(fifelse(filled == 0, t, NA_real_), "locf"), by = ticker]   # when the quote last changed
  res <- mkts[gid == this, .(ticker, result)]

  # decision points: a stoppage with a quote minute inside the break
  ks <- which(is_stop(act))
  state_rows <- which(act$actionType %in% state_types)
  dec <- rbindlist(lapply(ks, function(k) {
    nxt <- state_rows[state_rows > k][1]
    t_next <- if (is.na(nxt)) Inf else act$t[nxt]
    tq <- qm[t >= act$t[k] + quote_lag & t <= t_next, min(t, Inf)]
    if (!is.finite(tq)) return(NULL)
    data.table(k = k, tq = tq)
  }))
  if (nrow(dec) == 0) return(NULL)
  dec <- dec[, .SD[1], by = tq]            # one decision per quote minute

  # each strategy's cash and net positions (positive = yes contracts) through the game
  pos  <- lapply(strategies, function(s) data.table(ticker = character(), position = numeric()))
  cash <- sapply(names(strategies), function(s) bankroll)
  contracts <- list(); orders <- list()
  for (i in seq_len(nrow(dec))) {
    k <- dec$k[i]; st <- state_at(su, act, k)
    d <- score_dist(su, st, nsim = nsim, cores = 1)
    q <- qm[t == dec$tq[i] & !is.na(bid) & !is.na(ask)]
    if (nrow(q) == 0) next
    Y <- market_matrix(d, q)
    pm <- as.vector(crossprod(Y, d$p))
    t_play <- act[seq_len(k)][actionType %in% state_types][.N, t]   # the last play before the stoppage
    info <- data.table(gid = gid, dec = i, q = act$period[k], tr = act$tr[k], h = act$scoreHome[k],
                       a = act$scoreAway[k], what = act$actionType[k], state = st$kind[1], t_stop = act$t[k],
                       t_play = t_play, t_quote = dec$tq[i],
                       wp = sum(d$p[d$hf > d$af]), e_margin = sum(d$p * (d$hf - d$af)), e_total = sum(d$p * (d$hf + d$af)))
    contracts[[i]] <- cbind(info, q[, .(ticker, kind, x, yes_over, bid, ask, mid, last_real,
                                        stale = last_real < t_play)], p_model = pm)
    quotes <- q[, .(ticker, yes_ask = fifelse(ask < 1, ask, NA_real_), no_ask = fifelse(bid > 0, 1 - bid, NA_real_))]
    for (s in names(strategies)) {
      S <- strategies[[s]]
      keep <- q$kind %in% S$kinds
      if (!any(keep) || cash[[s]] < 1) next
      o <- kelly_orders(d$p, Y[, keep, drop = FALSE], quotes[keep], positions = pos[[s]], bankroll = cash[[s]],
                        gamma = S$gamma, min_edge = S$min_edge, price_range = S$range,
                        max_cost = max_order / cash[[s]])$orders
      if (nrow(o) == 0) next
      if (sum(o$cost) > cash[[s]]) {        # no more than the cash on hand
        o[, contracts := floor(contracts * cash[[s]] / sum(cost) * 0.999)]
        o <- o[contracts > 0]
        o[, `:=`(fee = kfee(price, contracts), cost = round(price * contracts, 2) + kfee(price, contracts))]
        if (nrow(o) == 0) next
      }
      o <- cbind(info[, .(gid, dec, q, tr)], strategy = s, o)
      orders[[length(orders) + 1]] <- o
      # buying the other side of a held contract closes the pair, which is worth $1 now
      old <- pos[[s]]$position[match(o$ticker, pos[[s]]$ticker)]; old[is.na(old)] <- 0
      signed <- ifelse(o$side == "yes", o$contracts, -o$contracts)
      closed <- ifelse(old * signed < 0, pmin(abs(old), abs(signed)), 0)
      cash[[s]] <- cash[[s]] - sum(o$cost) + sum(closed)
      add <- data.table(ticker = o$ticker, position = signed)
      pos[[s]] <- rbind(pos[[s]], add)[, .(position = sum(position)), by = ticker]
    }
  }
  contracts <- rbindlist(contracts); orders <- rbindlist(orders)
  if (nrow(contracts)) contracts <- merge(contracts, res, by = "ticker", all.x = TRUE)
  if (nrow(orders)) {
    orders <- merge(orders, res, by = "ticker", all.x = TRUE)
    orders[, payout := contracts * as.numeric((side == "yes" & result == "yes") | (side == "no" & result == "no"))]
    orders[, pnl := payout - cost]
  }
  list(contracts = contracts, orders = orders,
       game = cbind(gi[, .(gid, date, away, home)], line = su$game$line, total = su$game$total,
                    hscr = su$game$hscr, ascr = su$game$ascr, decisions = nrow(dec)))
}

# ── Run ──────────────────────────────────────────────────────────────────────
# load everything the workers share before forking
invisible(chart_engine()); invisible(snapshot())
invisible(cached("ratings", function() readRDS(rds("ratings"))))
for (s in unique(2000L + as.integer(substr(games$gid, 4, 5)))) invisible(team_games(s, games[, min(date)]))
t0 <- Sys.time()
out <- mclapply(games$gid, function(g) tryCatch(one_game(g), error = function(e) list(error = conditionMessage(e), gid = g)),
                mc.cores = detectCores(), mc.preschedule = FALSE)
err <- Filter(function(x) !is.null(x$error), out)
if (length(err)) message(length(err), " games failed, e.g. ", err[[1]]$gid, ": ", err[[1]]$error)
ok <- Filter(function(x) !is.null(x) && is.null(x$error), out)
bt <- list(contracts = rbindlist(lapply(ok, `[[`, "contracts")), orders = rbindlist(lapply(ok, `[[`, "orders")),
           games = rbindlist(lapply(ok, `[[`, "game")), strategies = strategies,
           settings = list(bankroll = bankroll, max_order = max_order, nsim = nsim, quote_lag = quote_lag,
                           from = date_from, to = date_to))
saveRDS(bt, rds(sprintf("backtest_%s_%s", format(date_from), format(date_to))))
message(sprintf("%d games, %d decisions, %d orders in %.1f min", nrow(bt$games), uniqueN(bt$contracts[, .(gid, dec)]),
                nrow(bt$orders), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
