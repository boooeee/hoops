# possim/console_lib.R - what the betting console (console.R) and the bot (bot.R) share: the order
# log, spending and paper positions, Kelly's orders as rows, replays, the feed, and when a
# stoppage can be bet. Each app sets run_mode, orders_file and kalshi_dir before calling these.

# ── The order log ────────────────────────────────────────────────────────────
# cash: what the order did to cash, negative for a buy (price and fee), positive for a sell
# What the model said about each order, and when, so a night's results can be set against what
# the model expected:
#   p_model     the model's probability that `outcome` (the side bought) wins
#   edge        p_model minus the limit price and fee, per contract (negative for most sells)
#   q, clock    the stoppage: period and game clock
#   t_stop      when it happened; seen_after: seconds until the console saw it (feed delay)
#   click_after seconds from the orders being worked out to this order being sent
#   checks      1 for the first look at the stoppage, 2+ after Recheck prices
log_cols <- data.table(time = character(), mode = character(), gid = character(), stop = character(), ticker = character(),
                       bet = character(), action = character(), outcome = character(), count = numeric(), limit = numeric(),
                       filled = numeric(), avg_price = numeric(), fee = numeric(), cash = numeric(), status = character(),
                       order_id = character(), note = character(), p_model = numeric(), edge = numeric(), q = numeric(),
                       clock = character(), t_stop = character(), seen_after = numeric(), click_after = numeric(), checks = numeric())
read_orders <- function() {
  if (!file.exists(orders_file)) return(copy(log_cols))
  chr <- intersect(c("time", "gid", "stop", "order_id", "note", "clock", "t_stop"), names(fread(orders_file, nrows = 0)))
  o <- fread(orders_file, colClasses = list(character = chr))
  if (nrow(o) == 0) return(copy(log_cols))
  for (cn in setdiff(names(log_cols), names(o))) set(o, j = cn, value = log_cols[[cn]][NA_integer_])   # a log from before a column was added
  o
}
log_order <- function(row) {
  # a log written before the newer columns: rewrite it with them (empty) first, so rows line up
  if (file.exists(orders_file) && !identical(names(fread(orders_file, nrows = 0)), names(log_cols)))
    fwrite(read_orders()[, names(log_cols), with = FALSE], orders_file)
  fwrite(row[, names(log_cols), with = FALSE], orders_file, append = file.exists(orders_file))
}

net_cost <- function(o, g = NULL) {                 # spent, net of sells, by this mode (and game)
  o <- o[o$mode == run_mode]
  if (!is.null(g)) o <- o[o$gid == g]
  x <- -sum(o$cash, na.rm = TRUE)
  if (abs(x) < 0.005) 0 else x
}
paper_book <- function(o, g, bankroll) {
  l <- o[o$mode == run_mode & o$gid == g & o$filled > 0]
  list(cash = bankroll + sum(l$cash),
       positions = if (nrow(l)) l[, .(position = sum(fifelse(outcome == "yes", filled, -filled))), by = ticker][position != 0]
                   else data.table(ticker = character(), position = numeric()))
}

# the most contracts at `price` that fit `budget`, fee included
fit_count <- function(n, price, budget) {
  if (is.na(budget) || budget <= 0 || is.na(price) || price <= 0) return(0)
  k <- min(n, floor(budget / price))
  while (k > 0 && k * price + kfee(price, k) > budget + 1e-9) k <- k - 1
  max(k, 0)
}

# ── Turning Kelly's orders into rows to click ────────────────────────────────
# Kelly says "buy N of this side". Against a held position on the other side, the first
# part of that sells what's held (a reduce-only order) and any rest is a new bet.
order_rows <- function(o, q, home, away) {
  if (is.null(o) || nrow(o) == 0) return(data.table())
  o <- merge(o, q[, .(ticker, kind, x, yes_over)], by = "ticker", sort = FALSE)
  rows <- rbindlist(lapply(seq_len(nrow(o)), function(j) {
    r <- o[j]; held <- if (is.null(r$held)) 0 else r$held
    closes <- (r$side == "no" && held > 0) || (r$side == "yes" && held < 0)
    n_sell <- if (closes) min(r$contracts, abs(held)) else 0
    parts <- list()
    if (n_sell > 0) parts[[1]] <- data.table(action = "SELL", n = n_sell, held_side = if (held > 0) "yes" else "no")
    if (r$contracts > n_sell) parts[[length(parts) + 1]] <- data.table(action = "BUY", n = r$contracts - n_sell, held_side = NA_character_)
    cbind(r[rep(1, length(parts)), .(ticker, kind, x, yes_over, outcome = side, price, p_win, edge)], rbindlist(parts))
  }))
  rows[, bet := fifelse(action == "SELL", bet_label(kind, x, yes_over, held_side, home, away),
                        bet_label(kind, x, yes_over, outcome, home, away))]
  rows[, `:=`(row = .I, status = "")]
  rows[]
}

# ── Replays: past games' play-by-play and saved Kalshi minutes ───────────────
# pick: a game id, or a date for every game that night. By game id: the game's row from the
# database (gd: teams, line, total, who won the tip), its actions, and Kalshi's minute quotes
replay_load <- function(pick) {
  mk <- fread(file.path(kalshi_dir, "markets.csv"), colClasses = list(character = "gid"))
  g <- unique(mk[!is.na(gid) & nzchar(gid), .(gid, date = as.Date(date), away, home)])
  g <- if (grepl("^00", pick)) g[gid == pick] else g[date == as.Date(pick)]
  g[, f := file.path(kalshi_dir, "minutes", sprintf("%s_%s%s.csv", date, away, home))]
  g <- g[file.exists(f)]
  out <- lapply(seq_len(nrow(g)), function(i) {
    gd <- tryCatch(game_states(g$gid[i]), error = function(e) NULL)
    if (is.null(gd)) return(NULL)
    act <- game_actions(list(gid = g$gid[i], game = gd$game))
    if (nrow(act) == 0) return(NULL)
    list(gd = gd, act = act, qm = fread(g$f[i]))
  })
  out <- setNames(out, g$gid)[!vapply(out, is.null, TRUE)]
  if (length(out) == 0) stop("no game with saved Kalshi minutes and play-by-play for ", pick)
  out
}
replay_quotes <- function(qm, t_now) {
  tq <- qm[t <= t_now, max(t, -Inf)]
  if (!is.finite(tq)) return(data.table())
  qm[t == tq & !is.na(bid) & !is.na(ask), .(ticker, kind, x, yes_over, yes_bid = bid,
     yes_ask = fifelse(ask < 1, ask, NA_real_), no_ask = fifelse(bid > 0, 1 - bid, NA_real_),
     yes_ask_size = Inf, no_ask_size = Inf, mid, status = "active")]
}

game_over <- function(a) a$actionType == "game" ||
  (a$actionType == "period" && a$subType == "end" && a$period >= 4 && a$scoreHome != a$scoreAway)

line_now <- function(q) {      # Kalshi's win price, and the margin and total its ladders imply, from tight quotes
  if (is.null(q) || nrow(q) == 0) return(c(wp = NA, margin = NA, total = NA))
  two <- q[!is.na(yes_bid) & !is.na(yes_ask) & yes_bid > 0 & yes_ask < 1 & yes_ask - yes_bid <= 0.10]
  c(wp = q[kind == "game" & yes_over == 1, mid][1],
    margin = implied_line(two[kind != "total", x], two[kind != "total", fifelse(yes_over == 1, mid, 1 - mid)]),
    total = implied_line(two[kind == "total", x], two[kind == "total", mid]))
}

# ── Every game's feed, for the scoreboard ────────────────────────────────────
# The game being bet needs its whole play-by-play (~400 KB late in a game). The others only need
# where they stand, so just the file's last 25 KB is read for them (about 25 actions, more than
# the substitutions at any stoppage). Requests run in the background, so the page never waits.
feed_req <- function(gid, tail = FALSE, src = 1L) {
  r <- httr2::request(sprintf(nba_feed[src], gid)) |> httr2::req_user_agent("hoops-possim-live") |> httr2::req_timeout(8)
  if (tail) httr2::req_headers(r, Range = "bytes=-25000") else r
}
parse_feed <- function(txt, gm, tail = FALSE) {
  if (!tail) return(feed_table(jsonlite::fromJSON(txt, flatten = TRUE)$game$actions, gm))
  i <- regexpr('{"actionNumber"', txt, fixed = TRUE)        # the first whole action in the tail
  if (i < 0) return(NULL)
  body <- sub("\\]\\s*\\}\\s*\\}\\s*$", "", substring(txt, i))  # less the file's closing ]}}
  feed_table(jsonlite::fromJSON(paste0("[", body, "]"), flatten = TRUE), gm)
}

# When a stoppage's orders can be clicked. seen: when the console first saw the stoppage, for
# any game (the scoreboard watches them all, so a game picked later still counts from then).
#   a break between periods: until break_open seconds after the period ended
#   anything else: valid_for seconds from being seen, and not at all if seen more than max_seen
#   seconds after it happened (90 for a timeout, 60 for anything else: live_settings in bet_live.R)
stop_window <- function(last, seen, s) {
  brk <- last$actionType == "period" && last$subType == "end"
  until <- if (brk) last$t + s$break_open[[if (last$period == 2) "half" else "quarter"]] else seen + s$valid_for
  list(brk = brk, until = until, seen_after = seen - last$t, late = if (brk) seen > until else seen - last$t > seen_limit(last, s))
}
per_name <- function(q) if (q <= 4) paste0("Q", q) else paste0("OT", q - 4)
stop_label <- function(last) switch(last$actionType, timeout = "Timeout", instantreplay = "Review",
  freethrow = "Free throws", foul = "Free throws",
  period = if (last$period == 2) "Halftime" else paste("End of", per_name(last$period)), last$actionType)
mmss <- function(secs) sprintf("%d:%02d", as.integer(secs) %/% 60L, as.integer(secs) %% 60L)

# ── The bot's lock ───────────────────────────────────────────────────────────
# A running bot (bot.R) holds data_dir/live/bot.lock: its process, mode and game. Only one bot
# runs at a time, and the console won't send orders in the bot's game while it runs.
bot_lock_file <- file.path(live_dir, "bot.lock")
bot_stop_file <- file.path(live_dir, "STOP_BOT")       # any bot stops when this file exists
bot_lock_read <- function() {
  if (!file.exists(bot_lock_file)) return(NULL)
  l <- tryCatch(as.list(fread(bot_lock_file, colClasses = "character")), error = function(e) NULL)
  if (is.null(l) || is.null(l$pid)) return(NULL)
  pid <- as.integer(l$pid)
  # a lock left by a process that has died doesn't count
  alive <- !is.na(pid) && isTRUE(tryCatch(tools::pskill(pid, 0L), error = function(e) FALSE))
  if (!alive) return(NULL)
  l
}
bot_lock_write <- function(gid, mode) fwrite(data.table(pid = Sys.getpid(), mode = mode, gid = gid, since = format(Sys.time())), bot_lock_file)
bot_lock_clear <- function() { l <- bot_lock_read(); if (is.null(l) || as.integer(l$pid) == Sys.getpid()) unlink(bot_lock_file) }
