# possim/bet_live.R - Kalshi bet recommendations at stoppages in today's games
#
#   Rscript bet_live.R              every game today, until the last one ends
#   Rscript bet_live.R PHX GSW      just these teams' games
#
# or from R:  source("~/hoops/possim/bet_live.R"); run_live("PHX")
#
# Every `poll` seconds it reads each live game's NBA play-by-play. When play is stopped -
# a timeout, a quarter break, free throws to come, a review - and that stoppage hasn't been
# handled yet, it:
#   1. turns the moment into the model's state and simulates final scores (live.R)
#   2. reads the game's Kalshi markets: best prices, and the size offered at each
#   3. sizes Kelly orders against that game's cash and positions in the ledger
#   4. prints them, and appends them to the log
#
# Nothing is sent to Kalshi. With paper = TRUE (the default) every recommended order goes
# into the ledger as if it filled, so later stoppages size against it. To track real bets,
# set paper = FALSE and add your own fills to the ledger file.
#
# Files, under data_dir/live/:
#   <date>_ledger.csv   one row per fill: time, gid, ticker, side, contracts, price, fee, cost
#   <date>_log.csv      every stoppage handled: the state, the model's numbers, the orders
#   <date>_games.rds    each game's setup (the line and total it was anchored to, team
#                       numbers), so a restart mid-game anchors the same way
#
# A game is anchored to the line and total in gms when the WP task has written them, and
# otherwise to the line and total Kalshi's prices implied just before the scheduled tip.

here <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "live.R"))

live_settings <- list(
  poll        = 5,                 # seconds between reads of the play-by-play
  nsim        = 50000,             # simulated finishes per stoppage
  bankroll    = 500,               # cash per game
  gamma       = 4,                 # 1 = full Kelly; 4 is about quarter Kelly
  min_edge    = 0.02,              # model probability minus fee-inclusive price
  price_range = c(0.05, 0.95),     # only buy at prices in this range
  max_order   = 100,               # most to spend on one market at one stoppage
  max_seen    = 60,                # skip a stoppage seen this many seconds after it happened (the
                                   # backtest's quotes were 20-80 s after; the feed can stall for minutes)
  # quarters to bet each kind of market in (5 = overtime). In the 2025-26 backtest, win
  # markets made money only in Q4 and totals only from Q3; spreads made money in every
  # quarter. Positions already held count either way.
  bet_when    = list(game = 4:5, spread = 1:5, total = 3:5),
  paper       = TRUE)

live_dir <- file.path(data_dir, "live")
dir.create(live_dir, showWarnings = FALSE)
live_file <- function(what, date = Sys.Date()) file.path(live_dir, sprintf("%s_%s", format(date), what))

# ── Stoppages ────────────────────────────────────────────────────────────────
is_stoppage <- function(a) {
  ft_left <- a$actionType == "freethrow" & grepl("[0-9]+ of [0-9]+", a$subType) &
    suppressWarnings(as.integer(sub("^[^0-9]*([0-9]+) of.*$", "\\1", a$subType)) <
                     as.integer(sub("^.* of ([0-9]+).*$", "\\1", a$subType)))
  a$actionType %in% c("timeout", "instantreplay") | (!is.na(ft_left) & ft_left) |
    (a$actionType == "period" & a$subType == "end") |
    (a$actionType == "foul" & grepl("[0-9] FT\\)", a$description))
}

# what stopped play: the latest action that isn't a substitution or a note
admin_only <- c("substitution", "memo")
stop_what <- function(act) act[!actionType %in% admin_only][.N, actionType]

# ── The ledger: cash and positions for one game ──────────────────────────────
read_ledger <- function(date = Sys.Date()) {
  f <- live_file("ledger.csv", date)
  if (!file.exists(f)) return(data.table(time = character(), gid = character(), ticker = character(), side = character(),
                                         contracts = numeric(), price = numeric(), fee = numeric(), cost = numeric()))
  fread(f, colClasses = list(character = "gid"))
}
game_book <- function(ledger, game_id, bankroll) {
  l <- ledger[gid == game_id]
  if (nrow(l) == 0) return(list(cash = bankroll, positions = data.table(ticker = character(), position = numeric())))
  by_t <- l[, .(yes = sum(contracts[side == "yes"]), no = sum(contracts[side == "no"])), by = ticker]
  # a yes and a no on the same market close each other out for $1
  list(cash = bankroll - sum(l$cost) + sum(pmin(by_t$yes, by_t$no)),
       positions = by_t[, .(ticker, position = yes - no)])
}

# ── One decision ─────────────────────────────────────────────────────────────
# su: the game's setup; act: its play-by-play so far; quotes: kalshi_quotes() (or the
# same columns from saved minutes); book: game_book(). Returns what to print and log.
decide <- function(su, act, quotes, book, s = live_settings) {
  gm <- su$game; k <- nrow(act); now <- act[k]
  st <- state_at(su, act, k)
  d <- score_dist(su, st, nsim = s$nsim)
  q <- quotes[!is.na(x)]
  Y <- market_matrix(d, q)
  bet_kinds <- names(Filter(function(p) now$period %in% p, s$bet_when))
  ko <- if (length(bet_kinds) && book$cash >= 1) {
    # every market stays in Y so held positions count; only this quarter's kinds can be bought
    ok <- q$kind %in% bet_kinds
    kelly_orders(d$p, Y, q[, .(ticker, yes_ask = fifelse(ok, yes_ask, NA_real_), no_ask = fifelse(ok, no_ask, NA_real_),
                               yes_depth = yes_ask_size, no_depth = no_ask_size)],
                 positions = book$positions, bankroll = book$cash, gamma = s$gamma, min_edge = s$min_edge,
                 price_range = s$price_range, max_cost = s$max_order / book$cash)
  } else list(orders = data.table())
  o <- ko$orders
  if (nrow(o) && sum(o$cost) > book$cash) {
    o[, contracts := floor(contracts * book$cash / sum(cost) * 0.999)]
    o <- o[contracts > 0][, `:=`(fee = kfee(price, contracts), cost = round(price * contracts, 2) + kfee(price, contracts))]
  }
  if (nrow(o)) {    # contracts held before this decision: buying the other side sells them
    held <- book$positions$position[match(o$ticker, book$positions$ticker)]
    o[, held := fifelse(is.na(held), 0, held)]
  }
  p_mkt <- q[kind == "game" & yes_over == 1, mid][1]
  lines <- c(margin = implied_line(q[kind != "total" & yes_bid > 0 & yes_ask < 1, x],
                                   q[kind != "total" & yes_bid > 0 & yes_ask < 1, fifelse(yes_over == 1, mid, 1 - mid)]),
             total = implied_line(q[kind == "total" & yes_bid > 0 & yes_ask < 1, x],
                                  q[kind == "total" & yes_bid > 0 & yes_ask < 1, mid]))
  info <- data.table(gid = gm$gid, order = now$orderNumber, q = now$period, clock = sprintf("%d:%02d", now$tr %/% 60, now$tr %% 60),
                     away = gm$atm, home = gm$htm, a = now$scoreAway, h = now$scoreHome, what = stop_what(act), state = st$kind[1],
                     wp_model = sum(d$p[d$hf > d$af]), wp_market = p_mkt,
                     margin_model = sum(d$p * (d$hf - d$af)), margin_market = lines[["margin"]],
                     total_model = sum(d$p * (d$hf + d$af)), total_market = lines[["total"]],
                     cash = book$cash, n_orders = nrow(o), spend = if (nrow(o)) sum(o$cost) else 0)
  # the best sides on offer, bought or not, and why each wasn't: so a stoppage with no bets says why
  pm <- as.vector(crossprod(Y, d$p))
  near <- rbind(q[, .(ticker, kind, x, yes_over, side = "yes", price = yes_ask, p_win = pm)],
                q[, .(ticker, kind, x, yes_over, side = "no", price = no_ask, p_win = 1 - pm)])[!is.na(price) & price > 0 & price < 1]
  near[, edge := p_win - price - kalshi_fee * price * (1 - price)]
  near[, why := fcase(!kind %in% bet_kinds, sprintf("%s not bet in this quarter", kind),
                      price < s$price_range[1] | price > s$price_range[2], "price outside the range",
                      edge <= s$min_edge, sprintf("edge under %.0f¢", 100 * s$min_edge),
                      ticker %in% o$ticker, "", default = "Kelly sized it to nothing")]
  near[, bet := bet_label(kind, x, yes_over, side, gm$htm, gm$atm)]
  near <- head(near[order(-edge)][!duplicated(bet)], 8)     # yes on one team = no on the other: keep the better
  list(info = info, orders = o, model = data.table(ticker = colnames(Y), p_yes = pm), near = near)
}

show_decision <- function(r, secs) {
  i <- r$info
  cat(sprintf("\n%s  %s %d at %s %d  Q%d %s  %s (%s)   [%.1f s%s]\n", format(Sys.time(), "%H:%M:%S"), i$away, i$a, i$home, i$h,
              i$q, i$clock, i$what, i$state, secs, if (!is.null(i$seen_after)) sprintf(", seen %.0f s after", i$seen_after) else ""))
  cat(sprintf("  %s wins: model %.1f%%, Kalshi %.1f%%   margin: model %+.1f, Kalshi %+.1f   total: model %.1f, Kalshi %.1f\n",
              i$home, 100 * i$wp_model, 100 * i$wp_market, i$margin_model, i$margin_market, i$total_model, i$total_market))
  if (nrow(r$orders) == 0) { cat(sprintf("  no bets (cash $%.0f)\n", i$cash)); return(invisible()) }
  o <- r$orders
  for (j in seq_len(nrow(o))) {
    # buying no against held yes is selling yes at 1 - price (and the reverse): say so
    sells <- (o$side[j] == "no" && o$held[j] > 0) || (o$side[j] == "yes" && o$held[j] < 0)
    if (sells) {
      n_sell <- min(o$contracts[j], abs(o$held[j]))   # the rest, if any, is a new position on this side
      cat(sprintf("  SELL %-3s %3d x %-34s @ %.2f   (buy %s @ %.2f)   of %d held\n",
                  toupper(if (o$side[j] == "no") "yes" else "no"), n_sell, o$ticker[j], 1 - o$price[j],
                  o$side[j], o$price[j], abs(o$held[j])))
      if (o$contracts[j] > n_sell)
        cat(sprintf("  BUY  %-3s %3d x %-34s @ %.2f   model %.1f%%  edge %+.1f\n", toupper(o$side[j]), o$contracts[j] - n_sell,
                    o$ticker[j], o$price[j], 100 * o$p_win[j], 100 * o$edge[j]))
      cat(sprintf("       ($%.2f for the %d)\n", o$cost[j], o$contracts[j]))
    } else {
      cat(sprintf("  BUY  %-3s %3d x %-34s @ %.2f   $%7.2f   model %.1f%%  edge %+.1f\n", toupper(o$side[j]), o$contracts[j],
                  o$ticker[j], o$price[j], o$cost[j], 100 * o$p_win[j], 100 * o$edge[j]))
    }
  }
  cat(sprintf("  total $%.2f of $%.0f cash\n", sum(o$cost), i$cash))
}

record <- function(r, s = live_settings, date = Sys.Date()) {
  fwrite(cbind(time = format(Sys.time()), r$info, orders = if (nrow(r$orders))
    paste(sprintf("%s %s %d@%.2f", r$orders$ticker, r$orders$side, r$orders$contracts, r$orders$price), collapse = "; ") else ""),
    live_file("log.csv", date), append = file.exists(live_file("log.csv", date)))
  if (s$paper && nrow(r$orders))
    fwrite(r$orders[, .(time = format(Sys.time()), gid = r$info$gid, ticker, side, contracts, price, fee, cost)],
           live_file("ledger.csv", date), append = file.exists(live_file("ledger.csv", date)))
}

# ── Setting up a game ────────────────────────────────────────────────────────
game_setup <- function(row, cache) {
  if (!is.null(cache[[row$gid]])) return(cache[[row$gid]])
  gm <- data.table(gid = row$gid, ssn = 2000L + as.integer(substr(row$gid, 4, 5)), date = row$date,
                   htm = row$htm, atm = row$atm, hscr = NA_real_, ascr = NA_real_, line = NA_real_, total = NA_real_)
  db <- tryCatch(sqlq(sprintf("SELECT line, total FROM gms WHERE gid = '%s'", row$gid)), error = function(e) NULL)
  if (!is.null(db) && nrow(db) && !is.na(db$total[1]) && db$total[1] > 150 && !(db$total[1] == 200.52 && db$line[1] == -3.25)) {
    gm[, `:=`(line = db$line[1], total = db$total[1])]; src <- "gms"
  } else {
    l <- kalshi_lines(gm, t = if (Sys.time() > row$tip_t) row$tip_t else NULL)
    gm[, `:=`(line = round(l[["line"]], 2), total = round(l[["total"]], 2))]; src <- "Kalshi before the tip"
  }
  if (is.na(gm$line) || is.na(gm$total)) stop("no line and total, from gms or Kalshi")   # retried on the next poll
  su <- setup_game(gm)
  su$line_source <- src
  message(sprintf("%s at %s: anchored to %s %+.1f, total %.1f (%s); ratings %s", gm$atm, gm$htm, gm$htm, gm$line, gm$total,
                  src, su$ratings))
  su
}

# ── The loop ─────────────────────────────────────────────────────────────────
run_live <- function(teams = NULL, s = live_settings) {
  cache_f <- live_file("games.rds")
  cache <- if (file.exists(cache_f)) readRDS(cache_f) else list()
  done <- character()
  repeat {
    tg <- tryCatch(todays_games(), error = function(e) data.table())
    if (length(teams)) tg <- tg[htm %in% teams | atm %in% teams]
    if (nrow(tg) == 0) { message("no games today"); return(invisible()) }
    for (i in which(tg$status %in% 1:2 & Sys.time() > tg$tip_t - 1800)) {   # set up half an hour before the tip
      g <- tg[i]
      if (is.null(cache[[g$gid]])) {
        su <- tryCatch(game_setup(g, cache), error = function(e) { message("setup failed for ", g$gid, ": ", conditionMessage(e)); NULL })
        if (!is.null(su)) { cache[[g$gid]] <- su; saveRDS(cache, cache_f) }
      }
    }
    for (i in which(tg$status == 2)) {
      g <- tg[i]; su <- cache[[g$gid]]
      if (is.null(su)) next
      act <- tryCatch(feed_actions(su), error = function(e) NULL)
      if (is.null(act) || nrow(act) == 0) next
      if (is.na(su$tip)) {
        p1 <- act[period == 1 & !is.na(possession)][1, possession]
        if (!is.na(p1)) { su$tip <- as.integer(p1 != act$home_id[1]); cache[[g$gid]] <- su; saveRDS(cache, cache_f) }
      }
      last <- act[!actionType %in% admin_only][.N]       # substitutions come during the stoppage
      key <- paste(g$gid, last$orderNumber)
      if (key %in% done || !is_stoppage(last) || is.na(su$tip)) next
      done <- c(done, key)
      t0 <- Sys.time()
      seen_after <- as.numeric(t0) - last$t
      if (!is.na(seen_after) && seen_after > s$max_seen) {
        cat(sprintf("\n%s  %s %d at %s %d  Q%d %s  %s seen %.0f s after it happened: skipped\n", format(t0, "%H:%M:%S"),
                    g$atm, last$scoreAway, g$htm, last$scoreHome, last$period, sprintf("%d:%02d", last$tr %/% 60, last$tr %% 60),
                    stop_what(act), seen_after))
        next
      }
      r <- tryCatch({
        q <- kalshi_quotes(su$game)
        decide(su, act, q, game_book(read_ledger(), g$gid, s$bankroll), s)
      }, error = function(e) { message("decision failed: ", conditionMessage(e)); NULL })
      if (is.null(r)) next
      # how late we saw the stoppage: the feed's delay plus up to one poll (the backtest assumed 20 s)
      r$info[, `:=`(t_action = last$t, seen_after = round(seen_after, 1))]
      show_decision(r, as.numeric(difftime(Sys.time(), t0, units = "secs")))
      record(r, s)
    }
    if (all(tg$status == 3)) { message("all games final"); return(invisible()) }
    Sys.sleep(if (any(tg$status == 2)) s$poll else 60)
  }
}

if (!interactive() && sys.nframe() == 0L) {
  teams <- toupper(commandArgs(trailingOnly = TRUE))
  run_live(if (length(teams)) teams else NULL)
}
