# possim/bot_lib.R - the betting bot's engine, without the page. bot.R runs it from a page;
# bot_test.R drives it directly against a simulated exchange. One bot bets one game.
#
# Each step reads the game's feed. At a new stoppage the bot works out Kelly's orders the way the
# console does (decide() in bet_live.R) and sends them itself: sells first, then buys, best edge
# first. Everything a person would have to click in the console is checked here instead, and
# anything unexpected stops the bot. The guardrails are listed in bot.R.
#
# What the bot touches goes through `io`, so a test can stand in for the NBA and Kalshi:
#   now()               the clock (Unix seconds; a replay's clock in a replay)
#   feed(gid, gm)       the game's actions so far, as feed_table() gives them
#   quotes(gm)          Kalshi's markets for the game, as kalshi_quotes() gives them
#   positions(events)   positions in the game's events, as kalshi_positions() gives them
#   order(body)         send one order, as kalshi_order() does
#   balance()           cash, as kalshi_balance() gives it
#   setup(row, t)       the game anchored to Kalshi's prices at time t (NULL: now), as game_setup() does
#   read_orders(), log_order(rec), event(type, detail)

bot_settings <- modifyList(live_settings, list(
  ceiling         = 1000,  # the most the page lets a bot spend on a game. Raise it here and nowhere else
  # the page starts with these; each can be changed there before launching
  bankroll        = 1000,  # Kelly sizes the game against this, less what the game has cost so far
  spend_cap       = 1000,  # the most the bot may spend on its game, net of sells (no more than ceiling)
  max_order       = 60,    # the most one order can cost, fee included (also Kelly's most per market per stoppage)
  max_stop        = 150,   # the most the bot spends at one stoppage
  max_loss        = 400,   # the bot stops if the game's positions, at Kalshi's mid prices, fall this far below where they were at launch
  # fixed
  max_contracts   = 1000,  # the most contracts in one order
  max_orders_stop = 12,    # the most orders at one stoppage
  max_orders_min  = 40,    # the most orders in any minute; past it, the rest wait for the next stoppage
  send_within     = 15,    # seconds after the orders are worked out that the last of them can go
  feed_stale      = 60,    # the page says paused when the feed hasn't been read for this long
  # as in the console: which quarters each kind of market is bet in, and when a stoppage can be bet
  bet_when        = list(game = 4:5, spread = 1:5, total = 3:5),
  valid_for       = 180,
  break_open      = c(quarter = 120, half = 840),
  poll            = 3))    # seconds between steps

# The limits a launch can set, checked against each other and the ceiling. "" when they're fine.
bot_check_limits <- function(l, s = bot_settings) {
  need <- c("spend_cap", "bankroll", "max_order", "max_stop", "max_loss")
  bad <- need[!vapply(need, function(n) is.numeric(l[[n]]) && length(l[[n]]) == 1 && !is.na(l[[n]]) && l[[n]] > 0, TRUE)]
  if (length(bad)) return(paste("Each limit needs a number above zero:", paste(bad, collapse = ", ")))
  if (l$spend_cap > s$ceiling) return(sprintf("The spend cap can't be more than $%s (ceiling in bot_settings).", format(s$ceiling)))
  if (l$bankroll > s$ceiling) return(sprintf("The bankroll can't be more than $%s (ceiling in bot_settings).", format(s$ceiling)))
  if (l$max_order > l$max_stop) return("The most per order can't be more than the most per stoppage.")
  if (l$max_stop > l$spend_cap) return("The most per stoppage can't be more than the spend cap.")
  if (l$max_loss > l$spend_cap) return("The loss stop can't be more than the spend cap.")
  ""
}

# A simulated exchange for paper and replay. It fills every order in full at its limit price (a
# best case: real orders often fill only in part or not at all), and keeps positions from the
# bot's own order log, in the shape kalshi_positions() gives them.
sim_order <- function(body) {
  n <- as.numeric(body$count); p_yes <- as.numeric(body$price)
  price <- if (body$side == "bid") p_yes else 1 - p_yes
  list(ok = TRUE, status = 201, order_id = paste0("sim-", body$client_order_id), filled = n, avg_price = price,
       fee = kfee(price, n), remaining = 0)
}
sim_positions <- function(o, gid, mode) {
  l <- o[o$mode == mode & o$gid == gid & o$filled > 0]
  if (nrow(l) == 0) return(data.table(ticker = character(), position = numeric(), cash_out = numeric()))
  l[, .(position = sum(fifelse(outcome == "yes", filled, -filled)), cash_out = sum(-cash)), by = ticker]
}

# Positions valued at Kalshi's mid (or the side they'd sell into, when the book is one-sided).
# NA when a held market has no price, so a missing quote can't look like a loss.
mark_value <- function(kp, q) {
  p <- kp[position != 0]
  if (nrow(p) == 0) return(0)
  p <- merge(p, q[, .(ticker, yes_bid, yes_ask, mid)], by = "ticker", all.x = TRUE)
  p[, v := fifelse(position > 0, position * fcoalesce(mid, yes_bid), -position * (1 - fcoalesce(mid, yes_ask)))]
  if (anyNA(p$v)) NA_real_ else sum(p$v)
}

# a dead ball: one set of orders for it, whatever the feed does to its order numbers. It changes
# only when the game state does (a shot, a free throw, a rebound...), and stays put through a
# timeout called at a foul, an edit of an earlier play, or a recheck
dead_ball <- function(act) sum(act$actionType %in% state_types)

bot_new <- function(io, s = bot_settings, mode, row, su) {
  b <- new.env()
  b$io <- io; b$s <- s; b$mode <- mode; b$live <- mode == "live"
  b$row <- row; b$gid <- row$gid; b$su <- su
  b$running <- FALSE; b$reason <- "not launched"; b$status <- "Not launched."
  b$done <- character(); b$sent_t <- numeric(); b$first_ok <- !b$live
  b$feed_ok <- io$now(); b$tip_real <- NULL; b$reanchor_t <- -Inf
  b$events <- data.table(time = character(), type = character(), detail = character())
  b$last <- NULL; b$act <- NULL; b$q <- NULL; b$kp <- NULL
  b$pnl <- NA_real_; b$spent <- 0; b$n_sent <- 0; b$n_filled <- 0
  b
}

bot_event <- function(b, type, detail) {
  b$events <- rbind(b$events, data.table(time = format(as.POSIXct(b$io$now(), origin = "1970-01-01"), "%H:%M:%S"), type = type, detail = detail))
  if (nrow(b$events) > 300) b$events <- tail(b$events, 300)
  b$io$event(type, detail)
  invisible()
}

bot_events_of <- function(su) paste0(kalshi_series, "-", kalshi_code(su$game))

# Positions the bot expects: those at launch plus every fill it has had since
bot_expected <- function(b) {
  o <- b$io$read_orders()
  o <- o[o$mode == b$mode & o$gid == b$gid & o$filled > 0 & o$order_id %in% b$order_ids]
  e <- rbind(b$base_pos[, .(ticker, position)],
             if (nrow(o)) o[, .(position = sum(fifelse(outcome == "yes", filled, -filled))), by = ticker] else NULL)
  e[, .(position = sum(position)), by = ticker]
}
bot_mismatch <- function(b, kp) {
  e <- bot_expected(b)
  m <- merge(e, kp[, .(ticker, actual = position)], by = "ticker", all = TRUE)
  m[is.na(position), position := 0][is.na(actual), actual := 0]
  m[abs(position - actual) > 1e-6]
}

# Launch, once the page has checked the limits and the person has confirmed. "" when it started.
bot_launch <- function(b, l) {
  io <- b$io; s <- b$s
  why <- bot_check_limits(l, s)
  if (why != "") return(why)
  if (file.exists(bot_stop_file)) return(sprintf("The STOP_BOT file is there (%s). Delete it to launch.", bot_stop_file))
  bl <- bot_lock_read()
  if (!is.null(bl)) return(sprintf("Another bot is running (%s mode, game %s, since %s). Stop it first.", bl$mode, bl$gid, bl$since))
  q <- tryCatch(io$quotes(b$su$game), error = function(e) e)
  if (inherits(q, "error") || is.null(q) || nrow(q) == 0) return("Couldn't read Kalshi's prices for this game.")
  kp <- tryCatch(io$positions(bot_events_of(b$su)), error = function(e) e)
  if (inherits(kp, "error")) return(paste("Couldn't read positions from Kalshi:", conditionMessage(kp)))
  if (b$live) {
    bal <- tryCatch(io$balance(), error = function(e) e)
    if (inherits(bal, "error")) return(paste("Couldn't read the balance from Kalshi:", conditionMessage(bal)))
    have <- shard_cash(bal, nba_shard)
    if (have < l$max_stop) return(sprintf("The NBA shard has $%.2f, less than one stoppage's limit ($%s). Fund it from the console first.", have, format(l$max_stop)))
  }
  v0 <- mark_value(kp, q)
  b$l <- l
  b$base_pos <- kp[, .(ticker, position)]
  b$base_cost <- sum(kp$cash_out)
  b$pnl0 <- if (is.na(v0)) 0 else v0 - b$base_cost
  b$order_ids <- character()
  b$q <- q; b$kp <- kp
  b$launched_t <- io$now(); b$feed_ok <- io$now()
  b$uid_launch <- substr(gsub("-", "", uuid::UUIDgenerate()), 1, 6)
  bot_lock_write(b$gid, b$mode)
  b$running <- TRUE; b$reason <- ""; b$status <- "Running: waiting for a stoppage."
  bot_event(b, "launch", sprintf("%s at %s: spend cap $%s, bankroll $%s, $%s an order, $%s a stoppage, stop at -$%s. Held at launch: %d markets, cost $%.2f.",
                                 b$su$game$atm, b$su$game$htm, format(l$spend_cap), format(l$bankroll), format(l$max_order),
                                 format(l$max_stop), format(l$max_loss), nrow(kp[position != 0]), b$base_cost))
  ""
}

bot_stop <- function(b, reason) {
  if (!isTRUE(b$running)) return(invisible())
  b$running <- FALSE; b$reason <- reason; b$status <- paste("Stopped:", reason)
  bot_lock_clear()
  bot_event(b, "stop", reason)
  invisible()
}

# One step: read the feed, and at a new stoppage, bet it
bot_step <- function(b) {
  if (!isTRUE(b$running)) return(invisible())
  io <- b$io; s <- b$s; now <- io$now()
  if (file.exists(bot_stop_file)) return(bot_stop(b, "the STOP_BOT file appeared"))
  act <- tryCatch(io$feed(b$gid, b$su$game), error = function(e) NULL)
  if (is.null(act) || nrow(act) == 0) {
    b$status <- if (now - b$feed_ok > s$feed_stale) sprintf("Paused: the feed hasn't been read for %.0f s.", now - b$feed_ok)
                else "Running: waiting for the feed."
    return(invisible())
  }
  b$feed_ok <- now; b$act <- act
  # the real tip, and a game anchored to Kalshi before it anchored again to the prices just before it
  if (is.null(b$tip_real)) {
    s0 <- act[actionType == "period" & subType == "start" & period == 1 & !is.na(t)]
    if (nrow(s0)) b$tip_real <- s0$t[1]
  }
  if (!is.null(b$tip_real) && grepl("^Kalshi", b$su$line_source %||% "") && (b$su$setup_t %||% 0) < b$tip_real) {
    if (now - b$reanchor_t < 60) { b$status <- "Paused: anchoring the model again at the tip."; return(invisible()) }
    b$reanchor_t <- now
    old <- b$su
    su <- tryCatch(io$setup(b$row, b$tip_real - 30), error = function(e) e)
    if (inherits(su, "error")) {
      bot_event(b, "anchor", paste("Anchoring at the tip failed; no bets until it works (tried again in a minute):", conditionMessage(su)))
      return(invisible())
    }
    su$tip <- old$tip; su$setup_t <- now
    b$su <- su
    bot_event(b, "anchor", sprintf("Anchored again at the tip: %s %+.1f, total %.1f (was %+.1f, %.1f).",
                                   su$game$htm, su$game$line, su$game$total, old$game$line, old$game$total))
  }
  if (is.na(b$su$tip)) {                     # who won the tip: the first possession
    p1 <- act[period == 1 & !is.na(possession)][1, possession]
    if (!is.na(p1)) b$su$tip <- as.integer(p1 != act$home_id[1])
  }
  last <- act[!actionType %in% admin_only][.N]
  b$last <- last
  if (game_over(last)) return(bot_stop(b, "the game is over"))
  if (!is_stoppage(last)) { b$status <- "Running: play on."; return(invisible()) }
  key <- sprintf("%s-%d", b$gid, dead_ball(act))
  if (key %in% b$done) return(invisible())
  b$done <- c(b$done, key)
  bot_stoppage(b, act, last, key, now)
}

bot_stoppage <- function(b, act, last, key, now) {
  io <- b$io; s <- b$s; l <- b$l
  gm <- b$su$game
  where <- sprintf("%s %d:%02d %s", per_name(last$period), last$tr %/% 60, last$tr %% 60, stop_what(act))
  skip <- function(why) { bot_event(b, "skip", sprintf("%s: %s", where, why)); b$status <- paste("Running. Last stoppage skipped:", why); invisible() }
  w <- stop_window(last, now, s)
  if (w$late) return(skip(sprintf("reached the feed %.0f s after it happened", w$seen_after)))
  if (is.na(b$su$tip)) return(skip("don't know yet who won the tip"))
  q <- tryCatch(io$quotes(gm), error = function(e) NULL)
  if (is.null(q) || nrow(q) == 0) return(skip("couldn't read Kalshi's prices"))
  b$q <- q
  kp <- tryCatch(io$positions(bot_events_of(b$su)), error = function(e) e)
  if (inherits(kp, "error")) {
    b$pos_fail <- (b$pos_fail %||% 0) + 1
    if (b$pos_fail >= 3) return(bot_stop(b, "couldn't read positions from Kalshi three stoppages running"))
    return(skip(paste("couldn't read positions:", conditionMessage(kp))))
  }
  b$pos_fail <- 0; b$kp <- kp
  # Kalshi's positions must be what the bot expects: those at launch plus its own fills
  mm <- bot_mismatch(b, kp)
  if (nrow(mm)) return(bot_stop(b, sprintf("positions on Kalshi don't match what the bot sent (%s: expected %g, Kalshi has %g). Someone else may have traded this game.",
                                           mm$ticker[1], mm$position[1], mm$actual[1])))
  game_cost <- sum(kp$cash_out)
  b$spent <- max(game_cost - b$base_cost, bot_net_spent(b))
  v <- mark_value(kp, q)
  if (!is.na(v)) {
    b$pnl <- v - game_cost - b$pnl0
    if (-b$pnl > l$max_loss) return(bot_stop(b, sprintf("the loss stop: down $%.2f since launch at Kalshi's mids (limit $%s)", -b$pnl, format(l$max_loss))))
  }
  book <- list(cash = l$bankroll - game_cost, positions = kp[position != 0, .(ticker, position)])
  if (book$cash < 1) return(skip(sprintf("no Kelly cash left (bankroll $%s, game cost $%.2f)", format(l$bankroll), game_cost)))
  s2 <- modifyList(s, list(max_order = l$max_order))
  t0 <- io$now()
  r <- tryCatch(decide(b$su, act, q, book, s2), error = function(e) e)
  if (inherits(r, "error")) return(bot_stop(b, paste("working out the orders failed:", conditionMessage(r))))
  rows <- order_rows(r$orders, q, gm$htm, gm$atm)
  if (nrow(rows) == 0) return(skip("no bets"))
  rows <- rows[order(action != "SELL", -edge)]          # sells first, then the best edges
  uid <- substr(gsub("-", "", uuid::UUIDgenerate()), 1, 8)
  spent_stop <- 0; n_stop <- 0; did <- character()
  for (j in seq_len(nrow(rows))) {
    rr <- rows[j]
    if (!isTRUE(b$running)) break
    if (file.exists(bot_stop_file)) return(bot_stop(b, "the STOP_BOT file appeared"))
    if (io$now() - t0 > s$send_within) { did <- c(did, sprintf("%d orders left unsent: past %d s", nrow(rows) - j + 1, s$send_within)); break }
    if (n_stop >= s$max_orders_stop) { did <- c(did, "the per-stoppage order limit"); break }
    if (sum(b$sent_t > io$now() - 60) >= s$max_orders_min) { did <- c(did, "the per-minute order limit"); break }
    n <- if (rr$action == "SELL") rr$n else {
      budget <- min(l$max_order, l$max_stop - spent_stop, l$spend_cap - b$spent)
      min(fit_count(rr$n, rr$price, budget), s$max_contracts)
    }
    if (!b$first_ok) n <- min(n, 1)                     # live: the first order is 1 contract, then checked on Kalshi
    if (n < 1) next
    cid <- sprintf("possimbot-%s-%.0f-%s-%d", b$gid, last$orderNumber, uid, rr$row)
    res <- tryCatch(io$order(order_body(rr$ticker, rr$outcome, n, rr$price, cid, reduce_only = rr$action == "SELL")),
                    error = function(e) list(ok = FALSE, status = NA, error = conditionMessage(e)))
    b$sent_t <- c(tail(b$sent_t, 200), io$now()); n_stop <- n_stop + 1; b$n_sent <- b$n_sent + 1
    rec <- data.table(time = format(as.POSIXct(io$now(), origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S"), mode = b$mode, gid = b$gid,
                      stop = key, ticker = rr$ticker, bet = rr$bet, action = rr$action, outcome = rr$outcome, count = n,
                      limit = rr$price, filled = 0, avg_price = NA_real_, fee = 0, cash = 0, status = "", order_id = "", note = "bot",
                      p_model = round(rr$p_win, 4), edge = round(rr$edge, 4), q = last$period, clock = r$info$clock,
                      t_stop = format(as.POSIXct(last$t, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S"),
                      seen_after = round(w$seen_after, 1), click_after = round(io$now() - t0, 1), checks = 1)
    if (!isTRUE(res$ok)) {
      rec[, `:=`(status = "error", note = paste("bot:", res$status, res$error))]
      io$log_order(rec)
      return(bot_stop(b, paste("an order failed:", res$status, res$error)))
    }
    rec[, `:=`(filled = res$filled, avg_price = res$avg_price, fee = res$fee, order_id = res$order_id,
               status = if (res$filled == 0) "nothing filled" else if (res$filled < n) "part filled" else "filled")]
    rec[filled > 0, cash := if (rr$action == "BUY") -(filled * avg_price + fee) else filled * (1 - avg_price) - fee]
    io$log_order(rec)
    b$order_ids <- c(b$order_ids, res$order_id)
    if (rec$filled > 0) {
      b$n_filled <- b$n_filled + 1
      spent_stop <- spent_stop - rec$cash; b$spent <- b$spent - rec$cash
      # a sell's avg_price is for the side bought to close it; the held side sold at 1 minus that
      did <- c(did, sprintf("%s %g %s @ %.0f¢", if (rr$action == "BUY") "bought" else "sold", rec$filled, rr$bet,
                            100 * if (rr$action == "BUY") rec$avg_price else 1 - rec$avg_price))
    } else did <- c(did, sprintf("%s %s: nothing filled", tolower(rr$action), rr$bet))
    # the first filled live order: Kalshi's positions must show it before anything else goes
    if (!b$first_ok && rec$filled > 0) {
      kp2 <- tryCatch(io$positions(bot_events_of(b$su)), error = function(e) e)
      if (inherits(kp2, "error")) return(bot_stop(b, "couldn't read positions to check the first order"))
      mm <- bot_mismatch(b, kp2)
      if (nrow(mm)) return(bot_stop(b, sprintf("the first order didn't show on Kalshi as expected (%s: expected %g, Kalshi has %g)",
                                               mm$ticker[1], mm$position[1], mm$actual[1])))
      b$first_ok <- TRUE
      bot_event(b, "check", "The first order shows on Kalshi as expected; orders now go at full size.")
    }
  }
  bot_event(b, "bet", sprintf("%s: %s", where, if (length(did)) paste(did, collapse = "; ") else "nothing sent (limits)"))
  b$status <- sprintf("Running. Last stoppage: %s, %d orders sent.", where, n_stop)
  invisible()
}

# what the bot's own orders since launch cost, net of sells
bot_net_spent <- function(b) {
  o <- b$io$read_orders()
  o <- o[o$mode == b$mode & o$gid == b$gid & o$order_id %in% b$order_ids]
  -sum(o$cash, na.rm = TRUE)
}
