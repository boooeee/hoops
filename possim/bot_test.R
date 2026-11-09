# possim/bot_test.R - the bot's guardrails, checked against a replayed game
#
#   Rscript bot_test.R [game id]        (default 0022600174: needs its saved Kalshi minutes)
#
# Drives bot_lib.R the way bot.R does, in live mode, but against a stand-in for Kalshi that
# keeps its own positions and can refuse or lose an order. Nothing is sent anywhere: the order
# log, the lock and the STOP_BOT file are temporary files. Prints PASS or FAIL for each check.

here <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "bet_live.R"))
source(file.path(here, "kalshi_trade.R"))
run_mode <- "replay"
kalshi_dir <- path.expand(Sys.getenv("KALSHI_DIR", "~/Dropbox/Nba/kalshi"))
orders_file <- tempfile("bot_test_orders_", fileext = ".csv")
source(file.path(here, "console_lib.R"))
source(file.path(here, "bot_lib.R"))
bot_lock_file <- tempfile("bot_test_lock_")             # never the real lock or STOP_BOT file
bot_stop_file <- tempfile("bot_test_stop_")

gid <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(gid)) gid <- "0022600174"
rp <- replay_load(gid)[[1]]
su0 <- setup_game(rp$gd$game, rp$gd$tip); su0$line_source <- "gms"; su0$setup_t <- 0
row <- data.table(gid = gid, date = rp$gd$game$date, htm = rp$gd$game$htm, atm = rp$gd$game$atm, tip_t = rp$act$t[1], status = 2L)
s <- modifyList(bot_settings, list(nsim = 10000))
lim <- list(spend_cap = 1000, bankroll = 1000, max_order = 60, max_stop = 150, max_loss = 400)
cat(sprintf("%s at %s, %s; %d actions\n\n", row$atm, row$htm, format(row$date), nrow(rp$act)))

fails <- 0
check <- function(ok, what) { cat(sprintf("%s  %s\n", if (isTRUE(ok)) "PASS" else "FAIL", what)); if (!isTRUE(ok)) fails <<- fails + 1 }

# a stand-in for Kalshi: fills at the limit, keeps its own positions; refuses order fail_at, and
# fills order drop_at without recording it (as if someone else's trade moved the position)
exchange <- function(fail_at = NA, drop_at = NA) {
  ex <- new.env(); ex$n <- 0; ex$bodies <- list()
  ex$pos <- data.table(ticker = character(), position = numeric(), cash_out = numeric())
  ex$order <- function(body) {
    ex$n <- ex$n + 1; ex$bodies[[ex$n]] <- body
    if (identical(ex$n, as.numeric(fail_at))) return(list(ok = FALSE, status = 400, error = "test: refused"))
    n <- as.numeric(body$count); p_yes <- as.numeric(body$price); bid <- body$side == "bid"
    held <- ex$pos[ticker == body$ticker, sum(position)]
    if (isTRUE(body$reduce_only)) n <- min(n, if (bid) max(-held, 0) else max(held, 0))
    price <- if (bid) p_yes else 1 - p_yes; fee <- kfee(price, n)
    if (n > 0 && !identical(ex$n, as.numeric(drop_at))) {
      dpos <- if (bid) n else -n
      dcash <- if (isTRUE(body$reduce_only)) -(n * (1 - price) - fee) else n * price + fee
      ex$pos <- rbind(ex$pos, data.table(ticker = body$ticker, position = dpos, cash_out = dcash))[, .(position = sum(position), cash_out = sum(cash_out)), by = ticker]
    }
    list(ok = TRUE, status = 201, order_id = sprintf("ex-%d", ex$n), filled = n, avg_price = price, fee = fee, remaining = 0)
  }
  ex
}

# one run: the bot in live mode, stepped every 3 s of the replay's clock
run_bot <- function(l = lim, ex = exchange(), from = rp$act$t[1] - 60, to = max(rp$act$t) + 60, feed_lag = 0, feed_down = NULL,
                    on_step = NULL, settings = s) {
  unlink(orders_file)
  clk <- new.env(); clk$t <- from
  io <- list(now = function() clk$t,
             feed = function(gid, gm) {
               if (!is.null(feed_down) && clk$t >= feed_down[1] && clk$t < feed_down[2]) stop("feed down")
               rp$act[t <= clk$t - feed_lag]
             },
             quotes = function(gm) replay_quotes(rp$qm, clk$t),
             positions = function(events) copy(ex$pos),
             order = ex$order,
             balance = function() list(cash = 5000, shards = c(`3` = 5000)),
             setup = function(row, t) su0,
             read_orders = read_orders, log_order = log_order, event = function(type, detail) NULL)
  su <- su0; su$tip <- rp$gd$tip
  b <- bot_new(io, settings, "live", row, su)
  msg <- bot_launch(b, l)
  if (msg != "") return(list(b = b, msg = msg))
  for (tt in seq(from, to, by = 3)) {
    clk$t <- tt
    if (!is.null(on_step)) on_step(b, tt)
    bot_step(b)
    if (!b$running) break
  }
  running_at_end <- b$running
  if (b$running) bot_stop(b, "the test run is over")
  list(b = b, ex = ex, o = read_orders(), msg = msg, t_end = clk$t, running_at_end = running_at_end)
}
kind_of <- function(tk) fcase(grepl("^KXNBAGAME", tk), "game", grepl("^KXNBASPREAD", tk), "spread", default = "total")

# ── A: a whole game at the defaults ──
t0 <- Sys.time()
A <- run_bot()
o <- A$o; buys <- o[action == "BUY" & filled > 0]
cat(sprintf("A: whole game: %d orders (%d filled), net $%.2f, %d stoppages bet, stopped: %s  [%.0f s]\n",
            nrow(o), sum(o$filled > 0), -sum(o$cash), uniqueN(o$stop), A$b$reason, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
check(A$msg == "", "launches")
check(nrow(o) > 0, "sends orders")
check(o$count[1] == 1, "the first live order is 1 contract")
check(isTRUE(A$b$first_ok), "then checks it on Kalshi and goes on")
check(any(o$count[-1] > 1), "later orders go at full size")
check(-sum(o$cash) <= lim$spend_cap + 1e-6, sprintf("net spend within the cap ($%.2f)", -sum(o$cash)))
check(all(buys$filled * buys$avg_price + buys$fee <= lim$max_order + 1e-6), "every buy within the per-order limit")
check(all(o[, -sum(cash), by = stop]$V1 <= lim$max_stop + 1e-6), "every stoppage within the per-stoppage limit")
check(all(o[, .N, by = stop]$N <= s$max_orders_stop), "no stoppage over 12 orders")
check(!any(buys$q <= 2 & kind_of(buys$ticker) == "total"), "no totals bought in the first half")
check(!any(buys$q <= 3 & kind_of(buys$ticker) == "game"), "no win markets bought before Q4")
bodies <- rbindlist(lapply(A$ex$bodies, function(x) as.data.table(x[c("ticker", "side", "time_in_force", "client_order_id")])))
check(all(bodies$time_in_force == "immediate_or_cancel"), "every order immediate-or-cancel")
check(!anyDuplicated(bodies$client_order_id), "every order id different")
check(all(vapply(A$ex$bodies, function(x) isTRUE(x$reduce_only), TRUE) == (o$action == "SELL")), "sells, and only sells, reduce-only")
check(A$b$reason == "the game is over", "stops when the game ends")
check(is.null(bot_lock_read()), "lets go of the lock when it stops")
ex_pos <- A$ex$pos[position != 0]; log_pos <- sim_positions(o[, mode := "live"], gid, "live")[position != 0]
check(isTRUE(all.equal(ex_pos[order(ticker), position], log_pos[order(ticker), position])), "its log matches the exchange's positions")

# ── B: a small cap ──
B <- run_bot(l = modifyList(lim, list(spend_cap = 100, max_stop = 40, max_order = 20, max_loss = 100)))
cat(sprintf("\nB: cap $100: net $%.2f, %d orders, stopped: %s\n", -sum(B$o$cash), nrow(B$o), B$b$reason))
check(-sum(B$o$cash) <= 100 + 1e-6, "never spends past a $100 cap")
check(all(B$o[, -sum(cash), by = stop]$V1 <= 40 + 1e-6), "keeps to a $40 stoppage limit")

# ── C: Kalshi refuses an order ──
C <- run_bot(ex = exchange(fail_at = 3))
cat(sprintf("\nC: third order refused: %d orders logged, stopped: %s\n", nrow(C$o), C$b$reason))
check(grepl("^an order failed", C$b$reason), "stops on a failed order")
check(nrow(C$o) == 3 && C$o$status[3] == "error", "sends nothing after it")
check(is.null(bot_lock_read()), "and lets go of the lock")

# ── D: a fill that doesn't show on Kalshi (someone else trading the game) ──
D <- run_bot(ex = exchange(drop_at = 4))
cat(sprintf("\nD: fourth fill lost: stopped: %s\n", D$b$reason))
check(grepl("don't match", D$b$reason), "stops when Kalshi's positions don't match what it sent")
D1 <- run_bot(ex = exchange(drop_at = 1))
check(grepl("first order didn't show", D1$b$reason) && nrow(D1$o) == 1, "stops right after a first order that doesn't show")

# ── E: the STOP_BOT file ──
n_at <- NA
E <- run_bot(on_step = function(b, tt) if (b$n_sent >= 5 && !file.exists(bot_stop_file)) { n_at <<- b$n_sent; file.create(bot_stop_file) })
cat(sprintf("\nE: STOP_BOT made after %s orders: %d orders in all, stopped: %s\n", n_at, nrow(E$o), E$b$reason))
check(grepl("STOP_BOT", E$b$reason) && nrow(E$o) == n_at, "stops at the next step, sending nothing more")
unlink(bot_stop_file)

# ── F: the loss stop ──
F <- run_bot(l = modifyList(lim, list(max_loss = 3)))
cat(sprintf("\nF: stop at -$3: stopped: %s\n", F$b$reason))
check(grepl("loss stop", F$b$reason), "stops when down past the loss stop")

# ── G: late and missing feeds ──
G <- run_bot(feed_lag = 75)
ev <- G$b$events
bet_at <- ev[type == "bet", sub("^\\S+ \\S+ (\\S+):.*$", "\\1", detail)]     # "Q2 5:31 timeout: ..." -> timeout
cat(sprintf("\nG: feed 75 s behind: bets at %d stoppages (%s), %d skipped as late\n", length(bet_at),
            paste(unique(bet_at), collapse = ", "), ev[type == "skip" & grepl("reached the feed", detail), .N]))
check(length(bet_at) > 0 && all(grepl("timeout|period", bet_at)) && ev[type == "skip" & grepl("reached the feed", detail), .N] > 0,
      "with the feed 75 s behind, bets only timeouts and breaks, and skips the rest as late")
tdown <- rp$act[period == 3][1, t]
G2 <- run_bot(feed_down = c(tdown, tdown + 600), to = tdown + 2400)
tt_o <- as.numeric(as.POSIXct(G2$o$time))             # the log's times are local
cat(sprintf("G2: feed down 10 minutes from the start of Q3: %d orders before, %d during, %d after\n",
            sum(tt_o < tdown), sum(tt_o >= tdown & tt_o < tdown + 600), sum(tt_o >= tdown + 600)))
check(!any(tt_o >= tdown & tt_o < tdown + 600) && G2$running_at_end && any(tt_o >= tdown + 600),
      "sends nothing while the feed can't be read, and carries on after")

# ── H: launching ──
check(bot_check_limits(modifyList(lim, list(spend_cap = 5000))) != "", "refuses a cap over the ceiling")
check(bot_check_limits(modifyList(lim, list(max_order = 200))) != "", "refuses a per-order limit over the per-stoppage one")
check(bot_check_limits(modifyList(lim, list(max_loss = NA))) != "", "refuses a missing limit")
invisible(file.create(bot_stop_file))
H1 <- run_bot(to = rp$act$t[1])
check(grepl("STOP_BOT", H1$msg), "won't launch while STOP_BOT is there")
unlink(bot_stop_file)
bot_lock_write("0099999999", "live")
H2 <- run_bot(to = rp$act$t[1])
check(grepl("Another bot", H2$msg), "won't launch while another bot holds the lock")
unlink(bot_lock_file)
fwrite(data.table(pid = 999999, mode = "live", gid = "0099999999", since = "then"), bot_lock_file)
H3 <- run_bot(to = rp$act$t[1])
check(H3$msg == "", "a lock left by a dead process doesn't block a launch")

cat(sprintf("\n%s: %d failed\n", if (fails == 0) "ALL PASSED" else "FAILURES", fails))
unlink(c(orders_file, bot_lock_file, bot_stop_file))
quit(status = if (fails == 0) 0 else 1)
