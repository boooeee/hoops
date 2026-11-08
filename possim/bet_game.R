# possim/bet_game.R - one backtest game, stoppage by stoppage: what the model and Kalshi said,
# and what a strategy bought and sold
#
#   Rscript bet_game.R                        a random game
#   Rscript bet_game.R 0022500431             a game id
#   Rscript bet_game.R 2026-01-15 BOS         a team's game on a date
#   Rscript bet_game.R BOS 2026-01-15 strategy=quarter_kelly backtest=backtest_2025-10-21_2026-06-30
#
# Bets are written the way a sportsbook would: "OKC -7.5" pays if OKC wins by 8 or more,
# "HOU +7.5" if HOU wins or loses by 7 or fewer, "Over 225.5" on 226 points or more.
# Buying the other side of a bet already held sells it, shown as SELL.
# The backtest bets every kind of market all game. The live tool doesn't (bet_when in bet_live.R).

here <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
suppressMessages(source(file.path(here, "live.R")))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) { a <- grep(paste0("^", name, "="), args, value = TRUE); if (length(a)) sub(".*=", "", a[1]) else default }
strat <- opt("strategy", "quarter_2c")
bt <- readRDS(rds(opt("backtest", "backtest_2025-10-21_2026-06-30_hedge")))
pick <- args[!grepl("=", args)]

games <- bt$games
gi <- if (length(pick) == 0) {
  games[sample(.N, 1)]
} else if (any(grepl("^00[0-9]{8}$", pick))) {
  games[gid == grep("^00[0-9]{8}$", pick, value = TRUE)[1]]
} else {
  d <- as.Date(grep("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", pick, value = TRUE)[1])
  tm <- toupper(grep("^[A-Za-z]{3}$", pick, value = TRUE)[1])
  games[(is.na(d) | date == d) & (is.na(tm) | home == tm | away == tm)][order(date)][1]
}
if (nrow(gi) == 0 || is.na(gi$gid)) stop("no such game in the backtest")
if (!strat %in% names(bt$strategies)) stop("strategies: ", paste(names(bt$strategies), collapse = ", "))
H <- gi$home; A <- gi$away

ct <- bt$contracts[gid == gi$gid]
od <- bt$orders[gid == gi$gid & strategy == strat]
setorder(od, dec, ticker)
desc <- unique(ct[, .(ticker, kind, x, yes_over, result)])
od <- merge(od, desc[, .(ticker, kind, x, yes_over)], by = "ticker")[order(dec)]

# a side of a contract as a sportsbook bet
bet_name <- function(kind, x, yes_over, side) bet_label(kind, x, yes_over, side, H, A)
# the median Kalshi's ladder implies (where P(above x) crosses 50%), or the side of the ladder it's off
line_text <- function(x, p, f) {
  v <- implied_line(x, p)
  if (!is.na(v)) return(sprintf(f, v))
  if (length(x) == 0) return("–")
  a <- data.table(x, p)[, .(p = mean(p)), keyby = x]
  if (a$p[1] < 0.5) paste0("<", sprintf(f, a$x[1])) else if (a$p[.N] > 0.5) paste0(">", sprintf(f, a$x[.N])) else "–"
}
clock <- function(s) sprintf("%d:%02d", s %/% 60, round(s %% 60))
stop_name <- function(what, q) switch(what, timeout = "timeout", instantreplay = "review", foul = "foul",
  period = if (q == 2) "halftime" else sprintf("end of %s", if (q <= 4) paste0("Q", q) else paste0("OT", q - 4)), what)
qname <- function(q) if (q <= 4) paste0("Q", q) else paste0("OT", q - 4)

st <- bt$strategies[[strat]]
cat(sprintf("\n%s at %s, %s.  Pregame: %s %+.1f, total %.1f.  Strategy %s (gamma %g, min edge %.0f¢, prices %.0f–%.0f¢)\n",
            A, H, format(gi$date, "%b %d, %Y"), H, gi$line, gi$total, strat, st$gamma, 100 * st$min_edge,
            100 * st$range[1], 100 * st$range[2]))
cat(sprintf("Game id %s.  $%s to start.  The model and Kalshi columns are for %s.\n",
            gi$gid, format(bt$settings$bankroll, big.mark = ","), H))

pos <- list()        # ticker -> list(n = signed yes contracts, avg = average price paid for the side held)
cash <- bt$settings$bankroll
for (k in sort(unique(ct$dec))) {
  c1 <- ct[dec == k]; i <- c1[1]
  two <- c1[bid > 0 & ask < 1 & ask - bid <= 0.10]      # wide, stale strikes would bend the implied line
  wp_mkt <- c1[kind == "game" & yes_over == 1, mid][1]
  m_mkt <- line_text(two[kind != "total", x], two[kind != "total", fifelse(yes_over == 1, mid, 1 - mid)], "%+.1f")
  t_mkt <- line_text(two[kind == "total", x], two[kind == "total", mid], "%.1f")
  cat(sprintf("\n%s %s  %s %d  %s %d   %s (%s)\n", qname(i$q), clock(i$tr), A, i$a, H, i$h, stop_name(i$what, i$q), i$state))
  num <- function(v, f) if (is.na(v)) formatC("–", width = nchar(sprintf(f, 0))) else sprintf(f, v)
  pad <- function(v) formatC(v, width = 6)
  cat(sprintf("   win: model %2.0f%%  Kalshi %s    margin: model %+5.1f  Kalshi %s    total: model %5.1f  Kalshi %s\n",
              100 * i$wp, num(100 * wp_mkt, "%2.0f%%"), i$e_margin, pad(m_mkt), i$e_total, pad(t_mkt)))
  o <- od[dec == k]
  if (nrow(o) == 0) { cat("   no bets\n"); next }
  for (j in seq_len(nrow(o))) {
    r <- o[j]; tk <- r$ticker
    p <- if (is.null(pos[[tk]])) list(n = 0, avg = 0) else pos[[tk]]
    signed <- if (r$side == "yes") r$contracts else -r$contracts
    unit <- r$cost / r$contracts                           # price plus fee, per contract
    n_sell <- if (p$n * signed < 0) min(abs(p$n), abs(signed)) else 0
    if (n_sell > 0) {
      held_side <- if (p$n > 0) "yes" else "no"
      cat(sprintf("   SELL %4d  %-14s at %2.0f¢   bought at %2.0f¢\n", n_sell, bet_name(r$kind, r$x, r$yes_over, held_side),
                  100 * (1 - r$price), 100 * p$avg))
      cash <- cash - n_sell * unit + n_sell                 # the pair is worth $1 now
      p$n <- p$n + sign(signed) * n_sell
    }
    n_buy <- r$contracts - n_sell
    if (n_buy > 0) {
      cat(sprintf("   BUY  %4d  %-14s at %2.0f¢   model %2.0f%%  edge %+4.1f¢   $%.2f\n", n_buy,
                  bet_name(r$kind, r$x, r$yes_over, r$side), 100 * r$price, 100 * r$p_win, 100 * r$edge, n_buy * unit))
      p$avg <- if (p$n == 0) unit else (abs(p$n) * p$avg + n_buy * unit) / (abs(p$n) + n_buy)
      p$n <- p$n + sign(signed) * n_buy
      cash <- cash - n_buy * unit
    }
    pos[[tk]] <- p
  }
  held <- Filter(function(p) p$n != 0, pos)
  if (length(held)) {
    hd <- desc[match(names(held), ticker)]
    lab <- mapply(function(t, p) sprintf("%s ×%d", bet_name(hd[ticker == t, kind], hd[ticker == t, x], hd[ticker == t, yes_over],
                                                             if (p$n > 0) "yes" else "no"), abs(p$n)), names(held), held)
    cat(strwrap(sprintf("holding: %s.  Cash $%.0f", paste(lab, collapse = ", "), cash), width = 110, indent = 3, exdent = 12), sep = "\n")
  }
}

cat(sprintf("\nFinal: %s %d  %s %d   (margin %+d, total %d)\n", A, gi$ascr, H, gi$hscr, gi$hscr - gi$ascr, gi$hscr + gi$ascr))
if (nrow(od) == 0) { cat("No bets.\n"); quit(save = "no") }
# by bet: each order pays $1 a contract if its side won, so closed pairs net out
s <- od[, .(pnl = sum(pnl), net = sum(fifelse(side == "yes", contracts, -contracts)), first = side[1]), by = .(ticker, kind, x, yes_over)]
s[, held := fifelse(net > 0, "yes", fifelse(net < 0, "no", first))]      # a closed-out bet is named by its first side
s[, bet := bet_name(kind, x, yes_over, held)]
s[, won := desc$result[match(ticker, desc$ticker)] == held]
setorder(s, -pnl)
for (j in seq_len(nrow(s)))
  cat(sprintf("   %-14s %s   P&L %+8.2f\n", s$bet[j],
              if (s$net[j] == 0) "closed out     " else sprintf("%4d held, %-4s", abs(s$net[j]), if (s$won[j]) "won" else "lost"), s$pnl[j]))
cat(sprintf("Staked $%.2f, P&L $%+.2f (%+.1f%%) on %d orders at %d of %d stoppages.  This strategy averaged $%+.2f a game in the backtest.\n",
            sum(od$cost), sum(od$pnl), 100 * sum(od$pnl) / sum(od$cost), nrow(od), uniqueN(od$dec), uniqueN(ct$dec),
            bt$orders[strategy == strat, sum(pnl)] / nrow(bt$games)))
