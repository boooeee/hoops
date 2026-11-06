# possim/bet_report.R - read a backtest from bet_backtest.R
#
#   Rscript bet_report.R backtest_2025-10-01_2026-06-30
#
# 1. Model against market: at every decision, each contract's model probability and
#    Kalshi's mid, scored against the result. Then the question that matters for betting:
#    when the model disagrees with the market, does the result move with the model?
#    (slope of result - mid on model - mid: 1 = the model's disagreement is all signal,
#    0 = all noise)
# 2. Expected margin and total: the model's against the line Kalshi's ladders imply,
#    and both against what happened.
# 3. Profit and loss by strategy, with 95% intervals from resampling games.

source(file.path(Sys.getenv("POSSIM_CODE", "~/hoops/possim"), "common.R"))
args <- commandArgs(trailingOnly = TRUE)
bt <- readRDS(rds(if (length(args)) args[1] else "backtest"))
kalshi_dir <- path.expand(Sys.getenv("KALSHI_DIR", "~/Dropbox/Nba/kalshi"))
set.seed(11)
boot <- function(d, stat, B = 1000) {   # resample games
  g <- unique(d$gid); idx <- split(seq_len(nrow(d)), d$gid)
  v <- replicate(B, { s <- sample(g, replace = TRUE); stat(d[unlist(idx[s], use.names = FALSE)]) })
  quantile(v, c(0.025, 0.975), na.rm = TRUE)
}
ci <- function(est, q, dp = 3) sprintf(paste0("%+.", dp, "f (%+.", dp, "f to %+.", dp, "f)"), est, q[1], q[2])

ct <- bt$contracts[!is.na(result) & bid > 0 & ask < 1]
ct[, y := as.integer(result == "yes")]
cat(sprintf("%d games, %d decisions, %d contract quotes with a two-sided market\n\n",
            nrow(bt$games), uniqueN(bt$contracts[, .(gid, dec)]), nrow(ct)))

# ── 1. Model against market ──────────────────────────────────────────────────
clip <- function(p) pmin(pmax(p, 0.005), 0.995)
ll <- function(p, y) -mean(y * log(clip(p)) + (1 - y) * log(1 - clip(p)))
cat("Model against Kalshi's mid, contract by contract (lower log loss / Brier is better)\n")
by_kind <- ct[, .(quotes = .N, games = uniqueN(gid),
                  ll_model = ll(p_model, y), ll_mid = ll(mid, y),
                  brier_model = mean((p_model - y)^2), brier_mid = mean((mid - y)^2)), keyby = kind]
print(by_kind[, lapply(.SD, function(v) if (is.numeric(v) && max(v) < 5) round(v, 4) else v)])
cat("\nLog loss by period: model minus mid (negative = the model is better)\n")
print(dcast(ct[, .(d = round(ll(p_model, y) - ll(mid, y), 4)), by = .(kind, period = pmin(q, 5))], period ~ kind, value.var = "d"))
if ("stale" %in% names(ct)) {
  cat("\nQuotes that last changed before the last play (stale), by period:\n")
  print(dcast(ct[, .(s = round(mean(stale, na.rm = TRUE), 3)), by = .(kind, period = pmin(q, 5))], period ~ kind, value.var = "s"))
}
cat("\nWhen the model disagrees with the mid, does the result follow? slope of (result - mid) on (model - mid)\n")
for (k in c("game", "spread", "total")) {
  d <- ct[kind == k]; if (nrow(d) < 50) next
  f <- function(z) { dz <- z$p_model - z$mid; sum(dz * (z$y - z$mid)) / sum(dz^2) }
  cat(sprintf("  %-6s %s\n", k, ci(f(d), boot(d, f, 500), 2)))
}
cat("\nBy size of the disagreement (model minus mid): what the market said, what the model said, what happened\n")
ct[, gap := cut(p_model - mid, c(-1, -0.15, -0.08, -0.04, -0.02, 0.02, 0.04, 0.08, 0.15, 1))]
print(ct[, .(n = .N, mid = round(mean(mid), 3), model = round(mean(p_model), 3), happened = round(mean(y), 3)), keyby = .(kind, gap)], nrows = 100)

# ── 2. Margin and total ──────────────────────────────────────────────────────
gmin <- fread(file.path(kalshi_dir, "game_minutes.csv"), colClasses = list(character = "gid"))
dec <- unique(bt$contracts[, .(gid, dec, q, tr, t_quote, e_margin, e_total)])
dec <- merge(dec, gmin[, .(gid, t_quote = t, margin_line, total_line)], by = c("gid", "t_quote"), all.x = TRUE)
dec <- merge(dec, bt$games[, .(gid, fm = hscr - ascr, ft = hscr + ascr)], by = "gid")
cat("\nExpected final margin and total at decisions: model against the line Kalshi's ladders imply\n")
print(dec[!is.na(total_line) & !is.na(margin_line), .(
  decisions = .N,
  total_model_minus_mkt = round(mean(e_total - total_line), 2),
  total_rmse_model = round(sqrt(mean((ft - e_total)^2)), 2), total_rmse_mkt = round(sqrt(mean((ft - total_line)^2)), 2),
  margin_model_minus_mkt = round(mean(e_margin - margin_line), 2),
  margin_rmse_model = round(sqrt(mean((fm - e_margin)^2)), 2), margin_rmse_mkt = round(sqrt(mean((fm - margin_line)^2)), 2)),
  keyby = .(period = pmin(q, 5))])

# ── 3. Profit and loss ───────────────────────────────────────────────────────
o <- bt$orders[!is.na(result)]
cat("\nProfit and loss by strategy ($1,000 a game; buying at the ask; 95% intervals from resampling games)\n")
for (s in names(bt$strategies)) {
  d <- o[strategy == s]
  if (nrow(d) == 0) { cat(sprintf("  %-15s no orders\n", s)); next }
  roi <- function(z) sum(z$pnl) / sum(z$cost)
  pg  <- function(z) sum(z$pnl) / nrow(bt$games)
  cat(sprintf("  %-15s %5d orders  staked $%9.0f  P&L $%8.0f  per game %s  ROI %s\n", s, nrow(d), sum(d$cost), sum(d$pnl),
              ci(pg(d), boot(d, pg), 2), ci(roi(d), boot(d, roi), 3)))
}
cat("\nBy market kind and price paid\n")
o[, price_bin := cut(price, c(0, 0.1, 0.3, 0.7, 0.9, 1), include.lowest = TRUE)]
print(o[, .(orders = .N, staked = round(sum(cost)), pnl = round(sum(pnl)), roi = round(sum(pnl) / sum(cost), 3),
            avg_edge = round(mean(edge), 3)), keyby = .(strategy, kind = sub("-.*", "", ticker), price_bin)], nrows = 200)
if ("stale" %in% names(bt$contracts)) {
  o <- merge(o, unique(bt$contracts[, .(gid, dec, ticker, stale)]), by = c("gid", "dec", "ticker"), all.x = TRUE)
  cat("\nBy whether the quote had changed since the last play\n")
  print(o[, .(orders = .N, staked = round(sum(cost)), pnl = round(sum(pnl)), roi = round(sum(pnl) / sum(cost), 3)),
          keyby = .(strategy, stale)], nrows = 100)
}
cat("\nBy period\n")
print(o[, .(orders = .N, staked = round(sum(cost)), pnl = round(sum(pnl)), roi = round(sum(pnl) / sum(cost), 3)),
        keyby = .(strategy, period = pmin(q, 5))], nrows = 100)
