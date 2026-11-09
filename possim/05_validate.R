# possim/05_validate.R - test the possession simulation on held-out seasons
#
# For every test game (anchored to its line and total, as in live use):
#   in-game   home win probability at the start of each possession (the exact
#             recursion, with the real tip winner), next to the current model's
#             wpg.wpl at the same play
#   pre-game  20,000 simulated games from the tip: win probability, expected
#             margin and total, and how spread out they are; also the expected
#             margin and total before anchoring
# Then scores both models against what happened. Writes validation.rds.
#
#   Rscript 05_validate.R            all test seasons
#   Rscript 05_validate.R 300        a random 300 test games (quicker)

here <- local({   # this script's folder, whether run with Rscript or source()d
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "04_engine.R"))
suppressMessages(library(parallel))
n_games <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)[1]))
set.seed(7)

eng <- load_engine()
if (is.null(eng$anchor_J)) stop("no anchoring Jacobian: run 04_engine.R first")
p  <- readRDS(rds("possessions"))[ssn %in% test_ssn]
rt <- readRDS(rds("ratings"))

gm <- unique(p[, .(gid, ssn, date, htm, atm, hscr, ascr, line, total, tip_home)])
tz <- rt[, .(gid, home, rat_ppp = ppp_exp, exp_spp = spp_exp, exp_tpar = tpar_exp)]
gm <- merge(gm, tz[home == 1, .(gid, h_rat = rat_ppp, h_spp = exp_spp, h_tpar = exp_tpar)], by = "gid")
gm <- merge(gm, tz[home == 0, .(gid, a_rat = rat_ppp, a_spp = exp_spp, a_tpar = exp_tpar)], by = "gid")
gm <- gm[!is.na(tip_home) & !is.na(h_rat) & !is.na(a_rat)]
if (!is.na(n_games)) gm <- gm[sample(.N, min(.N, n_games))]
message(nrow(gm), " test games")

# the current model's numbers at each possession's start row
cur <- sqlq(paste0("SELECT p.gid, p.posid, w.wpl FROM pos p JOIN wpg w ON p.gid = w.gid AND p.r = w.r ",
                   "WHERE p.gid IN ", in_list(gm$gid)))
cur <- unique(cur, by = c("gid", "posid"))

one_game <- function(i) {
  x <- gm[i]
  g <- game_inputs(eng, x$line, x$total,
                   home = list(rat_ppp = x$h_rat, exp_spp = x$h_spp, exp_tpar = x$h_tpar),
                   away = list(rat_ppp = x$a_rat, exp_spp = x$a_spp, exp_tpar = x$a_tpar))
  ps <- p[gid == x$gid, .(gid, posid, q, tr, m, side = 1L - home, st)]
  v <- wp_states(eng, g, tip = 1L - x$tip_home, ps, want_pts = TRUE)
  pg <- pregame(eng, g, nsim = 20000)
  an <- if (is.null(g$anchor)) list(shift = c(0, 0), steps = 0L, unanchored = c(NA, NA)) else g$anchor
  pg <- c(pg, shift_h = an$shift[[1]], shift_a = an$shift[[2]], anchor_steps = an$steps,
          raw_margin = an$unanchored[[1]], raw_total = an$unanchored[[2]])
  list(states = cbind(ps, v), game = cbind(x, as.data.table(pg)))
}

t0 <- Sys.time()
res <- mclapply(seq_len(nrow(gm)), one_game, mc.cores = detectCores())
failed <- vapply(res, inherits, logical(1), "try-error")
if (any(failed)) message(sum(failed), " games failed: ", conditionMessage(attr(res[[which(failed)[1]]], "condition")))
res <- res[!failed]
message("done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

st <- rbindlist(lapply(res, `[[`, "states"))
gv <- rbindlist(lapply(res, `[[`, "game"))
st <- merge(st, gv[, .(gid, hwin = as.integer(hscr > ascr))], by = "gid")
st <- merge(st, cur, by = c("gid", "posid"), all.x = TRUE)
saveRDS(list(states = st, games = gv), rds("validation"))

# ── Scores ───────────────────────────────────────────────────────────────────
clip <- function(x) pmin(pmax(x, 1e-4), 1 - 1e-4)
logloss <- function(pr, y) -mean(y * log(clip(pr)) + (1 - y) * log(1 - clip(pr)))
brier <- function(pr, y) mean((pr - y)^2)

st[, seg := fcase(q == 1, "Q1", q == 2, "Q2", q == 3, "Q3",
                  q == 4 & tr > 300, "Q4 12:00-5:00", q == 4 & tr > 120, "Q4 5:00-2:00",
                  q == 4, "Q4 last 2:00", default = "OT")]
both <- st[!is.na(wpl)]
cat("\nIn-game, at the start of each possession (", nrow(both), " possessions, ", uniqueN(both$gid), " games)\n", sep = "")
sc <- both[, .(n = .N, logloss_new = logloss(wp, hwin), logloss_cur = logloss(wpl, hwin),
               brier_new = brier(wp, hwin), brier_cur = brier(wpl, hwin)), by = seg]
sc <- rbind(sc, both[, .(seg = "All", n = .N, logloss_new = logloss(wp, hwin), logloss_cur = logloss(wpl, hwin),
                         brier_new = brier(wp, hwin), brier_cur = brier(wpl, hwin))])
print(sc[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 4) else v)])

cat("\nCalibration (all possession starts): predicted home win probability vs how often the home team won\n")
cal <- function(pr) both[, .(n = .N, predicted = mean(get(pr)), won = mean(hwin)),
                         by = .(bin = cut(get(pr), c(0, 0.05, 0.15, 0.3, 0.45, 0.55, 0.7, 0.85, 0.95, 1), include.lowest = TRUE))][order(bin)]
print(merge(cal("wp"), cal("wpl"), by = "bin", suffixes = c("_new", "_cur"))[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

cat("\nClose late games (Q4 last 5:00, within 6): calibration\n")
cl <- both[q == 4 & tr <= 300 & abs(m) <= 6]
print(cl[, .(n = .N, new = round(mean(wp), 3), cur = round(mean(wpl), 3), won = round(mean(hwin), 3)),
         by = .(bin = cut(wp, c(0, 0.15, 0.3, 0.45, 0.55, 0.7, 0.85, 1), include.lowest = TRUE))][order(bin)])

g2 <- gv[!is.na(line) & !is.na(total)]
g2[, `:=`(margin = hscr - ascr, tot = hscr + ascr)]
cat("\nPre-game (", nrow(g2), " games with a line and total)\n", sep = "")
cat(sprintf("  before anchoring: expected margin minus -line mean %+.2f (slope on -line %.3f); total minus market %+.2f (slope %.3f)\n",
            mean(g2$raw_margin + g2$line), coef(lm(raw_margin ~ I(-line), g2))[2],
            mean(g2$raw_total - g2$total), coef(lm(raw_total ~ total, g2))[2]))
cat(sprintf("  anchoring shifts (market points per possession): home mean %+.4f sd %.4f, away mean %+.4f sd %.4f; steps %.1f\n",
            mean(g2$shift_h), sd(g2$shift_h), mean(g2$shift_a), sd(g2$shift_a), mean(g2$anchor_steps)))
cat(sprintf("  actual margin on expected margin: slope %.3f; actual total on expected total: slope %.3f\n",
            coef(lm(margin ~ exp_margin, g2))[2], coef(lm(tot ~ exp_total, g2))[2]))
cat(sprintf("  expected margin minus the market's (-line):  mean %+.2f, sd %.2f\n",
            mean(g2$exp_margin + g2$line), sd(g2$exp_margin + g2$line)))
cat(sprintf("  expected total minus the market's:           mean %+.2f, sd %.2f\n",
            mean(g2$exp_total - g2$total), sd(g2$exp_total - g2$total)))
cat(sprintf("  final margin around the market (-line):  actual sd %.2f | simulated sd %.2f\n",
            sd(g2$margin + g2$line), sqrt(mean(g2$sd_margin^2))))
cat(sprintf("  final total around the market's total:   actual sd %.2f | simulated sd %.2f\n",
            sd(g2$tot - g2$total), sqrt(mean(g2$sd_total^2))))
cat(sprintf("  home cover rate %.3f (model %.3f); over rate %.3f (model %.3f); OT %.3f (model %.3f)\n",
            mean(g2$margin + g2$line > 0), mean(g2$home_cover), mean(g2$tot > g2$total), mean(g2$over),
            mean(p[gid %in% g2$gid, max(q), by = gid]$V1 > 4), mean(g2$p_ot)))
cat(sprintf("  pre-game win probability: log loss %.4f, brier %.4f (current model at tip: %.4f, %.4f)\n",
            logloss(g2$home_wp, as.integer(g2$margin > 0)), brier(g2$home_wp, as.integer(g2$margin > 0)),
            both[posid == 1 & gid %in% g2$gid, logloss(wpl, hwin)], both[posid == 1 & gid %in% g2$gid, brier(wpl, hwin)]))
