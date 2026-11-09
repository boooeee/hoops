# possim/03_fit.R - the possession model
#
# Two parts, each fitted on the training seasons:
#   outcome   P(how the possession ends | game state, teams)   - 13 outcomes (common.R)
#   seconds   P(seconds it takes | outcome, game state, teams) - 1..max_dur
#
# Each is additive on the log-odds scale:
#   game state  boosted trees (xgboost) on start type, period, clock and the
#               offense's lead. This is where late-game fouling, holding for the
#               last shot, garbage time and the trailing team pressing come from.
#   teams       a linear term in four numbers for this offense against this defense:
#               mkt_ppp   points per possession the line and total imply
#               rat_ppp   points per possession the efficiency ratings imply
#               exp_spp   seconds per possession the pace ratings imply
#               exp_tpar  3-point attempts per FGA the 3-point ratings imply
#               For outcomes, each outcome gets its own coefficient on each number.
#               For seconds, the numbers tilt the whole distribution longer or shorter.
#               Each coefficient has a base value plus a change for three late-game
#               situations (close, offense trailing, offense leading; regime_w in
#               common.R), because team strength and era matter less once a game
#               turns into fouling, clock-running or garbage time.
# The two parts are fitted in turn (backfitting) so the trees see the game state
# with team strength held out: a team that leads is often just the better team,
# and the trees must not credit the lead for that.
#
# Splitting it this way keeps the state part fixed for every game: it is computed
# once as a table, and a game only changes the small team term (04_engine.R).
# Writes possim_model.rds to data_dir.

here <- local({   # this script's folder, whether run with Rscript or source()d
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "common.R"))
suppressMessages(library(xgboost))
set.seed(20261103)
nthread <- 8L

# ── Data ─────────────────────────────────────────────────────────────────────
p <- readRDS(rds("possessions"))
rt <- readRDS(rds("ratings"))
p <- merge(p, rt[, .(gid, off, rat_ppp = ppp_exp, exp_spp = spp_exp, exp_tpar = tpar_exp)],
           by = c("gid", "off"))
gsp <- rt[, .(spp_h = spp_exp[home == 1][1], spp_a = spp_exp[home == 0][1]), by = gid]
p <- merge(p, gsp, by = "gid")
p[, mkt_ppp := market_ppp(line, total, home, spp_h, spp_a)]
K <- nrow(outcomes)

cat("Line sign check - correlation of home margin with -line, by season:\n")
print(unique(p[, .(gid, ssn, hm = hscr - ascr, line)])[!is.na(line), .(r = round(cor(hm, -line), 3)), by = ssn][order(ssn)])

usable <- p[!bad & !is.na(mkt_ppp) & !is.na(rat_ppp) & !is.na(exp_spp) & !is.na(exp_tpar)]
train <- usable[ssn %in% train_ssn]
test  <- usable[ssn %in% test_ssn]
# hold out a tenth of the training games for early stopping
val_gid <- sample(unique(train$gid), round(0.1 * uniqueN(train$gid)))
quick <- nzchar(Sys.getenv("POSSIM_QUICK"))   # smoke test: a small sample, few rounds
if (quick) {
  keep_gid <- sample(unique(train$gid), 600)
  train <- train[gid %in% keep_gid]; test <- test[gid %in% sample(unique(test$gid), 300)]
  val_gid <- intersect(val_gid, keep_gid)
}
cat("training possessions:", nrow(train), " test:", nrow(test), "\n")

z_center <- train[, lapply(.SD, mean), .SDcols = z_names]
zmat <- function(d) team_Z(sweep(as.matrix(d[, ..z_names]), 2, unlist(z_center)), d$gr, d$mo)

# The last three minutes and overtime are under a tenth of possessions but decide
# close games. With every possession counting the same, the trees stopped before
# they had fitted them: a leading team's possessions in the last two minutes ran
# over a second longer than the trees said, even in the training seasons, which
# left the trailing team too much time. So the tree fits keep every late
# possession and count each one late_wt times. The trees condition on the game
# state, so this sharpens the late game without biasing anything else.
late_wt <- 4
is_late <- function(d) (d$q == 4 & d$tr <= 180) | d$q >= 5
# every late possession plus a random n of the rest
pick <- function(d, n) {
  late <- is_late(d)
  d[late | seq_len(nrow(d)) %in% sample(which(!late), min(sum(!late), n))]
}

# ── Outcome model ────────────────────────────────────────────────────────────
fit_xgb <- function(X, y, nclass, base, w, is_val, label) {
  dtr <- xgb.DMatrix(X[!is_val, ], label = y[!is_val], weight = w[!is_val], base_margin = as.vector(t(base[!is_val, ])))
  dva <- xgb.DMatrix(X[is_val, ],  label = y[is_val],  weight = w[is_val],  base_margin = as.vector(t(base[is_val, ])))
  t0 <- Sys.time()
  m <- xgb.train(params = list(objective = "multi:softprob", num_class = nclass, eta = 0.2,
                               max_depth = 6, min_child_weight = 50, subsample = 0.8,
                               tree_method = "hist", nthread = nthread, eval_metric = "mlogloss"),
                 data = dtr, nrounds = if (quick) 15 else 600, watchlist = list(val = dva),
                 early_stopping_rounds = 15, verbose = 0)
  message(label, ": ", m$best_iteration, " rounds, val logloss (late weighted) ",
          round(m$best_score, 5), " (", round(difftime(Sys.time(), t0, units = "mins"), 1), " min)")
  m
}

tree_margin <- function(m, X, nclass) {
  # the trees' log-odds alone (no team term)
  matrix(predict(m, xgb.DMatrix(X), outputmargin = TRUE, iterationrange = c(1, m$best_iteration + 1)),
         ncol = nclass, byrow = TRUE)
}

# Multinomial logit for the team term, with the trees' log-odds as an offset.
# B is ncol(Z) x K; the first outcome's column is held at 0.
fit_team_outcome <- function(Fm, Z, y, B0 = NULL) {
  n <- nrow(Z); pz <- ncol(Z)
  Y <- matrix(0, n, K); Y[cbind(seq_len(n), y + 1L)] <- 1
  unpack <- function(par) cbind(0, matrix(par, pz, K - 1))
  cache <- new.env()
  eval_at <- function(par) {
    B <- unpack(par)
    eta <- Fm + Z %*% B
    mx <- rowmax(eta)
    e <- exp(eta - mx); s <- rowSums(e)
    P <- e / s
    cache$par <- par
    cache$ll <- sum(eta * Y) - sum(mx + log(s))
    cache$g <- (crossprod(Z, Y - P))[, -1]
  }
  fn <- function(par) { if (!identical(par, cache$par)) eval_at(par); -cache$ll }
  gr <- function(par) { if (!identical(par, cache$par)) eval_at(par); -as.vector(cache$g) }
  start <- if (is.null(B0)) rep(0, pz * (K - 1)) else as.vector(B0[, -1])
  o <- optim(start, fn, gr, method = "BFGS", control = list(maxit = 300, reltol = 1e-10))
  B <- unpack(o$par); dimnames(B) <- list(colnames(Z), outcomes$oc)
  B
}


# 1.6M possessions is plenty for trees on five state variables
otrain <- pick(train, 1.6e6)
Xo <- state_X(otrain); yo <- otrain$ock; Zo <- zmat(otrain); is_val <- otrain$gid %in% val_gid
wo <- ifelse(is_late(otrain), late_wt, 1)
B <- matrix(0, ncol(Zo), K)
for (it in 1:3) {
  mo_xgb <- fit_xgb(Xo, yo, K, Zo %*% B, wo, is_val, paste("outcome trees, pass", it))
  Fo <- tree_margin(mo_xgb, Xo, K)
  B_new <- fit_team_outcome(Fo[!is_val, ], Zo[!is_val, ], yo[!is_val], B)
  chg <- max(abs(B_new - B)); B <- B_new
  message("  team term change: ", signif(chg, 3))
  if (chg < 0.02) break
}
cat("\nOutcome team coefficients (log-odds per unit, relative to 0DRB):\n")
print(round(B, 2))

# ── Seconds model ────────────────────────────────────────────────────────────
# 0PEND always takes exactly the time that was left, so it is not modelled.
dtrain <- train[oc != "0PEND"]
# 700k possessions plus the late ones is plenty for a model this coarse
dtrain <- pick(dtrain, 7e5)
Xd <- dur_X(dtrain); yd <- dtrain$durc - 1L; Zd <- zmat(dtrain); dval <- dtrain$gid %in% val_gid
wd <- ifelse(is_late(dtrain), late_wt, 1)
dsec <- seq_len(max_dur)

fit_team_dur <- function(Fm, Z, y, g0) {
  n <- nrow(Z)
  dy <- dsec[y + 1L]
  Fy <- Fm[cbind(seq_len(n), y + 1L)]
  cache <- new.env()
  eval_at <- function(g) {
    s <- as.vector(Z %*% g)
    eta <- Fm + outer(s, dsec)
    mx <- rowmax(eta)
    P <- exp(eta - mx); den <- rowSums(P)
    cache$par <- g
    cache$ll <- sum(Fy + s * dy) - sum(mx + log(den))
    cache$g <- as.vector(crossprod(Z, dy - as.vector(P %*% dsec) / den))
  }
  fn <- function(g) { if (!identical(g, cache$par)) eval_at(g); -cache$ll }
  gr <- function(g) { if (!identical(g, cache$par)) eval_at(g); -cache$g }
  o <- optim(g0, fn, gr, method = "BFGS", control = list(maxit = 200, reltol = 1e-10))
  setNames(o$par, colnames(Z))
}

g <- setNames(rep(0, ncol(Zd)), colnames(Zd))
for (it in 1:3) {
  md_xgb <- fit_xgb(Xd, yd, max_dur, outer(as.vector(Zd %*% g), dsec), wd, dval, paste("seconds trees, pass", it))
  Fd <- tree_margin(md_xgb, Xd, max_dur)
  g_new <- fit_team_dur(Fd[!dval, ], Zd[!dval, ], yd[!dval], g)
  chg <- max(abs(g_new - g)); g <- g_new
  message("  team term change: ", signif(chg, 3))
  if (chg < 1e-3) break
}
cat("\nSeconds team coefficients (log-odds per second, per unit):\n"); print(round(g, 4))

# Technical free throws belong to no possession; the engine adds their average to totals.
tech_pts <- p[ssn %in% train_ssn, .(tp = sum(tech_pts)), by = gid][, mean(tp)]

model <- list(outcome_xgb = xgb.save.raw(mo_xgb), outcome_iter = mo_xgb$best_iteration, B = B,
              dur_xgb = xgb.save.raw(md_xgb), dur_iter = md_xgb$best_iteration, g = g,
              z_center = z_center, regime = regime, tech_pts_per_game = tech_pts,
              fitted = Sys.time(), train_ssn = train_ssn)
saveRDS(model, rds(if (quick) "possim_model_quick" else "possim_model"))

# ── Held-out seasons ─────────────────────────────────────────────────────────
Xt <- state_X(test); Zt <- zmat(test)
Ft <- tree_margin(mo_xgb, Xt, K)
P_full  <- { e <- exp(Ft + Zt %*% B - rowmax(Ft + Zt %*% B)); e / rowSums(e) }
P_state <- { e <- exp(Ft - rowmax(Ft)); e / rowSums(e) }
base_rate <- tabulate(train$ock + 1L, K) / nrow(train)
ll <- function(P) -mean(log(P[cbind(seq_len(nrow(P)), test$ock + 1L)]))
cat("\nOutcome log loss on", paste(range(test_ssn), collapse = "-"), "possessions:\n")
cat("  league rates only     ", round(-mean(log(base_rate[test$ock + 1L])), 5), "\n")
cat("  game state, no teams  ", round(ll(P_state), 5), "\n")
cat("  game state and teams  ", round(ll(P_full), 5), "\n")

test[, `:=`(xpts = as.vector(P_full %*% outcomes$pts), x3 = rowSums(P_full[, outcomes$pts == 3]))]
test[, is3 := as.integer(pts == 3)]
cat("\nPoints per possession by the market's number for the offense (test seasons):\n")
print(test[, .(n = .N, predicted = round(mean(xpts), 3), actual = round(mean(pts), 3)),
           by = .(mkt_ppp_decile = cut(mkt_ppp, quantile(mkt_ppp, 0:10 / 10), include.lowest = TRUE, dig.lab = 3))][order(mkt_ppp_decile)])
cat("\n3-point possessions by expected 3-point rate (test seasons):\n")
print(test[, .(n = .N, predicted = round(mean(x3), 4), actual = round(mean(is3), 4)),
           by = .(exp_tpar_decile = cut(exp_tpar, quantile(exp_tpar, 0:10 / 10), include.lowest = TRUE, dig.lab = 3))][order(exp_tpar_decile)])
cat("\nBy the offense's lead, Q4 last 6 minutes (test seasons):\n")
print(test[q == 4 & tr <= 360, .(n = .N, predicted = round(mean(xpts), 3), actual = round(mean(pts), 3)),
           by = .(lead = cut(mo, c(-99, -15, -10, -6, -3, 0, 2, 5, 9, 14, 99)))][order(lead)])
cat("\nOffense down 6-15, Q4 last 6 minutes, by season (test seasons):\n")
print(test[q == 4 & tr <= 360 & mo <= -6 & mo >= -15, .(n = .N, predicted = round(mean(xpts), 3), actual = round(mean(pts), 3)), by = ssn][order(ssn)])
cat("Saved the possession model to", rds(if (quick) "possim_model_quick" else "possim_model"), "\n")

when_bins <- function(q, tr) fcase(q <= 3, "1 Q1-Q3", q == 4 & tr > 360, "2 Q4 12:00-6:00",
                                   q == 4 & tr > 120, "3 Q4 6:00-2:00", q == 4 & tr > 35, "4 Q4 2:00-0:35",
                                   q == 4, "5 Q4 last 0:35", default = "6 OT")
lead_bins <- function(mo) cut(mo, c(-99, -7, -4, -1, 0, 3, 6, 99),
                              labels = c("<=-7", "-6..-4", "-3..-1", "tied", "+1..+3", "+4..+6", ">=+7"))
cat("\nPoints per possession, predicted minus actual, late, by the offense's lead (test seasons):\n")
print(dcast(test[q >= 4 & (q > 4 | tr <= 360), .(b = round(mean(xpts) - mean(pts), 3)),
                 by = .(when = when_bins(q, tr), lead = lead_bins(mo))], when ~ lead, value.var = "b"))

sec_pred <- function(Fm, d) {
  eta <- Fm + outer(as.vector(zmat(d) %*% g), dsec)
  P <- exp(eta - rowmax(eta)); as.vector((P / rowSums(P)) %*% dsec)
}
dt <- test[oc != "0PEND"]
dt[, xdur := sec_pred(tree_margin(md_xgb, dur_X(dt), max_dur), dt)]
cat("\nSeconds per possession by expected pace (test seasons):\n")
print(dt[, .(n = .N, predicted = round(mean(xdur), 2), actual = round(mean(durc), 2)),
         by = .(exp_spp_decile = cut(exp_spp, quantile(exp_spp, 0:10 / 10), include.lowest = TRUE, dig.lab = 3))][order(exp_spp_decile)])
cat("\nSeconds by start type (test seasons):\n")
print(dt[, .(n = .N, predicted = round(mean(xdur), 2), actual = round(mean(durc), 2)), by = st])
dtrain[, xdur := sec_pred(Fd, dtrain)]
for (s in list(list(dt, "test seasons"), list(dtrain, "training seasons, in sample"))) {
  cat("\nSeconds, predicted minus actual, by time left and the offense's lead (", s[[2]], "):\n", sep = "")
  print(dcast(s[[1]][, .(b = round(mean(xdur) - mean(durc), 2)), by = .(when = when_bins(q, tr), lead = lead_bins(mo))],
              when ~ lead, value.var = "b"))
}
