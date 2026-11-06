# possim/04_engine.R - from the possession model to win probability, spread and total
#
# Run it once after 03_fit.R: it writes the state tables (tables_*.bin) the
# engine reads, and the anchoring Jacobian. Then source() it to use the functions:
#
#   eng  <- load_engine()
#   g    <- game_inputs(eng, line = -6.5, total = 228.5,
#                       home = list(rat_ppp = 1.17, exp_spp = 14.1, exp_tpar = 0.43),
#                       away = list(rat_ppp = 1.12, exp_spp = 14.6, exp_tpar = 0.38))
#   pregame(eng, g)                       # win probability, expected margin and total, spread
#   wp_at(eng, g, tip = 0, q = 4, tr = 90, m = -3, side = 0, st = "DRB")
#
# Anchoring: with a line and total, game_inputs() nudges each team's market
# points per possession until the game played out from the tip has the market's
# expected margin (-line) and total. The nudge then holds for the whole game.
#
# State: q period (5+ = overtime), tr seconds left in it, m home lead, side 0 if
# the home team has the ball (1 away), st how that possession started
# (start_types), tip 0 if the home team won the opening tip (1 away).
# rat_ppp / exp_spp / exp_tpar are each team's expected points per possession,
# seconds per possession and 3-point rate against this opponent (02_ratings.R).

here <- local({   # this script's folder, whether run with Rscript or source()d
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "common.R"))
suppressMessages(library(xgboost))
Rcpp::sourceCpp(file.path(here, "engine.cpp"))

tables_files <- c(o = file.path(data_dir, "tables_outcome.bin"), d = file.path(data_dir, "tables_seconds.bin"),
                  j = file.path(data_dir, "anchor_jacobian.rds"))
n_dkey <- (dur_tr_cap) + (dur_gr_cap - 1L)   # seconds-table clock keys (engine.cpp dkey())

# ── State tables (built once per fitted model) ───────────────────────────────
softmax_rows <- function(Fm) { e <- exp(Fm - rowmax(Fm)); e / rowSums(e) }

build_tables <- function(model) {
  K <- nrow(outcomes)
  mo_xgb <- xgb.load.raw(model$outcome_xgb)
  md_xgb <- xgb.load.raw(model$dur_xgb)
  # outcome rows, in engine.cpp's orow() order: period-second, then lead, then start type
  qtr <- rbind(data.table(q = rep(1:4, each = 720), tr = rep(1:720, 4)), data.table(q = 5L, tr = 1:300))
  go <- qtr[, .(mo = rep(-mo_cap:mo_cap, each = length(start_types)), st = rep(start_types, 2 * mo_cap + 1)), by = .(q, tr)]
  go[, gr := fifelse(q <= 4, tr + 720L * (4L - q), tr)]
  Fo <- matrix(predict(mo_xgb, xgb.DMatrix(state_X(go)), outputmargin = TRUE,
                       iterationrange = c(1, model$outcome_iter + 1)), ncol = K, byrow = TRUE)
  P0o <- softmax_rows(Fo)
  # seconds rows, in drow() order: clock key, outcome, start type, lead. Keys
  # 0-60: Q1-Q3 by seconds left in the period (61 = 61 or more); then Q4 and
  # overtime by seconds left in regulation (in the period for overtime).
  ng <- dur_gr_cap - 1L
  keys <- rbind(data.table(dkey = 0:(dur_tr_cap - 1L), tr = 1:dur_tr_cap, gr = dur_gr_cap),
                data.table(dkey = dur_tr_cap + 0:(ng - 1L), tr = 1:ng, gr = 1:ng))
  stopifnot(nrow(keys) == n_dkey)
  dk <- outcomes$oc[outcomes$oc != "0PEND"]
  gd <- CJ(dkey = keys$dkey, oc = factor(dk, levels = dk), st = factor(start_types, levels = start_types),
           mo = -dur_mo_cap:dur_mo_cap, sorted = TRUE)
  gd <- merge(gd, keys, by = "dkey", sort = FALSE)
  setorder(gd, dkey, oc, st, mo)
  gd[, `:=`(oc = as.character(oc), st = as.character(st))]
  Fd <- matrix(predict(md_xgb, xgb.DMatrix(dur_X(gd)), outputmargin = TRUE,
                       iterationrange = c(1, model$dur_iter + 1)), ncol = max_dur, byrow = TRUE)
  P0d <- softmax_rows(Fd)
  writeBin(as.vector(t(P0o)), tables_files[["o"]], size = 4)
  writeBin(as.vector(t(P0d)), tables_files[["d"]], size = 4)
  message("wrote ", tables_files[["o"]], " and ", tables_files[["d"]])
}

# Regime weights by (seconds left in regulation 0..late_from, lead -12..12), in
# the order engine.cpp reads them.
regime_table <- function(rg) {
  gx <- CJ(gi = 0:rg[["late_from"]], mo = -dur_mo_cap:dur_mo_cap, sorted = TRUE)
  as.vector(t(regime_w(gx$gi, gx$mo, rg)))
}

load_engine <- function(model_file = rds("possim_model")) {
  model <- readRDS(model_file)
  K <- nrow(outcomes)
  no <- (4 * 720 + 300) * (2 * mo_cap + 1) * length(start_types) * K
  nd <- (K - 1) * length(start_types) * n_dkey * (2 * dur_mo_cap + 1) * max_dur
  stopifnot(file.size(tables_files[["o"]]) == 4 * no, file.size(tables_files[["d"]]) == 4 * nd)
  rg <- if (is.null(model$regime)) regime else model$regime
  eng <- list(model = model,
              P0o = readBin(tables_files[["o"]], "double", n = no, size = 4),
              P0d = readBin(tables_files[["d"]], "double", n = nd, size = 4),
              pts = as.integer(outcomes$pts), nxt = match(outcomes$nxt, start_types) - 1L,
              pend = which(outcomes$oc == "0PEND") - 1L,
              fw = if (nrow(model$B) > length(z_names)) regime_table(rg) else numeric(0),
              tech_pts = model$tech_pts_per_game)
  if (file.exists(tables_files[["j"]])) eng$anchor_J <- readRDS(tables_files[["j"]])
  eng
}

# ── A game's team numbers ────────────────────────────────────────────────────
# line: home spread (negative = home favoured). If there's no line, the ratings
# stand in for the market (mkt_ppp = rat_ppp) and there is nothing to anchor to.
game_inputs <- function(eng, line, total, home, away, anchor = TRUE) {
  spp_h <- home$exp_spp; spp_a <- away$exp_spp
  has_mkt <- !is.na(line) && !is.na(total)
  mk <- function(t, side) {
    mkt <- if (has_mkt) market_ppp(line, total, side, spp_h, spp_a) else t$rat_ppp
    c(mkt_ppp = mkt, rat_ppp = t$rat_ppp, exp_spp = t$exp_spp, exp_tpar = t$exp_tpar)
  }
  g <- list(line = line, total = total, zh = mk(home, 1), za = mk(away, 0), shift = c(0, 0))
  if (anchor && has_mkt) g <- anchor_game(eng, g)
  g
}

# The team term for each offense: K x R log multipliers on the outcomes and R
# seconds tilts (R = base + late-game regimes). g$shift moves each team's market
# number (anchoring).
team_terms <- function(eng, g) {
  m <- eng$model
  zc <- unlist(m$z_center)[z_names]
  nz <- length(z_names); R <- nrow(m$B) / nz
  dz_h <- g$zh[z_names] - zc; dz_a <- g$za[z_names] - zc
  if (!is.null(g$shift)) {
    dz_h[["mkt_ppp"]] <- dz_h[["mkt_ppp"]] + g$shift[1]
    dz_a[["mkt_ppp"]] <- dz_a[["mkt_ppp"]] + g$shift[2]
  }
  lt  <- function(dz) sapply(seq_len(R), function(r) as.vector(dz %*% m$B[(r - 1) * nz + 1:nz, , drop = FALSE]))
  lam <- function(dz) sapply(seq_len(R), function(r) sum(dz * m$g[(r - 1) * nz + 1:nz]))
  list(ltH = matrix(lt(dz_h), ncol = R), ltA = matrix(lt(dz_a), ncol = R), lamH = lam(dz_h), lamA = lam(dz_a))
}

st_index <- function(st) if (is.character(st)) match(st, start_types) - 1L else as.integer(st)

# Values at many states for one game and tip: a data.frame with columns q, tr, m,
# side, st. Returns home win probability and expected final scores added from here.
wp_states <- function(eng, g, tip, states, want_pts = TRUE, M = 60L) {
  tt <- team_terms(eng, g)
  qs <- cbind(as.integer(pmin(states$q, 5)), as.integer(states$tr), as.integer(states$m),
              as.integer(states$side), st_index(states$st))
  r <- dp_game(eng$P0o, eng$P0d, eng$pts, eng$nxt, eng$pend, tt$ltH, tt$ltA, tt$lamH, tt$lamA, eng$fw,
               max_dur, as.integer(tip), qs, as.integer(M), want_pts)
  # the float32 tables can leave a decided game a hair above 1
  data.table(wp = pmin(pmax(r$values[, 1], 0), 1), home_more = r$values[, 2], away_more = r$values[, 3])
}

wp_at <- function(eng, g, tip, q, tr, m, side, st, M = 60L) {
  wp_states(eng, g, tip, data.frame(q = q, tr = tr, m = m, side = side, st = st), M = M)
}

# ── Anchoring to the market ──────────────────────────────────────────────────
# Expected final margin and total from the opening tip (averaged over who wins
# it). A lead cap of 40 is plenty for expectations and a third quicker.
tip_values <- function(eng, g, M = 40L) {
  v <- rbind(wp_at(eng, g, 0, 1, 720, 0, 0, "PST", M), wp_at(eng, g, 1, 1, 720, 0, 1, "PST", M))
  c(margin = mean(v$home_more - v$away_more), total = mean(v$home_more + v$away_more) + eng$tech_pts)
}

# Shift each team's market points per possession so that the expected margin
# is -line and the expected total is the total, to within tol points. Broyden's
# method from the Jacobian of an average game (built with the tables); it
# usually takes two or three steps.
anchor_game <- function(eng, g, tol = 0.05, max_iter = 6L) {
  target <- c(-g$line, g$total)
  J <- if (is.null(eng$anchor_J)) matrix(c(90, 100, -90, 100), 2) else eng$anchor_J
  s <- c(0, 0); g$shift <- s
  f <- f0 <- tip_values(eng, g); r <- target - f; it <- 0L
  while (max(abs(r)) > tol && it < max_iter) {
    ds <- solve(J, r)
    g$shift <- s + ds
    f_new <- tip_values(eng, g)
    J <- J + outer((f_new - f) - as.vector(J %*% ds), ds) / sum(ds^2)
    s <- s + ds; f <- f_new; r <- target - f; it <- it + 1L
  }
  if (max(abs(r)) > tol) warning("anchoring stopped ", round(max(abs(r)), 2), " points from the market")
  g$anchor <- list(shift = s, steps = it, unanchored = f0, resid = r)
  g
}

# Pre-game: exact win probability (average over who wins the tip) plus a
# simulation for the spread of outcomes.
pregame <- function(eng, g, nsim = 20000) {
  v <- rbind(wp_at(eng, g, 0, 1, 720, 0, 0, "PST"), wp_at(eng, g, 1, 1, 720, 0, 1, "PST"))
  tt <- team_terms(eng, g)
  sim <- function(tip) sim_game(eng$P0o, eng$P0d, eng$pts, eng$nxt, eng$pend, tt$ltH, tt$ltA, tt$lamH, tt$lamA,
                                eng$fw, max_dur, 1L, 720L, 0L, tip, 5L, tip, as.integer(nsim / 2))
  sims <- rbind(sim(0L), sim(1L))
  mg <- sims[, 1] - sims[, 2]; tot <- sims[, 1] + sims[, 2] + eng$tech_pts
  out <- list(
    home_wp = mean(v$wp),
    exp_margin = mean(v$home_more - v$away_more),
    exp_total = mean(v$home_more + v$away_more) + eng$tech_pts,
    sd_margin = sd(mg), sd_total = sd(tot), p_ot = mean(sims[, 3] > 0))
  if (!is.na(g$line)) out$home_cover <- mean(mg + g$line > 0) + 0.5 * mean(mg + g$line == 0)
  if (!is.na(g$total)) out$over <- mean(tot > g$total) + 0.5 * mean(tot == g$total)
  out
}

# Jacobian of (expected margin, expected total) in the two market shifts, for an
# average game: the starting point for anchor_game().
anchor_jacobian <- function(eng, h = 0.01) {
  zc <- unlist(eng$model$z_center)[z_names]
  g <- list(line = 0, total = NA, zh = zc, za = zc, shift = c(0, 0))
  f0 <- tip_values(eng, g)
  J <- sapply(1:2, function(j) { g$shift <- replace(c(0, 0), j, h); (tip_values(eng, g) - f0) / h })
  dimnames(J) <- list(c("margin", "total"), c("shift_home", "shift_away"))
  J
}

if (sys.nframe() == 0L) {          # run as a script: build the tables and the anchoring Jacobian
  model <- readRDS(rds("possim_model"))
  t0 <- Sys.time(); build_tables(model)
  message("tables built in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
  eng <- load_engine()
  J <- anchor_jacobian(eng); saveRDS(J, tables_files[["j"]])
  cat("Anchoring Jacobian (points per unit of market points per possession):\n"); print(round(J, 1))
}
