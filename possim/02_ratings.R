# possim/02_ratings.R - opponent-adjusted team ratings as of the morning of each game
#
# Three ratings, each with an offense and a defense side:
#   ppp   points per possession            (offensive / defensive efficiency)
#   spp   seconds per offensive possession (offensive / defensive pace)
#   tpar  3-point attempts per FGA         (team 3-point rate / opponent 3-point rate)
# For each game date, one weighted ridge regression per rating, fitted on games
# before that date only:
#   y(offense i, defense j) = mean + home + off_i + def_j
# Older games count less (half-life `half_life` days); last season's games also
# count `prior_wt` as much, so early-season ratings lean on last season. `lambda`
# shrinks every team toward average by that many games' worth of data.
# The settings were picked by predicting each next game's numbers (see `tune` below).
# Writes ratings.rds (one row per game, both teams' expected numbers) to data_dir.

here <- local({   # this script's folder, whether run with Rscript or source()d
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "common.R"))

p <- readRDS(rds("possessions"))
all_ssn <- sort(unique(p$ssn))

# ── Team-game numbers ────────────────────────────────────────────────────────
tg <- p[, .(poss = .N, secs = sum(dur)), by = .(gid, ssn, date, off, def, home, htm, hscr, ascr)]
tg[, pts := fifelse(home == 1L, hscr, ascr)]
tg[, gposs := mean(poss), by = gid]   # both teams get (about) the same number of possessions
box <- sqlq(paste0("SELECT gid, tm, SUM(fga) fga, SUM(fg3a) fg3a FROM box WHERE LEFT(gid,5) IN ",
                   in_list(gid_prefixes(all_ssn)), " GROUP BY gid, tm"))
box[, tm := fix_team(tm)]
tg <- merge(tg, box, by.x = c("gid", "off"), by.y = c("gid", "tm"), all.x = TRUE)
message("team-games without a box score: ", tg[is.na(fga), .N], " of ", nrow(tg))
tg[, `:=`(ppp = pts / gposs, spp = secs / poss, tpar = fg3a / fga)]
tg[, `:=`(w_ppp = gposs / 100, w_spp = poss / 100, w_tpar = fga / 85)]

teams <- sort(unique(c(tg$off, tg$def)))
nt <- length(teams)
tg[, `:=`(oi = match(off, teams), di = match(def, teams))]
dates <- sort(unique(tg$date))

# ── One ridge fit ────────────────────────────────────────────────────────────
fit_one <- function(d, y, w, oi, di, home, lambda) {
  n <- length(y)
  X <- matrix(0, n, 2 + 2 * nt)
  X[, 1] <- 1
  X[, 2] <- home
  X[cbind(seq_len(n), 2 + oi)] <- 1
  X[cbind(seq_len(n), 2 + nt + di)] <- 1
  XtW <- t(X * w)
  A <- XtW %*% X
  diag(A)[-(1:2)] <- diag(A)[-(1:2)] + lambda
  b <- solve(A, XtW %*% y)
  list(mu = b[1], home = b[2], off = b[2 + 1:nt], def = b[2 + nt + 1:nt])
}

# Ratings as of each date in `ssn`, using games before that date.
ratings_for <- function(ssn_, metric, half_life, prior_wt, lambda) {
  y_all <- tg[[metric]]; w_all <- tg[[paste0("w_", metric)]]
  ok_all <- !is.na(y_all) & !is.na(w_all)
  ds <- sort(unique(tg[ssn == ssn_, date]))
  out <- vector("list", length(ds))
  for (k in seq_along(ds)) {
    d <- ds[k]
    use <- ok_all & tg$date < d & tg$ssn %in% c(ssn_ - 1L, ssn_)
    if (sum(use) < 60) { out[[k]] <- NULL; next }
    age <- as.numeric(d - tg$date[use])
    w <- w_all[use] * 0.5^(age / half_life) * ifelse(tg$ssn[use] == ssn_, 1, prior_wt)
    f <- fit_one(d, y_all[use], w, tg$oi[use], tg$di[use], tg$home[use], lambda)
    out[[k]] <- data.table(date = d, team = teams, mu = f$mu, hca = f$home, off = f$off, def = f$def)
  }
  rbindlist(out)
}

# Expected numbers for each team-game from the ratings as of that morning.
attach_ratings <- function(r, metric) {
  x <- tg[, .(gid, date, ssn, off, def, home, y = get(metric), w = get(paste0("w_", metric)))]
  x <- merge(x, r[, .(date, off = team, mu, hca, o = off)], by = c("date", "off"))
  x <- merge(x, r[, .(date, def = team, d = def)], by = c("date", "def"))
  x[, pred := mu + hca * home + o + d]
  x
}

# ── Tuning: how well does each setting predict the next game? ────────────────
tune <- function(metric, seasons = 2015:2018) {
  grid <- CJ(half_life = c(45, 90, 180, 1e6), prior_wt = c(0.25, 0.5, 1), lambda = c(3, 10, 30))
  grid[, rmse := NA_real_]
  for (i in seq_len(nrow(grid))) {
    r <- rbindlist(lapply(seasons, ratings_for, metric = metric, half_life = grid$half_life[i],
                          prior_wt = grid$prior_wt[i], lambda = grid$lambda[i]))
    x <- attach_ratings(r, metric)[!is.na(y)]
    grid$rmse[i] <- sqrt(weighted.mean((x$y - x$pred)^2, x$w))
  }
  grid[order(rmse)]
}

best <- list()
for (metric in c("ppp", "spp", "tpar")) {
  g <- tune(metric)
  cat("\n==", metric, "- best settings (next-game RMSE):\n"); print(head(g, 5))
  best[[metric]] <- g[1]
}
# chart.R fits ratings for games after the snapshot with the same settings
saveRDS(rbindlist(best, idcol = "metric"), rds("ratings_settings"))

# ── Ratings for every season with the chosen settings ────────────────────────
rt <- list()
for (metric in names(best)) {
  b <- best[[metric]]
  r <- rbindlist(lapply(setdiff(all_ssn, first_ssn), ratings_for, metric = metric,
                        half_life = b$half_life, prior_wt = b$prior_wt, lambda = b$lambda))
  x <- attach_ratings(r, metric)
  rt[[metric]] <- x[, .(gid, off, def, home, pred, o, d, mu)]
  setnames(rt[[metric]], c("pred", "o", "d", "mu"),
           paste0(metric, c("_exp", "_off", "_def", "_lg")))
}
ratings <- Reduce(function(a, b) merge(a, b, by = c("gid", "off", "def", "home")), rt)
saveRDS(ratings, rds("ratings"))

cat("\nSpread of the expected numbers (one row per team-game):\n")
print(ratings[, lapply(.SD, function(v) round(quantile(v, c(0.05, 0.5, 0.95)), 3)),
              .SDcols = c("ppp_exp", "spp_exp", "tpar_exp")])
cat("Saved ratings for", uniqueN(ratings$gid), "games to", rds("ratings"), "\n")
