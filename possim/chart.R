# possim/chart.R - chart a game: the new model's win probability beside the current model's
#
#   source("~/hoops/possim/chart.R")
#   wp_chart("0022600158")                          # by game id
#   wp_chart("BOS")                                 # Boston's game today, finished or in progress
#   wp_chart("BOS", date = "2026-11-04", file = "~/Desktop/bos.png")
#
# The game comes from the database (gms, pos, wpg). wpNBA_rtask.R rewrites
# recent games there, in-progress ones included, each time it runs, so a live
# game is charted as of that script's last run.
#
# Both models are read at the start of each possession, and the new model is
# anchored to the game's line and total. Team ratings come from ratings.rds when
# the game is in the possession snapshot. For a later game they're fitted as of
# that morning, the same way 02_ratings.R fits them (its settings are in
# ratings_settings.rds), on the snapshot plus the later games in the database.
#
# wp_chart() draws the chart, prints a summary, and returns (invisibly) the plot,
# one row per possession, and the game's inputs.

here <- local({   # this script's folder, whether run with Rscript or source()d
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "04_engine.R"))
suppressMessages({ library(ggplot2); library(patchwork) })

.chart <- new.env()   # the engine, snapshot, ratings and team-game numbers, loaded once a session
cached <- function(name, make) {
  if (is.null(.chart[[name]])) assign(name, make(), envir = .chart)
  .chart[[name]]
}
chart_engine <- function() cached("eng", load_engine)
snapshot     <- function() cached("pos", function() readRDS(rds("possessions")))

# ── Which game ───────────────────────────────────────────────────────────────
find_game <- function(game, date = Sys.Date()) {
  game <- toupper(as.character(game))
  if (grepl("^[0-9]{10}$", game)) return(game)
  if (!grepl("^[A-Z]{2,3}$", game)) stop("give a game id or a team code such as BOS")
  d <- format(as.Date(date))
  g <- sqlq(paste0("SELECT gid, htm, atm FROM gms WHERE LEFT(dt, 10) = '", d,
                   "' AND LEFT(gid, 3) IN ", in_list(gid_types)))
  g <- g[fix_team(htm) == fix_team(game) | fix_team(atm) == fix_team(game)]
  if (nrow(g) == 0) stop("no game for ", game, " on ", d, " in gms")
  g$gid[1]
}

# ── The game, one row per possession start ───────────────────────────────────
game_states <- function(gid) {
  q1 <- function(sql) sqlq(sprintf(sql, gid))
  gm <- q1("SELECT gid, ssn, dt, htm, atm, hscr, ascr, line, total FROM gms WHERE gid = '%s'")
  if (nrow(gm) == 0) stop("game ", gid, " isn't in gms")
  gm <- gm[1]
  gm[, `:=`(date = as.Date(substr(dt, 1, 10)), htm = fix_team(htm), atm = fix_team(atm), dt = NULL)]
  # wpNBA_rtask.R fills a missing line and total with -3.25 and 200.52
  if (!is.na(gm$total) && (gm$total < 150 || (gm$total == 200.52 && isTRUE(gm$line == -3.25)))) {
    warning("no real line and total for ", gid, ": the new model runs on team ratings alone")
    gm[, `:=`(line = NA_real_, total = NA_real_)]
  }

  pos <- q1("SELECT posid, r, gt, spt1, off FROM pos WHERE gid = '%s'")
  w   <- q1("SELECT r, q, gt, m, h, a, pt2, wpl, gst1 FROM wpg WHERE gid = '%s'")
  if (nrow(pos) == 0 || nrow(w) == 0) stop("no play-by-play for ", gid, " yet")
  w <- unique(w, by = "r"); setorder(w, r)

  p <- merge(pos, w[, .(r, q, m, h, a, pt2, wpl)], by = "r", all.x = TRUE)
  setorder(p, posid)
  p[, first_in_q := seq_len(.N) == 1L, by = q]
  p[, st := start_type(first_in_q, spt1, pt2)]
  p[, `:=`(tr = period_end(q) - gt, side = as.integer(fix_team(off) != gm$htm))]
  skipped <- p[is.na(q) | is.na(st) | tr <= 0 | tr > period_len(q), .N]
  p <- p[!(is.na(q) | is.na(st) | tr <= 0 | tr > period_len(q))]
  if (nrow(p) == 0) stop("no readable possessions for ", gid, " yet")
  p[, `:=`(e = period_end(q) - tr, left = fifelse(q <= 4, (4 - q) * 720 + tr, tr))]

  last <- w[.N]
  list(game = gm,
       states = p[, .(posid, q, tr, m, side, st, h, a, wpl, e, left)],
       tip = p[q == 1][1, side],             # 0 if the home team won the opening tip
       final = any(w$gst1 == "EG", na.rm = TRUE),
       now = list(q = last$q, e = last$gt, h = last$h, a = last$a, wpl = last$wpl),
       skipped = skipped)
}

# ── Team ratings ─────────────────────────────────────────────────────────────
# Team-game numbers for seasons s-1 and s, as 02_ratings.R builds them: the
# snapshot, plus finished games in the database after it and before `d`.
team_games <- function(s, d) {
  base <- cached(paste0("tg", s), function() {
    p <- snapshot()[ssn %in% c(s - 1L, s)]
    p[, .(poss = .N, secs = sum(dur)), by = .(gid, ssn, date, off, def, home, hscr, ascr)]
  })
  last <- max(snapshot()$date)
  extra <- NULL
  if (d > last + 1) {
    g <- sqlq(paste0("SELECT gid, ssn, dt, htm, atm, hscr, ascr FROM gms WHERE LEFT(dt, 10) > '", last,
                     "' AND LEFT(dt, 10) < '", d, "' AND ssn IN (", s - 1L, ",", s, ") AND LEFT(gid, 3) IN ",
                     in_list(gid_types)))
    if (nrow(g)) {
      ps <- sqlq(paste0("SELECT gid, off, def, COUNT(*) poss, SUM(GREATEST(time, 1)) secs FROM pos WHERE gid IN ",
                        in_list(g$gid), " GROUP BY gid, off, def"))
      g[, `:=`(date = as.Date(substr(dt, 1, 10)), htm = fix_team(htm))]
      extra <- merge(ps[, .(gid, off = fix_team(off), def = fix_team(def), poss, secs)],
                     g[, .(gid, ssn, date, htm, hscr, ascr)], by = "gid")
      extra[, home := as.integer(off == htm)][, htm := NULL]
    }
  }
  tg <- rbind(base, extra, use.names = TRUE)
  box <- cached(paste0("box", s), function() {
    b <- sqlq(paste0("SELECT gid, tm, SUM(fga) fga, SUM(fg3a) fg3a FROM box WHERE LEFT(gid, 5) IN ",
                     in_list(gid_prefixes(c(s - 1L, s))), " GROUP BY gid, tm"))
    b[, tm := fix_team(tm)]
  })
  tg <- merge(tg, box, by.x = c("gid", "off"), by.y = c("gid", "tm"), all.x = TRUE)
  tg[, pts := fifelse(home == 1L, hscr, ascr)]
  tg[, gposs := mean(poss), by = gid]
  tg[, `:=`(ppp = pts / gposs, spp = secs / poss, tpar = fg3a / fga,
            w_ppp = gposs / 100, w_spp = poss / 100, w_tpar = fga / 85)]
  tg[]
}

# One rating as of the morning of `d` in season s: the weighted ridge of 02_ratings.R.
ridge_asof <- function(tg, d, s, metric, half_life, prior_wt, lambda) {
  y <- tg[[metric]]; w <- tg[[paste0("w_", metric)]]
  use <- !is.na(y) & !is.na(w) & tg$date < d & tg$ssn %in% c(s - 1L, s)
  x <- tg[use]; y <- y[use]
  w <- w[use] * 0.5^(as.numeric(d - x$date) / half_life) * ifelse(x$ssn == s, 1, prior_wt)
  teams <- sort(unique(c(x$off, x$def))); nt <- length(teams); n <- nrow(x)
  X <- matrix(0, n, 2 + 2 * nt)
  X[, 1] <- 1; X[, 2] <- x$home
  X[cbind(seq_len(n), 2 + match(x$off, teams))] <- 1
  X[cbind(seq_len(n), 2 + nt + match(x$def, teams))] <- 1
  XtW <- t(X * w); A <- XtW %*% X
  diag(A)[-(1:2)] <- diag(A)[-(1:2)] + lambda
  b <- solve(A, XtW %*% y)
  list(mu = b[1], hca = b[2], off = setNames(b[2 + 1:nt], teams), def = setNames(b[2 + nt + 1:nt], teams))
}

team_numbers <- function(gm) {
  rt <- cached("ratings", function() readRDS(rds("ratings")))
  x <- rt[gid == gm$gid]
  if (nrow(x) == 2) {
    side <- function(r) list(rat_ppp = r$ppp_exp, exp_spp = r$spp_exp, exp_tpar = r$tpar_exp)
    return(list(home = side(x[home == 1]), away = side(x[home == 0]), source = "ratings.rds"))
  }
  set <- cached("settings", function() {
    f <- rds("ratings_settings")
    if (!file.exists(f)) stop("no ", f, ": run 02_ratings.R, which saves the settings")
    readRDS(f)
  })
  tg <- team_games(gm$ssn, gm$date)
  out <- list(home = list(), away = list(), source = paste("fitted as of", format(gm$date)))
  metrics <- c(rat_ppp = "ppp", exp_spp = "spp", exp_tpar = "tpar")
  for (nm in names(metrics)) {
    b <- set[metric == metrics[[nm]]]
    f <- ridge_asof(tg, gm$date, gm$ssn, metrics[[nm]], b$half_life, b$prior_wt, b$lambda)
    for (tm in c(gm$htm, gm$atm)) if (!tm %in% names(f$off)) stop("no games yet for ", tm, " to rate them on")
    out$home[[nm]] <- f$mu + f$hca + f$off[[gm$htm]] + f$def[[gm$atm]]
    out$away[[nm]] <- f$mu + f$off[[gm$atm]] + f$def[[gm$htm]]
  }
  out
}

# ── The chart ────────────────────────────────────────────────────────────────
clock <- function(e) {
  q <- ifelse(e < 2880, e %/% 720 + 1, 5 + (e - 2880) %/% 300)
  tr <- period_end(q) - e
  sprintf("%s %d:%02d", ifelse(q <= 4, paste0("Q", q), ifelse(q == 5, "OT", paste0(q - 4, "OT"))), tr %/% 60, tr %% 60)
}
fav <- function(gm) {
  if (is.na(gm$line)) return("no line")
  if (gm$line == 0) return("pick'em")
  if (gm$line < 0) paste0(gm$htm, " -", -gm$line) else paste0(gm$atm, " -", gm$line)
}
by_text <- function(pm, gm) ifelse(abs(pm) < 0.05, "even",
  ifelse(pm > 0, sprintf("%s by %.1f", gm$htm, pm), sprintf("%s by %.1f", gm$atm, -pm)))

wp_chart <- function(game, date = Sys.Date(), file = NULL, width = 10, height = 7.5, show = TRUE) {
  eng <- chart_engine()
  gid <- find_game(game, date)
  gd  <- game_states(gid)
  gm  <- gd$game
  tn  <- team_numbers(gm)
  g   <- game_inputs(eng, gm$line, gm$total, home = tn$home, away = tn$away)

  s <- cbind(gd$states, wp_states(eng, g, tip = gd$tip, gd$states))
  s[, `:=`(pm = m + home_more - away_more,
           pt = h + a + home_more + away_more + eng$tech_pts * left / 2880)]
  pre <- mean(rbind(wp_at(eng, g, 0, 1, 720, 0, 0, "PST"), wp_at(eng, g, 1, 1, 720, 0, 1, "PST"))$wp)

  # the lines run to the final buzzer for a finished game, to the last possession start for a live one
  end_e <- if (gd$final) max(2880, period_end(max(s$q))) else max(s$e)
  hwin <- as.integer(gm$hscr > gm$ascr)
  wl <- rbind(s[, .(e, model = "New", p = wp)], s[!is.na(wpl), .(e, model = "Current", p = wpl)])
  if (gd$final) wl <- rbind(wl, data.table(e = end_e, model = c("New", "Current"), p = hwin))
  wl[, model := factor(model, c("New", "Current"))]
  lead <- s[, .(xmin = e, xmax = shift(e, -1, fill = end_e), y = m)]
  pmd <- rbind(s[, .(e, pm)], if (gd$final) data.table(e = end_e, pm = gm$hscr - gm$ascr))

  span <- max(2880, end_e)
  bounds <- c(seq(0, 2880, 720), if (span > 2880) seq(3180, span, 300))
  mids <- (head(bounds, -1) + tail(bounds, -1)) / 2 / 60
  plabs <- c("Q1", "Q2", "Q3", "Q4", if (length(bounds) > 5) c("OT", paste0(seq_len(length(bounds) - 6) + 1, "OT")))
  xs <- scale_x_continuous(breaks = mids, labels = plabs, limits = c(0, span / 60), expand = expansion(mult = 0.01))
  quarter_lines <- geom_vline(xintercept = bounds[-c(1, length(bounds))] / 60, colour = "grey88", linewidth = 0.4)
  th <- theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank(), legend.position = "top", legend.justification = "left",
          plot.title.position = "plot")
  col <- c(New = "#2a78d6", Current = "#eb6834")

  status <- if (gd$final) "final" else sprintf("%s, live as of the last database update", clock(gd$now$e))
  title <- sprintf("%s %d at %s %d (%s)", gm$atm, gm$ascr, gm$htm, gm$hscr, status)
  sub <- sprintf("%s  ·  %s, total %s  ·  pregame %s win: new %.0f%%, current %s",
                 sub(" 0", " ", format(gm$date, "%b %d, %Y")), fav(gm), ifelse(is.na(gm$total), "n/a", gm$total), gm$htm,
                 100 * pre, ifelse(is.na(s$wpl[1]), "n/a", sprintf("%.0f%%", 100 * s$wpl[1])))

  p1 <- ggplot(wl, aes(e / 60, p, colour = model)) + quarter_lines +
    geom_hline(yintercept = 0.5, colour = "grey70", linewidth = 0.4) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = col, labels = c(New = "New possession model", Current = "Current model (wpl)"), name = NULL) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    xs + labs(title = title, subtitle = sub, x = NULL, y = paste(gm$htm, "win probability")) + th

  p2 <- ggplot() + quarter_lines +
    geom_rect(data = lead, aes(xmin = xmin / 60, xmax = xmax / 60, ymin = 0, ymax = y), fill = "grey80") +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    { if (!is.na(gm$line)) geom_hline(yintercept = -gm$line, colour = "grey45", linetype = "dashed", linewidth = 0.5) } +
    geom_line(data = pmd, aes(e / 60, pm), colour = col[["New"]], linewidth = 0.7) +
    xs + labs(x = NULL, y = paste(gm$htm, "lead"),
              caption = "Grey: the lead. Blue: the new model's projected final margin. Dashed: the line.") + th

  plot <- p1 / p2 + plot_layout(heights = c(3, 2))
  if (!is.null(file)) ggsave(path.expand(file), plot, width = width, height = height, dpi = 150, bg = "white")
  if (show && is.null(file)) print(plot)

  # summary
  ll <- function(p) { p <- pmin(pmax(p, 1e-4), 1 - 1e-4); -mean(hwin * log(p) + (1 - hwin) * log(1 - p), na.rm = TRUE) }
  gap <- s[, abs(wp - wpl)]; i <- which.max(gap); z <- s[.N]
  cat(sprintf("%s\n%s\n", title, sub))
  cat(sprintf("Team ratings: %s. %d possessions charted%s.\n", tn$source, nrow(s),
              if (gd$skipped) sprintf(", %d unreadable %s skipped", gd$skipped, ifelse(gd$skipped == 1, "one", "ones")) else ""))
  if (length(i)) cat(sprintf("Biggest gap: %.1f points at %s (new %.1f%%, current %.1f%%)\n",
                             100 * gap[i], clock(s$e[i]), 100 * s$wp[i], 100 * s$wpl[i]))
  if (gd$final) {
    cat(sprintf("Log loss over the game: new %.3f, current %.3f\n", ll(s$wp), ll(s$wpl)))
  } else {
    cat(sprintf("Latest possession (%s, %s %d, %s %d): %s wins new %.1f%%, current %s. New model projects %s, total %.1f\n",
                clock(z$e), gm$atm, z$a, gm$htm, z$h, gm$htm, 100 * z$wp,
                ifelse(is.na(z$wpl), "n/a", sprintf("%.1f%%", 100 * z$wpl)), by_text(z$pm, gm), z$pt))
  }
  invisible(list(plot = plot, states = s, game = gm, inputs = g, final = gd$final))
}
