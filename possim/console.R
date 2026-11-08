# possim/console.R - a betting console: the model against Kalshi at each stoppage, and a
# button for each recommended order
#
#   Rscript console.R                        paper: tonight's games, nothing sent to Kalshi
#   Rscript console.R live                   real orders become possible once armed in the page
#   Rscript console.R replay 2026-11-04 [speed]   a past night's games, against the Kalshi quotes
#                                            saved for them, played back at `speed` x (default 5)
#   Rscript console.R replay 0022500672 [speed]   one past game
#
# It opens http://127.0.0.1:8765 (this machine only; port=8766 for another). Along the top is a
# scoreboard of every game, read from the NBA's feed every few seconds. A game whose play is
# stopped, with time left to bet it, shows in amber with that time. Click a game to bet it: for
# that game the console also reads Kalshi's prices, and at each stoppage works out the bets the
# same way bet_live.R does, each with a button. Every game is set up (the model anchored to its
# line and total) in the background when the console starts, so switching is quick.
#
# Guardrails:
#   - Real orders only in `live` mode, and only after arming in the page by typing bet (Enter
#     arms). Any failed order disarms it. Switching games disarms it, unless "Stay armed across
#     games" is ticked.
#   - One click opens a confirmation; nothing is sent until it's confirmed. With "One-click
#     orders" ticked (live: after confirming that once), a Buy or Sell button sends its order
#     straight away; every other check here still applies. One order per button, once.
#   Both boxes start unticked unless console_settings says otherwise; untick to go back.
#   - Every order is a limit at the price shown, immediate-or-cancel: it fills now at that
#     price or better, or not at all. Nothing is ever left resting on Kalshi's book.
#   - Sells are reduce-only: they can close a position, never flip it.
#   - Spending limits (console_settings): per order, per game and per night, counted net of
#     anything sold, from the order log (and, live, from Kalshi's own positions for the game).
#     An order is cut to fit what's left; sells aren't limited.
#   - A stoppage's orders can be clicked only until play resumes in the feed or `valid_for`
#     seconds pass from when the console first saw it, for any game (at a break between
#     periods: until `break_open` seconds after the period ended), and not at all if the
#     stoppage reached the feed more than `max_seen` seconds late. The feed runs 20-60 seconds
#     behind, so watch the game: if play has restarted, don't click.
#
# Orders are logged to data_dir/live/<date>_orders.csv (replays to <date>_replay_orders.csv).

here <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "bet_live.R"))
source(file.path(here, "kalshi_trade.R"))
suppressPackageStartupMessages(library(shiny))

console_settings <- modifyList(live_settings, list(
  bankroll      = 500,    # Kelly sizes a game against this, less what the game has cost so far
  max_order     = 30,     # most one order can cost, fee included (also Kelly's most per market per stoppage)
  max_game      = 300,     # most one game's orders can cost in all, net of anything sold
  max_night     = 500,    # most tonight's orders can cost in all, net of anything sold
  max_contracts = 500,    # most contracts in one order
  # every kind of market in every quarter. bet_live.R's default (win markets from Q4, totals from
  # Q3) was picked from the same 2025-26 backtest it's judged on, so it may be fitting noise
  bet_when      = list(game = 1:5, spread = 1:5, total = 1:5),
  valid_for     = 180,     # seconds a stoppage's orders stay clickable
  # breaks between periods last longer (NBA: 130 s between quarters and before overtime, 15
  # minutes at halftime): their orders stay clickable, and a late one still counts, until this
  # many seconds after the period ended
  break_open    = c(quarter = 120, half = 840),
  poll          = 4,      # seconds between reads of the feed and Kalshi
  # the two boxes in the page, as they start. Both FALSE is the safer way: every order confirmed,
  # and the console disarmed whenever the game changes
  one_click     = FALSE,  # a Buy or Sell button sends its order at once, with no confirmation
  stay_armed    = FALSE)) # switching games leaves the console armed

args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("live" %in% args) "live" else if ("replay" %in% args) "replay" else "paper"
replay_pick <- if (run_mode == "replay") grep("^00[0-9]{8}$|^[0-9]{4}-[0-9]{2}-[0-9]{2}$", args, value = TRUE)[1] else NA
replay_speed <- if (run_mode == "replay") { v <- suppressWarnings(as.numeric(args[!grepl("^00|^replay$|=|^[0-9]{4}-", args)]))
                                            v <- v[!is.na(v)]; if (length(v)) v[1] else 5 } else 1
if (run_mode == "replay" && is.na(replay_pick)) stop("replay needs a date or a game id, e.g. Rscript console.R replay 2026-11-04")
kalshi_dir <- path.expand(Sys.getenv("KALSHI_DIR", "~/Dropbox/Nba/kalshi"))
port <- { a <- grep("^port=", args, value = TRUE); if (length(a)) as.integer(sub("port=", "", a[1])) else 8765L }

# ── The order log ────────────────────────────────────────────────────────────
# cash: what the order did to cash, negative for a buy (price and fee), positive for a sell
orders_file <- live_file(if (run_mode == "replay") "replay_orders.csv" else "orders.csv")
# What the model said about each order, and when, so a night's results can be set against what
# the model expected:
#   p_model     the model's probability that `outcome` (the side bought) wins
#   edge        p_model minus the limit price and fee, per contract (negative for most sells)
#   q, clock    the stoppage: period and game clock
#   t_stop      when it happened; seen_after: seconds until the console saw it (feed delay)
#   click_after seconds from the orders being worked out to this order being sent
#   checks      1 for the first look at the stoppage, 2+ after Recheck prices
log_cols <- data.table(time = character(), mode = character(), gid = character(), stop = character(), ticker = character(),
                       bet = character(), action = character(), outcome = character(), count = numeric(), limit = numeric(),
                       filled = numeric(), avg_price = numeric(), fee = numeric(), cash = numeric(), status = character(),
                       order_id = character(), note = character(), p_model = numeric(), edge = numeric(), q = numeric(),
                       clock = character(), t_stop = character(), seen_after = numeric(), click_after = numeric(), checks = numeric())
read_orders <- function() {
  if (!file.exists(orders_file)) return(copy(log_cols))
  chr <- intersect(c("time", "gid", "stop", "order_id", "note", "clock", "t_stop"), names(fread(orders_file, nrows = 0)))
  o <- fread(orders_file, colClasses = list(character = chr))
  if (nrow(o) == 0) return(copy(log_cols))
  for (cn in setdiff(names(log_cols), names(o))) set(o, j = cn, value = log_cols[[cn]][NA_integer_])   # a log from before a column was added
  o
}
log_order <- function(row) {
  # a log written before the newer columns: rewrite it with them (empty) first, so rows line up
  if (file.exists(orders_file) && !identical(names(fread(orders_file, nrows = 0)), names(log_cols)))
    fwrite(read_orders()[, names(log_cols), with = FALSE], orders_file)
  fwrite(row[, names(log_cols), with = FALSE], orders_file, append = file.exists(orders_file))
}

net_cost <- function(o, g = NULL) {                 # spent, net of sells, by this mode (and game)
  o <- o[o$mode == run_mode]
  if (!is.null(g)) o <- o[o$gid == g]
  x <- -sum(o$cash, na.rm = TRUE)
  if (abs(x) < 0.005) 0 else x
}
paper_book <- function(o, g, bankroll) {
  l <- o[o$mode == run_mode & o$gid == g & o$filled > 0]
  list(cash = bankroll + sum(l$cash),
       positions = if (nrow(l)) l[, .(position = sum(fifelse(outcome == "yes", filled, -filled))), by = ticker][position != 0]
                   else data.table(ticker = character(), position = numeric()))
}

# the most contracts at `price` that fit `budget`, fee included
fit_count <- function(n, price, budget) {
  if (is.na(budget) || budget <= 0 || is.na(price) || price <= 0) return(0)
  k <- min(n, floor(budget / price))
  while (k > 0 && k * price + kfee(price, k) > budget + 1e-9) k <- k - 1
  max(k, 0)
}

# ── Turning Kelly's orders into rows to click ────────────────────────────────
# Kelly says "buy N of this side". Against a held position on the other side, the first
# part of that sells what's held (a reduce-only order) and any rest is a new bet.
order_rows <- function(o, q, home, away) {
  if (is.null(o) || nrow(o) == 0) return(data.table())
  o <- merge(o, q[, .(ticker, kind, x, yes_over)], by = "ticker", sort = FALSE)
  rows <- rbindlist(lapply(seq_len(nrow(o)), function(j) {
    r <- o[j]; held <- if (is.null(r$held)) 0 else r$held
    closes <- (r$side == "no" && held > 0) || (r$side == "yes" && held < 0)
    n_sell <- if (closes) min(r$contracts, abs(held)) else 0
    parts <- list()
    if (n_sell > 0) parts[[1]] <- data.table(action = "SELL", n = n_sell, held_side = if (held > 0) "yes" else "no")
    if (r$contracts > n_sell) parts[[length(parts) + 1]] <- data.table(action = "BUY", n = r$contracts - n_sell, held_side = NA_character_)
    cbind(r[rep(1, length(parts)), .(ticker, kind, x, yes_over, outcome = side, price, p_win, edge)], rbindlist(parts))
  }))
  rows[, bet := fifelse(action == "SELL", bet_label(kind, x, yes_over, held_side, home, away),
                        bet_label(kind, x, yes_over, outcome, home, away))]
  rows[, `:=`(row = .I, status = "")]
  rows[]
}

# ── Replays: past games' play-by-play and saved Kalshi minutes ───────────────
# pick: a game id, or a date for every game that night. By game id: the game's row from the
# database (gd: teams, line, total, who won the tip), its actions, and Kalshi's minute quotes
replay_load <- function(pick) {
  mk <- fread(file.path(kalshi_dir, "markets.csv"), colClasses = list(character = "gid"))
  g <- unique(mk[!is.na(gid) & nzchar(gid), .(gid, date = as.Date(date), away, home)])
  g <- if (grepl("^00", pick)) g[gid == pick] else g[date == as.Date(pick)]
  g[, f := file.path(kalshi_dir, "minutes", sprintf("%s_%s%s.csv", date, away, home))]
  g <- g[file.exists(f)]
  out <- lapply(seq_len(nrow(g)), function(i) {
    gd <- tryCatch(game_states(g$gid[i]), error = function(e) NULL)
    if (is.null(gd)) return(NULL)
    act <- game_actions(list(gid = g$gid[i], game = gd$game))
    if (nrow(act) == 0) return(NULL)
    list(gd = gd, act = act, qm = fread(g$f[i]))
  })
  out <- setNames(out, g$gid)[!vapply(out, is.null, TRUE)]
  if (length(out) == 0) stop("no game with saved Kalshi minutes and play-by-play for ", pick)
  out
}
replay_quotes <- function(qm, t_now) {
  tq <- qm[t <= t_now, max(t, -Inf)]
  if (!is.finite(tq)) return(data.table())
  qm[t == tq & !is.na(bid) & !is.na(ask), .(ticker, kind, x, yes_over, yes_bid = bid,
     yes_ask = fifelse(ask < 1, ask, NA_real_), no_ask = fifelse(bid > 0, 1 - bid, NA_real_),
     yes_ask_size = Inf, no_ask_size = Inf, mid, status = "active")]
}

game_over <- function(a) a$actionType == "game" ||
  (a$actionType == "period" && a$subType == "end" && a$period >= 4 && a$scoreHome != a$scoreAway)

line_now <- function(q) {      # Kalshi's win price, and the margin and total its ladders imply, from tight quotes
  if (is.null(q) || nrow(q) == 0) return(c(wp = NA, margin = NA, total = NA))
  two <- q[!is.na(yes_bid) & !is.na(yes_ask) & yes_bid > 0 & yes_ask < 1 & yes_ask - yes_bid <= 0.10]
  c(wp = q[kind == "game" & yes_over == 1, mid][1],
    margin = implied_line(two[kind != "total", x], two[kind != "total", fifelse(yes_over == 1, mid, 1 - mid)]),
    total = implied_line(two[kind == "total", x], two[kind == "total", mid]))
}

# ── Every game's feed, for the scoreboard ────────────────────────────────────
# The game being bet needs its whole play-by-play (~400 KB late in a game). The others only need
# where they stand, so just the file's last 25 KB is read for them (about 25 actions, more than
# the substitutions at any stoppage). Requests run in the background, so the page never waits.
feed_req <- function(gid, tail = FALSE, src = 1L) {
  r <- httr2::request(sprintf(nba_feed[src], gid)) |> httr2::req_user_agent("hoops-possim-live") |> httr2::req_timeout(8)
  if (tail) httr2::req_headers(r, Range = "bytes=-25000") else r
}
parse_feed <- function(txt, gm, tail = FALSE) {
  if (!tail) return(feed_table(jsonlite::fromJSON(txt, flatten = TRUE)$game$actions, gm))
  i <- regexpr('{"actionNumber"', txt, fixed = TRUE)        # the first whole action in the tail
  if (i < 0) return(NULL)
  body <- sub("\\]\\s*\\}\\s*\\}\\s*$", "", substring(txt, i))  # less the file's closing ]}}
  feed_table(jsonlite::fromJSON(paste0("[", body, "]"), flatten = TRUE), gm)
}

# When a stoppage's orders can be clicked. seen: when the console first saw the stoppage, for
# any game (the scoreboard watches them all, so a game picked later still counts from then).
#   a break between periods: until break_open seconds after the period ended
#   anything else: valid_for seconds from being seen, and not at all if seen more than max_seen
#   seconds after it happened
stop_window <- function(last, seen, s) {
  brk <- last$actionType == "period" && last$subType == "end"
  until <- if (brk) last$t + s$break_open[[if (last$period == 2) "half" else "quarter"]] else seen + s$valid_for
  list(brk = brk, until = until, seen_after = seen - last$t, late = if (brk) seen > until else seen - last$t > s$max_seen)
}
per_name <- function(q) if (q <= 4) paste0("Q", q) else paste0("OT", q - 4)
stop_label <- function(last) switch(last$actionType, timeout = "Timeout", instantreplay = "Review",
  freethrow = "Free throws", foul = "Free throws",
  period = if (last$period == 2) "Halftime" else paste("End of", per_name(last$period)), last$actionType)
mmss <- function(secs) sprintf("%d:%02d", as.integer(secs) %/% 60L, as.integer(secs) %% 60L)

# ── Setting games up in the background ───────────────────────────────────────
# A setup takes about 10 s, nearly all of it anchoring the model to the game's line and total,
# so each game is set up in a forked process and the page never waits on it. Only the small
# parts come back; the model itself is the one this process already holds.
# f is built here, before the fork: a forked process can't read the session's reactive values.
setup_job <- function(gid, f) {
  force(f)
  parallel::mcparallel({
    su <- f(); list(game = su$game, g = su$g, src = su$line_source %||% "gms", ratings = su$ratings, tip = su$tip)
  }, name = gid, silent = TRUE)
}
build_su <- function(x, tip = x$tip, setup_t = as.numeric(Sys.time())) {
  eng <- chart_engine()
  su <- list(gid = x$game$gid, game = x$game, eng = eng, g = x$g, tt = team_terms(eng, x$g), tip = tip, ratings = x$ratings)
  su$line_source <- x$src; su$setup_t <- setup_t
  su
}
max_jobs <- max(1L, parallel::detectCores() - 2L)

# ── The page ─────────────────────────────────────────────────────────────────
css <- "
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; background: #f6f7f9; color: #1d2330; }
.bar { display: flex; gap: 14px; align-items: center; flex-wrap: wrap; padding: 10px 0; }
.badge-mode { font-weight: 700; padding: 6px 12px; border-radius: 6px; color: white; letter-spacing: .03em; }
.m-paper { background: #3b6fd8; } .m-replay { background: #7a4fd0; } .m-disarmed { background: #6b7280; } .m-armed { background: #d32f2f; }
.card { background: white; border: 1px solid #e3e6eb; border-radius: 10px; padding: 14px 16px; margin-bottom: 14px; }
.score { font-size: 30px; font-weight: 700; } .sub { color: #5b6474; }
.stopped { color: #b45309; font-weight: 700; } .playing { color: #15803d; font-weight: 700; } .final { color: #374151; font-weight: 700; }
table.t { width: 100%; border-collapse: collapse; font-variant-numeric: tabular-nums; }
table.t th { text-align: left; font-weight: 600; color: #5b6474; border-bottom: 1px solid #e3e6eb; padding: 6px 8px; font-size: 13px; }
table.t td { padding: 7px 8px; border-bottom: 1px solid #f0f2f5; }
.sell { color: #b45309; font-weight: 700; } .buy { color: #15803d; font-weight: 700; }
.why { color: #6b7280; font-size: 12px; }
.budget { font-variant-numeric: tabular-nums; }
.bar .form-group, .bar .checkbox { margin: 0; } .bar .checkbox label { font-weight: 600; }
.m-oneclick { background: #f59e0b; color: #1d2330; }
.warn { background: #fff4e5; border: 1px solid #f5c27a; border-radius: 8px; padding: 8px 12px; }
.board { display: flex; gap: 8px; flex-wrap: wrap; margin: 2px 0 14px; }
.tile { background: white; border: 1px solid #e3e6eb; border-radius: 8px; padding: 6px 10px; min-width: 158px;
        cursor: pointer; user-select: none; line-height: 1.35; }
.tile:hover { border-color: #9aa3b2; }
.tile.open { background: #fff4e5; border-color: #f59e0b; }
.tile.sel { outline: 2px solid #3b6fd8; outline-offset: -1px; }
.tile.final { opacity: .55; }
.tile .tm { font-weight: 700; font-variant-numeric: tabular-nums; white-space: nowrap; }
.tile .st { font-size: 12px; color: #5b6474; white-space: nowrap; }
.tile .flag { font-size: 12px; font-weight: 700; color: #b45309; white-space: nowrap; font-variant-numeric: tabular-nums; }
"
js <- "
// a scoreboard tile picks its game. On mousedown: the board redraws every second, and a click
// whose press and release land on different copies of a tile would be lost
$(document).on('mousedown', '.tile', function(e) {
  if (e.button === 0) Shiny.setInputValue('pick', $(this).attr('data-gid'), {priority: 'event'});
});
// arming: the word goes with the click (or Enter), so a word typed just before can't be missed
function armTry() { Shiny.setInputValue('arm_try', {word: $('#arm_word').val() || '', n: Math.random()}, {priority: 'event'}); }
$(document).on('keydown', '#arm_word', function(e) { if (e.key === 'Enter') { e.preventDefault(); armTry(); } });
$(document).on('shown.bs.modal', '#shiny-modal', function() { $('#arm_word').trigger('focus'); });
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css)), tags$title("possim console"), tags$script(HTML(js))),
  div(class = "bar",
      uiOutput("mode_badge", inline = TRUE),
      if (run_mode == "live") tagList(actionButton("arm", "Arm real orders", class = "btn-danger"),
                                  actionButton("disarm", "Disarm", class = "btn-secondary"),
                                  checkboxInput("stay_armed", "Stay armed across games", value = isTRUE(console_settings$stay_armed)),
                                  actionButton("check", "Check Kalshi connection", class = "btn-outline-secondary"),
                                  actionButton("fund", "Fund NBA shard", class = "btn-outline-secondary")),
      if (run_mode == "replay") tagList(actionButton("pause", "Pause / resume"), actionButton("next_stop", "Jump to next stoppage")),
      checkboxInput("one_click", "One-click orders", value = isTRUE(console_settings$one_click)),
      uiOutput("budget", inline = TRUE)),
  uiOutput("board"),
  uiOutput("conn"),
  fluidRow(
    column(5, div(class = "card", uiOutput("game_card"))),
    column(7, div(class = "card", uiOutput("lines")))),
  div(class = "card", uiOutput("orders")),
  fluidRow(
    column(6, div(class = "card", h5("Positions in this game"), uiOutput("positions"))),
    column(6, div(class = "card", h5("Orders this game"), uiOutput("log")))))

server <- function(input, output, session) {
  s <- console_settings
  rv <- reactiveValues(games = data.table(), sel = NULL, su = NULL, act = NULL, q = NULL, dec = NULL, armed = FALSE,
                       orders = read_orders(), kpos = NULL, conn = NULL, err = NULL, pending = NULL, open = FALSE,
                       one_click = isTRUE(s$one_click), stay_armed = isTRUE(s$stay_armed), redraw = 0)
  env <- new.env()                     # state that shouldn't trigger redraws
  # each game's anchoring (line, total, tip, and the model's fit to them), so a restart needn't
  # redo it. Just those: a whole setup carries the model and takes minutes to save
  cache_f <- live_file("console_games.rds")
  env$cache <- if (file.exists(cache_f)) readRDS(cache_f) else list()
  remember <- function(su) {
    env$cache[[su$gid]] <- list(game = su$game, tip = su$tip, src = su$line_source, setup_t = su$setup_t, g = su$g, ratings = su$ratings)
    saveRDS(env$cache, cache_f)
  }
  env$done <- character(); env$paused <- FALSE; env$last_real <- Sys.time(); env$games_t <- as.numeric(Sys.time())
  env$board <- list()       # by game: its latest action (not a substitution) and any feed error
  env$seen <- list()        # by stoppage ("gid orderNumber"): when the console first saw it
  env$decs <- list()        # by game: the latest stoppage's orders, kept while another game is picked
  env$setups <- list(); env$jobs <- list(); env$job_t <- list(); env$queue <- character(); env$setup_err <- list()
  env$inflight <- list(); env$src <- list()
  now_t <- function() if (run_mode == "replay") env$sim_t else as.numeric(Sys.time())
  # a promise callback runs outside the session: give it the session back, and log its errors
  quietly <- function(expr) withReactiveDomain(session, isolate(tryCatch(expr, error = function(e) message("console: ", conditionMessage(e)))))
  game_row <- function(id) { g <- rv$games; g[g$gid == id] }

  # ── setting games up ──
  setup_fun <- function(id) {
    if (run_mode == "replay") { gd <- env$rp[[id]]$gd; return(function() setup_game(gd$game, gd$tip)) }
    row <- game_row(id)
    function() game_setup(row, list())
  }
  queue_setup <- function(id) {
    if (!is.null(env$jobs[[id]]) || id %in% env$queue) return()
    env$queue <- c(env$queue, id)
  }
  collect_setups <- function() {
    while (length(env$queue) && length(env$jobs) < max_jobs) {
      id <- env$queue[1]; env$queue <- env$queue[-1]
      env$jobs[[id]] <- setup_job(id, setup_fun(id)); env$job_t[[id]] <- as.numeric(Sys.time())
    }
    if (length(env$jobs) == 0) return()
    res <- parallel::mccollect(env$jobs, wait = FALSE)
    pids <- vapply(env$jobs, function(j) as.character(j$pid), "")
    for (nm in names(res)) {
      id <- if (nm %in% names(env$jobs)) nm else names(pids)[pids == nm][1]
      if (is.na(id)) next
      env$jobs[[id]] <- NULL; x <- res[[nm]]
      if (is.null(x) || inherits(x, "try-error")) {
        msg <- if (inherits(x, "try-error")) conditionMessage(attr(x, "condition")) else "no result"
        env$setup_err[[id]] <- list(msg = msg, t = as.numeric(Sys.time()))
        message(sprintf("%s: setup failed (%s); trying again in a minute", id, msg))
        next
      }
      env$setup_err[[id]] <- NULL
      old <- env$setups[[id]]
      su <- build_su(x, tip = if (!is.null(old) && !is.na(old$tip)) old$tip else x$tip)
      env$setups[[id]] <- su
      if (run_mode != "replay") remember(su)
      message(sprintf("%s at %s: ready, anchored to %s %+.1f, total %.1f (%s)", su$game$atm, su$game$htm, su$game$htm,
                      su$game$line, su$game$total, su$line_source))
      if (identical(id, rv$sel)) rv$su <- su
    }
    # a setup that never reported back (its process died): try again
    for (id in names(env$jobs)) if (as.numeric(Sys.time()) - env$job_t[[id]] > 180) { env$jobs[[id]] <- NULL; queue_setup(id) }
  }
  # every game that isn't over, at once: from the cache when it has the model's fit, else in the background
  setup_all <- function() {
    g <- rv$games
    for (id in g$gid[g$status != 3]) {
      if (!is.null(env$setups[[id]])) next
      cg <- env$cache[[id]]
      if (!is.null(cg$g)) env$setups[[id]] <- build_su(list(game = cg$game, g = cg$g, src = cg$src, ratings = cg$ratings),
                                                      tip = cg$tip, setup_t = cg$setup_t)
      else queue_setup(id)
    }
  }
  # failed setups again after a minute (often: no line yet), and a game set up from Kalshi's prices
  # before its tip again once it has tipped, anchored to the prices at the tip
  retry_setups <- function() {
    now <- as.numeric(Sys.time())
    done <- rv$games$gid[rv$games$status == 3]
    for (id in intersect(names(env$setup_err), done)) env$setup_err[[id]] <- NULL     # over: nothing left to bet
    for (id in names(env$setup_err)) if (now - env$setup_err[[id]]$t > 60) { env$setup_err[[id]]$t <- Inf; queue_setup(id) }
    for (id in names(env$setups)) {
      su <- env$setups[[id]]; row <- game_row(id)
      if (nrow(row) && row$status != 3 && grepl("^Kalshi", su$line_source %||% "") && (su$setup_t %||% 0) < row$tip_t && now > row$tip_t)
        queue_setup(id)
    }
  }

  # ── the games ──
  if (run_mode == "replay") {
    message("Loading the replay...")
    env$rp <- replay_load(replay_pick)
    first <- vapply(env$rp, function(r) r$act$t[1], 0)
    env$sim_t <- min(first) - 60
    rv$games <- data.table(gid = names(env$rp), status = 1L, tip_t = first,
                           atm = vapply(env$rp, function(r) r$gd$game$atm, ""), htm = vapply(env$rp, function(r) r$gd$game$htm, ""))[order(tip_t)]
    for (id in names(env$rp)) queue_setup(id)
  } else {
    rv$games <- tryCatch(todays_games(), error = function(e) data.table())
    isolate(setup_all())
  }
  # the list of games, and whether each has started or finished, from the NBA's scoreboard file
  refresh_games <- function() {
    env$games_t <- as.numeric(Sys.time())
    tg <- tryCatch(todays_games(), error = function(e) NULL)
    if (!is.null(tg) && nrow(tg)) { rv$games <- tg; setup_all() }
  }

  # ── reading the feeds ──
  # where a game stands: its latest action that isn't a substitution or a note. A stoppage is
  # timed from the first time any read here shows it
  board_note <- function(id, act) {
    if (is.null(act) || nrow(act) == 0) return()
    last <- act[!actionType %in% admin_only][.N]
    if (nrow(last) == 0) return()
    key <- paste(id, last$orderNumber)
    if (is_stoppage(last) && is.null(env$seen[[key]])) env$seen[[key]] <- now_t()
    env$board[[id]] <- list(last = last, key = key, err = NULL)
  }
  # one read of a game's feed, in the background: the whole file for the game being bet (and then
  # its orders), the tail for the rest
  fetch_feed <- function(id) {
    if (isTRUE(env$inflight[[id]])) return(invisible())
    su <- env$setups[[id]]
    full <- identical(id, rv$sel) && !is.null(su)
    gm <- if (full) su$game else game_row(id)
    src <- env$src[[id]] %||% 1L
    env$inflight[[id]] <- TRUE
    promises::then(httr2::req_perform_promise(feed_req(id, tail = !full, src = src)),
      onFulfilled = function(resp) quietly({
        env$inflight[[id]] <- FALSE
        act <- parse_feed(httr2::resp_body_string(resp), gm, tail = !full)
        board_note(id, act)
        if (full && identical(id, rv$sel)) poll_once(act)
      }),
      onRejected = function(e) quietly({
        env$inflight[[id]] <- FALSE
        env$src[[id]] <- 3L - src                      # the NBA's other copy of the file next time
        env$board[[id]]$err <- conditionMessage(e)
      }))
    invisible()
  }
  is_final <- function(id) { b <- env$board[[id]]; !is.null(b$last) && game_over(b$last) }
  poll_cycle <- function() {
    collect_setups()
    if (run_mode == "replay") {
      if (!env$paused) env$sim_t <- env$sim_t + as.numeric(difftime(Sys.time(), env$last_real, units = "secs")) * replay_speed
      env$last_real <- Sys.time()
      for (id in names(env$rp)) {
        act <- env$rp[[id]]$act[t <= env$sim_t]
        board_note(id, act)
        if (identical(id, rv$sel)) poll_once(act)
      }
      return()
    }
    if (as.numeric(Sys.time()) - env$games_t > 30) refresh_games()
    retry_setups()
    g <- rv$games; now <- as.numeric(Sys.time())
    if (nrow(g) == 0) return()
    # games under way, about to start, or just finished (read once more, for the final score)
    watch <- g$gid[g$status == 2 | (g$status == 1 & now > g$tip_t - 300) |
                   (g$status == 3 & !vapply(g$gid, is_final, TRUE)) | g$gid %in% rv$sel]
    for (id in watch) fetch_feed(id)
  }

  # ── the game being bet ──
  # from a read of its whole feed: Kalshi's prices, who won the tip, and at a new stoppage the orders
  poll_once <- function(act) {
    su <- rv$su
    if (is.null(su) || is.null(act) || nrow(act) == 0) return()
    q <- if (run_mode == "replay") replay_quotes(env$rp[[su$gid]]$qm, env$sim_t) else tryCatch(kalshi_quotes(su$game), error = function(e) NULL)
    if (!is.null(q)) rv$q <- q
    if (is.na(su$tip)) {
      p1 <- act[period == 1 & !is.na(possession)][1, possession]
      if (!is.na(p1)) { su$tip <- as.integer(p1 != act$home_id[1]); rv$su <- su; env$setups[[su$gid]] <- su
        if (run_mode != "replay") remember(su) }
    }
    rv$act <- act
    dec <- rv$dec
    # play resumed since the stoppage: its orders can't be clicked any more
    if (!is.null(dec) && !dec$expired && nrow(act[orderNumber > dec$stop_order & actionType %in% state_types])) {
      dec$expired <- TRUE; rv$dec <- dec
    }
    last <- act[!actionType %in% admin_only][.N]
    key <- paste(su$gid, last$orderNumber)
    if (key %in% env$done || !is_stoppage(last) || game_over(last) || is.na(su$tip) || is.null(rv$q) || nrow(rv$q) == 0) return()
    env$done <- c(env$done, key)
    make_decision(su, act, last, key)
  }
  # the orders for a stoppage, from the latest prices and positions. A recheck keeps the stoppage's
  # window and gets a new key, so its buttons (and Kalshi order ids) are new.
  make_decision <- function(su, act, last, key, prev = NULL) {
    w <- if (!is.null(prev)) prev[c("brk", "until", "late", "seen_after")] else stop_window(last, env$seen[[key]] %||% now_t(), s)
    seen_after <- w$seen_after
    book <- if (run_mode == "live") {
      kp <- tryCatch(kalshi_positions(unique(sub("-[^-]*$", "", rv$q$ticker))), error = function(e) e)
      if (inherits(kp, "error")) { rv$err <- paste("Couldn't read positions from Kalshi:", conditionMessage(kp)); return() }
      rv$kpos <- kp
      list(cash = s$bankroll - sum(kp$cash_out), positions = kp[position != 0, .(ticker, position)])
    } else paper_book(rv$orders, su$gid, s$bankroll)
    t0 <- Sys.time()
    r <- tryCatch(decide(su, act, rv$q, book, s), error = function(e) { rv$err <- paste("Decision failed:", conditionMessage(e)); NULL })
    if (is.null(r)) return()
    rv$err <- NULL
    late <- w$late
    rv$dec <- list(key = key, gid = su$gid, stop_order = last$orderNumber, info = r$info, model = r$model, book = book, near = r$near,
                   rows = order_rows(r$orders, rv$q, su$game$htm, su$game$atm),
                   t_made = now_t(), until = w$until, brk = w$brk, seen_after = seen_after, late = late,
                   expired = FALSE, secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
                   lines = line_now(rv$q), last = last, n_checks = if (is.null(prev)) 1 else prev$n_checks + 1)
    message(sprintf("%s  %s %d at %s %d  Q%d %s  %s: seen %.0f s after, %d order rows%s%s", format(Sys.time(), "%H:%M:%S"),
                    su$game$atm, last$scoreAway, su$game$htm, last$scoreHome, last$period, r$info$clock, stop_what(act),
                    seen_after, nrow(rv$dec$rows), if (late) " (late: locked)" else "", if (!is.null(prev)) " (rechecked)" else ""))
  }
  observe({
    invalidateLater(s$poll * 1000)
    isolate(tryCatch(poll_cycle(), error = function(e) message("console: ", conditionMessage(e))))
  })
  # each game's latest orders, to show again when it's picked again
  observe({ d <- rv$dec; if (!is.null(d)) env$decs[[d$gid]] <- d })

  # picking a game on the scoreboard. Its orders from earlier come back, unless play has moved on
  select_game <- function(id) {
    if (is.null(id) || !nzchar(id) || identical(id, rv$sel) || !id %in% rv$games$gid) return()
    rv$sel <- id
    if (!isTRUE(rv$stay_armed)) rv$armed <- FALSE
    rv$act <- NULL; rv$q <- NULL; rv$kpos <- NULL; rv$err <- NULL; rv$conn <- NULL
    d <- env$decs[[id]]; b <- env$board[[id]]
    if (!is.null(d) && !d$expired && !identical(b$key, paste(id, d$stop_order))) d$expired <- TRUE
    rv$dec <- d
    rv$su <- env$setups[[id]]
    if (is.null(rv$su)) queue_setup(id)
    else if (run_mode == "replay") poll_once(env$rp[[id]]$act[t <= env$sim_t])
    else fetch_feed(id)                                  # now, not at the next poll
  }
  observeEvent(input$pick, select_game(input$pick))
  if (run_mode == "replay" && length(env$rp) == 1) isolate(select_game(names(env$rp)))

  if (run_mode == "replay") {
    observeEvent(input$pause, { env$paused <- !env$paused; env$last_real <- Sys.time() })
    # to the next stoppage in the game picked (with none picked, in any game)
    observeEvent(input$next_stop, {
      ids <- if (is.null(rv$sel)) names(env$rp) else rv$sel
      a <- rbindlist(lapply(env$rp[ids], function(r) r$act[!actionType %in% admin_only]))
      a <- a[is_stoppage(a) & t > env$sim_t + 1]
      a <- a[!vapply(seq_len(nrow(a)), function(i) game_over(a[i]), logical(1))]
      if (nrow(a)) { env$sim_t <- min(a$t) + 20; env$last_real <- Sys.time(); poll_cycle() }
    })
  }

  # ── arming (live only) ──
  if (run_mode == "live") {
    observeEvent(input$arm, {
      showModal(modalDialog(title = "Arm real orders?",
        HTML(sprintf(paste0("<p>Orders will be sent to Kalshi with real money, ",
                            if (rv$one_click) "<b>one click each, with no confirmation</b> (one-click orders are on).</p>"
                            else "one confirmed click at a time.</p>",
                            "<p>Limits: <b>$%d</b> an order, <b>$%d</b> a game, <b>$%d</b> tonight, net of sells. Each order is a limit at the price shown, ",
                            "immediate-or-cancel.</p><p>Type <b>bet</b> and press Enter to arm.</p>"), s$max_order, s$max_game, s$max_night)),
        tags$input(id = "arm_word", type = "text", class = "form-control", autocomplete = "off", spellcheck = "false"),
        footer = tagList(modalButton("Cancel"), tags$button(type = "button", class = "btn btn-danger", onclick = "armTry()", "Arm"))))
    })
    observeEvent(input$arm_try, {
      if (identical(tolower(trimws(input$arm_try$word)), "bet")) { rv$armed <- TRUE; removeModal() }
      else showNotification("Not armed: type bet.", type = "warning")
    })
    observeEvent(input$disarm, { rv$armed <- FALSE })
    observeEvent(input$stay_armed, { rv$stay_armed <- isTRUE(input$stay_armed) }, ignoreInit = TRUE)
    # the shard this game's markets trade on (Kalshi says so on each market; NBA has been 3)
    game_shard <- function() { v <- if (!is.null(rv$q) && !is.null(rv$q$shard)) rv$q$shard[!is.na(rv$q$shard)][1] else NA
                               if (length(v) == 0 || is.na(v)) nba_shard else as.integer(v) }
    check_conn <- function() {
      rv$conn <- tryCatch({
        b <- kalshi_balance()
        kp <- if (!is.null(rv$q) && nrow(rv$q)) kalshi_positions(unique(sub("-[^-]*$", "", rv$q$ticker))) else NULL
        if (!is.null(kp)) rv$kpos <- kp
        env$shard_cash <- shard_cash(b, game_shard())
        sprintf("Connected. Cash $%.2f in all, <b>$%.2f on the NBA shard (%d)</b>, which is what API orders can use.%s%s",
                b$cash, env$shard_cash, game_shard(),
                if (!is.null(kp)) sprintf(" %d positions in this game.", nrow(kp[position != 0])) else "",
                if (env$shard_cash < 1) " Fund the NBA shard before betting." else "")
      }, error = function(e) paste("Connection failed:", conditionMessage(e)))
    }
    observeEvent(input$check, check_conn())
    # moving cash to the NBA shard: Kalshi's app does this by itself, API orders need it done first
    observeEvent(input$fund, {
      check_conn()
      have <- env$shard_cash %||% 0
      showModal(modalDialog(title = "Move cash to the NBA shard?",
        HTML(sprintf(paste0("<p>Kalshi keeps a separate balance for each group of markets. NBA markets trade on shard %d, which has ",
                            "<b>$%.2f</b>. API orders can only use that.</p><p>This moves cash within your Kalshi account, from ",
                            "your main balance (shard 0) to shard %d. Up to $%d.</p>"), game_shard(), have, game_shard(), s$max_night)),
        numericInput("fund_amt", "Dollars to move", value = max(0, round(s$max_night - have)), min = 1, max = s$max_night, step = 5),
        footer = tagList(modalButton("Cancel"), actionButton("fund_go", "Move it", class = "btn-danger"))))
    })
    observeEvent(input$fund_go, {
      amt <- input$fund_amt
      if (is.null(amt) || is.na(amt) || amt < 1 || amt > s$max_night) {
        showNotification(sprintf("Not moved: pick between $1 and $%d.", s$max_night), type = "warning"); return()
      }
      removeModal()
      res <- tryCatch(kalshi_shard_transfer(amt, to = game_shard()), error = function(e) e)
      if (inherits(res, "error")) { showNotification(paste("Transfer failed:", conditionMessage(res)), type = "error", duration = NULL); return() }
      showNotification(sprintf("Moving $%.2f to the NBA shard. Kalshi does it within a few seconds.", amt), duration = 8)
      Sys.sleep(4); check_conn()
    })
  }

  # ── clicking an order ──
  budget_left <- function() {
    o <- rv$orders; g <- rv$su$gid
    game_cost <- net_cost(o, g)
    if (run_mode == "live" && !is.null(rv$kpos)) game_cost <- max(game_cost, sum(rv$kpos$cash_out))
    min(s$max_order, s$max_game - game_cost, s$max_night - net_cost(o))
  }
  # the night's first real order is 1 contract: check on Kalshi that it went through as meant
  first_live <- function() run_mode == "live" && !any(rv$orders$mode == "live" & rv$orders$filled > 0)
  row_count <- function(r) {
    n <- if (r$action == "SELL") r$n else min(fit_count(r$n, r$price, budget_left()), s$max_contracts)
    if (first_live()) min(n, 1) else n
  }
  # whether the latest stoppage's orders can still be clicked: a value that changes only when it
  # flips, so the order table (and its buttons) redraws only then, never under the mouse
  observe({
    invalidateLater(1000)
    dec <- rv$dec
    open <- !is.null(dec) && !dec$late && !dec$expired && now_t() <= dec$until
    if (!identical(isolate(rv$open), open)) rv$open <- open
  })
  why_not <- function(dec, r) {
    if (r$status != "") return(r$status)
    if (run_mode == "live" && !rv$armed) return("not armed")
    if (dec$late) return(sprintf("stoppage seen %.0f s late", dec$seen_after))
    if (dec$expired) return("play resumed")
    if (!isTRUE(rv$open) || now_t() > dec$until) return("too old")
    # a buy on a contract with a sell still to send would only close the held position
    if (r$action == "BUY" && nrow(dec$rows[ticker == r$ticker & action == "SELL" & status == ""])) return("sell first")
    if (row_count(r) < 1) return("over the spending limit")
    ""
  }
  observeEvent(input$recheck, {
    dec <- rv$dec
    if (is.null(dec) || !isTRUE(rv$open) || is.null(rv$act)) return()
    make_decision(rv$su, rv$act[orderNumber <= dec$stop_order], dec$last, sprintf("%s r%d", dec$key, dec$n_checks), prev = dec)
  })
  observeEvent(input$place, {
    rv$orders <- read_orders()         # another console may have spent some of tonight's limit
    dec <- rv$dec; p <- input$place
    if (is.null(dec) || !identical(p$key, dec$key)) return()
    r <- dec$rows[row == p$row]
    if (nrow(r) == 0) return()
    why <- why_not(dec, r)
    if (why != "") {
      if (!grepl("^(sending|filled|part filled|nothing filled|error)", why)) showNotification(paste("Not sent:", why), type = "warning")
      rv$redraw <- rv$redraw + 1; return()
    }
    n <- row_count(r)
    if (rv$one_click) return(send_order(list(key = dec$key, row = r$row, n = n)))
    worst <- n * r$price + kfee(r$price, n)
    what <- if (r$action == "BUY")
      sprintf("<b>Buy %d × %s</b> at %.0f¢ or better: at most <b>$%.2f</b> with the fee. Model %.0f%%, edge %+.1f¢.",
              n, r$bet, 100 * r$price, worst, 100 * r$p_win, 100 * r$edge)
    else sprintf("<b>Sell %d × %s</b> at %.0f¢ or better (reduce-only). Model gives it %.0f%%.",
                 n, r$bet, 100 * (1 - r$price), 100 * (1 - r$p_win))
    rv$pending <- list(key = dec$key, row = r$row, n = n)
    showModal(modalDialog(title = if (run_mode == "live") "Send this order to Kalshi?" else "Paper order",
      HTML(sprintf("<p>%s</p><p class='why'>Kalshi: %s, %s %s at a YES price of %.2f, immediate-or-cancel.</p>%s", what, r$ticker,
                   if (r$outcome == "yes") "bid (buy YES)" else "ask (sell YES / buy NO)", n,
                   if (r$outcome == "yes") r$price else 1 - r$price,
                   if (run_mode == "live") paste0("<p class='warn'>Real money. Make sure play is still stopped.",
                     if (first_live()) " Tonight's first real order is held to 1 contract: check it on Kalshi before the next." else "", "</p>") else "")),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm", if (run_mode == "live") "Send order" else "Fill on paper",
                                    class = if (run_mode == "live") "btn-danger" else "btn-primary")), easyClose = TRUE))
  })
  observeEvent(input$confirm, {
    removeModal()
    pd <- rv$pending; rv$pending <- NULL
    send_order(pd)
  })
  # one order, as confirmed in the dialog or (one-click) as clicked. Everything is checked again here
  send_order <- function(pd) {
    rv$orders <- read_orders()
    dec <- rv$dec
    if (is.null(pd) || is.null(dec) || !identical(pd$key, dec$key)) return()
    r <- dec$rows[row == pd$row]
    if (nrow(r) == 0 || why_not(dec, r) != "") { showNotification("Not sent: the order is no longer clickable.", type = "warning"); return() }
    n <- min(pd$n, row_count(r))                   # never more than what was confirmed
    if (n < 1 || (run_mode == "live" && !rv$armed)) return()
    rows <- copy(dec$rows); rows[row == pd$row, status := "sending"]; dec$rows <- rows; rv$dec <- dec
    rec <- data.table(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), mode = run_mode, gid = dec$gid, stop = dec$key,
                      ticker = r$ticker, bet = r$bet, action = r$action, outcome = r$outcome, count = n, limit = r$price,
                      filled = 0, avg_price = NA_real_, fee = 0, cash = 0, status = "", order_id = "", note = "",
                      p_model = round(r$p_win, 4), edge = round(r$edge, 4), q = dec$info$q, clock = dec$info$clock,
                      t_stop = format(as.POSIXct(dec$last$t, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S"),
                      seen_after = round(dec$seen_after, 1), click_after = round(now_t() - dec$t_made, 1), checks = dec$n_checks)
    if (run_mode == "live") {
      cid <- sprintf("possim-%s-%d", gsub(" ", "-", dec$key), r$row)     # e.g. possim-0022600027-456-r1-2
      res <- tryCatch(kalshi_order(order_body(r$ticker, r$outcome, n, r$price, cid, reduce_only = r$action == "SELL")),
                      error = function(e) list(ok = FALSE, status = NA, error = conditionMessage(e)))
      if (isTRUE(res$ok)) {
        rec[, `:=`(filled = res$filled, avg_price = res$avg_price, fee = res$fee, order_id = res$order_id,
                   status = if (res$filled == 0) "nothing filled" else if (res$filled < n) "part filled" else "filled")]
      } else {
        rec[, `:=`(status = "error", note = paste(res$status, res$error))]
        rv$armed <- FALSE
        showNotification(paste("Order failed, and the console is disarmed:", res$error), type = "error", duration = NULL)
      }
    } else {
      rec[, `:=`(filled = n, avg_price = r$price, fee = kfee(r$price, n), status = "filled (paper)")]
    }
    rec[filled > 0, cash := if (r$action == "BUY") -(filled * avg_price + fee) else filled * (1 - avg_price) - fee]
    log_order(rec)
    rv$orders <- read_orders()
    rows <- copy(dec$rows)
    rows[row == pd$row, status := if (rec$filled > 0) sprintf("%s %g @ %.0f¢", rec$status, rec$filled, 100 * rec$avg_price) else rec$status]
    dec$rows <- rows; rv$dec <- dec
    if (rec$status != "error")
      showNotification(sprintf("%s %d × %s: %s", if (r$action == "BUY") "Buy" else "Sell", n, r$bet,
                               if (rec$filled > 0) sprintf("%s %g @ %.0f¢", rec$status, rec$filled, 100 * rec$avg_price) else rec$status),
                       type = if (rec$filled > 0) "message" else "warning", duration = 6)
    if (run_mode == "live") rv$kpos <- tryCatch(kalshi_positions(unique(sub("-[^-]*$", "", rv$q$ticker))), error = function(e) rv$kpos)
  }
  # one-click orders: in live mode, turned on only after saying so once; off at once
  observeEvent(input$one_click, {
    on <- isTRUE(input$one_click)
    if (identical(on, rv$one_click)) return()
    if (on && run_mode == "live")
      showModal(modalDialog(title = "Turn on one-click orders?",
        HTML(paste0("<p>A Buy or Sell button will send its order to Kalshi <b>the moment it's clicked</b>, with no confirmation.</p>",
                    "<p>Everything else still holds: the console must be armed, the spending limits, the price limit, ",
                    "immediate-or-cancel, one order per button, and the time window. Untick the box to go back.</p>")),
        footer = tagList(actionButton("one_click_no", "Cancel"), actionButton("one_click_yes", "Turn on", class = "btn-danger"))))
    else rv$one_click <- on
  }, ignoreInit = TRUE)
  observeEvent(input$one_click_yes, { rv$one_click <- TRUE; removeModal() })
  observeEvent(input$one_click_no, { updateCheckboxInput(session, "one_click", value = FALSE); removeModal() })

  # ── drawing ──
  output$mode_badge <- renderUI({
    cls <- switch(run_mode, paper = "m-paper", replay = "m-replay", live = if (rv$armed) "m-armed" else "m-disarmed")
    txt <- switch(run_mode, paper = "PAPER", replay = "REPLAY · PAPER", live = if (rv$armed) "LIVE · ARMED" else "LIVE · DISARMED")
    tagList(span(class = paste("badge-mode", cls), txt), if (rv$one_click) span(class = "badge-mode m-oneclick", "ONE-CLICK"))
  })
  output$budget <- renderUI({
    o <- rv$orders
    g <- if (!is.null(rv$su)) net_cost(o, rv$su$gid) else 0
    span(class = "budget sub", sprintf("Spent tonight $%.2f of $%d · this game $%.2f of $%d · ≤ $%d an order",
                                       net_cost(o), s$max_night, g, s$max_game, s$max_order))
  })
  output$conn <- renderUI({
    msgs <- c(rv$err, rv$conn)
    if (length(msgs)) div(class = "warn", style = "margin-bottom: 12px;", HTML(paste(msgs, collapse = "<br>")))
  })
  # every game: the score, and whether play is stopped with time left to bet it (amber)
  output$board <- renderUI({
    invalidateLater(1000); g <- rv$games; sel <- rv$sel
    if (nrow(g) == 0) return(div(class = "warn", style = "margin-bottom: 12px;", "No games found on the NBA's scoreboard."))
    now <- now_t()
    div(class = "board", lapply(seq_len(nrow(g)), function(i) {
      r <- g[i]; id <- r$gid; b <- env$board[[id]]; last <- b$last; cls <- "tile"
      if (is.null(last)) {
        top <- sprintf("%s at %s", r$atm, r$htm)
        line <- span(class = "st", if (r$status == 3) "Final" else sub("^0", "", format(as.POSIXct(r$tip_t, origin = "1970-01-01"), "%I:%M %p")))
      } else {
        top <- sprintf("%s %d  %s %d", r$atm, last$scoreAway, r$htm, last$scoreHome)
        clk <- sprintf("%s %s", per_name(last$period), mmss(last$tr))
        if (game_over(last)) { cls <- c(cls, "final"); line <- span(class = "st", "Final") }
        else if (is_stoppage(last)) {
          w <- stop_window(last, env$seen[[b$key]] %||% now, s); left <- w$until - now
          if (!w$late && left >= 0) { cls <- c(cls, "open"); line <- span(class = "flag", sprintf("%s · %s to bet", stop_label(last), mmss(left))) }
          else line <- span(class = "st", sprintf("%s · %s, %s", clk, tolower(stop_label(last)), if (w$late) "seen too late" else "closed"))
        } else line <- span(class = "st", sprintf("%s · play on", clk))
      }
      if (identical(id, sel)) cls <- c(cls, "sel")
      note <- if ("final" %in% cls || r$status == 3) NULL
              else if (!is.null(env$jobs[[id]]) || id %in% env$queue) "setting up the model"
              else if (!is.null(env$setup_err[[id]])) "setup failed, retrying"
              else if (!is.null(b$err)) "couldn't read the feed"
      div(class = paste(cls, collapse = " "), `data-gid` = id, div(class = "tm", top), line, if (!is.null(note)) div(class = "st", note))
    }))
  })
  output$game_card <- renderUI({
    invalidateLater(1000); su <- rv$su; act <- rv$act
    if (is.null(rv$sel)) return(div(class = "sub", "Pick a game on the scoreboard."))
    if (is.null(su)) {
      r <- game_row(rv$sel); err <- env$setup_err[[rv$sel]]
      return(div(div(class = "score", sprintf("%s at %s", r$atm, r$htm)),
                 div(class = "sub", if (!is.null(err)) paste("Setup failed:", err$msg, "(trying again shortly)")
                                    else "Setting up the model for this game (about 10 seconds)...")))
    }
    gm <- su$game
    if (is.null(act) || nrow(act) == 0) return(div(div(class = "score", sprintf("%s at %s", gm$atm, gm$htm)),
      div(class = "sub", sprintf("Anchored to %s %+.1f, total %.1f. Waiting for the tip.", gm$htm, gm$line, gm$total))))
    last <- act[!actionType %in% admin_only][.N]
    status <- if (game_over(last)) span(class = "final", "FINAL")
      else if (is_stoppage(last)) span(class = "stopped", sprintf("STOPPED: %s", stop_what(act)))
      else span(class = "playing", "PLAY ON")
    per <- if (last$period <= 4) paste0("Q", last$period) else paste0("OT", last$period - 4)
    div(div(class = "score", sprintf("%s %d  ·  %s %d", gm$atm, last$scoreAway, gm$htm, last$scoreHome)),
        div(sprintf("%s %d:%02d   ", per, last$tr %/% 60, round(last$tr %% 60)), status),
        div(class = "sub", sprintf("Last play: %s", last$description)),
        div(class = "sub", sprintf("It happened %.0f s ago%s.  Anchored to %s %+.1f, total %.1f.", now_t() - last$t,
                                   if (run_mode == "replay") " (replay clock)" else "", gm$htm, gm$line, gm$total)))
  })
  output$lines <- renderUI({
    su <- rv$su; dec <- rv$dec
    if (is.null(su)) return(NULL)
    now <- line_now(rv$q); gm <- su$game
    f <- function(v, fmt) if (length(v) == 0 || is.na(v)) "–" else sprintf(fmt, v)
    i <- if (!is.null(dec)) dec$info else NULL
    tags$table(class = "t",
      tags$tr(tags$th(""), tags$th("Model at the stoppage"), tags$th("Kalshi at the stoppage"), tags$th("Kalshi now")),
      tags$tr(tags$td(sprintf("%s wins", gm$htm)), tags$td(f(i$wp_model * 100, "%.0f%%")), tags$td(f(dec$lines[["wp"]] * 100, "%.0f%%")),
              tags$td(f(now[["wp"]] * 100, "%.0f%%"))),
      tags$tr(tags$td(sprintf("%s margin", gm$htm)), tags$td(f(i$margin_model, "%+.1f")), tags$td(f(dec$lines[["margin"]], "%+.1f")),
              tags$td(f(now[["margin"]], "%+.1f"))),
      tags$tr(tags$td("Total"), tags$td(f(i$total_model, "%.1f")), tags$td(f(dec$lines[["total"]], "%.1f")), tags$td(f(now[["total"]], "%.1f"))))
  })
  output$orders <- renderUI({
    dec <- rv$dec; rv$open; rv$redraw; one <- rv$one_click
    if (is.null(dec)) return(div(h5("Orders"), div(class = "sub", "Nothing yet: orders appear at the next stoppage.")))
    i <- dec$info
    head <- sprintf("Orders at %s %s, %s (%s): seen %.0f s after it happened, worked out in %.1f s",
                    if (i$q <= 4) paste0("Q", i$q) else paste0("OT", i$q - 4), i$clock, i$what, i$state, dec$seen_after, dec$secs)
    # outside the timer, which redraws every second: a button there could vanish mid-click
    recheck <- if (isTRUE(rv$open)) div(style = "margin: 6px 0;", actionButton("recheck", "Recheck prices", class = "btn-sm btn-outline-secondary"),
                                        span(class = "why", " works the orders out again from Kalshi's latest prices and your positions"))
    if (nrow(dec$rows) == 0) return(div(h5(head), uiOutput("orders_timer"), recheck,
                                        div(class = "sub", sprintf("No bets. Kelly's cash for this game: $%.0f.", dec$book$cash)),
                                        near_table(dec$near, rv$su$game)))
    rows <- lapply(seq_len(nrow(dec$rows)), function(j) {
      r <- dec$rows[j]; why <- why_not(dec, r); n <- row_count(r)
      btn <- if (why == "") tags$button(class = paste("btn btn-sm", if (run_mode == "live") "btn-danger" else "btn-primary"),
        # one-click: the button goes dead at once, so a second click can't queue another order
        onclick = sprintf("%sShiny.setInputValue('place', {row: %d, key: '%s', n: Math.random()}, {priority: 'event'})",
                          if (one) "this.disabled = true; this.textContent = 'Sending...'; " else "", r$row, dec$key),
        sprintf("%s %d%s", if (r$action == "BUY") "Buy" else "Sell", n, if (one) " now" else "")) else span(class = "why", why)
      tags$tr(tags$td(span(class = tolower(r$action), r$action)), tags$td(r$bet),
              tags$td(if (r$action == "BUY") sprintf("%d of %d", n, r$n) else r$n),
              tags$td(sprintf("%.0f¢", 100 * if (r$action == "BUY") r$price else 1 - r$price)),
              tags$td(sprintf("%.0f%%", 100 * if (r$action == "BUY") r$p_win else 1 - r$p_win)),
              tags$td(if (r$action == "BUY") sprintf("%+.1f¢", 100 * r$edge) else ""),
              tags$td(if (r$action == "BUY") sprintf("$%.2f", n * r$price + kfee(r$price, n)) else ""), tags$td(btn))
    })
    div(h5(head), uiOutput("orders_timer"), recheck,
        tags$table(class = "t", tags$tr(tags$th(""), tags$th("Bet"), tags$th("Contracts"), tags$th("Price"), tags$th("Model"),
                                        tags$th("Edge"), tags$th("Cost"), tags$th("")), rows),
        if (one) div(class = "why", style = "margin-top: 8px; color: #b45309;",
                     "One-click orders are on: a Buy or Sell button sends its order as soon as it's clicked."),
        div(class = "why", style = "margin-top: 8px;",
            "Contracts are cut to fit the spending limits. Price: the most you pay per contract (buys) or the least you take (sells); fees on top."),
        near_table(dec$near[why != ""], rv$su$game))
  })
  # the best sides it passed on, and why
  near_table <- function(nr, gm) {
    if (is.null(nr) || nrow(nr) == 0) return(NULL)
    div(style = "margin-top: 12px;", div(class = "sub", "Closest bets it passed on:"),
      tags$table(class = "t", tags$tr(tags$th("Bet"), tags$th("Ask"), tags$th("Model"), tags$th("Edge after fee"), tags$th("Why not")),
        lapply(seq_len(nrow(nr)), function(j) tags$tr(
          tags$td(bet_label(nr$kind[j], nr$x[j], nr$yes_over[j], nr$side[j], gm$htm, gm$atm)),
          tags$td(sprintf("%.0f¢", 100 * nr$price[j])), tags$td(sprintf("%.0f%%", 100 * nr$p_win[j])),
          tags$td(sprintf("%+.1f¢", 100 * nr$edge[j])), tags$td(span(class = "why", nr$why[j]))))))
  }
  output$orders_timer <- renderUI({
    invalidateLater(1000); dec <- rv$dec
    if (is.null(dec)) return(NULL)
    left <- dec$until - now_t()
    if (dec$late) div(class = "warn", "Too late to bet: the stoppage reached the feed late.")
    else if (dec$expired) div(class = "warn", "Play has resumed: these can't be sent.")
    else if (left < 0) div(class = "warn", "Too old: these can't be sent.")
    else div(class = "sub", sprintf("Clickable for %d:%02d more%s, while play is stopped. Prices from %.0f s ago.",
                                    as.integer(left) %/% 60, as.integer(left) %% 60,
                                    if (dec$brk) sprintf(" (the %s)", if (dec$info$q == 2) "halftime break" else "break between periods") else "",
                                    now_t() - dec$t_made))
  })
  output$positions <- renderUI({
    su <- rv$su; q <- rv$q
    if (is.null(su)) return(NULL)
    p <- if (run_mode == "live") { if (is.null(rv$kpos)) return(div(class = "sub", "Read from Kalshi at each stoppage.")); rv$kpos[position != 0, .(ticker, position)] }
         else paper_book(rv$orders, su$gid, s$bankroll)$positions
    if (nrow(p) == 0) return(div(class = "sub", "None."))
    if (!is.null(q) && nrow(q)) p <- merge(p, q[, .(ticker, kind, x, yes_over, yes_bid, yes_ask)], by = "ticker", all.x = TRUE)
    else p[, `:=`(kind = NA_character_, x = NA_real_, yes_over = NA_integer_, yes_bid = NA_real_, yes_ask = NA_real_)]
    pm <- if (!is.null(rv$dec)) rv$dec$model else NULL
    p[, side := fifelse(position > 0, "yes", "no")]
    p[, bet := fifelse(is.na(kind), paste(ticker, toupper(side)), bet_label(kind, x, yes_over, side, su$game$htm, su$game$atm))]
    p[, bid := fifelse(side == "yes", yes_bid, 1 - yes_ask)]
    p[, model := if (!is.null(pm)) { m <- pm$p_yes[match(ticker, pm$ticker)]; fifelse(side == "yes", m, 1 - m) } else NA_real_]
    tags$table(class = "t", tags$tr(tags$th("Bet"), tags$th("Contracts"), tags$th("Bid now"), tags$th("Model")),
      lapply(seq_len(nrow(p)), function(j) tags$tr(tags$td(p$bet[j]), tags$td(abs(p$position[j])),
        tags$td(if (is.na(p$bid[j])) "–" else sprintf("%.0f¢", 100 * p$bid[j])),
        tags$td(if (is.na(p$model[j])) "–" else sprintf("%.0f%%", 100 * p$model[j])))))
  })
  output$log <- renderUI({
    su <- rv$su
    if (is.null(su)) return(NULL)
    o <- rv$orders[rv$orders$mode == run_mode & rv$orders$gid == su$gid]
    if (nrow(o) == 0) return(div(class = "sub", "None yet."))
    o <- tail(o, 12)[.N:1]
    tags$table(class = "t", tags$tr(tags$th("Time"), tags$th(""), tags$th("Bet"), tags$th("Filled"), tags$th("Cash"), tags$th("Status")),
      lapply(seq_len(nrow(o)), function(j) tags$tr(tags$td(substr(o$time[j], 12, 19)), tags$td(o$action[j]), tags$td(o$bet[j]),
        tags$td(sprintf("%g of %g%s", o$filled[j], o$count[j], if (is.na(o$avg_price[j])) "" else sprintf(" @ %.0f¢", 100 * o$avg_price[j]))),
        tags$td(sprintf("%+.2f", o$cash[j])), tags$td(o$status[j], if (nzchar(o$note[j])) span(class = "why", o$note[j])))))
  })
}

# what every game's setup shares, loaded once here so the background setups (forked from this
# process) start with it
message("Loading the model...")
invisible(chart_engine()); invisible(snapshot())
invisible(cached("ratings", function() readRDS(rds("ratings"))))
invisible(tryCatch(team_games(as.integer(format(Sys.Date() - 182, "%Y")), Sys.Date()), error = function(e) NULL))   # this season's
message(sprintf("possim console, %s mode: http://127.0.0.1:%d", run_mode, port))
runApp(shinyApp(ui, server), host = "127.0.0.1", port = port, launch.browser = !("nobrowser" %in% args))
