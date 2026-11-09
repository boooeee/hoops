# possim/bot.R - a page that runs a betting bot on one game: it bets every stoppage by itself,
# the way the console would recommend, with no clicks
#
#   Rscript bot.R                        paper: tonight's games, orders filled on paper at their limit
#   Rscript bot.R live                   real orders on Kalshi, once launched in the page
#   Rscript bot.R replay 0022600174 [speed]   a past game against its saved Kalshi minutes, on paper,
#                                        played back at `speed` x (default 10) from the launch
#
# It opens http://127.0.0.1:8770 (port=8771 for another). Pick a game, set the limits, launch.
# The bot runs only while this page is open, and only from here: there's no other way to start it.
#
# Guardrails:
#   - Launching: only from this page, for one game. Live, the launch shows every limit and the bot
#     starts only after you type the spend cap back. One bot at a time (a lock file, bot.lock), and
#     the console sends no orders in the bot's game while it runs.
#   - Limits (bot_settings in bot_lib.R; the page can lower or change them before launch):
#       spend cap   the most the bot spends on the game, net of sells, counted from Kalshi's own
#                   positions and the bot's log, whichever is more. Never above `ceiling` ($1,000),
#                   which only bot_lib.R can raise
#       bankroll    what Kelly sizes against, less what the game has cost so far
#       per order, per stoppage, 12 orders a stoppage, 40 a minute, 1,000 contracts an order
#       loss stop   stops when the game's positions, at Kalshi's mids, are down this much since launch
#   - Orders: limits at the price seen, immediate-or-cancel (nothing ever rests on Kalshi's book);
#     sells are reduce-only. One set of orders per dead ball, sent within 15 s of being worked out;
#     no rechecks. Live, the first order is 1 contract, and nothing more goes until Kalshi's
#     positions show it.
#   - What it bets: as the console, at stoppages seen in time (90 s for a timeout, 60 for anything
#     else, breaks until 2 minutes after the period ends), in each kind of market's quarters
#     (bet_when), at 2c+ edges, prices 5-95c. A game anchored to Kalshi before the tip is anchored
#     again just before the real tip, and the bot doesn't bet until that's done.
#   - It stops itself: on any failed order; if Kalshi's positions don't match what it sent (someone
#     else traded the game); on the loss stop; if positions can't be read three stoppages running;
#     on any error in its own code; when the game ends; when this page is closed or reloaded.
#   - Stopping it yourself: the Stop button, or from anywhere, create the file
#     data_dir/live/STOP_BOT (touch it). The bot stops at its next step, even between two orders.
#     Positions stay as they are; nothing is left on the book.
#
# Orders go to data_dir/live/<date>_bot_orders.csv (replays: <date>_bot_replay_orders.csv), in the
# console's columns, and what the bot did to <date>_bot_events.csv.

here <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "bet_live.R"))
source(file.path(here, "kalshi_trade.R"))

args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("live" %in% args) "live" else if ("replay" %in% args) "replay" else "paper"
replay_pick <- if (run_mode == "replay") grep("^00[0-9]{8}$", args, value = TRUE)[1] else NA
replay_speed <- if (run_mode == "replay") { v <- suppressWarnings(as.numeric(args[!grepl("^00|^replay$|=", args)]))
                                            v <- v[!is.na(v)]; if (length(v)) v[1] else 10 } else 1
if (run_mode == "replay" && is.na(replay_pick)) stop("replay needs a game id, e.g. Rscript bot.R replay 0022600174")
kalshi_dir <- path.expand(Sys.getenv("KALSHI_DIR", "~/Dropbox/Nba/kalshi"))
port <- { a <- grep("^port=", args, value = TRUE); if (length(a)) as.integer(sub("port=", "", a[1])) else 8770L }
orders_file <- live_file(if (run_mode == "replay") "bot_replay_orders.csv" else "bot_orders.csv")
events_file <- live_file(if (run_mode == "replay") "bot_replay_events.csv" else "bot_events.csv")
source(file.path(here, "console_lib.R"))
source(file.path(here, "bot_lib.R"))
suppressPackageStartupMessages(library(shiny))

css <- "
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; background: #f6f7f9; color: #1d2330; }
.bar { display: flex; gap: 14px; align-items: center; flex-wrap: wrap; padding: 10px 0; }
.badge-mode { font-weight: 700; padding: 6px 12px; border-radius: 6px; color: white; letter-spacing: .03em; }
.m-paper { background: #3b6fd8; } .m-replay { background: #7a4fd0; } .m-live { background: #d32f2f; }
.b-off { background: #6b7280; } .b-run { background: #15803d; } .b-run-live { background: #d32f2f; } .b-stop { background: #374151; }
.card { background: white; border: 1px solid #e3e6eb; border-radius: 10px; padding: 14px 16px; margin-bottom: 14px; }
.score { font-size: 26px; font-weight: 700; } .sub { color: #5b6474; }
table.t { width: 100%; border-collapse: collapse; font-variant-numeric: tabular-nums; }
table.t th { text-align: left; font-weight: 600; color: #5b6474; border-bottom: 1px solid #e3e6eb; padding: 6px 8px; font-size: 13px; }
table.t td { padding: 6px 8px; border-bottom: 1px solid #f0f2f5; font-size: 14px; }
.warn { background: #fff4e5; border: 1px solid #f5c27a; border-radius: 8px; padding: 8px 12px; margin-bottom: 10px; }
.bad { background: #fdecec; border: 1px solid #f1a7a7; border-radius: 8px; padding: 8px 12px; margin-bottom: 10px; }
.big-stop { font-size: 18px; font-weight: 700; padding: 8px 22px; }
.why { color: #6b7280; font-size: 12px; }
.kv td:first-child { color: #5b6474; width: 42%; }
.meter { height: 8px; background: #eef0f3; border-radius: 4px; overflow: hidden; margin: 4px 0 2px; }
.meter > div { height: 100%; background: #3b6fd8; }
"
js <- "
function launchTry() { Shiny.setInputValue('launch_go', {word: $('#launch_word').val() || '', n: Math.random()}, {priority: 'event'}); }
$(document).on('keydown', '#launch_word', function(e) { if (e.key === 'Enter') { e.preventDefault(); launchTry(); } });
$(document).on('shown.bs.modal', '#shiny-modal', function() { $('#launch_word').trigger('focus'); });
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css)), tags$title("possim bot"), tags$script(HTML(js))),
  div(class = "bar",
      span(class = paste("badge-mode", switch(run_mode, paper = "m-paper", replay = "m-replay", live = "m-live")),
           switch(run_mode, paper = "PAPER", replay = "REPLAY · PAPER", live = "LIVE · REAL MONEY")),
      uiOutput("bot_badge", inline = TRUE),
      actionButton("stop", "Stop the bot", class = "btn-danger big-stop"),
      if (run_mode == "live") actionButton("check", "Check Kalshi connection", class = "btn-outline-secondary"),
      span(class = "why", sprintf("To stop it from anywhere: touch %s", bot_stop_file))),
  uiOutput("conn"),
  fluidRow(
    column(5, div(class = "card", uiOutput("setup"))),
    column(7, div(class = "card", uiOutput("status")))),
  div(class = "card", h5("What the bot did"), uiOutput("events")),
  fluidRow(
    column(7, div(class = "card", h5("Its orders"), uiOutput("orders"))),
    column(5, div(class = "card", h5("Positions in the game"), uiOutput("positions")))))

server <- function(input, output, session) {
  s <- bot_settings
  env <- new.env(); env$b <- NULL; env$su <- list(); env$src <- 1L; env$gid <- NULL
  rv <- reactiveValues(games = data.table(), setup_busy = FALSE, conn = NULL, launched = 0)
  now_t <- function() if (run_mode == "replay") env$sim_t else as.numeric(Sys.time())
  running <- function() !is.null(env$b) && isTRUE(env$b$running)

  # ── the games ──
  if (run_mode == "replay") {
    message("Loading the replay...")
    env$rp <- replay_load(replay_pick)
    first <- vapply(env$rp, function(r) r$act$t[1], 0)
    env$sim_t <- min(first) - 60; env$last_real <- Sys.time()
    rv$games <- data.table(gid = names(env$rp), status = 1L, tip_t = first, date = as.Date(vapply(env$rp, function(r) format(r$gd$game$date), "")),
                           atm = vapply(env$rp, function(r) r$gd$game$atm, ""), htm = vapply(env$rp, function(r) r$gd$game$htm, ""))
  } else {
    rv$games <- tryCatch(todays_games(), error = function(e) data.table())
    observe({                                    # the scoreboard again each minute, while no bot runs
      invalidateLater(60000)
      isolate(if (!running()) {
        g <- tryCatch(todays_games(), error = function(e) NULL)
        # only when a game is added or changes status, so the limits being typed aren't redrawn under the cursor
        if (!is.null(g) && nrow(g) && !identical(g[, .(gid, status)], rv$games[, .(gid, status)])) rv$games <- g
      })
    })
  }
  game_row <- function(id) { g <- isolate(rv$games); g[g$gid == id] }

  # ── what the bot touches ──
  read_feed <- function(gid, gm) {
    for (k in 1:2) {
      a <- tryCatch(parse_feed(httr2::resp_body_string(httr2::req_perform(feed_req(gid, src = env$src))), gm), error = function(e) NULL)
      if (!is.null(a)) return(a)
      env$src <- 3L - env$src                    # the NBA's other copy of the file
    }
    NULL
  }
  io <- list(
    now = now_t,
    feed = if (run_mode == "replay") function(gid, gm) env$rp[[gid]]$act[t <= env$sim_t] else read_feed,
    quotes = if (run_mode == "replay") function(gm) replay_quotes(env$rp[[gm$gid]]$qm, env$sim_t) else kalshi_quotes,
    positions = if (run_mode == "live") kalshi_positions else function(events) sim_positions(read_orders(), env$gid, run_mode),
    order = if (run_mode == "live") kalshi_order else sim_order,
    balance = if (run_mode == "live") kalshi_balance else function() list(cash = Inf, shards = c(`3` = Inf)),
    setup = function(row, t) {
      if (run_mode == "replay") { gd <- env$rp[[row$gid]]$gd; su <- setup_game(gd$game, gd$tip); su$line_source <- "the replay's line"; return(su) }
      if (!is.null(t)) row$tip_t <- t
      game_setup(row, list())
    },
    read_orders = read_orders, log_order = log_order,
    event = function(type, detail)
      fwrite(data.table(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), mode = run_mode, gid = env$gid %||% "", type = type, detail = detail),
             events_file, append = file.exists(events_file)))

  # ── picking a game: the model is anchored to it then (10-30 seconds) ──
  observeEvent(input$game, {
    id <- input$game
    if (running() || is.null(id) || !nzchar(id)) return()
    env$gid <- id
    if (!is.null(env$su[[id]])) return()
    row <- game_row(id)
    rv$setup_busy <- TRUE
    withProgress(message = sprintf("Anchoring the model to %s at %s...", row$atm, row$htm), value = 0.3, {
      t <- NULL
      if (run_mode != "replay" && row$status != 1) {     # under way: anchor to Kalshi just before the real tip
        a <- read_feed(id, row)
        s0 <- if (!is.null(a)) a[actionType == "period" & subType == "start" & period == 1 & !is.na(t)] else NULL
        if (!is.null(s0) && nrow(s0)) t <- s0$t[1] - 30
      }
      su <- tryCatch(io$setup(row, t), error = function(e) e)
    })
    rv$setup_busy <- FALSE
    if (inherits(su, "error")) { showNotification(paste("Couldn't set the game up:", conditionMessage(su)), type = "error", duration = NULL); return() }
    su$setup_t <- as.numeric(Sys.time())
    env$su[[id]] <- su
    rv$launched <- rv$launched + 0.001                   # redraw
  }, ignoreInit = TRUE)

  limits <- function() list(spend_cap = input$spend_cap, bankroll = input$bankroll, max_order = input$max_order,
                            max_stop = input$max_stop, max_loss = input$max_loss)

  # ── launching ──
  observeEvent(input$launch, {
    if (running()) return()
    id <- input$game
    if (is.null(id) || !nzchar(id) || is.null(env$su[[id]])) { showNotification("Pick a game and wait for it to be set up.", type = "warning"); return() }
    l <- limits(); why <- bot_check_limits(l, s)
    if (why != "") { showNotification(why, type = "error", duration = 10); return() }
    su <- env$su[[id]]; gm <- su$game
    showModal(modalDialog(title = if (run_mode == "live") "Launch the bot with real money?" else "Launch the bot on paper?",
      HTML(paste0(
        sprintf("<p><b>%s at %s</b>, anchored to %s %+.1f, total %.1f (%s).</p>", gm$atm, gm$htm, gm$htm, gm$line, gm$total, su$line_source),
        "<p>The bot will send orders by itself at every stoppage until the game ends, it's stopped, or a guardrail stops it.</p>",
        sprintf("<table class='t kv'><tr><td>Spend cap (net of sells)</td><td><b>$%s</b></td></tr><tr><td>Kelly bankroll</td><td>$%s</td></tr>",
                format(l$spend_cap, big.mark = ","), format(l$bankroll, big.mark = ",")),
        sprintf("<tr><td>Most per order / per stoppage</td><td>$%s / $%s</td></tr><tr><td>Stop if down</td><td>$%s since launch</td></tr>",
                format(l$max_order), format(l$max_stop), format(l$max_loss)),
        sprintf("<tr><td>Markets</td><td>spreads every quarter, totals from Q3, win from Q4; edges of %.0f¢+</td></tr></table>", 100 * s$min_edge),
        if (run_mode == "live") sprintf(paste0("<p class='bad' style='margin-top:10px'>Real money. The first order is 1 contract and is checked on Kalshi before the rest. ",
                                               "Keep this page open: closing or reloading it stops the bot.</p><p>Type the spend cap, <b>%s</b>, and press Enter to launch.</p>"),
                                        format(l$spend_cap))
        else "<p style='margin-top:10px'>Paper: orders fill on paper at their limit price, in full (a best case).</p>")),
      if (run_mode == "live") tags$input(id = "launch_word", type = "text", class = "form-control", autocomplete = "off", spellcheck = "false"),
      footer = tagList(modalButton("Cancel"),
                       tags$button(type = "button", class = paste("btn", if (run_mode == "live") "btn-danger" else "btn-primary"),
                                   onclick = "launchTry()", "Launch"))))
  })
  observeEvent(input$launch_go, {
    if (running()) return()
    l <- limits(); id <- input$game
    if (run_mode == "live" && !identical(trimws(gsub("[$,]", "", input$launch_go$word)), format(l$spend_cap, scientific = FALSE))) {
      showNotification(sprintf("Not launched: type the spend cap, %s.", format(l$spend_cap)), type = "warning"); return()
    }
    removeModal()
    if (is.null(env$su[[id]])) return()
    if (run_mode == "replay") {                          # the replay starts with the launch, a minute before the tip
      env$sim_t <- env$rp[[id]]$act$t[1] - 60; env$last_real <- Sys.time()
    }
    env$gid <- id
    b <- bot_new(io, s, run_mode, game_row(id), env$su[[id]])
    msg <- withProgress(message = "Checking Kalshi...", value = 0.5, bot_launch(b, l))
    if (msg != "") { showNotification(paste("Not launched:", msg), type = "error", duration = NULL); return() }
    env$b <- b
    rv$launched <- rv$launched + 1
  })

  observeEvent(input$stop, {
    if (running()) { bot_stop(env$b, "stopped from the page"); rv$launched <- rv$launched + 1; showNotification("The bot is stopped.", duration = 6) }
  })
  session$onSessionEnded(function() if (running()) isolate(bot_stop(env$b, "the page was closed")))

  # ── the bot's loop ──
  observe({
    invalidateLater(if (run_mode == "replay") 1000 else s$poll * 1000)
    isolate({
      if (!running()) return()
      if (run_mode == "replay") {
        env$sim_t <- env$sim_t + as.numeric(difftime(Sys.time(), env$last_real, units = "secs")) * replay_speed
        env$last_real <- Sys.time()
      }
      tryCatch(bot_step(env$b), error = function(e) bot_stop(env$b, paste("the bot hit an error:", conditionMessage(e))))
      if (!running()) rv$launched <- rv$launched + 1     # it stopped itself: redraw the setup
    })
  })

  if (run_mode == "live") observeEvent(input$check, {
    rv$conn <- tryCatch({
      bal <- kalshi_balance()
      sprintf("Connected. Cash $%.2f in all, <b>$%.2f on the NBA shard</b>, which is what the bot can spend. Fund it from the console if it's short.",
              bal$cash, shard_cash(bal, nba_shard))
    }, error = function(e) paste("Connection failed:", conditionMessage(e)))
  })

  # ── drawing ──
  output$conn <- renderUI(if (!is.null(rv$conn)) div(class = "warn", HTML(rv$conn)))
  output$bot_badge <- renderUI({
    invalidateLater(1000); rv$launched
    b <- env$b
    if (is.null(b)) span(class = "badge-mode b-off", "BOT NOT LAUNCHED")
    else if (b$running) span(class = paste("badge-mode", if (run_mode == "live") "b-run-live" else "b-run"), "BOT RUNNING")
    else span(class = "badge-mode b-stop", "BOT STOPPED")
  })
  output$setup <- renderUI({
    rv$launched; g <- rv$games; busy <- rv$setup_busy
    b <- env$b
    if (!is.null(b) && b$running) {
      l <- b$l; gm <- b$su$game
      return(div(h5(sprintf("%s at %s", gm$atm, gm$htm)),
                 tags$table(class = "t kv",
                   tags$tr(tags$td("Spend cap"), tags$td(sprintf("$%s", format(l$spend_cap, big.mark = ",")))),
                   tags$tr(tags$td("Kelly bankroll"), tags$td(sprintf("$%s", format(l$bankroll, big.mark = ",")))),
                   tags$tr(tags$td("Per order / per stoppage"), tags$td(sprintf("$%s / $%s", format(l$max_order), format(l$max_stop)))),
                   tags$tr(tags$td("Loss stop"), tags$td(sprintf("down $%s since launch", format(l$max_loss)))),
                   tags$tr(tags$td("Anchored to"), tags$td(sprintf("%s %+.1f, total %.1f (%s)", gm$htm, gm$line, gm$total, b$su$line_source)))),
                 div(class = "why", style = "margin-top: 8px;", "Stop the bot to change anything.")))
    }
    open <- if (nrow(g)) g[status != 3] else g
    choices <- c("Pick a game" = "", if (nrow(open)) setNames(open$gid, sprintf("%s at %s, %s", open$atm, open$htm,
      ifelse(open$status == 2, "under way", sub("^0", "", format(as.POSIXct(open$tip_t, origin = "1970-01-01"), "%I:%M %p"))))))
    sel <- isolate(input$game) %||% ""
    su <- if (nzchar(sel)) env$su[[sel]] else NULL
    v <- function(id, d) isolate(input[[id]]) %||% d
    div(h5("Game and limits"),
        selectInput("game", NULL, choices = choices, selected = sel, width = "100%"),
        if (busy) div(class = "sub", "Setting the game up...")
        else if (!is.null(su)) div(class = "sub", sprintf("Anchored to %s %+.1f, total %.1f (%s).", su$game$htm, su$game$line, su$game$total, su$line_source)),
        fluidRow(column(6, numericInput("spend_cap", "Spend cap, $", v("spend_cap", s$spend_cap), min = 1, max = s$ceiling, step = 50)),
                 column(6, numericInput("bankroll", "Kelly bankroll, $", v("bankroll", s$bankroll), min = 1, max = s$ceiling, step = 50))),
        fluidRow(column(4, numericInput("max_order", "Per order, $", v("max_order", s$max_order), min = 1, step = 5)),
                 column(4, numericInput("max_stop", "Per stoppage, $", v("max_stop", s$max_stop), min = 1, step = 10)),
                 column(4, numericInput("max_loss", "Stop if down, $", v("max_loss", s$max_loss), min = 1, step = 25))),
        div(class = "why", sprintf("Spend cap: the most the bot spends on the game, net of sells, never above $%s. Edges of %.0f¢+, quarter Kelly, prices %.0f-%.0f¢.",
                                   format(s$ceiling, big.mark = ","), 100 * s$min_edge, 100 * s$price_range[1], 100 * s$price_range[2])),
        div(style = "margin-top: 10px;", actionButton("launch", if (run_mode == "live") "Launch the bot (real money)" else "Launch the bot",
                                                     class = if (run_mode == "live") "btn-danger" else "btn-primary")),
        if (!is.null(b) && !b$running) div(class = "why", style = "margin-top: 6px;", "Launching again starts a new run; the last one's orders and positions count."))
  })
  output$status <- renderUI({
    invalidateLater(1000); rv$launched
    b <- env$b
    if (is.null(b)) return(div(h5("Bot"), div(class = "sub", "Not launched. Pick a game, check the limits, launch.")))
    last <- b$last; gm <- b$su$game; l <- b$l
    score <- if (is.null(last)) sprintf("%s at %s", gm$atm, gm$htm)
             else sprintf("%s %d · %s %d   %s %d:%02d", gm$atm, last$scoreAway, gm$htm, last$scoreHome, per_name(last$period), last$tr %/% 60, last$tr %% 60)
    pct <- max(0, min(100, 100 * b$spent / l$spend_cap))
    div(div(class = "score", score),
        if (!b$running) div(class = "bad", style = "margin-top: 8px;", paste("Stopped:", b$reason))
        else div(class = "sub", b$status),
        tags$table(class = "t kv", style = "margin-top: 8px;",
          tags$tr(tags$td("Spent, net of sells"), tags$td(sprintf("$%.2f of $%s", b$spent, format(l$spend_cap, big.mark = ",")),
                                                           div(class = "meter", div(style = sprintf("width: %.0f%%;", pct))))),
          tags$tr(tags$td("Positions vs launch, at Kalshi's mids"), tags$td(if (is.na(b$pnl)) "–" else sprintf("%+.2f (stops at -%s)", b$pnl, format(l$max_loss)))),
          tags$tr(tags$td("Orders sent / filled"), tags$td(sprintf("%d / %d", b$n_sent, b$n_filled))),
          tags$tr(tags$td("Feed read"), tags$td(sprintf("%.0f s ago%s", now_t() - b$feed_ok, if (run_mode == "replay") " (replay clock)" else ""))),
          tags$tr(tags$td("First order checked on Kalshi"), tags$td(if (run_mode != "live") "paper: not needed" else if (b$first_ok) "yes" else "not yet"))))
  })
  output$events <- renderUI({
    invalidateLater(1000); rv$launched
    b <- env$b
    if (is.null(b) || nrow(b$events) == 0) return(div(class = "sub", "Nothing yet."))
    e <- tail(b$events, 25)[.N:1]
    tags$table(class = "t", tags$tr(tags$th("Time"), tags$th(""), tags$th("")),
      lapply(seq_len(nrow(e)), function(j) tags$tr(tags$td(e$time[j]), tags$td(tags$b(e$type[j])), tags$td(e$detail[j]))))
  })
  output$orders <- renderUI({
    invalidateLater(2000); rv$launched
    b <- env$b
    if (is.null(b)) return(div(class = "sub", "None yet."))
    o <- read_orders(); o <- o[o$mode == run_mode & o$gid == b$gid & grepl("^bot", o$note) & (o$order_id %in% b$order_ids | o$status == "error")]
    if (nrow(o) == 0) return(div(class = "sub", "None yet."))
    o <- tail(o, 20)[.N:1]
    tags$table(class = "t", tags$tr(tags$th("Time"), tags$th(""), tags$th("Bet"), tags$th("Filled"), tags$th("Cash"), tags$th("Status")),
      lapply(seq_len(nrow(o)), function(j) tags$tr(tags$td(substr(o$time[j], 12, 19)), tags$td(o$action[j]), tags$td(o$bet[j]),
        tags$td(sprintf("%g of %g%s", o$filled[j], o$count[j], if (is.na(o$avg_price[j])) ""
                        else sprintf(" @ %.0f¢", 100 * if (o$action[j] == "SELL") 1 - o$avg_price[j] else o$avg_price[j]))),
        tags$td(sprintf("%+.2f", o$cash[j])), tags$td(o$status[j]))))
  })
  output$positions <- renderUI({
    invalidateLater(2000); rv$launched
    b <- env$b
    if (is.null(b) || is.null(b$kp)) return(div(class = "sub", "Read from Kalshi at each stoppage."))
    p <- b$kp[position != 0]
    if (nrow(p) == 0) return(div(class = "sub", "None."))
    q <- b$q; gm <- b$su$game
    p <- merge(p, q[, .(ticker, kind, x, yes_over, mid)], by = "ticker", all.x = TRUE)
    p[, side := fifelse(position > 0, "yes", "no")]
    p[, bet := fifelse(is.na(kind), paste(ticker, toupper(side)), bet_label(kind, x, yes_over, side, gm$htm, gm$atm))]
    tags$table(class = "t", tags$tr(tags$th("Bet"), tags$th("Contracts"), tags$th("Mid at the last stoppage")),
      lapply(seq_len(nrow(p)), function(j) tags$tr(tags$td(p$bet[j]), tags$td(abs(p$position[j])),
        tags$td(if (is.na(p$mid[j])) "–" else sprintf("%.0f¢", 100 * if (p$side[j] == "yes") p$mid[j] else 1 - p$mid[j])))))
  })
}

message("Loading the model...")
invisible(chart_engine()); invisible(snapshot())
invisible(cached("ratings", function() readRDS(rds("ratings"))))
invisible(tryCatch(team_games(as.integer(format(Sys.Date() - 182, "%Y")), Sys.Date()), error = function(e) NULL))
message(sprintf("possim bot, %s mode: http://127.0.0.1:%d", run_mode, port))
runApp(shinyApp(ui, server), host = "127.0.0.1", port = port, launch.browser = !("nobrowser" %in% args))
