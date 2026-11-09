# possim/kalshi_trade.R - signed Kalshi requests: balance, positions, and one kind of order
#
# Credentials: KALSHI_API_KEY_ID (in ~/.Renviron) and the RSA private key at KALSHI_KEY_PATH
# (default: the key in ~/Dropbox/kalshi). The key is only ever handed to openssl to sign.
#
# Orders use Kalshi's V2 endpoint, POST /portfolio/events/orders. There, side "bid" buys YES
# and "ask" sells YES; selling YES with none held opens a NO position, so buying NO at 62¢ is
# an ask at a YES price of 38¢. The price is always the YES price. Every order sent from here is
# immediate-or-cancel at a limit: it fills now at that price or better, or not at all, and
# nothing is left resting on the book.

kalshi_host <- "https://api.elections.kalshi.com"
kalshi_key_path <- function() path.expand(Sys.getenv("KALSHI_KEY_PATH", "~/Dropbox/kalshi/Mikes Kalshi Key.txt"))

# RSA-PSS (SHA-256, salt 32) signature of msg, base64. R's openssl package can't do PSS.
kalshi_sign <- function(msg) {
  key <- kalshi_key_path()
  if (!file.exists(key)) stop("no Kalshi private key at KALSHI_KEY_PATH")
  m <- tempfile("kmsg_"); s <- tempfile("ksig_"); on.exit(unlink(c(m, s)))
  writeBin(charToRaw(msg), m)
  rc <- system2("openssl", c("dgst", "-sha256", "-sigopt", "rsa_padding_mode:pss", "-sigopt", "rsa_pss_saltlen:32",
                             "-sign", shQuote(key), "-out", shQuote(s), shQuote(m)), stdout = FALSE, stderr = FALSE)
  if (rc != 0 || !file.exists(s)) stop("openssl couldn't sign the request")
  openssl::base64_encode(readBin(s, "raw", file.info(s)$size))
}

# A signed request. path is after /trade-api/v2, e.g. "/portfolio/balance". Returns the HTTP
# status and the parsed body; never stops on an HTTP error, so the caller can show it.
kalshi_req <- function(method, path, query = NULL, body = NULL, timeout = 8) {
  id <- Sys.getenv("KALSHI_API_KEY_ID")
  if (!nzchar(id)) stop("KALSHI_API_KEY_ID isn't set")
  full <- paste0("/trade-api/v2", path)
  ts <- sprintf("%.0f", as.numeric(Sys.time()) * 1000)
  req <- httr2::request(paste0(kalshi_host, full)) |>
    httr2::req_headers(`KALSHI-ACCESS-KEY` = id, `KALSHI-ACCESS-TIMESTAMP` = ts,
                       `KALSHI-ACCESS-SIGNATURE` = kalshi_sign(paste0(ts, method, full)), .redact = "KALSHI-ACCESS-SIGNATURE") |>
    httr2::req_method(method) |> httr2::req_timeout(timeout) |> httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_user_agent("hoops-possim-console")
  if (length(query)) req <- httr2::req_url_query(req, !!!query)
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  resp <- httr2::req_perform(req)
  txt <- httr2::resp_body_string(resp)
  list(status = httr2::resp_status(resp),
       body = tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) txt))
}

# Kalshi runs its markets on separate exchange shards, each holding its own share of the
# account's cash. NBA markets are on shard 3 (sports); deposits sit on shard 0. Orders placed in
# Kalshi's app move cash across by themselves, but API orders don't: the NBA shard needs cash first,
# or Kalshi refuses the order ("insufficient shard balance").
nba_shard <- 3L

# cash: across all shards. shards: cash on each, by shard number (a shard never funded is missing)
kalshi_balance <- function() {
  r <- kalshi_req("GET", "/portfolio/balance")
  if (r$status != 200) stop("balance: HTTP ", r$status, " ", substr(jsonlite::toJSON(r$body, auto_unbox = TRUE), 1, 200))
  b <- r$body
  bd <- b$balance_breakdown
  list(cash = if (!is.null(b$balance_dollars)) as.numeric(b$balance_dollars) else b$balance / 100,
       portfolio = if (!is.null(b$portfolio_value)) b$portfolio_value / 100 else NA_real_,
       shards = if (!is.null(bd) && NROW(bd)) setNames(as.numeric(bd$balance), bd$exchange_index) else numeric())
}
shard_cash <- function(b, shard = nba_shard) { v <- b$shards[as.character(shard)]; if (length(v) == 0 || is.na(v)) 0 else unname(v) }

# Move cash within the account from one shard to another (default: the main one to the NBA one).
# Kalshi takes the amount in hundredths of a cent and makes the move shortly after accepting it.
kalshi_shard_transfer <- function(dollars, to = nba_shard, from = 0L) {
  stopifnot(is.numeric(dollars), dollars > 0, dollars <= 5000, to != from)
  body <- list(source = "event_contract", destination = "event_contract", amount = as.integer(round(dollars * 10000)),
               source_exchange_shard = as.integer(from), destination_exchange_shard = as.integer(to),
               client_transfer_id = uuid::UUIDgenerate())
  r <- kalshi_req("POST", "/portfolio/intra_exchange_instance_transfer", body = body)
  if (r$status != 200 || !is.list(r$body) || is.null(r$body$transfer_id))
    stop("transfer: HTTP ", r$status, " ", substr(if (is.character(r$body)) r$body else jsonlite::toJSON(r$body, auto_unbox = TRUE), 1, 300))
  r$body$transfer_id
}

# Positions in the given events (each of a game's three: win, spread, total), including
# markets traded and since closed out. position > 0 is YES contracts, < 0 is NO.
# cash_out: what the market has cost so far, net of anything sold, fees included.
kalshi_positions <- function(events) {
  empty <- data.table(ticker = character(), position = numeric(), exposure = numeric(), realized = numeric(), fees = numeric())
  rbindlist(c(list(empty), lapply(events, function(ev) {
    r <- kalshi_req("GET", "/portfolio/positions", query = list(event_ticker = ev, count_filter = "total_traded", limit = 1000))
    if (r$status != 200) stop("positions: HTTP ", r$status)
    m <- r$body$market_positions
    if (is.null(m) || length(m) == 0 || NROW(m) == 0) return(NULL)
    m <- as.data.table(m)
    num <- function(v) if (is.null(v)) rep(0, nrow(m)) else suppressWarnings(as.numeric(v))
    data.table(ticker = m$ticker, position = num(m$position_fp), exposure = num(m$market_exposure_dollars),
               realized = num(m$realized_pnl_dollars), fees = num(m$fees_paid_dollars))
  })))[, cash_out := exposure - realized + fees][]
}

# The body of an immediate-or-cancel limit order to buy `count` contracts of `outcome`
# ("yes" or "no") at `price` dollars or better for that outcome. reduce_only = TRUE when the
# order is meant to sell a held position: Kalshi then caps it at what's held.
order_body <- function(ticker, outcome, count, price, client_id, reduce_only = FALSE) {
  stopifnot(outcome %in% c("yes", "no"), count >= 1, count == round(count), price > 0, price < 1)
  b <- list(ticker = ticker, side = if (outcome == "yes") "bid" else "ask",
            count = sprintf("%d", as.integer(count)),
            price = sprintf("%.4f", if (outcome == "yes") price else 1 - price),     # always the YES price
            time_in_force = "immediate_or_cancel", self_trade_prevention_type = "taker_at_cross",
            client_order_id = client_id)
  if (reduce_only) b$reduce_only <- TRUE
  b
}

# Send it. Returns what filled: contracts, the average price for `outcome`, and fees.
kalshi_order <- function(body) {
  r <- kalshi_req("POST", "/portfolio/events/orders", body = body)
  b <- r$body
  if (r$status != 201 || !is.list(b) || is.null(b$order_id))
    return(list(ok = FALSE, status = r$status, error = substr(if (is.character(b)) b else jsonlite::toJSON(b, auto_unbox = TRUE), 1, 300)))
  filled <- as.numeric(b$fill_count %||% 0)
  avg_yes <- if (filled > 0 && !is.null(b$average_fill_price)) as.numeric(b$average_fill_price) else NA_real_
  fee <- if (filled > 0 && !is.null(b$average_fee_paid)) as.numeric(b$average_fee_paid) * filled else 0
  list(ok = TRUE, status = r$status, order_id = b$order_id, filled = filled,
       avg_price = if (body$side == "bid") avg_yes else 1 - avg_yes,     # in the outcome's terms
       fee = fee, remaining = as.numeric(b$remaining_count %||% NA))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
