# possim/common.R - settings and helpers shared by the possession-simulation scripts
#
# The scripts run in order:
#   01_possessions.R  one row per possession, from the wpg / pos / gms tables
#   02_ratings.R      opponent-adjusted team ratings as they stood the morning of each game
#   03_fit.R          the possession model: outcome, then seconds given the outcome
#   04_engine.R       turns the possession model into win probability, spread and total
#   05_validate.R     checks against real results and against the current WP model
# chart.R charts any game, finished or live, next to the current model.

suppressMessages({
  library(DBI)
  library(RMySQL)
  library(data.table)
})

# ── Settings ──────────────────────────────────────────────────────────────────
data_dir   <- path.expand(Sys.getenv("POSSIM_DATA", "~/Dropbox/Nba/playbyplay/possim/"))
first_ssn  <- 2012L         # first season pulled (ratings need one season of history)
train_ssn  <- 2013:2023     # seasons the possession model is fitted on
test_ssn   <- 2024:2026     # held out
gid_types  <- c("002", "004", "005")   # regular season, playoffs, play-in

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# ── Possession vocabulary ────────────────────────────────────────────────────
# How a possession starts. MFG/MFT: inbound after a made field goal / made last
# free throw. DRB: defensive rebound. STL: live-ball turnover (a steal). DTO:
# dead-ball turnover or a held ball. PST: first possession of a period.
start_types <- c("MFG", "MFT", "DRB", "STL", "DTO", "PST")

# A possession's start type from pos.spt1 (the play that started it) and wpg.pt2
# at that play. The first possession of each period is PST, whatever started it.
start_type <- function(first_in_q, spt1, pt2) {
  fcase(first_in_q,                           "PST",
        spt1 %in% c("MK2", "MK3"),            "MFG",
        spt1 %in% c("MK11", "MK22", "MK33"),  "MFT",
        spt1 == "DRB",                        "DRB",
        spt1 == "TO" & pt2 == "STL",          "STL",
        spt1 %in% c("TO", "JB"),              "DTO",
        default = NA_character_)
}

# How a possession ends: points scored (4 = 4 or more) and how the other team
# gets the ball, which is the next possession's start type. 0PEND: the clock ran
# out on a scoreless possession, so it always lasts exactly the time that was
# left. A possession that scores as the period ends (a buzzer-beater) keeps its
# made-basket outcome: its seconds reach the end of the period, so the next
# possession is the next period's first either way.
outcomes <- data.table(
  oc  = c("0DRB", "0STL", "0DTO", "0PEND",
          "1MFT", "1DRB",
          "2MFG", "2MFT", "2DRB",
          "3MFG", "3MFT", "3DRB",
          "4MFT"),
  pts = c(0, 0, 0, 0, 1, 1, 2, 2, 2, 3, 3, 3, 4),
  nxt = c("DRB", "STL", "DTO", "PST", "MFT", "DRB", "MFG", "MFT", "DRB",
          "MFG", "MFT", "DRB", "MFT"))
outcomes[, k := .I - 1L]   # 0-based class index for xgboost

max_dur <- 40L   # possession seconds are modelled as 1..max_dur (max_dur = that or longer)

# Game-state features. The offense's lead is capped at mo_cap for the outcome
# model. The seconds model sees a coarser state - seconds left in the period
# capped at dur_tr_cap, in regulation at dur_gr_cap (so the whole fourth quarter
# and overtime by the second, and only the end of the period in Q1-Q3), lead at
# +/- dur_mo_cap - which keeps its lookup table manageable (04_engine.R).
mo_cap     <- 30L
dur_tr_cap <- 61L
dur_gr_cap <- 721L
dur_mo_cap <- 12L

# Late-game regimes for the team term. How much team strength, pace and 3-point
# rate matter can change late in a game (a trailing team chucking quick threes, a
# leading team running clock, benches in a blowout), so the team term has a base
# part plus a part for each of three late-game situations: close, offense
# trailing, offense leading. Their weights ramp in from late_from to late_to
# seconds left in regulation (overtime counts as late), and the lead splits from
# lead_from to lead_to points either way.
regime <- c(late_from = 480, late_to = 240, lead_from = 3, lead_to = 9)
regime_names <- c("late_close", "late_trail", "late_lead")

regime_w <- function(gr, mo, rg = regime) {
  late  <- pmin(pmax((rg[["late_from"]] - gr) / (rg[["late_from"]] - rg[["late_to"]]), 0), 1)
  span  <- rg[["lead_to"]] - rg[["lead_from"]]
  trail <- pmin(pmax((-mo - rg[["lead_from"]]) / span, 0), 1)
  lead  <- pmin(pmax(( mo - rg[["lead_from"]]) / span, 0), 1)
  w <- cbind(late * (1 - trail - lead), late * trail, late * lead)
  colnames(w) <- regime_names
  w
}

# The team term's inputs for each possession: the centred team numbers dz
# (n x 4), then the same numbers times each late-game weight.
team_Z <- function(dz, gr, mo, rg = regime) {
  w <- regime_w(gr, mo, rg)
  Z <- cbind(dz, dz * w[, 1], dz * w[, 2], dz * w[, 3])
  colnames(Z) <- c(z_names, as.vector(outer(z_names, regime_names, paste, sep = ":")))
  Z
}

# ── Team numbers for one offense against one defense ─────────────────────────
z_names <- c("mkt_ppp", "rat_ppp", "exp_spp", "exp_tpar")

# Points per possession the market implies. line is the home spread (negative =
# home favoured), total the game total; each team gets 2880 / (home seconds per
# possession + away seconds per possession) possessions.
market_ppp <- function(line, total, home, spp_home, spp_away) {
  pts <- ifelse(home == 1, (total - line) / 2, (total + line) / 2)
  pts / (2880 / (spp_home + spp_away))
}

# Model inputs: the game state for the outcome trees, and the coarser state
# (plus the outcome) for the seconds trees.
state_X <- function(d) {
  cbind(st_MFG = d$st == "MFG", st_MFT = d$st == "MFT", st_DRB = d$st == "DRB",
        st_STL = d$st == "STL", st_DTO = d$st == "DTO", st_PST = d$st == "PST",
        q = pmin(d$q, 5), tr = d$tr, gr = d$gr, mo = pmax(pmin(d$mo, mo_cap), -mo_cap)) * 1
}

dur_X <- function(d) {
  oc_cols <- sapply(outcomes$oc[outcomes$oc != "0PEND"], function(o) d$oc == o)
  if (is.null(dim(oc_cols))) oc_cols <- matrix(oc_cols, nrow = 1)
  colnames(oc_cols) <- paste0("oc_", outcomes$oc[outcomes$oc != "0PEND"])
  cbind(oc_cols,
        st_MFG = d$st == "MFG", st_MFT = d$st == "MFT", st_DRB = d$st == "DRB",
        st_STL = d$st == "STL", st_DTO = d$st == "DTO", st_PST = d$st == "PST",
        trc = pmin(d$tr, dur_tr_cap), grc = pmin(d$gr, dur_gr_cap),
        moc = pmax(pmin(d$mo, dur_mo_cap), -dur_mo_cap)) * 1
}

rowmax <- function(M) {
  m <- M[, 1]
  for (k in seq_len(ncol(M))[-1]) m <- pmax(m, M[, k])
  m
}

period_len <- function(q) ifelse(q <= 4, 720L, 300L)
period_end <- function(q) ifelse(q <= 4, 720L * q, 2880L + 300L * (q - 4L))

# Team codes that changed: map the old code to the current one so ratings carry over.
team_alias <- c(NOH = "NOP", NOK = "NOP", NJN = "BKN", SEA = "OKC", VAN = "MEM", CHH = "CHA")
fix_team <- function(x) ifelse(x %in% names(team_alias), team_alias[x], x)

# ── Database ─────────────────────────────────────────────────────────────────
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")

sqlq <- function(x) {
  con <- dbConnect(RMySQL::MySQL(),
    username = Sys.getenv("NBA_DB_USER", Sys.getenv("WNBA_DB_USER", "root")),
    password = Sys.getenv("NBA_DB_PASS", Sys.getenv("WNBA_DB_PASS")),
    dbname = "nba", host = "localhost", port = 3306L)
  on.exit(dbDisconnect(con))
  as.data.table(suppressWarnings(dbGetQuery(con, x)))
}

gid_prefixes <- function(ssn) {
  as.vector(outer(gid_types, sprintf("%02d", ssn %% 100), paste0))
}
in_list <- function(x) paste0("('", paste(x, collapse = "','"), "')")

rds <- function(name) file.path(data_dir, paste0(name, ".rds"))
