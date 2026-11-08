# possim: NBA win probability from a possession-by-possession simulation

The model predicts what the next possession will produce, then plays the game
out to the final buzzer:

- **Points**: 0, 1, 2, 3 or 4+.
- **How the other team gets the ball**: after a make, a defensive rebound, a steal, a dead-ball turnover, or the period ending.
- **Seconds it takes.**

Win probability, expected margin and expected total all come from the same
model, so they can't contradict each other.

## Inputs

| | |
|---|---|
| Market | pre-game line and total, turned into each team's expected points per possession |
| Efficiency | opponent-adjusted offensive and defensive points per possession |
| Pace | opponent-adjusted seconds per possession, offense and defense |
| 3-point rate | opponent-adjusted 3PA/FGA, team and opponent |
| Game state | current lead, period, seconds left, how the possession started |

The team ratings are opponent adjusted and taken as of the morning of each
game: a ridge regression on games before that date, with older games counting
less ([02_ratings.R](02_ratings.R)).

## How it fits together

Each part of the possession model adds two terms on the log-odds scale
([03_fit.R](03_fit.R)):

- **Game state**: boosted trees on start type, period, clock and the offense's
  lead. Late-game fouling, holding for the last shot, garbage time and a
  trailing team's push all come from here. The last three minutes and
  overtime are a small share of possessions, so the tree fits keep all of them
  and count each four times. Otherwise the trees stop before they've learned
  the endgame.
- **Teams**: a linear term in the four team numbers (market points per
  possession, rated points per possession, pace, 3-point rate). For outcomes,
  each outcome gets its own coefficient on each number. For seconds, the
  numbers stretch or shrink the possession.
- **Late-game changes to the team term**: each coefficient also gets a change
  for three late-game situations: close, offense trailing, offense leading.
  These fade in from 8:00 to 4:00 left in the fourth, and overtime counts as
  late. Without them, the market's era-wide scoring level carried straight into
  late-game possessions and over-credited trailing teams.

The two are fitted in turn, so the trees can't credit a lead for what is really
team strength.

The seconds trees see the fourth quarter and overtime clock to the second, so
they know a leading team runs clock with 5 minutes left. In Q1–Q3 they see only
the last minute of the period.

Because the state part is the same for every game, it's computed once as a
table. A game only supplies its small team term. From there
[engine.cpp](engine.cpp) works backward from the final buzzer over every state
(seconds left × lead × ball × how the possession started, overtime included).
That gives the exact win probability and expected points for every state in a
game in about 3 seconds. A simulation of the same model gives the full
spread of final margins and totals.

**Anchoring.** With a line and total, each team's market points per possession
is nudged, per game, until the game played out from the tip has exactly the
market's expected margin (−line) and total. The nudge then holds for every
in-game state. It takes 1–2 steps of Broyden's method, about 8 seconds a game.

## Running it

```bash
Rscript 01_possessions.R   # possession table from pos / wpg / gms   (~7 min)
Rscript 02_ratings.R       # as-of-date team ratings                  (~12 min)
Rscript 03_fit.R           # possession model                         (~45 min)
Rscript 04_engine.R        # state tables + anchoring Jacobian        (~3 min)
Rscript 05_validate.R 800  # held-out games vs the current model      (~50 min for 800; no number = all ~2,700, ~4 h)
```

Data is written to `~/Dropbox/Nba/playbyplay/possim/` (set `POSSIM_DATA` to
change it). Training seasons are 2013-14 through 2023-24. 2024-25 onward is
held out.

Using it on a game:

```r
source("~/hoops/possim/04_engine.R")
eng <- load_engine()
g <- game_inputs(eng, line = -6.5, total = 228.5,      # anchors to the line and total
                 home = list(rat_ppp = 1.17, exp_spp = 14.1, exp_tpar = 0.43),
                 away = list(rat_ppp = 1.12, exp_spp = 14.6, exp_tpar = 0.38))
pregame(eng, g)        # win probability, expected margin/total, their spread, cover and over
wp_at(eng, g, tip = 0, q = 4, tr = 90, m = -3, side = 0, st = "DRB")
```

`tip`: 0 if the home team won the opening tip. `side`: 0 if the home team has
the ball. `m`: the home lead. `st`: how the possession started (MFG, MFT, DRB,
STL, DTO, PST).

### Charting a game

[chart.R](chart.R) charts any game in the database next to the current model:
win probability, then the lead with the new model's projected final margin.

```r
source("~/hoops/possim/chart.R")
wp_chart("0022600158")                                    # by game id
wp_chart("BOS")                                           # Boston's game today, finished or live
wp_chart("OKC", date = "2026-11-04", file = "~/Desktop/okc.png")
```

- **Data.** The game comes from `gms`, `pos` and `wpg`. `wpNBA_rtask.R`
  rewrites recent games there each time it runs, so a live game is charted as
  of its last run, up to the latest possession start.
- **Output.** It prints a summary:
  - pregame win probability from both models;
  - the biggest gap between them;
  - for a live game, both models' numbers now, with the projected final margin
    and total;
  - for a finished game, each model's log loss.

  It returns the plot and one row per possession.
- **Team ratings.** They come from `ratings.rds` when the game is in the
  possession snapshot. For a later game they're fitted as of that morning, with
  the same ridge and settings as `02_ratings.R`. 02 saves those settings in
  `ratings_settings.rds`, and the as-of fit reproduces `ratings.rds` exactly.
- **Speed.** The first chart of a session takes about 40 seconds, most of it
  loading the engine. Later ones take about 20.

## Live betting on Kalshi

Separate from the website model. At a stoppage, the tool works out what Kalshi
bets to make and how many contracts of each:

1. It turns the moment into the model's state.
2. It simulates final scores.
3. It prices every win, spread and total contract on Kalshi.
4. It sizes orders with Kelly.

| File | What it does |
|---|---|
| [live.R](live.R) | The pieces: game setup, play-by-play (from `pbpr2` or the NBA's live file), state at a stoppage, final-score simulation, contract payoffs, Kelly orders |
| [bet_live.R](bet_live.R) | The live loop for today's games: prints recommended orders at each stoppage and keeps a paper ledger. Sends nothing to Kalshi |
| [bet_backtest.R](bet_backtest.R) | Replays held-out games against the Kalshi quotes saved by `kalshiNBA_rtask.R` |
| [bet_report.R](bet_report.R) | Reads a backtest: model against market, and profit and loss by strategy, kind, price and period |
| [bet_game.R](bet_game.R) | One backtest game, stoppage by stoppage: the model's numbers and Kalshi's, and every bet a strategy bought or sold |
| [console.R](console.R) | A betting console in the browser: a scoreboard of every game that flags stoppages, and for the game picked, the model against Kalshi at each stoppage with a button for each order. Paper, replay, or live |
| [kalshi_trade.R](kalshi_trade.R) | Signed Kalshi requests: balance, positions, and immediate-or-cancel limit orders |

```bash
Rscript bet_live.R             # every game today, until the last one ends
Rscript bet_live.R PHX         # one team's game
Rscript bet_backtest.R 2025-10-21 2026-06-30
Rscript bet_report.R backtest_2025-10-21_2026-06-30
Rscript bet_game.R 2026-01-28 IND    # one game from the backtest (no arguments: a random one)
```

**The console.** `Rscript console.R` opens http://127.0.0.1:8765 (this machine only).
- **Modes:**
  - `Rscript console.R` is paper: tonight's games, nothing sent to Kalshi.
  - `Rscript console.R replay 2026-11-04 5` plays back a past night's games at 5× against
    their saved quotes, also paper. `replay 0022500672` plays back one game.
  - `Rscript console.R live` can send real orders, but only after arming in the page by
    typing `bet` and pressing Enter.
- **The scoreboard** along the top shows every game, read from the NBA's feed every few
  seconds. A game whose play is stopped, with time left to bet it, turns amber with the time
  left ("Timeout · 0:52 to bet"). Click a game to bet it. Each game is set up in the background
  when the console starts (the model anchored to its line and total, about 10 seconds a game),
  so switching is quick. A game's orders come back if you switch away and return while its
  stoppage lasts.
- **Which markets:** unlike `bet_live.R`, it bets every kind of market in every quarter.
  The quarter rules came from the same backtest they were judged on.
- **At each stoppage** it lists the orders Kelly wants, each with a button. A click opens a
  confirmation, and nothing is sent until it's confirmed.
- **Two boxes in the bar** trade a gate for speed, for when prices move too fast. Both start
  unticked (set `one_click` or `stay_armed` in `console_settings` to start them ticked); untick
  either to go back.
  - **One-click orders:** a Buy or Sell button sends its order the moment it's clicked, with no
    confirmation. Live, turning it on asks once. The badge shows ONE-CLICK while it's on, and
    every other check still applies: armed, the limits, the price, the time window, and one
    order per button (the button goes dead on the first click).
  - **Stay armed across games** (live): switching games no longer disarms the console.
- **Every order is a limit at the price shown, immediate-or-cancel.** It fills now at that
  price or better, or not at all, so nothing is left resting on the book. Sells are
  reduce-only.
- **Spending limits** in `console_settings`: $30 an order, $300 a game, $500 a night, net of
  sells. Orders are cut to fit. Kelly sizes against a $500 bankroll a game.
- **When buttons lock:** once play resumes in the feed; at an ordinary stoppage, 180 seconds after
  the console first sees it (in any game, so picking a game late doesn't restart the clock), or
  straight away if it reached the feed more than 60 seconds late; at a
  break between periods, 2 minutes after the period ended (14 at halftime), however late it
  was seen. The feed runs 20–60 seconds behind, so watch the game too. **Recheck prices**
  works a stoppage's orders out again from Kalshi's latest prices, for long breaks.
- **Live safeguards:**
  - A failed order disarms the console, and so does switching games (unless **Stay armed
    across games** is ticked).
  - The night's first real order is held to 1 contract, so you can check it on Kalshi.
  - Positions and cash for Kelly come from Kalshi itself.
- **Fund the NBA shard first.** Kalshi keeps a separate cash balance for each group of markets
  (shard). NBA markets are on shard 3, and deposits sit on shard 0. Kalshi's app moves cash across
  by itself; API orders don't, and Kalshi refuses them with "insufficient shard balance". **Fund NBA
  shard** moves a confirmed amount, up to the night's limit, so the shard's cash is a second cap.
- **Logs:** every order goes to `data_dir/live/<date>_orders.csv`, with what filled and at what price.
  It also records the model's probability and edge for the side bought, and the stoppage. Timing too:
  how long the feed took to show the stoppage, and how long after the orders were worked out you clicked.
- **Credentials:** `KALSHI_API_KEY_ID`, and the key at `KALSHI_KEY_PATH` (default: the one in
  `~/Dropbox/kalshi`).
- **Kalshi's API:** orders use its V2 endpoint (`POST /portfolio/events/orders`, side `bid` to
  buy YES and `ask` to sell YES, priced in YES terms).

**Turning a stoppage into a state.**
- **Quarter break:** the next period's first possession.
- **Timeout right after a make, defensive rebound, steal or dead-ball turnover:**
  that possession's start, exactly.
- **Free throws to come:** every make/miss sequence at a 78% make rate. After the
  last one, the other team has the ball.
- **Anything else:** an inbound for the team with the ball, at the clock as it
  stands. This ignores the shot clock.

**Kelly.** It maximizes expected utility over the simulated final scores.
- Gamma 1 is full Kelly; gamma 4 is about quarter Kelly.
- Costs include Kalshi's fee: 0.07 × P × (1−P) per contract on the NBA series.
- The game's cash and existing positions count. So buying the other side of a
  held contract sells it, and the live printout says SELL. A side below the
  minimum edge can sell what's held but not go past flat.
- Settings at the top of `bet_live.R`: cash per game, gamma, minimum edge,
  price range, the most per market per stoppage, how late a stoppage can be
  seen, and the quarters to bet each kind of market in. By default, win markets from Q4, spreads all game, totals
  from Q3, from the backtest below.

**Backtest: 2025-26, 1,315 games including the playoffs, 24,832 stoppages.**
Buying at the ask, $1,000 a game, 95% intervals from resampling games. Saved as
`backtest_2025-10-21_2026-06-30_hedge`.

- **Profit by strategy** (return on money staked):

  | Strategy | Return | Per game |
  |---|---|---|
  | Full Kelly | 1.3% (0.2 to 2.3) | $17 |
  | Quarter Kelly | 2.5% (2.0 to 2.9) | $30 |
  | Quarter Kelly, 2¢ minimum edge, prices 5–95¢ | **6.2% (5.4 to 7.1)** | **$49** |
  | The same, win markets only | 3.4% (1.9 to 5.0) | $3 |

  An earlier run, before a side below the minimum edge was stopped from going
  past flat, had the third strategy at 5.7% (5.0 to 6.6), the same $49 a game
  on more money staked. The checks under "Is it real?" and "Part of it is
  speed" were done on that run.
- **Where the third strategy made money** (return by quarter):

  | | Q1 | Q2 | Q3 | Q4 | OT |
  |---|---|---|---|---|---|
  | Win | 2.2% (−6.1 to 10.8) | 5.5% (−0.7 to 12.4) | 1.5% (−3.3 to 6.5) | **5.6%** (2.2 to 9.0) | 7.1% |
  | Spread | **4.0%** (1.4 to 6.3) | **3.2%** (1.1 to 5.5) | **4.3%** (2.6 to 5.9) | **8.5%** (6.6 to 10.3) | 9.1% |
  | Total | 1.8% (−1.6 to 5.2) | −1.9% (−4.7 to 0.5) | **3.3%** (1.2 to 5.2) | **15.9%** (12.8 to 19.8) | 45% |

  Q4 totals made the most: in the last two minutes, in close games, on the
  over, and at free-throw breaks. That's where late fouling adds points, and
  the market undershoots it.
- **Is it real?**
  - It made money in every month, 3.4% to 10.7%; only June (5 Finals games) is
    flat.
  - It made money where the market was 1¢ wide (5–7%), not just on wide, thin
    strikes.
  - It made money where more than 5,000 contracts traded in the next five
    minutes. Capping each fill at what traded in the next five minutes barely
    changes it.
  - The first 106 games of 2026-27 agree: 5.0% (2.8 to 7.3).
- **Part of it is speed.**
  - The sooner after the stoppage the quote, the more it made. Q4 totals made
    16.6% on quotes 20–40 seconds after the stoppage and 10.6% at 60–90
    seconds.
  - Where the model and the market differed by more than 8¢ in Q4, the market
    closed about half the gap within two minutes.
  - Live, the tool sees a stoppage only after the NBA's feed posts it. On
    Miami at Golden State (Nov 5, 2026), plays reached the feed a median 23
    seconds after they happened; 47% arrived within 20 seconds and 63% within 60.
    The feed also stopped updating twice, for 11 minutes and for 4½, and some
    requests hung for 60 seconds. So the tool skips any stoppage it sees more than
    `max_seen` (60) seconds after it happened, and gives up on a read after 8
    seconds.
- **Against the market's mid, where it's 1–2¢ wide:**
  - Kalshi is slightly better than the model in Q1–Q3 on all three kinds, and
    on win markets in every quarter.
  - The model is better on spreads and totals in Q4.
  - Most spread and total strikes are wider than 2¢, and there the mid is a
    poor guess. Scored against it, the model looks better than it is.
- **Hedging doesn't pay.** `kelly_orders(hedge = TRUE)` also lets Kelly buy
  sides below the minimum edge, counted as having no edge, so it buys them only
  to cut risk. Each hedging strategy was compared with its counterpart game by
  game, on the same simulations:
  - Quarter Kelly with the 2¢ minimum: $44 a game instead of $49 (−$5.83,
    −8.78 to −2.96), and 4.7% instead of 6.2%.
  - Plain quarter Kelly: $25 a game instead of $30 (−$4.90, −7.29 to −2.48).
  - It does cut the swings: the SD of a game's result is 10% lower ($117
    against $130). But profit fell about as much, so profit per unit of risk
    didn't improve (0.37 against 0.38). That's no better than betting a little
    less, and the log growth of the bankroll is lower.
  - The hedges themselves about broke even. The loss came from the edge bets made
    alongside them, mostly totals.
  - So it's off by default, and not a setting in `bet_live.R`.
- **Depth:** minute quotes carry no depth, so every backtest order is assumed to
  fill, up to $100 per market per stoppage.

## How it does

All 2,667 held-out games in the possession table (2024-25, 2025-26 and the
start of 2026-27), scored at the start of every possession against the current
model (`wpg.wpl`). Log loss, with 95% game-clustered intervals on the
difference:

| | possessions | new | current | new − current |
|---|---|---|---|---|
| Q1 | 135,336 | 0.5617 | 0.5634 | −0.0018 (−0.0050 to +0.0013) |
| Q2 | 134,288 | 0.5018 | 0.5036 | −0.0018 (−0.0049 to +0.0012) |
| Q3 | 133,497 | 0.4081 | 0.4116 | −0.0035 (−0.0068 to +0.0001) |
| Q4 12:00–5:00 | 74,593 | 0.2962 | 0.2984 | −0.0023 (−0.0051 to +0.0003) |
| Q4 5:00–2:00 | 30,939 | 0.1984 | 0.1995 | −0.0011 (−0.0037 to +0.0011) |
| Q4 last 2:00 | 24,513 | 0.1132 | 0.1142 | −0.0011 (−0.0029 to +0.0008) |
| OT | 1,587 | 0.4700 | 0.4700 | 0.0000 (−0.0191 to +0.0171) |
| **All** | 534,753 | **0.4294** | **0.4316** | **−0.0022 (−0.0049 to +0.0004)** |
| Pre-game | 2,667 games | 0.5784 | 0.5809 | |

- **Overall.** The new model is ahead in every regulation segment. The overall
  edge of 0.002 falls just short of 95% significance.
- **Sample.** The edge was 0.004 in the first 800 games scored and 0.0014 in
  the other 1,867.
- **Last two minutes.** The new model trailed here in the first 800 games
  (0.0936 against 0.0923) but leads over all of them.
- **Calibration.** The current model is too sure of the leader:
  - When it gives the home team 70–85%, the home team wins 73% (predicted 78%).
  - At 85–95%, the home team wins 87% (predicted 90%).
  - Every bin of the new model is within 2 points of what happened.
- **Trailing teams.** The checks are down 1–3, 4–6 and 7–9, in Q4 6:00–2:00 and
  in the last 2:00. The new model is within about 1 point of the real win rate
  in each, and no gap is significant. The current model underrates trailing
  teams by 1–2 points.
- **Final margin.** RMSE is 10.54 from the expected margin at each possession,
  against 10.74 for the current score plus the line's share of the time left.
  It's lower in every segment.

Weak spots:

- **Overtime games are missing (data bug).** `01_possessions.R` leaves out a
  whole game if any possession runs past the end of its period. In some
  seasons the first overtime possession is logged at the tip, a few seconds
  into overtime, so the last fourth-quarter possession looks like it runs past
  0:00. The game is dropped.
  - In 2025-26, 56 of 60 overtime games are lost, plus 9 other games for a
    different reason.
  - In the training seasons about 90 are lost, mostly 2014–2018.
  - These are games tied after regulation. So the late-game training data is
    short on ties, and the scores above leave out some of the closest held-out
    games.
  - This is also why the held-out games show only 2.8% going to overtime,
    against the model's 4.4%. In `wpg` the rate is 4.8%.
- **Spread of outcomes.** Simulated final margins have an SD of 13.5 around
  the line, against 14.0 in real games. Totals: 17.0 against 17.9.
- **3-point rate.** It's over-predicted for the most 3-heavy matchups (0.184
  against 0.175 in the top decile).

## Data notes

- **Points.** A possession's points come from the score changes in `wpg`, not
  from `pos.pts`. `pos.pts` loses the play right after a technical free throw,
  about 1.5 points a game before 2025-26.
- **Possessions left out.** Possessions where the defense's score moves have
  the wrong team on offense: `wpg` flips the ball on technical and flagrant
  free throws coded as ordinary ones, which happens throughout the live-feed
  seasons. These and the possession after each (0.3%) are left out of fitting.
- **Technical free throws** belong to no possession. Their average (about
  0.8 a game) is added to projected totals.
