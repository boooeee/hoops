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

```bash
Rscript bet_live.R             # every game today, until the last one ends
Rscript bet_live.R PHX         # one team's game
Rscript bet_backtest.R 2025-10-21 2026-06-30
Rscript bet_report.R backtest_2025-10-21_2026-06-30
```

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
Buying at the ask, $1,000 a game, 95% intervals from resampling games.

- **Profit by strategy** (return on money staked):

  | Strategy | Return | Per game |
  |---|---|---|
  | Full Kelly | 1.3% (0.2 to 2.3) | $17 |
  | Quarter Kelly | 2.5% (2.0 to 2.9) | $30 |
  | Quarter Kelly, 2¢ minimum edge, prices 5–95¢ | **5.7% (5.0 to 6.6)** | **$49** |
  | The same, win markets only | 3.1% (1.7 to 4.6) | $3 |

- **Where the third strategy made money** (return by quarter):

  | | Q1 | Q2 | Q3 | Q4 | OT |
  |---|---|---|---|---|---|
  | Win | 2.8% (−4.4 to 11.1) | −0.7% (−8.0 to 6.8) | 1.6% (−3.4 to 6.1) | **5.5%** (2.3 to 8.8) | 5.5% |
  | Spread | **4.6%** (2.4 to 7.0) | **3.1%** (0.9 to 5.5) | **4.0%** (2.7 to 5.4) | **7.9%** (6.3 to 9.8) | 7.4% |
  | Total | 1.9% (−1.7 to 5.3) | −1.8% (−4.4 to 1.0) | **3.5%** (1.5 to 5.4) | **14.4%** (11.2 to 17.3) | 34% |

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
