"""
wp_check.py - test the win-probability model against what actually happened.

The Python twin of wp_check.R: same settings, same query, same answers. The
comments marked "R:" show the dplyr version of each pandas step, so the two files
can be read side by side.

Pick a game situation - a range of scoring margins and a stretch of game clock -
and wp_check() finds every game where a team was in it, then compares how often
those teams went on to win with what the model (wpg.wp) said at the time.

In Python (IPython, Jupyter, VS Code, Positron):
    from wp_check import wp_check, plot_wp_check
    res = wp_check(margin=(-27, -23), time=("Q2 10:00", "Q2 7:00"))
    res                   # summary table + the upsets
    res.summary           # pandas DataFrame, one row per season group
    res.games             # one row per team-game in the sample
    plot_wp_check(res, "wp_check.png")

From a terminal:
    python wp_check.py --margin -27 -23 --time "Q2 10:00" "Q2 7:00"
    python wp_check.py --margin -27 -23 --time "Q2 10:00" "Q2 7:00" --by 5 --games regular --plot q2.png
    python wp_check.py --margin -15 -10 --time 6 3 --seasons 2015 2025 --by season

Arguments (see wp_check.R for the long version):
    margin   team's score minus the opponent's, both ends included; (-27, -23) = down 23 to 27
    time     two clock points, "Q2 10:00" / "OT1 2:00", or minutes left in regulation
    seasons  None, or a list/range of seasons, e.g. range(2015, 2026)
    by       None, "season", a block length like 5, or era starts like [1997, 2006, 2016]
    games    "all", "regular" or "playoffs"
    side     "any", "home" or "away"

Each game counts once per team: at the first play inside the time window where the
team's margin is in range.

Needs:  pip install pandas numpy pymysql matplotlib
"""

import argparse
import math
import os
import re
from dataclasses import dataclass
from numbers import Real
from pathlib import Path

import numpy as np
import pandas as pd
import pymysql

# =====================================================================
# SETTINGS - check these first. (Python convention: module-level constants
# are written in CAPITALS. Nothing enforces it - it's a signal to the reader.)
# =====================================================================
DB_NAME = "wnba"                # MySQL database holding gms and wpg
QTR_SECS = 600                  # quarter length in seconds: 600 for the WNBA, 720 for the NBA
OT_SECS = 300                   # overtime length in seconds
GID_PREFIXES = ["102", "104"]   # first 3 characters of gid to keep: regular season and playoffs.
                                # None keeps every game in gms.
PLAY_ORDER = None               # a wpg column numbering the plays in each game, if it has one
OUT_DIR = "~/Dropbox/wnba/"     # where plot_wp_check() writes its PNG

# house style, as in wp_check.R
INK, INK_SOFT, RULE, PAPER = "#22211e", "#6f6b64", "#d9d6d0", "#ffffff"
ACCENT, MODEL_COL = "#3b6ea5", "#d0703a"   # actual, model


# =====================================================================
# Database. The login comes from ~/.Renviron, the same file R reads:
#   WNBA_DB_USER, WNBA_DB_PASS, WNBA_DB_HOST (optional)
# =====================================================================
def read_renviron(path="~/.Renviron"):
    """Copy KEY=value lines from ~/.Renviron into the environment, like R's readRenviron()."""
    p = Path(path).expanduser()
    if not p.exists():
        return
    for line in p.read_text().splitlines():
        line = line.strip()
        if line and not line.startswith("#") and "=" in line:
            key, value = line.split("=", 1)
            os.environ[key.strip()] = value.strip().strip("'\"")


def sqlf(sql):
    """Run a query and return a DataFrame - the R script's sqlf()."""
    read_renviron()
    con = pymysql.connect(host=os.environ.get("WNBA_DB_HOST", "localhost"), port=3306,
                          user=os.environ.get("WNBA_DB_USER", "root"),
                          password=os.environ.get("WNBA_DB_PASS", ""), database=DB_NAME)
    try:
        with con.cursor() as cur:   # "with" closes the cursor when the block ends, error or not
            cur.execute(sql)
            columns = [c[0] for c in cur.description]
            return pd.DataFrame(cur.fetchall(), columns=columns)
    finally:                        # R: on.exit(dbDisconnect(con))
        con.close()


# =====================================================================
# Game clock <-> seconds left in regulation (negative in overtime)
# =====================================================================
CLOCK_RE = re.compile(r"^(?:Q([1-4])|OT([0-9]*)) ?([0-9]+)(?::([0-9]+(?:\.[0-9]+)?))?$")


def clock_secs(x):
    """'Q2 10:00' -> 1800 (WNBA). A number is minutes left in regulation."""
    if isinstance(x, Real):                            # any number: int, float, numpy
        return x * 60
    m = CLOCK_RE.match(" ".join(x.upper().split()))
    if not m:
        raise ValueError(f'Can\'t read time "{x}" - use "Q2 10:00", "OT1 2:00" or minutes left in regulation')
    q, ot, mins, secs = m.groups()   # unmatched groups come back as None
    clock = int(mins) * 60 + float(secs or 0)
    if clock > (QTR_SECS if q else OT_SECS):
        raise ValueError(f"Clock longer than the period in {x}")
    if q:
        return (4 - int(q)) * QTR_SECS + clock
    return clock - int(ot or 1) * OT_SECS


def secs_clock(s, end=False):
    """1800 -> 'Q2 10:00'. A moment on a period boundary is the start of the next period
    when it opens a window, and the end of the period before when it closes one (end=True)."""
    if s < 0 or (s == 0 and not end):
        per = math.ceil(-s / OT_SECS) if end else math.floor(-s / OT_SECS) + 1
        left = s + per * OT_SECS
        name = f"OT{per}"
    else:
        per = max(1, 4 - math.floor(s / QTR_SECS)) if end else max(1, 5 - math.ceil(s / QTR_SECS))
        left = s - (4 - per) * QTR_SECS
        name = f"Q{per}"
    return f"{name} {int(left // 60)}:{int(left % 60):02d}"


def num(x):
    """Number for SQL text: -27.0 -> '-27', 1620.5 -> '1620.5'."""
    return f"{x:g}"


# =====================================================================
# The query - the same SQL wp_check.R sends
# =====================================================================
def situation_sql(margin, secs, seasons, games, side):
    clock = "CAST(w.`min` AS SIGNED) * 60 + CAST(w.sec AS DECIMAL(8,3))"
    t_left = (f"CASE WHEN CAST(w.q AS SIGNED) <= 4 THEN (4 - CAST(w.q AS SIGNED)) * {num(QTR_SECS)} + {clock}\n"
              f"                          ELSE {clock} - (CAST(w.q AS SIGNED) - 4) * {num(OT_SECS)} END")
    inner = ["w.wp IS NOT NULL", "g.hscr <> g.ascr"]
    if seasons is not None:
        inner.append(f"g.ssn IN ({', '.join(str(int(s)) for s in seasons)})")
    if GID_PREFIXES is not None:
        inner.append(f"LEFT(g.gid, 3) IN ({', '.join(repr(str(p)) for p in GID_PREFIXES)})")
    if games == "playoffs":
        inner.append("g.po = 1")
    if games == "regular":
        inner.append("(g.po IS NULL OR g.po <> 1)")
    home_ok = f"x.home_margin BETWEEN {num(margin[0])} AND {num(margin[1])}"
    away_ok = f"x.home_margin BETWEEN {num(-margin[1])} AND {num(-margin[0])}"
    side_ok = {"any": f"({home_ok} OR {away_ok})", "home": home_ok, "away": away_ok}[side]
    outer = [f"x.t_left BETWEEN {num(secs[0])} AND {num(secs[1])}", side_ok]
    # the regulation buzzer and "OT1 5:00" are the same moment: an overtime window
    # uses only overtime plays, a regulation window only regulation plays
    if secs[1] <= 0 and secs[0] < 0:
        outer.append("x.q > 4")
    elif secs[0] >= 0:
        outer.append("x.q <= 4")
    play_seq = f" w.`{PLAY_ORDER}` AS play_seq," if PLAY_ORDER else ""
    nl_and = "\n         AND "
    return f"""SELECT * FROM (
       SELECT g.ssn, g.dt, g.gid, g.po, g.htm, g.atm, g.hscr, g.ascr,
              w.q, w.`min`, w.sec, w.h, w.a, w.wp,{play_seq}
              {t_left} AS t_left,
              CAST(w.h AS SIGNED) - CAST(w.a AS SIGNED) AS home_margin
       FROM wpg w INNER JOIN gms g ON g.gid = w.gid
       WHERE {nl_and.join(inner)}
     ) x
     WHERE {nl_and.join(outer)}"""


# =====================================================================
# Stats
# =====================================================================
def norm_cdf(z):
    return 0.5 * (1 + math.erf(z / math.sqrt(2)))       # R: pnorm(z)


def wilson(x, n, z=1.959963984540054):                 # R: qnorm(0.975)
    """95% interval for a win rate."""
    p = x / n
    mid = (p + z**2 / (2 * n)) / (1 + z**2 / n)
    half = z * math.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / (1 + z**2 / n)
    return mid - half, mid + half


def poisbinom_p(k, p):
    """Two-sided p-value for k wins when game i is won with probability p[i]."""
    if len(p) > 20000:                                 # normal approximation
        mu, sd = p.sum(), math.sqrt((p * (1 - p)).sum())
        return min(1.0, 2 * min(norm_cdf((k + 0.5 - mu) / sd), 1 - norm_cdf((k - 0.5 - mu) / sd)))
    dist = np.array([1.0])                             # dist[j] = P(j wins so far)
    for pr in p:                                       # R: dist <- c(dist * (1 - pr), 0) + c(0, dist * pr)
        dist = np.append(dist * (1 - pr), 0) + np.append(0, dist * pr)
    return min(1.0, 2 * min(dist[:k + 1].sum(), dist[k:].sum()))   # Python slices start at 0, stop before the end


def tally(d):
    """One summary row (as a dict) for a group of team-games."""
    n, wins = len(d), int(d["won"].sum())
    lo, hi = wilson(wins, n)
    win_pct, model_pct = wins / n, d["team_wp"].mean()
    return {"n": n, "wins": wins, "win_pct": win_pct, "ci_lo": lo, "ci_hi": hi,
            "model_pct": model_pct, "exp_wins": d["team_wp"].sum(), "gap": win_pct - model_pct,
            "p_value": poisbinom_p(wins, d["team_wp"].to_numpy()), "avg_margin": d["margin"].mean()}


def season_group(ssn, by):
    """Label each season with its group: None, "season", a block length, or era starts."""
    if by is None:
        return pd.Series("All", index=ssn.index)
    if isinstance(by, str):
        if by != "season":
            raise ValueError('by must be None, "season", a block length like 5, or era starts like [1997, 2006, 2016]')
        return ssn.astype(str)
    first, last = int(ssn.min()), int(ssn.max())
    if isinstance(by, Real):
        starts = list(range(first, last + 1, int(by)))           # R: seq(first, last, by = by)
    else:
        starts = sorted({first, *[int(b) for b in by if b > first]})
    ends = [s - 1 for s in starts[1:]] + [last]
    labels = np.array([str(s) if s == e else f"{s}-{e}" for s, e in zip(starts, ends)])
    return pd.Series(labels[np.searchsorted(starts, ssn, side="right") - 1], index=ssn.index)   # R: findInterval()


# =====================================================================
# wp_check()
# =====================================================================
@dataclass
class WpCheck:
    """What wp_check() returns. Typing its name at the prompt prints the report."""
    summary: pd.DataFrame
    games: pd.DataFrame
    situation: str
    filters: str
    by: object

    def __repr__(self):
        s = self.summary
        tbl = pd.DataFrame({
            "Seasons": s["group"], "Games": s["n"], "Won": s["wins"], "Win %": s["win_pct"].map(pct),
            "95% CI": [f"{pct(a)}-{pct(b)}" for a, b in zip(s["ci_lo"], s["ci_hi"])],
            "Model %": s["model_pct"].map(pct), "Exp. wins": s["exp_wins"].map("{:.1f}".format),
            "Gap (pts)": (100 * s["gap"]).map("{:+.1f}".format),
            "p": s["p_value"].map(lambda v: "<0.001" if v < 0.001 else f"{v:.3f}")})
        if self.by is None:
            tbl = tbl.drop(columns="Seasons")
        out = [f"\n{self.situation} · {self.filters}\n", tbl.to_string(index=False),
               "p = chance of a gap at least this big if the model were right"]

        g = self.games
        odds = g["team_wp"].where(g["won"], 1 - g["team_wp"])     # R: ifelse(won, team_wp, 1 - team_wp)
        upsets = g[odds < 0.5].assign(odds=odds).sort_values("odds", kind="stable").head(10)
        if upsets.empty:
            out.append("\nNo upsets: every game went the way the model favored.")
        else:
            out.append("\nUpsets - results the model called less likely, least likely first:")
            out.append(pd.DataFrame({
                "Date": upsets["dt"],
                "Game": upsets["team"] + upsets["side"].map({"home": " vs ", "away": " @ "}) + upsets["opp"],
                "When": upsets["clock"], "Margin": upsets["margin"].map("{:+g}".format),
                "Model": upsets["team_wp"].map(pct),
                "Result": upsets["won"].map({True: "W ", False: "L "}) + upsets["final"],
                "gid": upsets["gid"]}).to_string(index=False))
        return "\n".join(out)


def pct(x):
    return f"{100 * x:.1f}%"


def wp_check(margin, time, seasons=None, by=None, games="all", side="any"):
    """How often did teams in this situation win, and what did the model say?"""
    if games not in ("all", "regular", "playoffs"):
        raise ValueError('games must be "all", "regular" or "playoffs"')
    if side not in ("any", "home", "away"):
        raise ValueError('side must be "any", "home" or "away"')
    margins = [margin, margin] if isinstance(margin, Real) else list(margin)
    times = [clock_secs(t) for t in time]
    if len(margins) != 2 or len(times) != 2:
        raise ValueError("margin takes one or two values, time takes two")
    lo_m, hi_m = sorted(margins)
    lo_t, hi_t = sorted(times)

    rows = sqlf(situation_sql((lo_m, hi_m), (lo_t, hi_t), seasons, games, side))
    if rows.empty:
        raise ValueError("No game was ever in that situation - widen margin or time")
    numeric = ["po", "hscr", "ascr", "q", "min", "sec", "h", "a", "wp", "t_left", "home_margin"]
    rows[numeric] = rows[numeric].astype(float)        # MySQL DECIMALs arrive as Python Decimal objects
    rows["ssn"] = rows["ssn"].astype(int)
    rows["dt"] = rows["dt"].astype(str)
    rows["seq"] = np.arange(len(rows)) if PLAY_ORDER is None else rows["play_seq"].astype(float)

    # one row per team per play, from that team's side
    # R: bind_rows(home = transmute(...), away = transmute(...), .id = "side")
    keep = ["ssn", "dt", "gid", "po", "q", "min", "sec", "t_left", "seq"]
    home = rows[keep].assign(side="home", team=rows["htm"], opp=rows["atm"], margin=rows["home_margin"],
                             team_wp=rows["wp"], pts=rows["hscr"], opp_pts=rows["ascr"])
    away = rows[keep].assign(side="away", team=rows["atm"], opp=rows["htm"], margin=-rows["home_margin"],
                             team_wp=1 - rows["wp"], pts=rows["ascr"], opp_pts=rows["hscr"])
    both = pd.concat([home, away], ignore_index=True)

    in_range = both["margin"].between(lo_m, hi_m) & ((side == "any") | (both["side"] == side))
    sample = (both[in_range]
              .sort_values(["gid", "side", "t_left", "seq"], ascending=[True, True, False, True])
              .drop_duplicates(["gid", "side"])           # R: distinct(gid, side, .keep_all = TRUE)
              .assign(won=lambda d: d["pts"] > d["opp_pts"]))
    sample["group"] = season_group(sample["ssn"], by)

    # R: sample |> group_by(group) |> group_modify(~ tally(.x))
    summary = pd.DataFrame([{"group": g, **tally(d)} for g, d in sample.groupby("group")])
    if by is not None:
        summary = pd.concat([summary, pd.DataFrame([{"group": "All", **tally(sample)}])], ignore_index=True)

    # dict.fromkeys() keeps the unique values in order, like R's unique()
    if hi_m < 0:
        margin_txt = "down " + "–".join(dict.fromkeys(f"{-m:g}" for m in (hi_m, lo_m)))
    elif lo_m > 0:
        margin_txt = "up " + "–".join(dict.fromkeys(f"{m:g}" for m in (lo_m, hi_m)))
    elif lo_m == hi_m == 0:
        margin_txt = "tied"
    else:
        margin_txt = f"with a margin of {lo_m:+g} to {hi_m:+g}"
    who = {"any": "Teams", "home": "Home teams", "away": "Road teams"}[side]
    situation = f"{who} {margin_txt}, {secs_clock(hi_t)} to {secs_clock(lo_t, end=True)}"
    seasons_txt = "–".join(dict.fromkeys(str(s) for s in (sample["ssn"].min(), sample["ssn"].max())))
    filters = {"all": "All games", "regular": "Regular season", "playoffs": "Playoffs"}[games] + ", " + seasons_txt

    q = sample["q"].astype(int)
    period = ("Q" + q.astype(str)).where(q <= 4, "OT" + (q - 4).astype(str))   # R: ifelse(q > 4, ..., ...)
    games_tbl = (sample.assign(clock=period + " " + sample["min"].astype(int).astype(str) + ":"
                               + np.floor(sample["sec"]).astype(int).astype(str).str.zfill(2),
                               final=sample["pts"].astype(int).astype(str) + "-"
                               + sample["opp_pts"].astype(int).astype(str))
                 [["ssn", "dt", "gid", "side", "team", "opp", "clock", "margin", "team_wp", "won", "final", "group"]]
                 .sort_values(["dt", "gid", "side"])
                 .reset_index(drop=True))
    return WpCheck(summary, games_tbl, situation, filters, by)


# =====================================================================
# Chart: actual win rate (dot + 95% interval) vs. the model (diamond), per group
# =====================================================================
def plot_wp_check(res, file=None):
    import matplotlib.pyplot as plt                    # imported here so wp_check() runs without matplotlib
    from matplotlib import font_manager
    from matplotlib.ticker import PercentFormatter

    s = res.summary
    if s["group"].nunique() > 1:
        s = s[s["group"] != "All"]
    x = np.arange(len(s))
    font = "Open Sans" if any(f.name == "Open Sans" for f in font_manager.fontManager.ttflist) else None
    with plt.rc_context({"font.family": font} if font else {}):
        fig, ax = plt.subplots(figsize=(max(7, 1.5 + 0.5 * len(s)), 4.8), facecolor=PAPER)
        ax.vlines(x, s["ci_lo"], s["ci_hi"], color=ACCENT, linewidth=2.4, zorder=2)
        ax.scatter(x, s["win_pct"], s=80, marker="o", color=ACCENT, edgecolor=PAPER, linewidth=1.3,
                   zorder=3, label="Actual win rate, with 95% interval")
        ax.scatter(x, s["model_pct"], s=80, marker="D", color=PAPER, edgecolor=MODEL_COL, linewidth=2,
                   zorder=4, label="Model's average win probability")
        ax.set_xticks(x, [f"{g}\n{n}" for g, n in zip(s["group"], s["n"])])
        ax.set_xlim(-0.6, len(s) - 0.4)
        ax.set_ylim(bottom=min(0, ax.get_ylim()[0]))  # R: expand_limits(y = 0)
        ax.yaxis.set_major_formatter(PercentFormatter(1.0, decimals=0 if ax.get_ylim()[1] >= 0.05 else 1))
        ax.grid(axis="y", color=RULE, linewidth=0.6)
        ax.set_axisbelow(True)
        ax.tick_params(colors=INK_SOFT, length=0, labelsize=9.5)
        for spine in ax.spines.values():
            spine.set_visible(False)

        fig.text(0.02, 0.97, res.situation, ha="left", va="top", fontsize=14, fontweight="bold", color=INK)
        fig.text(0.02, 0.905, f"{res.filters}. How often they won, against what the model gave them at the time.",
                 ha="left", va="top", fontsize=10.5, color=INK_SOFT)
        fig.legend(loc="upper left", bbox_to_anchor=(0.012, 0.855), ncol=2, frameon=False, fontsize=10,
                   handletextpad=0.3, columnspacing=1.5)
        fig.text(0.02, 0.025, "Number under each label = games in the sample. Each team-game counts once, at its\n"
                 "first play in the time window with the margin in range.",
                 ha="left", va="bottom", fontsize=8.5, color=INK_SOFT, linespacing=1.4)
        fig.subplots_adjust(left=0.08, right=0.98, top=0.76, bottom=0.2)
        if file:
            fig.savefig(Path(OUT_DIR).expanduser() / file, dpi=300, facecolor=PAPER)
    return fig


# =====================================================================
# Command line. This block runs only when the file is run as a script
# ("python wp_check.py ..."), not when another file imports it.
# =====================================================================
def main():
    ap = argparse.ArgumentParser(description="Test the win-probability model in one game situation.")
    ap.add_argument("--margin", nargs="+", type=float, required=True,
                    help="team's score minus opponent's: one value, or a low and high end, e.g. -27 -23")
    ap.add_argument("--time", nargs=2, required=True,
                    help='two clock points, e.g. "Q2 10:00" "Q2 7:00", or minutes left in regulation')
    ap.add_argument("--seasons", nargs=2, type=int, metavar=("FIRST", "LAST"), help="e.g. 2015 2025")
    ap.add_argument("--by", nargs="+", help='"season", a block length like 5, or era starts like 1997 2006 2016')
    ap.add_argument("--games", choices=["all", "regular", "playoffs"], default="all")
    ap.add_argument("--side", choices=["any", "home", "away"], default="any")
    ap.add_argument("--plot", metavar="FILE.png", help="also save the chart to OUT_DIR")
    args = ap.parse_args()

    margin = [int(m) if m.is_integer() else m for m in args.margin]
    time = [float(t) if re.fullmatch(r"[0-9.]+", t) else t for t in args.time]
    seasons = range(args.seasons[0], args.seasons[1] + 1) if args.seasons else None
    by = args.by
    if by is not None:
        by = by[0] if by == ["season"] else int(by[0]) if len(by) == 1 else [int(b) for b in by]

    res = wp_check(margin, time, seasons=seasons, by=by, games=args.games, side=args.side)
    print(res)
    if args.plot:
        plot_wp_check(res, args.plot)
        print(f"\nChart saved to {Path(OUT_DIR).expanduser() / args.plot}")


if __name__ == "__main__":
    main()
