"""
Step 3: raw win rates by time bin, and a few ways to smooth them over time.

Input:  output/situations.csv (from 02_build_situations.py)
Output: output/raw_by_bin.csv          binned raw rates with Wilson 95% CIs
        output/smoothed_curves.csv     every smoother on a 0-15 minute grid
        output/model_summary.txt       fitted logistic-spline models
        output/win_prob_by_time.png    small multiples, one panel per deficit
        output/adjusted_curves.png     adjusted model, all four deficits

Smoothers, from least to most structure:
  1. LOWESS: local regression of the 0/1 outcome on minutes left. No model,
     only a bandwidth (`frac`). In R: lowess() or loess(); in ggplot,
     geom_smooth(method = "loess").
  2. Isotonic: the best-fitting curve that never goes DOWN as time left goes
     up. Down 7 with 8 minutes left should never be worse than with 4 left;
     isotonic enforces that. In R: isoreg().
  3. Logistic regression with a natural cubic spline in minutes left, one
     curve per deficit. In R:
        glm(t_win ~ factor(deficit) * splines::ns(mins_left, 3), family = binomial)
     Standard errors are clustered by game, because one game can add several
     rows (a kickoff and a later opponent-ball snap share one final result).
  4. Adjusted model: one shared time shape for all deficits, a level shift
     for each deficit, plus the pregame spread and the situation type. This
     borrows strength across deficits, which matters for the thin 9-point
     group. The time shape is ADJ_MODEL, one of the CANDIDATES compared by
     AIC/BIC in model_summary.txt. log(t) + sqrt(t) is tied for best BIC,
     best on AIC among the smooth shapes, and fits the last two minutes far
     better than log(t) alone (see the calibration table). Per-deficit time
     slopes did not earn their extra parameters.
     Curves are drawn for an even matchup (spread 0), kickoff rows.
     For a fully penalized GAM, use mgcv::gam(t_win ~ s(mins_left, by =
     factor(deficit)) + ...) in R, or pygam / statsmodels GLMGam in Python.

Change OUTCOME to "t_forced_ot_or_won_reg" to model "tied or led at the end of
regulation" instead of "won the game".
"""

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
from sklearn.isotonic import IsotonicRegression
from statsmodels.nonparametric.smoothers_lowess import lowess
from statsmodels.stats.proportion import proportion_confint

HERE = Path(__file__).resolve().parent
OUT = HERE / "output"

OUTCOME = "t_win"          # or "t_forced_ot_or_won_reg"
DEFICITS = [6, 7, 8, 9]
LOWESS_FRAC = 0.6          # bigger = smoother
SPLINE_DF = 3              # spline degrees of freedom per deficit (model 3)
BIN_EDGES = [0, 2, 4, 6, 8, 10, 12, 15]  # minutes left
GRID = np.linspace(0, 15, 61)
ADJ_MODEL = "log + sqrt time"   # any key of CANDIDATES in section 4

# ---- colors (validated categorical slots; text tokens for all text)
SURFACE, INK, INK2, GRIDC = "#fcfcfb", "#0b0b0b", "#52514e", "#e6e5e1"
BLUE, ORANGE, AQUA, YELLOW = "#2a78d6", "#eb6834", "#1baf7a", "#eda100"
MODEL_GRAY = "#9a9893"

# --------------------------------------------------------------------------
# data
# --------------------------------------------------------------------------
S = pd.read_csv(OUT / "situations.csv")
S = S[S["exclude_free_kick_after_safety"] == 0].copy()
S["y"] = S[OUTCOME]
S["game"] = pd.factorize(S["game_id"])[0]   # integer code for clustering
print(f"{len(S):,} situations from {S['game_id'].nunique():,} games, outcome = {OUTCOME}")

# --------------------------------------------------------------------------
# 1. raw rates by time bin (the table you'd make with dplyr::summarise)
# --------------------------------------------------------------------------
# labels like "02-04" sort in clock order; the last bin includes 15:00
labels = [f"{a:02d}-{b:02d}" for a, b in zip(BIN_EDGES[:-1], BIN_EDGES[1:])]
S["time_bin"] = pd.cut(S["mins_left"], BIN_EDGES, labels=labels, include_lowest=True, right=False)
S.loc[S["mins_left"] >= BIN_EDGES[-1], "time_bin"] = labels[-1]
# 0/1 columns for the non-reference deficits (used for deficit-specific slopes)
for d in DEFICITS[1:]:
    S[f"d{d}"] = (S["deficit"] == d).astype(int)
both = S.assign(source="both")


def summarise_bins(df: pd.DataFrame) -> pd.DataFrame:
    g = df.groupby(["source", "deficit", "time_bin"], observed=True)
    t = g.agg(
        n=("y", "size"),
        wins=("y", "sum"),
        mean_mins_left=("mins_left", "mean"),
        nflfastr_vegas_wp=("nflfastr_vegas_wp_t", "mean"),
        onside_rate=("ko_onside", "mean"),
    ).reset_index()
    lo, hi = proportion_confint(t["wins"], t["n"], method="wilson")
    t.insert(5, "rate", t["wins"] / t["n"])
    t.insert(6, "ci_lo", lo)
    t.insert(7, "ci_hi", hi)
    return t


raw = summarise_bins(pd.concat([S, both]))
raw["time_bin"] = raw["time_bin"].astype(str)
raw.round(4).to_csv(OUT / "raw_by_bin.csv", index=False)

wide = raw[raw["source"] == "both"].pivot(index="time_bin", columns="deficit", values=["n", "rate"])
print("\nRaw rates, kickoff + opp_ball pooled (rows = minutes left):")
print(wide.round(3).to_string())

# --------------------------------------------------------------------------
# 2. smoothers per deficit
# --------------------------------------------------------------------------
curves = []
for d in DEFICITS:
    sub = S[S["deficit"] == d].sort_values("mins_left")
    x, y = sub["mins_left"].to_numpy(), sub["y"].to_numpy()

    # it=0: the default robustness passes treat every 1 in 0/1 data as an
    # outlier and shrink the curve to zero
    lw = lowess(y, x, frac=LOWESS_FRAC, it=0, xvals=GRID)
    iso = IsotonicRegression(increasing=True, y_min=0, y_max=1, out_of_bounds="clip").fit(x, y)
    model_lw = lowess(sub["nflfastr_vegas_wp_t"].to_numpy(), x, frac=LOWESS_FRAC, it=0, xvals=GRID)

    curves.append(pd.DataFrame({
        "deficit": d, "mins_left": GRID,
        "lowess": np.clip(lw, 0, 1),
        "isotonic": iso.predict(GRID),
        "nflfastr_vegas_wp_lowess": model_lw,
    }))
curves = pd.concat(curves, ignore_index=True)

# ---- 3. logistic spline, one curve per deficit (unadjusted)
# constraints="center" keeps the spline from duplicating the intercept
f_sep = f"y ~ C(deficit) * cr(mins_left, df={SPLINE_DF}, constraints='center')"
m_sep = smf.glm(f_sep, data=S, family=sm.families.Binomial()).fit(
    cov_type="cluster", cov_kwds={"groups": S["game"]}
)
pred = m_sep.get_prediction(curves[["deficit", "mins_left"]]).summary_frame(alpha=0.05)
curves["spline"] = pred["mean"].to_numpy()
curves["spline_lo"] = pred["mean_ci_lower"].to_numpy()
curves["spline_hi"] = pred["mean_ci_upper"].to_numpy()

# ---- 4. adjusted, partially pooled model; time shape chosen by BIC
LOGT = "np.log(mins_left + 0.25)"   # +0.25 keeps log() finite at 0:00
ADJ = " + C(deficit) + t_pregame_spread + C(source)"
SLOPES = " + d7:{t} + d8:{t} + d9:{t}"
CANDIDATES = {
    "linear time": "y ~ mins_left" + ADJ,
    "log time": f"y ~ {LOGT}" + ADJ,
    "log + sqrt time": f"y ~ {LOGT} + np.sqrt(mins_left)" + ADJ,
    "spline df=3": "y ~ cr(mins_left, df=3, constraints='center')" + ADJ,
    "spline df=4": "y ~ cr(mins_left, df=4, constraints='center')" + ADJ,
    "log time + deficit slopes": f"y ~ {LOGT}" + ADJ + SLOPES.format(t=LOGT),
    "spline df=3 + deficit slopes": "y ~ cr(mins_left, df=3, constraints='center')" + ADJ
                                    + SLOPES.format(t="mins_left"),
}
fits = {k: smf.glm(f, data=S, family=sm.families.Binomial()).fit() for k, f in CANDIDATES.items()}
bic = pd.DataFrame({
    "params": {k: len(m.params) for k, m in fits.items()},
    "AIC": {k: m.aic for k, m in fits.items()},
    "BIC": {k: m.bic_llf for k, m in fits.items()},
}).sort_values("BIC")
best = ADJ_MODEL
f_adj = CANDIDATES[best]
print(f"\nadjusted-model candidates (lower is better), using '{best}':")
print(bic.round(1).to_string())
m_adj = smf.glm(f_adj, data=S, family=sm.families.Binomial()).fit(
    cov_type="cluster", cov_kwds={"groups": S["game"]}
)
new = curves[["deficit", "mins_left"]].assign(t_pregame_spread=0.0, source="kickoff")
for d in DEFICITS[1:]:
    new[f"d{d}"] = (new["deficit"] == d).astype(int)
pred = m_adj.get_prediction(new).summary_frame(alpha=0.05)
curves["adjusted"] = pred["mean"].to_numpy()
curves["adjusted_lo"] = pred["mean_ci_lower"].to_numpy()
curves["adjusted_hi"] = pred["mean_ci_upper"].to_numpy()

# calibration: observed vs in-sample predicted, by time bin (all deficits)
S["p_adj"] = m_adj.predict(S)
calib = S.groupby("time_bin", observed=True).agg(
    n=("y", "size"), observed=("y", "mean"), predicted=("p_adj", "mean"))
print(f"\ncalibration of '{best}' by minutes left:")
print(calib.round(3).to_string())

curves.round(4).to_csv(OUT / "smoothed_curves.csv", index=False)
with open(OUT / "model_summary.txt", "w") as fh:
    fh.write(f"Outcome: {OUTCOME}\n\n=== Model 3: {f_sep} ===\n")
    fh.write(str(m_sep.summary()))
    fh.write("\n\n=== Model 4 candidates (fit without clustering; lower BIC is better) ===\n")
    fh.write(bic.round(1).to_string())
    fh.write(f"\n\n=== Model 4 ({best}): {f_adj} ===\n")
    fh.write("calibration by minutes left:\n" + calib.round(3).to_string() + "\n\n")
    fh.write(str(m_adj.summary()))

print("\nAdjusted model (even matchup, kickoff), win prob at selected minutes left:")
print(curves[curves["mins_left"].isin([1, 2, 4, 6, 8, 10, 12, 14])]
      .pivot(index="mins_left", columns="deficit", values="adjusted").round(3).to_string())
print(f"\nmodel 4: pregame spread coef {m_adj.params['t_pregame_spread']:.3f} per point (log-odds)")
print(f"model 4: opp_ball vs kickoff coef {m_adj.params['C(source)[T.opp_ball]']:.3f} (log-odds)")


# --------------------------------------------------------------------------
# charts
# --------------------------------------------------------------------------
def style(ax):
    ax.set_facecolor(SURFACE)
    ax.grid(axis="y", color=GRIDC, linewidth=0.8)
    ax.set_axisbelow(True)
    for s in ["top", "right", "left"]:
        ax.spines[s].set_visible(False)
    ax.spines["bottom"].set_color(GRIDC)
    ax.tick_params(colors=INK2, labelsize=9, length=0)
    ax.set_xlim(15.3, -0.3)  # time runs left to right, like a game clock
    ax.set_xticks([15, 12, 9, 6, 3, 0])
    ax.yaxis.set_major_formatter(matplotlib.ticker.PercentFormatter(1, decimals=0))


plt.rcParams.update({"font.family": "DejaVu Sans", "text.color": INK})

# ---- small multiples: raw bins + every smoother
fig, axes = plt.subplots(1, 4, figsize=(15, 4.6), sharey=True, facecolor=SURFACE)
pts = raw[raw["source"] == "both"]
ymax = max(0.45, float(pts["ci_hi"].max()) + 0.02)
for ax, d in zip(axes, DEFICITS):
    style(ax)
    c = curves[curves["deficit"] == d]
    p = pts[pts["deficit"] == d]
    ax.fill_between(c["mins_left"], c["spline_lo"], c["spline_hi"], color=BLUE, alpha=0.12, linewidth=0)
    ax.plot(c["mins_left"], c["nflfastr_vegas_wp_lowess"], color=MODEL_GRAY, lw=1.5, label="nflfastR Vegas WP (avg)")
    ax.plot(c["mins_left"], c["isotonic"], color=AQUA, lw=1.5, drawstyle="steps-mid", label="Isotonic (monotone)")
    ax.plot(c["mins_left"], c["lowess"], color=ORANGE, lw=2, label=f"LOWESS (frac={LOWESS_FRAC})")
    ax.plot(c["mins_left"], c["spline"], color=BLUE, lw=2, label="Logistic spline ±95%")
    ax.errorbar(p["mean_mins_left"], p["rate"], yerr=[p["rate"] - p["ci_lo"], p["ci_hi"] - p["rate"]],
                fmt="none", ecolor=INK2, elinewidth=0.8, alpha=0.6)
    ax.scatter(p["mean_mins_left"], p["rate"], s=12 + p["n"] * 0.35, color=INK, edgecolor=SURFACE,
               linewidth=1.5, zorder=5, label="Raw rate by bin (size = n)")
    n, w = int((S["deficit"] == d).sum()), int(S.loc[S["deficit"] == d, "y"].sum())
    ax.set_title(f"Down {d}", loc="left", fontsize=13, fontweight="bold", color=INK)
    ax.set_title(f"n = {n:,}  ·  wins = {w}", loc="right", fontsize=9, color=INK2)
    ax.set_xlabel("Minutes left in the 4th quarter", fontsize=9, color=INK2)
    ax.set_ylim(0, ymax)
axes[0].set_ylabel("Trailing team win %" if OUTCOME == "t_win" else "Tied or led at end of regulation",
                   fontsize=10, color=INK2)
h, l = axes[0].get_legend_handles_labels()
order = [4, 3, 2, 1, 0]
fig.legend([h[i] for i in order], [l[i] for i in order], loc="lower center", ncol=5, frameon=False,
           fontsize=9, bbox_to_anchor=(0.5, -0.02))
fig.suptitle("Trailing team without the ball in the 4th quarter: kickoffs after a score + opponent 1st down at own 20-35, "
             f"{S['season'].min()}-{S['season'].max()}", x=0.01, ha="left", fontsize=12, color=INK)
fig.tight_layout(rect=(0, 0.06, 1, 0.95))
fig.savefig(OUT / "win_prob_by_time.png", dpi=160, facecolor=SURFACE)
plt.close(fig)

# ---- adjusted curves, all deficits on one axis, direct-labeled
fig, ax = plt.subplots(figsize=(8, 5), facecolor=SURFACE)
style(ax)
colors = dict(zip(DEFICITS, [BLUE, ORANGE, AQUA, YELLOW]))
ends = {}
for d in DEFICITS:
    c = curves[curves["deficit"] == d]
    ax.fill_between(c["mins_left"], c["adjusted_lo"], c["adjusted_hi"], color=colors[d], alpha=0.10, linewidth=0)
    ax.plot(c["mins_left"], c["adjusted"], color=colors[d], lw=2, label=f"Down {d}")
    ends[d] = float(c.loc[c["mins_left"] == 15, "adjusted"].iloc[0])
# direct labels at 15:00, nudged apart so they don't overlap
gap = 0.016
placed = []
for d, y in sorted(ends.items(), key=lambda kv: kv[1]):
    y_lab = max(y, placed[-1] + gap) if placed else y
    placed.append(y_lab)
    ax.text(15.45, y_lab, f"Down {d}  {y:.0%}", color=INK, fontsize=9, va="center", ha="right")
ax.set_xlim(17.6, -0.3)
ax.set_ylim(0, None)
ax.set_xlabel("Minutes left in the 4th quarter", fontsize=9, color=INK2)
ax.set_ylabel("Trailing team win %" if OUTCOME == "t_win" else "Tied or led at end of regulation",
              fontsize=10, color=INK2)
ax.legend(frameon=False, fontsize=9, loc="upper right")
ax.set_title(f"Adjusted model ({best}): even matchup, trailing team kicking off",
             loc="left", fontsize=11, color=INK)
fig.tight_layout()
fig.savefig(OUT / "adjusted_curves.png", dpi=160, facecolor=SURFACE)
plt.close(fig)
print(f"\nwrote raw_by_bin.csv, smoothed_curves.csv, model_summary.txt and two PNGs to {OUT}")
