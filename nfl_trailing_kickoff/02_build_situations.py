"""
Step 2: build one row per "trailing team doesn't have the ball" situation.

Situation A, "kickoff":
    4th quarter. Team T has just scored and is kicking off, and still trails
    by 6, 7, 8 or 9 points after the extra point / 2-point try.

Situation B, "opp_ball":
    4th quarter. Team T trails by 6-9 and the leading team L has a 1st down
    between its own 20 and own 35 (yardline_100 from 65 to 80). This is
    about where a normal kickoff leaves L. One row per L drive: the first
    snap that qualifies. Drives that began with a Situation A kickoff are
    dropped, because the kickoff row already covers them.

In both situations nflfastR has L as `posteam` (on kickoffs posteam is the
RECEIVING team), T as `defteam`, and score_differential equals T's deficit.

Output: output/situations.csv, the raw data, one row per situation.
Columns are described in README.md.

pandas <-> dplyr cheat sheet for this file:
    df[df.x > 1]                      filter(x > 1)
    df.assign(y=...)                  mutate(y = ...)
    df.groupby(g).head(1)             group_by(g) |> slice_head(n = 1)
    df.drop_duplicates(keys)          distinct(across(keys), .keep_all=TRUE)
    df.merge(other, on=k, how=...)    left_join(other, by = k)
    np.where(cond, a, b)              if_else(cond, a, b)
"""

from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
PBP = HERE / "data" / "pbp_q3_to_ot.parquet"
OUT = HERE / "output" / "situations.csv"

DEFICITS = [6, 7, 8, 9]
OWN_20, OWN_35 = 80, 65  # as yardline_100 (yards from the opponent's end zone)

KICK_REGEX = r"\bkicks\b.{0,20}?\b-?\d+ yards? from\b"


# --------------------------------------------------------------------------
# helpers
# --------------------------------------------------------------------------
def kick_era(season: int) -> str:
    """Broad kickoff-rule eras (see README for the specific rule changes)."""
    if season <= 2010:
        return "1999-2010 kick from 30"
    if season <= 2017:
        return "2011-2017 kick from 35"
    if season <= 2023:
        return "2018-2023 new kick formation"
    return "2024+ dynamic kickoff"


def ot_rule(season: int, season_type: str) -> str:
    """Overtime format the game was played under."""
    if season_type == "POST":
        if season <= 2009:
            return "sudden death"
        return "both teams possess" if season >= 2022 else "modified sudden death"
    if season <= 2011:
        return "sudden death"
    if season >= 2025:
        return "both teams possess (10 min)"
    return "modified sudden death (15 min)" if season <= 2016 else "modified sudden death (10 min)"


def classify_points(pts: float) -> str:
    return {3: "FG", 6: "TD, no try", 7: "TD + XP", 8: "TD + 2pt", 0: "none (after safety?)"}.get(
        pts, "other" if pd.notna(pts) else "unknown"
    )


def classify_score(delta: float) -> str:
    if delta >= 6:
        return "TD"
    if delta == 3:
        return "FG"
    if delta == 2:
        return "safety"
    return "other"


# --------------------------------------------------------------------------
# load
# --------------------------------------------------------------------------
pbp = pd.read_parquet(PBP)
# file order is the true play order (order_sequence is missing in 1999-2000)
pbp = pbp.sort_values(["season", "row_in_season"]).reset_index(drop=True)
pbp["desc"] = pbp["desc"].fillna("")

is_scrimmage = pbp["down"].notna() & pbp["posteam"].notna()
is_kick = (pbp["kickoff_attempt"] == 1) | pbp["desc"].str.contains(KICK_REGEX, regex=True)
pbp["is_scrimmage"] = is_scrimmage
pbp["is_kick"] = is_kick & pbp["posteam"].notna()

# row positions of each game's plays: like split(seq_len(nrow(pbp)), pbp$game_id)
game_rows = pbp.groupby("game_id", sort=False).indices

# --------------------------------------------------------------------------
# Situation A: trailing team kicking off
# --------------------------------------------------------------------------
kicks = pbp[(pbp["qtr"] == 4) & pbp["is_kick"]].copy()
kicks["pos"] = kicks.index
# a re-kick after a penalty is a second row at the same score: keep the first
# row for the game state, and remember the last row (the kick that counted)
key = ["game_id", "posteam", "posteam_score", "defteam_score"]
first_kick = kicks.drop_duplicates(key, keep="first")
last_kick = kicks.drop_duplicates(key, keep="last")[key + ["pos"]].rename(columns={"pos": "live_pos"})
n_kicks = kicks.groupby(key).size().rename("n_kicks").reset_index()  # count(game_id, ...)
A = first_kick.merge(last_kick, on=key, how="left").merge(n_kicks, on=key, how="left")
A = A[A["score_differential"].isin(DEFICITS)].copy()
A["source"] = "kickoff"

# --------------------------------------------------------------------------
# Situation B: leading team has 1st down at its own 20-35
# --------------------------------------------------------------------------
snap_types = ["run", "pass", "no_play", "qb_kneel", "qb_spike"]
B = pbp[
    (pbp["qtr"] == 4)
    & pbp["is_scrimmage"]
    & (pbp["down"] == 1)
    & pbp["play_type"].isin(snap_types)
    & pbp["score_differential"].isin(DEFICITS)
    & pbp["yardline_100"].between(OWN_35, OWN_20)
].copy()
B["pos"] = B.index
B = B.drop_duplicates(["game_id", "fixed_drive"], keep="first")
B["source"] = "opp_ball"

# first scrimmage play of each drive, to flag whether B's snap started the drive
drive_first = (
    pbp[pbp["is_scrimmage"]]
    .drop_duplicates(["game_id", "fixed_drive"], keep="first")
    .assign(drive_first_pos=lambda d: d.index)[["game_id", "fixed_drive", "drive_first_pos"]]
)
B = B.merge(drive_first, on=["game_id", "fixed_drive"], how="left")


# --------------------------------------------------------------------------
# outcomes for each situation, looking forward from that play
# --------------------------------------------------------------------------
cols = {c: pbp[c].to_numpy() for c in [
    "qtr", "posteam", "fixed_drive", "is_scrimmage", "yardline_100",
    "total_home_score", "total_away_score", "touchdown", "td_team",
    "own_kickoff_recovery", "fixed_drive_result", "drive_start_transition",
]}


def outcomes(row) -> dict:
    g = row["game_id"]
    rows = game_rows[g]
    i = row["pos"]
    T, L = row["defteam"], row["posteam"]
    t_home = T == row["home_team"]
    t_pre, l_pre = row["defteam_score"], row["posteam_score"]

    later = rows[rows >= i]
    t_after = np.where(t_home, cols["total_home_score"][later], cols["total_away_score"][later])
    l_after = np.where(t_home, cols["total_away_score"][later], cols["total_home_score"][later])
    in_reg = cols["qtr"][later] == 4

    out = {}
    # ---- next score in regulation
    changed = in_reg & ((t_after != t_pre) | (l_after != l_pre))
    if changed.any():
        k = np.argmax(changed)
        dt, dl = t_after[k] - t_pre, l_after[k] - l_pre
        out["next_score_team"] = "trailing" if dt > 0 else "leading"
        out["next_score_type"] = classify_score(max(dt, dl))
        out["next_score_secs_left"] = pbp.at[later[k], "game_seconds_remaining"]
    else:
        out["next_score_team"] = "none"
        out["next_score_type"] = "none"
        out["next_score_secs_left"] = np.nan

    # ---- did T ever tie or lead later in regulation?
    out["t_tied_or_led_later_reg"] = int(((t_after - l_after >= 0) & in_reg).any())

    # ---- score at end of regulation
    reg_idx = np.where(in_reg)[0]
    last_reg = reg_idx[-1]
    out["reg_end_margin"] = t_after[last_reg] - l_after[last_reg]

    # ---- T possessions after this play (regulation only)
    after = later[(later > i) & in_reg]
    t_poss = after[(cols["posteam"][after] == T) & cols["is_scrimmage"][after]]
    out["t_possessions_after"] = len(np.unique(cols["fixed_drive"][t_poss]))

    # ---- what happened right away
    scrim_after = later[cols["is_scrimmage"][later] & (later >= i)]
    first_scrim = scrim_after[0] if len(scrim_after) else None
    if row["source"] == "kickoff":
        live = row["live_pos"]
        td = cols["touchdown"][live] == 1
        td_team = cols["td_team"][live]
        kick_td_against = td and td_team == L
        kick_td_for = td and td_team == T
        # score unchanged between kick and first scrimmage play?
        same_score = False
        if first_scrim is not None:
            j = np.searchsorted(later, first_scrim)
            same_score = (t_after[: j].max(initial=t_pre) == t_pre) and (l_after[: j].max(initial=l_pre) == l_pre)
        got_ball = kick_td_for or (
            first_scrim is not None and same_score and cols["posteam"][first_scrim] == T
        )
        out["ko_kicking_team_recovered"] = int(got_ball)
        out["ko_return_td_against"] = int(kick_td_against)
        if (not got_ball) and (not kick_td_against) and first_scrim is not None and same_score:
            out["lead_start_own_yardline"] = 100 - cols["yardline_100"][first_scrim]
            out["lead_drive_result"] = cols["fixed_drive_result"][first_scrim]
        else:
            out["lead_start_own_yardline"] = np.nan
            out["lead_drive_result"] = "Touchdown (kick return)" if kick_td_against else None
        out["lead_drive_key"] = (
            cols["fixed_drive"][first_scrim] if first_scrim is not None else np.nan
        )
    else:
        out["lead_start_own_yardline"] = 100 - row["yardline_100"]
        out["lead_drive_result"] = row["fixed_drive_result"]
    return out


A_out = pd.DataFrame([outcomes(r) for _, r in A.iterrows()], index=A.index)
A = pd.concat([A, A_out], axis=1)
B_out = pd.DataFrame([outcomes(r) for _, r in B.iterrows()], index=B.index)
B = pd.concat([B, B_out], axis=1)

# drop B drives already covered by a Situation A kickoff (the receiving drive)
a_drives = set(zip(A["game_id"], A["lead_drive_key"]))
covered = np.array([(g, d) in a_drives for g, d in zip(B["game_id"], B["fixed_drive"])])
print(f"B: dropping {covered.sum():,} drives that follow a Situation A kickoff")
B = B[~covered].copy()

# ---- Situation A specifics: what T just scored, onside, kick details
def points_just_scored(row) -> float:
    rows = game_rows[row["game_id"]]
    prev = rows[(rows < row["pos"]) & cols["is_scrimmage"][rows]]
    if len(prev) == 0:
        return np.nan
    j = prev[-1]
    T = row["defteam"]
    t_before = pbp.at[j, "posteam_score"] if pbp.at[j, "posteam"] == T else pbp.at[j, "defteam_score"]
    return row["defteam_score"] - t_before


A["t_points_just_scored"] = A.apply(points_just_scored, axis=1)
A["t_score_type"] = A["t_points_just_scored"].map(classify_points)
live = pbp.loc[A["live_pos"].to_numpy()]
A["ko_onside"] = (
    live["desc"].str.contains("onside", case=False).to_numpy()
    | (live["kick_distance"].to_numpy() <= 20)
).astype(int)
A["ko_touchback"] = live["touchback"].fillna(0).astype(int).to_numpy()
A["ko_kick_distance"] = live["kick_distance"].to_numpy()
A["ko_return_yards"] = live["return_yards"].to_numpy()
A["ko_desc"] = live["desc"].to_numpy()

B["b_first_snap_of_drive"] = (B["pos"] == B["drive_first_pos"]).astype(int)
B["b_ydstogo"] = B["ydstogo"]
# older seasons spell these "Punt", newer ones "PUNT"
B["b_how_lead_got_ball"] = B["drive_start_transition"].str.upper().str.replace(" ", "_")

# --------------------------------------------------------------------------
# combine and add shared columns
# --------------------------------------------------------------------------
S = pd.concat([A, B], ignore_index=True, sort=False)

S["trailing_team"] = S["defteam"]
S["leading_team"] = S["posteam"]
S["deficit"] = S["score_differential"].astype(int)
S["secs_left"] = S["game_seconds_remaining"].astype(int)
S["mins_left"] = (S["secs_left"] / 60).round(2)
S["t_is_home"] = (S["trailing_team"] == S["home_team"]).astype(int)
S["t_score"] = S["defteam_score"].astype(int)
S["l_score"] = S["posteam_score"].astype(int)
S["t_timeouts"] = S["defteam_timeouts_remaining"]
S["l_timeouts"] = S["posteam_timeouts_remaining"]

# pregame closing line from T's side: positive = T was favored
S["t_pregame_spread"] = np.where(S["t_is_home"] == 1, S["spread_line"], -S["spread_line"])
# nflfastR model probabilities, flipped to T's side (posteam is L here)
S["nflfastr_wp_t"] = 1 - S["wp"]
S["nflfastr_vegas_wp_t"] = 1 - S["vegas_wp"]

# final outcome for T
S["t_final_margin"] = np.where(S["t_is_home"] == 1, S["result"], -S["result"])
S["t_win"] = (S["t_final_margin"] > 0).astype(int)
S["t_tie"] = (S["t_final_margin"] == 0).astype(int)
S["t_loss"] = (S["t_final_margin"] < 0).astype(int)
S["went_ot"] = (S["reg_end_margin"] == 0).astype(int)
S["t_won_in_reg"] = (S["reg_end_margin"] > 0).astype(int)
S["t_won_in_ot"] = ((S["went_ot"] == 1) & (S["t_win"] == 1)).astype(int)
S["t_forced_ot_or_won_reg"] = (S["reg_end_margin"] >= 0).astype(int)

S["kick_era"] = S["season"].map(kick_era)
S["pat_from_15"] = (S["season"] >= 2015).astype(int)
S["ot_rule"] = [ot_rule(s, t) for s, t in zip(S["season"], S["season_type"])]

KEEP = [
    # who / when
    "source", "season", "season_type", "week", "game_date", "game_id", "play_id",
    "home_team", "away_team", "trailing_team", "leading_team", "t_is_home",
    "home_coach", "away_coach", "roof",
    # state at the moment
    "deficit", "t_score", "l_score", "secs_left", "mins_left",
    "t_timeouts", "l_timeouts", "lead_start_own_yardline",
    "t_pregame_spread", "total_line", "nflfastr_wp_t", "nflfastr_vegas_wp_t",
    # Situation A only
    "t_points_just_scored", "t_score_type", "n_kicks", "ko_onside",
    "ko_kicking_team_recovered", "ko_touchback", "ko_kick_distance",
    "ko_return_yards", "ko_return_td_against", "ko_desc",
    # Situation B only
    "b_first_snap_of_drive", "b_ydstogo", "b_how_lead_got_ball",
    # what happened next
    "lead_drive_result", "next_score_team", "next_score_type",
    "next_score_secs_left", "t_possessions_after", "t_tied_or_led_later_reg",
    # how it ended
    "reg_end_margin", "went_ot", "t_final_margin", "t_win", "t_tie", "t_loss",
    "t_won_in_reg", "t_won_in_ot", "t_forced_ot_or_won_reg",
    # eras
    "kick_era", "pat_from_15", "ot_rule",
]
S = S[KEEP].sort_values(["season", "game_id", "secs_left"], ascending=[True, True, False])

# Situation A rows that weren't preceded by a T score (free kick after a
# safety) are a different situation: flag them so they can be dropped
S["exclude_free_kick_after_safety"] = (
    (S["source"] == "kickoff") & (S["t_points_just_scored"] <= 0)
).astype(int)

OUT.parent.mkdir(exist_ok=True)
S.to_csv(OUT, index=False)

# --------------------------------------------------------------------------
# quick report
# --------------------------------------------------------------------------
print(f"wrote {OUT}: {len(S):,} rows")
print(pd.crosstab([S["source"]], S["deficit"], margins=True))
print("\nfree kicks after a safety (flagged):", S["exclude_free_kick_after_safety"].sum())
print("\nT win rate by source x deficit:")
print(S[S.exclude_free_kick_after_safety == 0].pivot_table(
    index="source", columns="deficit", values="t_win", aggfunc="mean").round(3))
