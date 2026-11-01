"""
Step 1: pull nflverse play-by-play for every season and cache a slim copy.

R equivalent:
    pbp <- nflreadr::load_pbp(1999:2026) |>
      dplyr::filter(qtr >= 3) |>
      dplyr::select(all_of(KEEP))

nflreadpy is the Python version of nflreadr. It reads the same nflfastR
files, so every column name here matches what you see in R.

We keep 3rd-quarter through overtime plays. Situations are always in the 4th
quarter; the 3rd quarter is only there to look back (a drive or a score that
began just before the 4th started). Everything else the analysis needs
(final score, Vegas line, nflfastR win probability) is already on each play.

Run:  python3 01_pull_pbp.py            # all seasons 1999 -> current
      python3 01_pull_pbp.py 2015 2025  # a range of seasons
"""

import sys
from pathlib import Path

import nflreadpy as nfl
import polars as pl

HERE = Path(__file__).resolve().parent
OUT = HERE / "data" / "pbp_q3_to_ot.parquet"

# Columns kept. Each one is used in 02_build_situations.py.
KEEP = [
    # game identifiers
    "game_id", "play_id", "order_sequence", "season", "season_type", "week",
    "game_date", "home_team", "away_team", "home_coach", "away_coach", "roof",
    # play state
    "qtr", "game_seconds_remaining", "quarter_seconds_remaining",
    "posteam", "defteam", "down", "ydstogo", "yardline_100",
    "play_type", "play_type_nfl", "desc", "penalty", "penalty_team",
    "timeout", "timeout_team",
    # score state (posteam_score etc. are BEFORE the play,
    # total_home_score / total_away_score are AFTER the play)
    "posteam_score", "defteam_score", "score_differential",
    "total_home_score", "total_away_score",
    "posteam_timeouts_remaining", "defteam_timeouts_remaining",
    # kickoff details
    "kickoff_attempt", "own_kickoff_recovery", "touchback", "kick_distance",
    "return_yards", "return_touchdown", "kickoff_inside_twenty",
    "kickoff_in_endzone", "kickoff_out_of_bounds", "kickoff_fair_catch",
    "fumble_lost",
    # scoring flags
    "touchdown", "td_team", "field_goal_result", "safety",
    "extra_point_result", "two_point_conv_result", "defensive_two_point_conv",
    # drive info
    "fixed_drive", "fixed_drive_result", "drive_start_transition",
    # models and Vegas (wp / vegas_wp are from posteam's point of view)
    "wp", "vegas_wp", "ep",
    "spread_line", "total_line",
    # final outcome (result = home_score - away_score, includes OT)
    "home_score", "away_score", "result",
]


def pull_season(season: int) -> pl.DataFrame:
    pbp = nfl.load_pbp(season)
    cols = [c for c in KEEP if c in pbp.columns]
    missing = sorted(set(KEEP) - set(cols))
    if missing:
        print(f"  {season}: missing columns {missing}")
    return (
        pbp
        # remember the file's own play order before filtering
        .with_row_index("row_in_season")
        .filter(pl.col("qtr") >= 3)
        .select(["row_in_season"] + cols)
    )


def main(first: int, last: int) -> None:
    frames = []
    for season in range(first, last + 1):
        try:
            df = pull_season(season)
        except Exception as e:  # current season may not be posted yet
            print(f"  {season}: skipped ({e})")
            continue
        print(f"  {season}: {df.height:,} Q3-OT plays")
        frames.append(df)
    # how="diagonal_relaxed" works like dplyr::bind_rows(): it lines up
    # columns by name and reconciles small type differences between seasons
    pbp = pl.concat(frames, how="diagonal_relaxed")
    OUT.parent.mkdir(exist_ok=True)
    pbp.write_parquet(OUT)
    print(f"wrote {OUT} ({pbp.height:,} rows, {pbp['season'].n_unique()} seasons)")


if __name__ == "__main__":
    args = [int(a) for a in sys.argv[1:]]
    first = args[0] if args else 1999
    last = args[1] if len(args) > 1 else nfl.get_current_season()
    main(first, last)
