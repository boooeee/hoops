# possim/01_possessions.R - one row per possession, ready for modelling
#
# Possession boundaries, start rows and offense/defense come from the pos table
# (written by wpNBA_rtask.R). Everything else comes from the wpg plays:
#   pts   the offense's score change over the possession, from the wpg score
#         columns, leaving out technical free throws. (pos.pts loses the play
#         after a technical free throw, about 1.5 points a game before 2025.)
#   st    how the possession started (start_types in common.R)
#   oc    how it ended: points and how the other team got the ball (outcomes)
#   tr    seconds left in the period when it started
#   mo    the offense's lead when it started (real score, technicals included)
#   dur   seconds it took (at least 1, see below)
# A possession where the defense's score moves has the wrong team on offense
# (wpg can flip the ball on flagrant and technical free throws). Those, and the
# possession after each, are flagged bad: kept for the sequence, left out of fitting.
# Writes possessions.rds to data_dir.

here <- local({   # this script's folder, whether run with Rscript or source()d
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) Sys.getenv("POSSIM_CODE", "~/hoops/possim") else dirname(normalizePath(f))
})
source(file.path(here, "common.R"))

all_ssn <- first_ssn:max(test_ssn)

# ── Games ────────────────────────────────────────────────────────────────────
gms <- sqlq(paste0("SELECT ssn, gid, dt, htm, atm, hscr, ascr, line, total FROM gms WHERE ssn IN ",
                   in_list(all_ssn), " AND LEFT(gid,3) IN ", in_list(gid_types)))
gms[, `:=`(date = as.Date(substr(dt, 1, 10)), dt = NULL,
           htm = fix_team(htm), atm = fix_team(atm))]
gms[is.na(total) | total < 150, total := NA]

# ── Possessions, a season at a time ──────────────────────────────────────────
one_season <- function(s) {
  pf <- in_list(gid_prefixes(s))
  pos <- sqlq(paste0("SELECT gid, posid, r, gt, spt1, off, def, gtnx, time FROM pos ",
                     "WHERE LEFT(gid,5) IN ", pf))
  w <- sqlq(paste0("SELECT gid, r, q, m, h, a, pt2, ",
                   "(pt1 LIKE 'M%' AND d LIKE '%echnical%') AS tech FROM wpg WHERE LEFT(gid,5) IN ", pf))
  w <- unique(w, by = c("gid", "r"))
  setorder(w, gid, r)
  w[, `:=`(dh = h - shift(h, fill = 0), da = a - shift(a, fill = 0)), by = gid]

  # Each play belongs to the possession whose start row comes before it: the
  # possession's own plays, through the play that ends it.
  setorder(pos, gid, r)
  w[, rj := r - 0.5]
  own <- pos[, .(gid, rj = as.numeric(r), posid)][w, on = .(gid, rj), roll = Inf]
  sc <- own[!is.na(posid), .(sh = sum(dh[tech == 0]), sa = sum(da[tech == 0]),
                             tech_pts = sum((dh + da)[tech == 1])), by = .(gid, posid)]

  pos <- merge(pos, w[, .(gid, r, q, m, pt2)], by = c("gid", "r"), all.x = TRUE)
  pos <- merge(pos, sc, by = c("gid", "posid"), all.x = TRUE)
  message(s, ": ", uniqueN(pos$gid), " games, ", nrow(pos), " possessions")
  pos
}
p <- rbindlist(lapply(all_ssn, one_season))
p[, `:=`(off = fix_team(off), def = fix_team(def))]
p <- merge(p, gms[, .(gid, ssn, date, htm, atm, hscr, ascr, line, total)], by = "gid")
setorder(p, gid, posid)
p[is.na(sh), `:=`(sh = 0, sa = 0, tech_pts = 0)]

# ── Points ───────────────────────────────────────────────────────────────────
p[, home := as.integer(off == htm)]
p[, `:=`(pts = fifelse(home == 1L, sh, sa), def_pts = fifelse(home == 1L, sa, sh))]

# ── Start type ───────────────────────────────────────────────────────────────
# The first possession of each period is PST, whatever started it (the opening
# tip, an overtime tip, or the period-start row for Q2-Q4).
p[, first_in_q := seq_len(.N) == 1L, by = .(gid, q)]
p[, st := start_type(first_in_q, spt1, pt2)]

# ── End type = the next possession's start type ──────────────────────────────
p[, nxt := shift(st, -1L), by = gid]
p[is.na(nxt), nxt := "PST"]            # last possession of the game
p[, ptsc := pmin(pmax(pts, 0), 4)]
p[, oc := fcase(
  ptsc == 0 & nxt == "PST",                       "0PEND",
  ptsc == 0 & nxt %in% c("DRB", "STL", "DTO"),    paste0("0", nxt),
  ptsc == 0,                                      "0DTO",   # 0 points then an inbound: a slip
  ptsc == 4,                                      "4MFT",
  nxt == "PST" & ptsc == 1,                       "1MFT",   # scored as the period ended
  nxt == "PST",                                   paste0(ptsc, "MFG"),
  ptsc == 1 & nxt == "MFG",                       "1MFT",
  nxt %in% c("STL", "DTO"),                       paste0(ptsc, "DRB"),
  default = paste0(ptsc, nxt))]
p[, ock := outcomes$k[match(oc, outcomes$oc)]]

# ── Clock and score ──────────────────────────────────────────────────────────
p[, tr := period_end(q) - gt]
p[, dur := time]
p[, mo := fifelse(home == 1L, m, -m)]
p[, gr := fifelse(q <= 4, tr + 720L * (4L - q), tr)]   # seconds left in regulation (in OT: in the period)

# The opening tip's winner gets Q4; the loser gets Q2 and Q3.
tip <- p[q == 1 & first_in_q, .(gid, tip_home = home)]
p <- merge(p, tip, by = "gid", all.x = TRUE)
setorder(p, gid, posid)

# ── Checks ───────────────────────────────────────────────────────────────────
broken <- p[, .(is.na(q) | is.na(st) | is.na(ock) | dur < 0 | dur > tr | tr < 0 | tr > period_len(q)), by = gid]
bad_gid <- unique(broken[V1 == TRUE, gid])
message(length(bad_gid), " games with a possession that can't be read are left out entirely")
p <- p[!(gid %in% bad_gid)]

# Wrong team on offense: the defense's score moved, or the offense scored 5+.
p[, bad := def_pts != 0 | pts < 0 | pts > 4]
p[, bad := bad | shift(bad, 1L, fill = FALSE), by = gid]
message("possessions flagged bad (wrong offense, and the one after): ", sum(p$bad), " of ", nrow(p),
        " (", round(100 * mean(p$bad), 2), "%)")

# Every point is accounted for: the offense's, the technicals', and the defense's
# on the bad possessions.
chk <- p[, .(sp = sum(pts) + sum(def_pts) + sum(tech_pts), tp = sum(tech_pts)), by = .(gid, hscr, ascr)]
message("games where the points don't add up to the final score: ",
        chk[sp != hscr + ascr, .N], " of ", nrow(chk),
        "; technical free-throw points a game: ", round(mean(chk$tp), 2))

# Seconds: 0 means two plays logged in the same second. The simulation needs
# the clock to move, so those count as 1 second (1.5% of possessions).
p[, dur := pmin(pmax(dur, 1L), tr)]
# A possession that starts with 0 left is the scorekeeper's, not a real one
# (the other team scored at the buzzer).
message("possessions starting with no time left (dropped): ", p[tr == 0, .N])
p <- p[tr > 0]
p[, durc := pmin(dur, max_dur)]

keep <- c("gid", "ssn", "date", "posid", "q", "gt", "tr", "gr", "off", "def", "home", "htm", "atm",
          "st", "oc", "ock", "pts", "dur", "durc", "mo", "m", "tech_pts", "bad", "tip_home",
          "line", "total", "hscr", "ascr")
p <- p[, ..keep]
saveRDS(p, rds("possessions"))

cat("\nOutcome mix (training seasons, good possessions):\n")
print(p[ssn %in% train_ssn & !bad, .N, by = oc][order(-N)][, pct := round(100 * N / sum(N), 2)][])
cat("\nStart types:\n")
print(p[ssn %in% train_ssn & !bad, .(.N, ppp = mean(pts), secs = mean(dur)), by = st][order(-N)])
cat("\nBy season:\n")
print(p[bad == FALSE, .(games = uniqueN(gid), poss = .N, ppp = round(mean(pts), 3), secs = round(mean(dur), 2)), by = ssn][order(ssn)])
cat("\nSaved", nrow(p), "possessions from", uniqueN(p$gid), "games to", rds("possessions"), "\n")
