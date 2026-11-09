// possim/engine.cpp - win probability, expected points and simulation from the
// possession model. Loaded by 04_engine.R with Rcpp::sourceCpp().
//
// Game state: period q (1-4, 5 = any overtime), seconds left in the period tr,
// home lead m (clamped to +/- M), team with the ball (0 home, 1 away) and how
// the possession started (0..5 = MFG MFT DRB STL DTO PST).
//
// Tables (built by 04_engine.R from the fitted model, the same for every game):
//   P0o  outcome probabilities for an average offense/defense, by state
//   P0d  seconds probabilities (1..ND) for an average pair, by a coarse state and outcome
// A game's teams enter as a multiplier on each outcome and an exponential tilt
// on seconds, for the home and away offense. Both have a base part and a part
// for each late-game regime (common.R: regime_w):
//   ltH, ltA    K x R log multipliers (column 1 base, then one per regime)
//   lamH, lamA  R seconds tilts
//   fw          regime weights by (seconds left in regulation, capped, x lead), from R

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

static const int NS = 6;        // start types
static const int PST = 5;
static const int MOCAP = 30;    // outcome model lead cap
static const int DMOCAP = 12;   // seconds model lead cap
static const int DTRCAP = 61;    // these three mirror dur_tr_cap / dur_gr_cap in common.R
static const int DGRCAP = 721;
static const int NDKEY = 61 + 720;
static const int NMW = 2 * DMOCAP + 1;   // lead values in the regime tables

static inline int plen(int q) { return q <= 4 ? 720 : 300; }
static inline int qoff(int q) { return q <= 4 ? 720 * (q - 1) : 2880; }
static inline int greg(int q, int tr) { return q <= 4 ? tr + 720 * (4 - q) : tr; }   // seconds left in regulation (OT: in the period)

struct Model {
  const double *P0o, *P0d;
  int K, ND, pend, GI;           // outcomes, seconds classes, index of 0PEND, regime clock values
  std::vector<int> pts, nxt, dk; // per outcome: points, next start type, seconds-table column (-1 for 0PEND)
  std::vector<double> tilt[2];   // by (clock, lead): K outcome multipliers
  std::vector<double> elam[2];   // by (clock, lead): exp(lam * d), d = 0..ND
  inline long rkey(int q, int tr, int mo) const {
    int gi = std::min(greg(q, tr), GI - 1);
    int moc = std::max(-DMOCAP, std::min(DMOCAP, mo));
    return (long)gi * NMW + moc + DMOCAP;
  }
  inline const double *tilt_at(int side, int q, int tr, int mo) const { return tilt[side].data() + rkey(q, tr, mo) * K; }
  inline const double *elam_at(int side, int q, int tr, int mo) const { return elam[side].data() + rkey(q, tr, mo) * (ND + 1); }
};

// row of P0o: ((qoff + tr - 1) * 61 + mo + 30) * NS + st
static inline long orow(int q, int tr, int mo, int st) {
  int moc = std::max(-MOCAP, std::min(MOCAP, mo));
  return ((long)(qoff(q) + tr - 1) * (2 * MOCAP + 1) + (moc + MOCAP)) * NS + st;
}
// Q1-Q3: the last 61 seconds of the period by the second, otherwise one key.
// Q4 and overtime: every second left.
static inline int dkey(int q, int tr) {
  int gr = greg(q, tr);
  if (gr >= DGRCAP) return std::min(tr, DTRCAP) - 1;   // 0..60
  return 61 + gr - 1;                                  // 61..780
}
// row of P0d: ((dkey * (K-1) + dk_outcome) * NS + st) * 25 + moc + 12, so one
// second's rows sit together
static inline long drow(int nk, int dko, int st, int key, int mo) {
  int moc = std::max(-DMOCAP, std::min(DMOCAP, mo));
  return (((long)key * nk + dko) * NS + st) * NMW + (moc + DMOCAP);
}

static Model make_model(NumericVector P0o, NumericVector P0d, IntegerVector pts, IntegerVector nxt, int pend,
                        NumericMatrix ltH, NumericMatrix ltA, NumericVector lamH, NumericVector lamA,
                        NumericVector fw, int ND) {
  Model md;
  md.P0o = P0o.begin(); md.P0d = P0d.begin();
  md.K = pts.size(); md.ND = ND; md.pend = pend;
  md.pts.assign(pts.begin(), pts.end());
  md.nxt.assign(nxt.begin(), nxt.end());
  md.dk.resize(md.K);
  int j = 0;
  for (int k = 0; k < md.K; k++) md.dk[k] = (k == pend) ? -1 : j++;
  int R = ltH.ncol();
  if (ltH.nrow() != md.K || ltA.nrow() != md.K || ltA.ncol() != R || lamH.size() != R || lamA.size() != R)
    stop("team terms: expected K x R multipliers and R seconds tilts for both teams");
  md.GI = R > 1 ? (int)(fw.size() / (NMW * (R - 1))) : 1;
  if (R > 1 && (long)md.GI * NMW * (R - 1) != fw.size()) stop("regime weights have the wrong length");
  for (int side = 0; side < 2; side++) {
    const NumericMatrix &lt = side == 0 ? ltH : ltA;
    const NumericVector &lm = side == 0 ? lamH : lamA;
    md.tilt[side].resize((long)md.GI * NMW * md.K);
    md.elam[side].resize((long)md.GI * NMW * (ND + 1));
    for (long r = 0; r < (long)md.GI * NMW; r++) {
      const double *w = R > 1 ? &fw[r * (R - 1)] : nullptr;
      double lam = lm[0];
      for (int c = 1; c < R; c++) lam += w[c - 1] * lm[c];
      for (int k = 0; k < md.K; k++) {
        double e = lt(k, 0);
        for (int c = 1; c < R; c++) e += w[c - 1] * lt(k, c);
        md.tilt[side][r * md.K + k] = std::exp(e);
      }
      for (int d = 0; d <= ND; d++) md.elam[side][r * (ND + 1) + d] = std::exp(lam * d);
    }
  }
  return md;
}

// outcome probabilities for one state, this game's teams
static inline void outcome_probs(const Model &md, int q, int tr, int mo, int st, int side, double *po) {
  const double *b = md.P0o + orow(q, tr, mo, st) * md.K;
  const double *tl = md.tilt_at(side, q, tr, mo);
  double s = 0;
  for (int k = 0; k < md.K; k++) {
    double v = b[k] * tl[k];
    if (k == md.pend && tr > md.ND) v = 0;   // the clock can't run out with that much left
    po[k] = v; s += v;
  }
  for (int k = 0; k < md.K; k++) po[k] /= s;
}

// seconds probabilities (1..ND) for one outcome; mass past the period's end stays at d >= tr
static inline void dur_probs(const Model &md, int k, int q, int tr, int mo, int st, int side, double *pd) {
  const double *b = md.P0d + drow(md.K - 1, md.dk[k], st, dkey(q, tr), mo) * md.ND;
  const double *el = md.elam_at(side, q, tr, mo);
  double s = 0;
  for (int d = 1; d <= md.ND; d++) { double v = b[d - 1] * el[d]; pd[d] = v; s += v; }
  for (int d = 1; d <= md.ND; d++) pd[d] /= s;
}

// ── Backward recursion over one period ───────────────────────────────────────
// V holds, for each state, the home win probability and the expected points
// still to come for each team. Layout [((m+M)*2 + side)*NS + st] * (len+1) + tr,
// so seconds run contiguously. endW/endH/endA: values when the period ends with
// home lead m (index m+M).
struct PeriodV {
  int q, len, M;
  std::vector<double> W, H, A;
  inline long at(int m, int side, int st, int tr) const {
    return ((long)((m + M) * 2 + side) * NS + st) * (len + 1) + tr;
  }
};

static PeriodV dp_period(const Model &md, int q, int M, const std::vector<double> &endW,
                         const std::vector<double> &endH, const std::vector<double> &endA, bool want_pts) {
  PeriodV v; v.q = q; v.len = plen(q); v.M = M;
  long n = (long)(2 * M + 1) * 2 * NS * (v.len + 1);
  v.W.assign(n, 0); if (want_pts) { v.H.assign(n, 0); v.A.assign(n, 0); }
  for (int m = -M; m <= M; m++) for (int side = 0; side < 2; side++) for (int st = 0; st < NS; st++) {
    long i = v.at(m, side, st, 0);
    v.W[i] = endW[m + M];
    if (want_pts) { v.H[i] = endH[m + M]; v.A[i] = endA[m + M]; }
  }
  double po[32], pd[64];
  for (int tr = 1; tr <= v.len; tr++) {
    for (int m = -M; m <= M; m++) for (int side = 0; side < 2; side++) {
      int mo = side == 0 ? m : -m;
      const double *el = md.elam_at(side, q, tr, mo);
      int key = dkey(q, tr);
      for (int st = 0; st < NS; st++) {
        outcome_probs(md, q, tr, mo, st, side, po);
        double w = 0, eh = 0, ea = 0;
        for (int k = 0; k < md.K; k++) {
          if (po[k] < 1e-12) continue;
          if (k == md.pend) {                  // clock runs out, no points
            w += po[k] * endW[m + M];
            if (want_pts) { eh += po[k] * endH[m + M]; ea += po[k] * endA[m + M]; }
            continue;
          }
          int pk = md.pts[k];
          int m2 = side == 0 ? m + pk : m - pk;
          m2 = std::max(-M, std::min(M, m2));
          int side2 = 1 - side, st2 = md.nxt[k];
          // seconds weights, left unnormalised: divide the sums by their total once
          const double *b = md.P0d + drow(md.K - 1, md.dk[k], st, key, mo) * md.ND;
          const double *VW = v.W.data() + v.at(m2, side2, st2, 0) + tr;
          double sw = 0, sh = 0, sa = 0, tot = 0, tail = 0;
          int dmax = std::min(md.ND, tr - 1);
          if (want_pts) {
            const double *VH = v.H.data() + v.at(m2, side2, st2, 0) + tr;
            const double *VA = v.A.data() + v.at(m2, side2, st2, 0) + tr;
            for (int d = 1; d <= dmax; d++) {
              double p = b[d - 1] * el[d];
              tot += p; sw += p * VW[-d]; sh += p * VH[-d]; sa += p * VA[-d];
            }
          } else {
            for (int d = 1; d <= dmax; d++) { double p = b[d - 1] * el[d]; tot += p; sw += p * VW[-d]; }
          }
          for (int d = dmax + 1; d <= md.ND; d++) tail += b[d - 1] * el[d];
          tot += tail;
          sw += tail * endW[m2 + M];
          w += po[k] * sw / tot;
          if (want_pts) {
            sh += tail * endH[m2 + M]; sa += tail * endA[m2 + M];
            eh += po[k] * (sh / tot + (side == 0 ? pk : 0));
            ea += po[k] * (sa / tot + (side == 1 ? pk : 0));
          }
        }
        long i = v.at(m, side, st, tr);
        v.W[i] = w;
        if (want_pts) { v.H[i] = eh; v.A[i] = ea; }
      }
    }
  }
  return v;
}

// Who has the ball to start each period, given who won the opening tip
// (tip = 0 home, 1 away): the loser starts Q2 and Q3, the winner Q4.
static inline int start_side(int q, int tip) { return (q == 2 || q == 3) ? 1 - tip : tip; }

// Look up states in a finished period: queries has columns q, tr, m, side, st.
static void answer(const PeriodV &v, const IntegerMatrix &qs, NumericMatrix &res, bool want_pts) {
  for (int i = 0; i < qs.nrow(); i++) {
    int q = std::min(qs(i, 0), 5);
    if (q != v.q) continue;
    int tr = std::max(0, std::min(v.len, qs(i, 1)));
    int m = std::max(-v.M, std::min(v.M, qs(i, 2)));
    long j = v.at(m, qs(i, 3), qs(i, 4), tr);
    res(i, 0) = v.W[j];
    if (want_pts) { res(i, 1) = v.H[j]; res(i, 2) = v.A[j]; }
  }
}

// Win probability (home) and expected points still to come for each team, at
// each state in `queries` (columns q, tr, m = home lead, side, st; q >= 5 is
// overtime). return_tables = TRUE also returns every state's values.
// [[Rcpp::export]]
List dp_game(NumericVector P0o, NumericVector P0d, IntegerVector pts, IntegerVector nxt, int pend,
             NumericMatrix ltH, NumericMatrix ltA, NumericVector lamH, NumericVector lamA,
             NumericVector fw, int ND, int tip, IntegerMatrix queries, int M = 60, bool want_pts = true,
             int ot_iter = 3, bool return_tables = false) {
  Model md = make_model(P0o, P0d, pts, nxt, pend, ltH, ltA, lamH, lamA, fw, ND);
  NumericMatrix res(queries.nrow(), 3);
  std::fill(res.begin(), res.end(), NA_REAL);
  int nm = 2 * M + 1;
  std::vector<double> eW(nm), eH(nm, 0), eA(nm, 0);

  // Overtime: a tie after an overtime means another one, which starts like
  // this one (jump ball, so either team may get the ball). Iterate to the fixed point.
  double xw = 0.5, xh = 0, xa = 0;
  PeriodV ot;
  for (int it = 0; it < ot_iter; it++) {
    for (int m = -M; m <= M; m++) { eW[m + M] = m > 0 ? 1 : (m < 0 ? 0 : xw); eH[m + M] = m == 0 ? xh : 0; eA[m + M] = m == 0 ? xa : 0; }
    ot = dp_period(md, 5, M, eW, eH, eA, want_pts);
    long i0 = ot.at(0, 0, PST, 300), i1 = ot.at(0, 1, PST, 300);
    xw = 0.5 * (ot.W[i0] + ot.W[i1]);
    if (want_pts) { xh = 0.5 * (ot.H[i0] + ot.H[i1]); xa = 0.5 * (ot.A[i0] + ot.A[i1]); }
  }
  // End of regulation: tied goes to overtime.
  for (int m = -M; m <= M; m++) { eW[m + M] = m > 0 ? 1 : (m < 0 ? 0 : xw); eH[m + M] = m == 0 ? xh : 0; eA[m + M] = m == 0 ? xa : 0; }

  answer(ot, queries, res, want_pts);
  List tables(5);
  auto keep = [&](const PeriodV &v) {
    return List::create(_["W"] = wrap(v.W), _["H"] = want_pts ? wrap(v.H) : R_NilValue,
                        _["A"] = want_pts ? wrap(v.A) : R_NilValue);
  };
  if (return_tables) tables[4] = keep(ot);
  for (int q = 4; q >= 1; q--) {
    PeriodV v = dp_period(md, q, M, eW, eH, eA, want_pts);
    answer(v, queries, res, want_pts);
    if (return_tables) tables[q - 1] = keep(v);
    // the value of starting this period with home lead m feeds the period before
    int s = start_side(q, tip);
    for (int m = -M; m <= M; m++) {
      long i = v.at(m, s, PST, v.len);
      eW[m + M] = v.W[i];
      if (want_pts) { eH[m + M] = v.H[i]; eA[m + M] = v.A[i]; }
    }
  }
  return List::create(_["values"] = res, _["tables"] = return_tables ? tables : List());
}

// ── Simulation from a state ──────────────────────────────────────────────────
// Returns nsim x 3: points the home team adds, points the away team adds, and
// the number of overtimes.
// [[Rcpp::export]]
IntegerMatrix sim_game(NumericVector P0o, NumericVector P0d, IntegerVector pts, IntegerVector nxt, int pend,
                       NumericMatrix ltH, NumericMatrix ltA, NumericVector lamH, NumericVector lamA,
                       NumericVector fw, int ND, int q0, int tr0, int m0, int side0, int st0, int tip, int nsim) {
  Model md = make_model(P0o, P0d, pts, nxt, pend, ltH, ltA, lamH, lamA, fw, ND);
  IntegerMatrix out(nsim, 3);
  double po[32], pd[64];
  RNGScope scope;
  for (int i = 0; i < nsim; i++) {
    int q = q0, tr = tr0, m = m0, side = side0, st = st0, hp = 0, ap = 0, nots = 0;
    while (true) {
      if (tr <= 0) {   // period over
        if (q >= 4 && m != 0) break;
        if (q >= 4) { q = 5; nots++; tr = 300; side = R::runif(0, 1) < 0.5 ? 0 : 1; st = PST; continue; }
        q++; tr = 720; side = start_side(q, tip); st = PST; continue;
      }
      int mo = side == 0 ? m : -m;
      outcome_probs(md, q, tr, mo, st, side, po);
      double u = R::runif(0, 1), c = 0; int k = 0;
      for (k = 0; k < md.K - 1; k++) { c += po[k]; if (u < c) break; }
      if (k == md.pend) { tr = 0; continue; }
      dur_probs(md, k, q, tr, mo, st, side, pd);
      u = R::runif(0, 1); c = 0; int d = 1;
      for (d = 1; d < md.ND; d++) { c += pd[d]; if (u < c) break; }
      int pk = md.pts[k];
      if (side == 0) { hp += pk; m += pk; } else { ap += pk; m -= pk; }
      tr -= d; side = 1 - side; st = md.nxt[k];
    }
    out(i, 0) = hp; out(i, 1) = ap; out(i, 2) = nots;
  }
  return out;
}
