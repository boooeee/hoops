# Step 3: raw win rates by time bin, and a few ways to smooth them over time.
#
# Input:  output/situations.csv (from 02_build_situations.R)
# Output: output/raw_by_bin.csv          binned raw rates with Wilson 95% CIs
#         output/smoothed_curves.csv     every smoother on a 0-15 minute grid
#         output/model_summary.txt       fitted models, AIC/BIC comparison, calibration
#         output/win_prob_by_time.png    small multiples, one panel per deficit
#         output/adjusted_curves.png     adjusted model, all four deficits
#
# Smoothers, from least to most structure:
#   1. LOWESS: local linear regression of the 0/1 outcome on minutes left.
#      Only a bandwidth (lowess_f). iter = 0 matters: the default robustness
#      passes treat every win in 0/1 data as an outlier and pull the curve to 0.
#   2. Isotonic: the best-fitting curve that never goes DOWN as time left goes
#      up. Down 7 with 8 minutes left should never be worse than with 4 left.
#      (Weighted pool-adjacent-violators below; base isoreg() doesn't handle
#      tied x values cleanly.)
#   3. Logistic regression with a natural spline in minutes left, one curve per
#      deficit: glm(y ~ factor(deficit) * ns(mins_left, 3), binomial).
#      Standard errors are clustered by game, because one game can add several
#      rows (a kickoff and a later opponent-ball snap share one final result).
#   4. Adjusted model: one shared time shape for all deficits, a level shift for
#      each deficit, plus the pregame spread and the situation type. This
#      borrows strength across deficits, which matters for the thin 9-point
#      group. The time shape is adj_model, one of `candidates`, compared by
#      AIC/BIC in model_summary.txt. log(t) + sqrt(t) is tied for best BIC,
#      best on AIC among the smooth shapes, and fits the last two minutes far
#      better than log(t) alone (see the calibration table). Per-deficit time
#      slopes did not earn their extra parameters. Curves are drawn for an
#      even matchup (spread 0), kickoff rows.
#      For penalized smooths instead: mgcv::gam(y ~ s(mins_left, by = factor(deficit))
#      + factor(deficit) + t_pregame_spread + source, family = binomial).

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(splines)
library(sandwich)
library(lmtest)

# =====================================================================
# SETTINGS
# =====================================================================
proj_dir  <- "."   # folder holding these scripts: setwd() there, or put the full path here
out_dir   <- file.path(proj_dir, "output")
outcome   <- "t_win"            # or "t_forced_ot_or_won_reg"
deficits  <- 6:9
lowess_f  <- 0.6                # bigger = smoother
spline_df <- 3                  # spline degrees of freedom per deficit (model 3)
bin_edges <- c(0, 2, 4, 6, 8, 10, 12, 15)   # minutes left
grid      <- seq(0, 15, by = 0.25)
adj_model <- "log + sqrt time"  # any name in `candidates` (section 4)

# colors: validated categorical slots; text uses the ink colors only
SURFACE <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"; GRIDC <- "#e6e5e1"
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; AQUA <- "#1baf7a"; YELLOW <- "#eda100"
MODEL_GRAY <- "#9a9893"

y_label <- if (outcome == "t_win") "Trailing team win %" else "Tied or led at end of regulation"

# =====================================================================
# data
# =====================================================================
bin_labels <- sprintf("%02d-%02d", head(bin_edges, -1), tail(bin_edges, -1))

S <- read_csv(file.path(out_dir, "situations.csv"), show_col_types = FALSE, guess_max = 1e5) %>%
  filter(exclude_free_kick_after_safety == 0) %>%
  mutate(
    y        = .data[[outcome]],
    time_bin = cut(mins_left, bin_edges, labels = bin_labels, right = FALSE, include.lowest = TRUE),
    d7 = as.integer(deficit == 7), d8 = as.integer(deficit == 8), d9 = as.integer(deficit == 9)
  )
message(format(nrow(S), big.mark = ","), " situations from ", n_distinct(S$game_id),
        " games, outcome = ", outcome)

# =====================================================================
# 1. raw rates by time bin
# =====================================================================
wilson <- function(x, n, z = qnorm(0.975)) {
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  list(lo = centre - half, hi = centre + half)
}

raw <- bind_rows(S, mutate(S, source = "both")) %>%
  group_by(source, deficit, time_bin) %>%
  summarise(
    n                 = n(),
    wins              = sum(y),
    mean_mins_left    = mean(mins_left),
    nflfastr_vegas_wp = mean(nflfastr_vegas_wp_t),
    onside_rate       = if (all(is.na(ko_onside))) NA_real_ else mean(ko_onside, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rate  = wins / n,
         ci_lo = wilson(wins, n)$lo,
         ci_hi = wilson(wins, n)$hi,
         .after = wins)

write_csv(raw %>% mutate(across(where(is.double), ~ round(.x, 4))),
          file.path(out_dir, "raw_by_bin.csv"), na = "")

cat("\nRaw rates, kickoff + opp_ball pooled (rows = minutes left):\n")
raw %>%
  filter(source == "both") %>%
  mutate(cell = sprintf("%3.0f%% (%d/%d)", 100 * rate, wins, n)) %>%
  select(time_bin, deficit, cell) %>%
  pivot_wider(names_from = deficit, names_prefix = "down_", values_from = cell) %>%
  arrange(desc(time_bin)) %>%
  print()

# =====================================================================
# 2. LOWESS and isotonic, per deficit
# =====================================================================
# weighted pool-adjacent-violators on the unique x values
iso_fit <- function(x, y) {
  agg  <- tibble(x, y) %>% group_by(x) %>% summarise(y = mean(y), w = n(), .groups = "drop")
  vals <- numeric(0); wts <- numeric(0); cnt <- integer(0)
  for (i in seq_len(nrow(agg))) {
    vals <- c(vals, agg$y[i]); wts <- c(wts, agg$w[i]); cnt <- c(cnt, 1L)
    while (length(vals) > 1 && vals[length(vals) - 1] > vals[length(vals)]) {
      k <- length(vals)
      vals[k - 1] <- (vals[k - 1] * wts[k - 1] + vals[k] * wts[k]) / (wts[k - 1] + wts[k])
      wts[k - 1]  <- wts[k - 1] + wts[k]
      cnt[k - 1]  <- cnt[k - 1] + cnt[k]
      vals <- vals[-k]; wts <- wts[-k]; cnt <- cnt[-k]
    }
  }
  fitted <- rep(vals, cnt)
  function(xnew) approx(agg$x, fitted, xout = xnew, rule = 2)$y
}

lowess_grid <- function(x, y) {
  lw <- lowess(x, y, f = lowess_f, iter = 0)
  approx(lw$x, lw$y, xout = grid, rule = 2, ties = mean)$y
}

curves <- lapply(deficits, function(d) {
  sub <- S %>% filter(deficit == d)
  tibble(
    deficit   = d,
    mins_left = grid,
    lowess    = pmin(pmax(lowess_grid(sub$mins_left, sub$y), 0), 1),
    isotonic  = iso_fit(sub$mins_left, sub$y)(grid),
    nflfastr_vegas_wp_lowess = lowess_grid(sub$mins_left, sub$nflfastr_vegas_wp_t)
  )
}) %>% bind_rows()

# =====================================================================
# 3 and 4. logistic models with game-clustered standard errors
# =====================================================================
# predictions with a 95% band from a given covariance matrix
predict_ci <- function(m, V, newdata) {
  tt <- delete.response(terms(m))
  X  <- model.matrix(tt, model.frame(tt, newdata, xlev = m$xlevels))
  eta <- drop(X %*% coef(m))
  se  <- sqrt(rowSums((X %*% V) * X))
  tibble(fit = plogis(eta), lo = plogis(eta - 1.96 * se), hi = plogis(eta + 1.96 * se))
}

# ---- 3. natural spline, one curve per deficit (unadjusted)
f_sep <- as.formula(sprintf("y ~ factor(deficit) * ns(mins_left, df = %d)", spline_df))
m_sep <- glm(f_sep, family = binomial, data = S)
V_sep <- vcovCL(m_sep, cluster = ~ game_id)
p_sep <- predict_ci(m_sep, V_sep, curves)
curves <- curves %>% mutate(spline = p_sep$fit, spline_lo = p_sep$lo, spline_hi = p_sep$hi)

# ---- 4. adjusted, partially pooled model
adj_terms <- " + factor(deficit) + t_pregame_spread + source"
candidates <- c(
  "linear time"                  = "y ~ mins_left",
  "log time"                     = "y ~ log(mins_left + 0.25)",   # +0.25 keeps log() finite at 0:00
  "log + sqrt time"              = "y ~ log(mins_left + 0.25) + sqrt(mins_left)",
  "spline df=3"                  = "y ~ ns(mins_left, df = 3)",
  "spline df=4"                  = "y ~ ns(mins_left, df = 4)",
  "log time + deficit slopes"    = "y ~ log(mins_left + 0.25) * factor(deficit)",
  "spline df=3 + deficit slopes" = "y ~ ns(mins_left, df = 3) + d7:mins_left + d8:mins_left + d9:mins_left"
)
fits <- lapply(candidates, function(f) glm(as.formula(paste0(f, adj_terms)), family = binomial, data = S))
ic <- tibble(model  = names(fits),
             params = sapply(fits, function(m) length(coef(m))),
             AIC    = sapply(fits, AIC),
             BIC    = sapply(fits, BIC)) %>%
  arrange(BIC)
cat(sprintf("\nadjusted-model candidates (lower is better), using '%s':\n", adj_model))
print(ic %>% mutate(across(c(AIC, BIC), ~ round(.x, 1))))

m_adj <- fits[[adj_model]]
V_adj <- vcovCL(m_adj, cluster = ~ game_id)
new_adj <- curves %>% select(deficit, mins_left) %>%
  mutate(t_pregame_spread = 0, source = "kickoff",
         d7 = as.integer(deficit == 7), d8 = as.integer(deficit == 8), d9 = as.integer(deficit == 9))
p_adj <- predict_ci(m_adj, V_adj, new_adj)
curves <- curves %>% mutate(adjusted = p_adj$fit, adjusted_lo = p_adj$lo, adjusted_hi = p_adj$hi)

# calibration: observed vs in-sample predicted, by time bin (all deficits)
calib <- S %>%
  mutate(p_adj = fitted(m_adj)) %>%
  group_by(time_bin) %>%
  summarise(n = n(), observed = mean(y), predicted = mean(p_adj), .groups = "drop")
cat(sprintf("\ncalibration of '%s' by minutes left:\n", adj_model))
print(calib %>% mutate(across(c(observed, predicted), ~ round(.x, 3))))

write_csv(curves %>% mutate(across(where(is.double), ~ round(.x, 4))),
          file.path(out_dir, "smoothed_curves.csv"))

ct_adj <- coeftest(m_adj, vcov = V_adj)
sink(file.path(out_dir, "model_summary.txt"))
cat("Outcome:", outcome, "\n\n")
cat("=== Model 3:", deparse(f_sep), "===\n(game-clustered standard errors)\n")
print(coeftest(m_sep, vcov = V_sep))
cat("\n=== Model 4 candidates (lower is better) ===\n")
print(as.data.frame(ic %>% mutate(across(c(AIC, BIC), ~ round(.x, 1)))), row.names = FALSE)
cat(sprintf("\n=== Model 4 (%s): %s ===\n", adj_model, deparse(formula(m_adj))))
cat("calibration by minutes left:\n")
print(as.data.frame(calib %>% mutate(across(c(observed, predicted), ~ round(.x, 3)))), row.names = FALSE)
cat("\n(game-clustered standard errors)\n")
print(ct_adj)
sink()

cat("\nAdjusted model (even matchup, kickoff), win prob at selected minutes left:\n")
curves %>%
  filter(mins_left %in% c(1, 2, 4, 6, 8, 10, 12, 14)) %>%
  select(mins_left, deficit, adjusted) %>%
  mutate(adjusted = round(adjusted, 3)) %>%
  pivot_wider(names_from = deficit, names_prefix = "down_", values_from = adjusted) %>%
  print()
cat(sprintf("\nmodel 4: pregame spread coef %.3f per point (log-odds)\n", coef(m_adj)[["t_pregame_spread"]]))
cat(sprintf("model 4: opp_ball vs kickoff coef %.3f (log-odds), p = %.3f\n",
            coef(m_adj)[["sourceopp_ball"]], ct_adj["sourceopp_ball", 4]))

# =====================================================================
# charts
# =====================================================================
theme_viz <- function() {
  theme_minimal(base_size = 10) +
    theme(
      plot.background    = element_rect(fill = SURFACE, colour = NA),
      panel.background   = element_rect(fill = SURFACE, colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = GRIDC, linewidth = 0.3),
      axis.line.x        = element_line(colour = GRIDC, linewidth = 0.3),
      axis.text          = element_text(colour = INK2),
      axis.title         = element_text(colour = INK2, size = 9),
      plot.title         = element_text(colour = INK, size = 12),
      plot.subtitle      = element_text(colour = INK2, size = 9),
      strip.text         = element_text(colour = INK, hjust = 0, size = 11, face = "bold"),
      legend.position    = "bottom",
      legend.text        = element_text(colour = INK, size = 9),
      plot.title.position = "plot"
    )
}

# ---- small multiples: raw bins + every smoother
panel_labels <- S %>%
  group_by(deficit) %>%
  summarise(panel = sprintf("Down %d   (n = %s, wins = %d)", first(deficit),
                            format(n(), big.mark = ","), sum(y)), .groups = "drop")

method_levels <- c(spline = "Logistic spline (95% band)", lowess = sprintf("LOWESS (f = %.1f)", lowess_f),
                   isotonic = "Isotonic (monotone)", nflfastr_vegas_wp_lowess = "nflfastR Vegas WP (avg)")
method_colors <- setNames(c(BLUE, ORANGE, AQUA, MODEL_GRAY), method_levels)

curves_p <- curves %>% left_join(panel_labels, by = "deficit")
lines_long <- curves_p %>%
  select(panel, mins_left, all_of(names(method_levels))) %>%
  pivot_longer(all_of(names(method_levels)), names_to = "method", values_to = "p") %>%
  mutate(method = factor(method_levels[method], levels = rev(method_levels)))
pts <- raw %>% filter(source == "both") %>% left_join(panel_labels, by = "deficit")

p1 <- ggplot() +
  geom_ribbon(data = curves_p, aes(mins_left, ymin = spline_lo, ymax = spline_hi),
              fill = BLUE, alpha = 0.12) +
  geom_line(data = filter(lines_long, !grepl("Isotonic", method)),
            aes(mins_left, p, colour = method), linewidth = 0.7) +
  geom_step(data = filter(lines_long, grepl("Isotonic", method)),
            aes(mins_left, p, colour = method), linewidth = 0.55, direction = "mid") +
  geom_errorbar(data = pts, aes(x = mean_mins_left, ymin = ci_lo, ymax = ci_hi),
                width = 0, colour = INK2, alpha = 0.6, linewidth = 0.3) +
  geom_point(data = pts, aes(mean_mins_left, rate, size = n),
             shape = 21, fill = INK, colour = SURFACE, stroke = 0.6) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_reverse(breaks = c(15, 12, 9, 6, 3, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim = c(0, max(0.45, max(pts$ci_hi) + 0.02))) +
  scale_colour_manual(values = method_colors, breaks = method_levels) +
  scale_size_area(max_size = 4, guide = "none") +
  labs(title = sprintf("Trailing team without the ball in the 4th quarter, %d-%d",
                       min(S$season), max(S$season)),
       subtitle = "Kickoffs after a score + opponent 1st down at own 20-35. Dots: raw rate by time bin (size = n) with 95% CI.",
       x = "Minutes left in the 4th quarter", y = y_label, colour = NULL) +
  theme_viz()
ggsave(file.path(out_dir, "win_prob_by_time.png"), p1, width = 15, height = 4.8, dpi = 160, bg = SURFACE)

# ---- adjusted curves, all deficits on one axis, direct-labeled at 15:00
deficit_colors <- setNames(c(BLUE, ORANGE, AQUA, YELLOW), paste("Down", deficits))
adj <- curves %>% mutate(series = factor(paste("Down", deficit), levels = names(deficit_colors)))
labs_15 <- adj %>%
  filter(mins_left == 15) %>%
  arrange(adjusted) %>%
  mutate(y_lab = adjusted)
for (i in seq_len(nrow(labs_15))[-1])   # nudge labels apart so they don't overlap
  labs_15$y_lab[i] <- max(labs_15$y_lab[i], labs_15$y_lab[i - 1] + 0.016)

p2 <- ggplot(adj, aes(mins_left, adjusted, colour = series, fill = series)) +
  geom_ribbon(aes(ymin = adjusted_lo, ymax = adjusted_hi), alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_text(data = labs_15, aes(x = 15.3, y = y_lab, label = sprintf("%s  %.0f%%", series, 100 * adjusted)),
            colour = INK, hjust = 1, size = 3.2) +
  scale_x_reverse(breaks = c(15, 12, 9, 6, 3, 0)) +
  coord_cartesian(xlim = c(17.6, 0)) +   # reversed axis: limits go high to low
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.04))) +
  scale_colour_manual(values = deficit_colors) +
  scale_fill_manual(values = deficit_colors) +
  labs(title = sprintf("Adjusted model (%s): even matchup, trailing team kicking off", adj_model),
       x = "Minutes left in the 4th quarter", y = y_label, colour = NULL, fill = NULL) +
  theme_viz()
ggsave(file.path(out_dir, "adjusted_curves.png"), p2, width = 8, height = 5, dpi = 160, bg = SURFACE)

message("wrote raw_by_bin.csv, smoothed_curves.csv, model_summary.txt and two PNGs to ", out_dir)
