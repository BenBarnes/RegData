library(data.table)

set.seed(42)

# ── Load data ─────────────────────────────────────────────────────────────────
# Semicolon-delimited; first row is a header with gender labels only,
# so we skip it and assign column names manually.

dt <- fread("incidence_correct.csv", sep = ";", header = FALSE, skip = 1,
            col.names = c("year", "diagnosis", "age_group", "women", "men", "extra"))
dt[, extra := NULL]

# ── Parse counts ──────────────────────────────────────────────────────────────
# Periods are thousands separators. "<5" and "x" → NA (suppressed values).

parse_count <- function(x) {
  x <- trimws(x)
  fifelse(x %in% "x" | startsWith(x, "<"), NA_real_,
          as.numeric(gsub("\\.", "", x)))
}

dt[, is_u5_w := trimws(women) == "<5"]
dt[, is_u5_m := trimws(men)   == "<5"]
dt[, women2   := parse_count(women)]
dt[, men2     := parse_count(men)]

# ── Split Krebs gesamt from sub-diagnoses ─────────────────────────────────────

krebs  <- dt[diagnosis %in% "Krebs gesamt (C00-C97 ohne C44)"]
others <- dt[!diagnosis %in% "Krebs gesamt (C00-C97 ohne C44)"]

# ── Compute per-group sums and differences ────────────────────────────────────

group_stats <- others[, .(
  sum_w  = sum(women2, na.rm = TRUE),
  sum_m  = sum(men2,   na.rm = TRUE),
  n_u5_w = sum(is_u5_w),
  n_u5_m = sum(is_u5_m)
), by = .(year, age_group)]

krebs_totals <- krebs[, .(year, age_group, krebs_w = women2, krebs_m = men2)]

group_stats <- merge(group_stats, krebs_totals, by = c("year", "age_group"), all.x = TRUE)

# Negative diffs → 0 (sub-diagnoses exceed total due to source inconsistency).
# Diff > n_cells × 4 → capped inside distribute() since each cell max is 4.
group_stats[, diff_w := pmax(0, krebs_w - sum_w)]
group_stats[, diff_m := pmax(0, krebs_m - sum_m)]

# ── Distribution function ─────────────────────────────────────────────────────
# Randomly distributes `total` across `n` integers each in [0, 4].
# "deal cards" analogy: imagine a deck of n × 4 cards where 4 cards are
# labeled for each bin — sample total cards without replacement and count
# how many land in each bin.

distribute <- function(total, n) {
  if (n == 0L || total <= 0) return(integer(n))
  total <- min(as.integer(total), n * 4L)
  tabulate(sample(rep(seq_len(n), each = 4L), total), nbins = n)
}

# ── Impute <5 cells ───────────────────────────────────────────────────────────
# For each year × age_group, draw a random allocation and write values back
# into the rows that were originally "<5".

others[, women_imputed := women2]
others[, men_imputed   := men2]

for (i in seq_len(nrow(group_stats))) {
  yr <- group_stats$year[i]
  ag <- group_stats$age_group[i]

  # Women
  idx_w <- others[year %in% yr & age_group %in% ag & is_u5_w %in% TRUE, which = TRUE]
  if (length(idx_w) > 0)
    others[idx_w, women_imputed := distribute(group_stats$diff_w[i], length(idx_w))]

  # Men
  idx_m <- others[year %in% yr & age_group %in% ag & is_u5_m %in% TRUE, which = TRUE]
  if (length(idx_m) > 0)
    others[idx_m, men_imputed := distribute(group_stats$diff_m[i], length(idx_m))]
}

# ── Combine and write output ──────────────────────────────────────────────────

krebs[, women_imputed := women2]
krebs[, men_imputed   := men2]

result <- rbind(
  krebs[,  .(year, diagnosis, age_group, women = women_imputed, men = men_imputed)],
  others[, .(year, diagnosis, age_group, women = women_imputed, men = men_imputed)]
)
setorder(result, year, diagnosis, age_group)

fwrite(result, "incidence_imputed.csv")
message("Done. Written to incidence_imputed.csv")

# ── Verification ──────────────────────────────────────────────────────────────

verify <- result[diagnosis != "Krebs gesamt (C00-C97 ohne C44)",
                 .(sum_w = sum(women, na.rm = TRUE),
                   sum_m = sum(men,   na.rm = TRUE)),
                 by = .(year, age_group)]

krebs_check <- result[diagnosis %in% "Krebs gesamt (C00-C97 ohne C44)",
                      .(year, age_group, krebs_w = women, krebs_m = men)]

check <- merge(verify, krebs_check, by = c("year", "age_group"))
check[, diff_w := krebs_w - sum_w]
check[, diff_m := krebs_m - sum_m]

message("Residual differences after imputation (should be 0 except for edge cases):")
message("  Women — max: ", max(check$diff_w), "  min: ", min(check$diff_w))
message("  Men   — max: ", max(check$diff_m), "  min: ", min(check$diff_m))

nonzero <- check[diff_w != 0 | diff_m != 0]
message("  Non-zero residuals: ", nrow(nonzero), " groups")
