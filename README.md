# RegData — Cancer Incidence Explorer

Interactive Shiny app for exploring cancer incidence in Germany (1999–2023) by diagnosis, sex, age group, and year.

---

## Data

Source data (`incidence_correct.csv`) is semicolon-delimited with case counts by diagnosis, age group, year, and sex, downloaded from the interactive database at krebsdaten.de/database. Counts below 5 are suppressed in the source as `<5`.

---

## Imputation (`impute_under5.R`)

Suppressed `<5` cells are imputed so that diagnosis-specific counts sum to the published *Krebs gesamt* (all-cancer) total for each year × age-group stratum.

1. **Parse counts** — strip thousands separators (`.`), flag `<5` cells, treat `x` as `NA`.
2. **Compute residuals** — for each year × age-group, subtract the sum of known diagnosis-specific counts from the *Krebs gesamt* total. This residual is the pool of cases to distribute across suppressed cells.
3. **Distribute residuals** — each suppressed cell is constrained to `[0, 4]`. A random allocation is drawn via a "deal cards" method: `n_cells × 4` slots are created (4 per cell), `residual` slots are sampled without replacement, and the count per cell is tallied. This ensures the imputed values are integers, respect the `<5` ceiling, and sum exactly to the residual.
4. **Write output** — results are written to `incidence_imputed.csv`. A verification step checks that residual differences after imputation are zero (edge cases arise when diagnosis-specific sums exceed *Krebs gesamt* due to source inconsistencies; these are clamped to zero).

---

## App (`app.r`)

Built with R Shiny and plotly.

**Sidebar controls**

| Control | Description |
|---|---|
| Age Range | Slider selecting a contiguous range of 5-year age groups (0–4 through 85+) |
| Y-Axis Scale | Toggle between linear and square-root y-axis transformation |
| Years | Checkbox list to include/exclude individual years (1999–2023) |

**Main panel**

Two interactive bar charts (women / men) show aggregated case counts by diagnosis for the selected filters. Hovering a bar displays the diagnosis name and exact case count. Summary boxes in the sidebar show total cases for women and men across the selection.

**Dependencies:** `shiny`, `ggplot2`, `data.table`, `plotly`, `scales`

Fonts (DM Mono, Fraunces) are self-hosted as WOFF2 files in `www/fonts/`.
