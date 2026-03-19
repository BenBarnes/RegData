# RegData — Cancer Incidence Explorer

Interactive Shiny app for exploring cancer incidence in Germany by diagnosis, sex, age group, and year (1999–2023).

---

## Data

Source data (`incidence_correct.csv`) are estimated incident case counts by ICD-10 diagnosis, age group, year, and sex. Data were downloaded from the interactive database of the [German Center for Cancer Registry Data](https://www.krebsdaten.de/database) in February 2026. Case counts reflect estimated national incidence based on cancer registry data. Counts below 5 are suppressed in the source as `<5`.

Source: German Centre for Cancer Registry Data, Robert Koch Institute: Database Query with estimates for cancer incidence, prevalence and survival in Germany, based on data of the population based cancer registries. Mortality data provided by the Federal Statistical Office. www.krebsdaten.de/database, Latest Update: 19.11.2025, Retrieved: (18 Feb 2026)

---

## Imputation (`impute_under5.R`)

Censored `<5` cells are imputed so that diagnosis-specific counts sum to the published *Krebs gesamt* (all-cancer) total for each year × age-group × sex stratum.

1. **Parse counts** — strip thousands separators (`.`), flag `<5` cells, treat `x` as `NA`.
2. **Compute residuals** — for each year × age-group × sex, subtract the sum of non-censored diagnosis-specific counts (`>=5`) from the *Krebs gesamt* total. This `residual` is the pool of cases to distribute across censored cells.
3. **Distribute residuals** — the number of censored cells `n_cells` is determined. Each censored cell is constrained to `[0, 4]`. A random allocation is drawn via a "deal cards" method: `n_cells × 4` slots are created (4 per cell), `residual` slots are sampled without replacement, and the count per cell is tallied. This ensures the imputed values are integers, respect the `<5` ceiling, and sum exactly to the residual.
4. **Write output** — results are written to `incidence_imputed.csv`. A verification step checks that residual differences after imputation are zero. Edge cases arise when diagnosis-specific sums exceed *Krebs gesamt* due to source data inconsistencies (probably rounding errors); these are clamped to zero.

---

## App (`app.r`)

Built with R Shiny and plotly.

**Sidebar controls**

| Control | Description |
|---|---|
| Age Range | Slider selecting a contiguous range of 5-year age groups (0–4 through 85+) |
| Y-Axis Scale | Toggle between linear and square-root y-axis transformation |
| Years | Slider selecting a contiguous range of diagnosis years (1999–2023) |

**Main panel**

Two interactive bar charts (women / men) show aggregated case counts by diagnosis for the selected filters. Hovering a bar displays the diagnosis name and case count. Summary boxes in the sidebar show total cases for women and men across the selection.

**Dependencies:** `shiny`, `ggplot2`, `data.table`, `plotly`, `scales`

Fonts (DM Mono, Fraunces) are self-hosted as WOFF2 files in `www/fonts/`.
