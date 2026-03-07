library(shiny)
library(ggplot2)
library(data.table)

# ── Data ──────────────────────────────────────────────────────────────────────

df_raw <- fread("incidence_imputed.csv")

# Define ordered age groups
age_levels <- c("0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54",
                "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79",
                "80 - 84", "85+")

df <- df_raw[, age_group := factor(age_group, levels = age_levels)]

all_years  <- sort(unique(df$year))
n_ages     <- length(age_levels)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @font-face {
        font-family: 'DM Mono';
        font-style: normal;
        font-weight: 400;
        font-display: swap;
        src: url('fonts/dm-mono-400.woff2') format('woff2');
      }
      @font-face {
        font-family: 'DM Mono';
        font-style: normal;
        font-weight: 500;
        font-display: swap;
        src: url('fonts/dm-mono-500.woff2') format('woff2');
      }
      @font-face {
        font-family: 'Fraunces';
        font-style: normal;
        font-weight: 100 900;
        font-display: swap;
        src: url('fonts/fraunces-variable.woff2') format('woff2');
      }
    ")),
    tags$style(HTML("

      /* ── Reset & base ──────────────────────────────── */
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background: #0e0e10;
        color: #e8e4dc;
        font-family: 'DM Mono', monospace;
        font-size: 13px;
        min-height: 100vh;
        padding: 0;
      }

      /* ── Header ─────────────────────────────────────── */
      .app-header {
        background: #0e0e10;
        border-bottom: 1px solid #2a2a2e;
        padding: 28px 40px 22px;
        display: flex;
        align-items: baseline;
        gap: 18px;
      }
      .app-title {
        font-family: 'Fraunces', serif;
        font-size: 26px;
        font-weight: 600;
        color: #f0ebe0;
        letter-spacing: -0.5px;
        line-height: 1;
      }
      .app-subtitle {
        color: #6b6b72;
        font-size: 11px;
        letter-spacing: 0.08em;
        text-transform: uppercase;
      }

      /* ── Layout ─────────────────────────────────────── */
      .main-layout {
        display: flex;
        gap: 0;
        min-height: calc(100vh - 75px);
      }

      /* ── Sidebar ─────────────────────────────────────── */
      .sidebar {
        width: 280px;
        min-width: 280px;
        background: #131316;
        border-right: 1px solid #2a2a2e;
        padding: 28px 24px;
        display: flex;
        flex-direction: column;
        gap: 32px;
      }

      .ctrl-label {
        font-size: 10px;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        color: #c4a882;
        margin-bottom: 12px;
      }

      /* age range display */
      .age-range-display {
        font-family: 'Fraunces', serif;
        font-size: 22px;
        font-weight: 300;
        color: #f0ebe0;
        margin-bottom: 14px;
        letter-spacing: -0.3px;
      }

      /* slider override */
      .irs--shiny .irs-bar,
      .irs--shiny .irs-bar--single { background: #c4a882 !important; border-color: #c4a882 !important; }
      .irs--shiny .irs-handle       { background: #f0ebe0 !important; border-color: #c4a882 !important; }
      .irs--shiny .irs-from,
      .irs--shiny .irs-to,
      .irs--shiny .irs-single        { background: #c4a882 !important; color: #0e0e10 !important; font-family: 'DM Mono', monospace !important; font-size: 11px !important; }
      .irs--shiny .irs-line          { background: #2a2a2e !important; border-color: #2a2a2e !important; }
      .irs--shiny .irs-grid-text     { color: #6b6b72 !important; font-family: 'DM Mono', monospace !important; font-size: 10px !important; }
      .irs--shiny .irs-min,
      .irs--shiny .irs-max           { color: #6b6b72 !important; font-family: 'DM Mono', monospace !important; font-size: 10px !important; }

      /* year checkboxes */
      .year-scroll {
        max-height: 340px;
        overflow-y: auto;
        padding-right: 4px;
      }
      .year-scroll::-webkit-scrollbar       { width: 4px; }
      .year-scroll::-webkit-scrollbar-track { background: transparent; }
      .year-scroll::-webkit-scrollbar-thumb { background: #2a2a2e; border-radius: 2px; }

      .year-scroll .checkbox            { margin: 0 !important; padding: 0 !important; }
      .year-scroll .checkbox label      {
        display: flex; align-items: center; gap: 10px;
        padding: 5px 6px; border-left: 2px solid transparent; border-radius: 4px; cursor: pointer;
        color: #4e4e58; font-size: 12px; transition: color .15s, background .15s;
      }
      .year-scroll .checkbox label:hover { color: #f0ebe0; background: #1e1e22; }
      .year-scroll input[type=checkbox]  { accent-color: #c4a882; width: 13px; height: 13px; }
      .year-scroll .checkbox:has(input:checked) label {
        color: #f0ebe0; background: #1e1e22; border-left-color: #c4a882;
      }

      .sel-btns { display: flex; gap: 8px; margin-bottom: 10px; }
      .sel-btn  {
        background: none; border: 1px solid #2a2a2e; color: #6b6b72;
        font-family: 'DM Mono', monospace; font-size: 10px; letter-spacing: 0.08em;
        text-transform: uppercase; padding: 4px 10px; border-radius: 3px;
        cursor: pointer; transition: border-color .15s, color .15s;
      }
      .sel-btn:hover { border-color: #c4a882; color: #c4a882; }

      /* stat pills */
      .stat-row { display: flex; flex-direction: column; gap: 8px; margin-top: auto; }
      .stat-pill {
        background: #1a1a1e; border: 1px solid #2a2a2e; border-radius: 6px;
        padding: 10px 14px; display: flex; justify-content: space-between; align-items: center;
      }
      .stat-pill .sp-label { color: #6b6b72; font-size: 10px; letter-spacing: 0.08em; text-transform: uppercase; }
      .stat-pill .sp-val   { font-family: 'Fraunces', serif; font-size: 17px; font-weight: 300; color: #f0ebe0; }
      .stat-pill.women .sp-val { color: #e07b8a; }
      .stat-pill.men   .sp-val { color: #6a9bd4; }

      /* ── Plot area ───────────────────────────────────── */
      .plot-area {
        flex: 1;
        padding: 32px 36px;
        display: flex;
        flex-direction: column;
        gap: 8px;
      }
      .plot-row {
        flex: 1;
        display: flex;
        flex-direction: column;
      }
      .plot-sex-label {
        font-size: 10px; letter-spacing: 0.14em; text-transform: uppercase;
        margin-bottom: 6px;
      }
      .plot-sex-label.women { color: #e07b8a; }
      .plot-sex-label.men   { color: #6a9bd4; }

      .shiny-plot-output {
        flex: 1;
        min-height: 220px;
      }

      /* shiny busy indicator */
      .shiny-busy-indicator { display: none !important; }

    "))
  ),

  # ── Header ────────────────────────────────────────────────────────────────
  div(class = "app-header",
    div(class = "app-title",  "Cancer Incidence"),
    div(class = "app-subtitle", "Germany · 1999 - 2023 · ICD-10")
  ),

  div(class = "main-layout",

    # ── Sidebar ─────────────────────────────────────────────────────────────
    div(class = "sidebar",

      # Age range
      div(
        div(class = "ctrl-label", "Age Range"),
        uiOutput("age_range_display"),
        sliderInput("age_range", label = NULL,
                    min = 1, max = n_ages, value = c(1, n_ages), step = 1,
                    ticks = FALSE)
      ),

      # Years
      div(
        div(class = "ctrl-label", "Years"),
        div(class = "sel-btns",
          tags$button("All",  class = "sel-btn", id = "btn_all",
                      onclick = "Shiny.setInputValue('year_btn', 'all',  {priority: 'event'})"),
          tags$button("None", class = "sel-btn", id = "btn_none",
                      onclick = "Shiny.setInputValue('year_btn', 'none', {priority: 'event'})")
        ),
        div(class = "year-scroll",
          checkboxGroupInput("years", label = NULL,
                             choices  = all_years,
                             selected = all_years)
        )
      ),

      # Stats
      div(class = "stat-row",
        div(class = "stat-pill women",
          div(class = "sp-label", "Women · Total"),
          div(class = "sp-val", textOutput("total_women", inline = TRUE))
        ),
        div(class = "stat-pill men",
          div(class = "sp-label", "Men · Total"),
          div(class = "sp-val", textOutput("total_men", inline = TRUE))
        )
      )
    ),

    # ── Plot area ────────────────────────────────────────────────────────────
    div(class = "plot-area",
      div(class = "plot-row",
        div(class = "plot-sex-label women", "Women"),
        plotOutput("hist_women", height = "100%")
      ),
      div(class = "plot-row",
        div(class = "plot-sex-label men", "Men"),
        plotOutput("hist_men", height = "100%")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Handle All / None buttons
  observeEvent(input$year_btn, {
    if (input$year_btn %in% "all")
      updateCheckboxGroupInput(session, "years", selected = all_years)
    else
      updateCheckboxGroupInput(session, "years", selected = character(0))
  })

  # Age range label
  output$age_range_display <- renderUI({
    lo <- age_levels[input$age_range[1]]
    hi <- age_levels[input$age_range[2]]
    label <- if (lo == hi) lo else paste(lo, "-", hi)
    div(class = "age-range-display", label)
  })

  # Filtered data
  filtered <- reactive({
    req(input$years)
    sel_ages <- age_levels[input$age_range[1]:input$age_range[2]]
    df[year %in% as.integer(input$years) &
        age_group %in% sel_ages &
        diagnosis != "Krebs gesamt (C00-C97 ohne C44)"
    ]
  })

  # Shared ggplot theme
  base_theme <- function() {
    theme_minimal(base_family = "mono") +
    theme(
      plot.background  = element_rect(fill = "#0e0e10", color = NA),
      panel.background = element_rect(fill = "#0e0e10", color = NA),
      panel.grid.major = element_line(color = "#1e1e22", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(color = "#6b6b72", size = 10),
      axis.title       = element_text(color = "#6b6b72", size = 10),
      plot.margin      = margin(4, 4, 4, 4)
    )
  }

  make_hist <- function(data, col, fill_col, label) {
    agg <- data[, .(cases = sum(get(col), na.rm = TRUE)), keyby = diagnosis]
    if (nrow(agg) == 0) return(
      ggplot() + base_theme() +
        annotate("text", x = .5, y = .5, label = "No data",
                 color = "#6b6b72", family = "mono", size = 4) +
        theme(axis.text = element_blank(), axis.title = element_blank())
    )
    agg[, diagnosis := factor(diagnosis, levels = diagnosis)]
    ggplot(agg, aes(x = diagnosis, y = cases)) +
      geom_col(fill = fill_col, alpha = 0.85, width = 0.8) +
      scale_y_continuous(labels = scales::comma_format()) +
      labs(x = NULL, y = "Case count") +
      base_theme() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8,
        color = "#6b6b72")
      )
  }

  output$hist_women <- renderPlot(
    make_hist(filtered(), "women", "#e07b8a", "Women"),
    bg = "#0e0e10"
  )
  output$hist_men <- renderPlot(
    make_hist(filtered(), "men", "#6a9bd4", "Men"),
    bg = "#0e0e10"
  )

  fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
  output$total_women <- renderText(fmt(sum(filtered()$women, na.rm = TRUE)))
  output$total_men   <- renderText(fmt(sum(filtered()$men,   na.rm = TRUE)))
}

shinyApp(ui, server)
