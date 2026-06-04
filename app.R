# =============================================================================
# Australia's International Student Impact Dashboard
# FIT5147 — Data Exploration and Visualisation (Monash University)
# Run with: shiny::runApp()  (working directory = project root)
# =============================================================================

# --- Package dependencies (install if missing on first run) ------------------
required_pkgs <- c("shiny", "bslib", "ggplot2", "plotly", "dplyr", "tidyr", "scales")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)

# --- Load and pre-process data (before server logic) -------------------------
data_dir <- file.path(getwd(), "data")

enrolments_raw <- read.csv(file.path(data_dir, "enrolments_by_sector.csv"),
                           stringsAsFactors = FALSE)
country_raw      <- read.csv(file.path(data_dir, "country_enrolments.csv"),
                             stringsAsFactors = FALSE)
revenue_raw      <- read.csv(file.path(data_dir, "export_revenue.csv"),
                             stringsAsFactors = FALSE)
competitors_raw  <- read.csv(file.path(data_dir, "global_competitors.csv"),
                             stringsAsFactors = FALSE)
rank_raw         <- read.csv(file.path(data_dir, "global_rank.csv"),
                             stringsAsFactors = FALSE)

# Standardise sector factor order for consistent legends
SECTOR_LEVELS <- c("Higher Education", "VET", "ELICOS", "Schools")
SECTOR_COLORS <- c(
  "Higher Education" = "#1B6CA8",
  "VET"              = "#2A9D8F",
  "ELICOS"           = "#F4A261",
  "Schools"          = "#E76F51"
)

enrolments <- enrolments_raw %>%
  mutate(
    sector = factor(sector, levels = SECTOR_LEVELS),
    year   = as.integer(year)
  )

# Total enrolments per year (all sectors)
totals_by_year <- enrolments %>%
  group_by(year) %>%
  summarise(total_enrolments = sum(enrolments), .groups = "drop")

# Country concentration: share held by top 5 source countries per year
concentration_by_year <- country_raw %>%
  group_by(year) %>%
  summarise(
    total = sum(enrolments),
    top5  = sum(sort(enrolments, decreasing = TRUE)[1:min(5, n())]),
    concentration_pct = round(100 * top5 / total, 1),
    .groups = "drop"
  )

# Interpolate concentration for years used in KPI slider but missing country rows
all_years <- sort(unique(enrolments$year))
concentration_full <- data.frame(year = all_years) %>%
  left_join(concentration_by_year %>% select(year, concentration_pct), by = "year") %>%
  arrange(year) %>%
  mutate(
    concentration_pct = ifelse(
      is.na(concentration_pct),
      approx(
        concentration_by_year$year,
        concentration_by_year$concentration_pct,
        xout = year,
        rule = 2
      )$y,
      concentration_pct
    )
  )

# Merge revenue with totals for derived metrics
revenue <- revenue_raw %>%
  mutate(year = as.integer(year)) %>%
  left_join(totals_by_year, by = "year")

competitors <- competitors_raw %>%
  mutate(
    country = factor(country, levels = c("USA", "UK", "Australia", "Canada")),
    year    = as.integer(year)
  )

global_rank <- rank_raw %>% mutate(year = as.integer(year))

# Story tab order for Next / Back navigation
TAB_ORDER <- c("Overview", "Growth", "COVID-19", "Countries", "Economy", "Global")

# Narrative copy and one-line takeaways per tab
STORY_CONTENT <- list(
  Overview = list(
    main = paste(
      "Australia's international education sector has grown from roughly 187,000",
      "enrolments in 2005 to over one million by 2024 — one of the fastest expansions",
      "among major destination countries. Higher education leads growth, but VET and",
      "pathway sectors now contribute a substantial share of total enrolments."
    ),
    why = paste(
      "Understanding the scale and trajectory of enrolments helps policymakers plan",
      "infrastructure, housing, and labour-market integration for students and graduates."
    ),
    summary = "Australia's international student sector has more than quintupled since 2005, reaching record scale by 2024."
  ),
  Growth = list(
    main = paste(
      "Long-term growth is strong but uneven across sectors. Higher education enrolments",
      "rose steadily from 2005–2019, while VET and ELICOS expanded faster in the 2010s.",
      "Schools remain the smallest segment but show consistent year-on-year gains."
    ),
    why = paste(
      "Sector imbalance affects provider revenue mix, graduate outcomes, and which",
      "regions and institutions benefit most from international demand."
    ),
    summary = "Growth is strong overall, but Higher Education, VET, and ELICOS follow different trajectories."
  ),
  `COVID-19` = list(
    main = paste(
      "COVID-19 caused a historic drop in international enrolments in 2020–2021,",
      "with border closures and online study displacing onshore commencements.",
      "Recovery differs by sector: ELICOS and VET rebounded quickly; Higher Education",
      "took longer to return to pre-pandemic levels."
    ),
    why = paste(
      "Pandemic-era volatility exposed sector resilience and informs future risk",
      "planning for education providers and state economies reliant on student spending."
    ),
    summary = "COVID-19 triggered a sharp 2020–2021 fall — sector recovery paths diverged through 2024."
  ),
  Countries = list(
    main = paste(
      "China and India dominate Australia's international student intake, together",
      "accounting for a large share of enrolments. Nepal, Vietnam, and other markets",
      "have grown rapidly, suggesting gradual diversification despite top-country concentration."
    ),
    why = paste(
      "Heavy reliance on a few source countries creates geopolitical and policy risk;",
      "diversification supports more stable enrolment pipelines over the long term."
    ),
    summary = "China and India lead source markets, but Nepal, Vietnam, and others are gaining share."
  ),
  Economy = list(
    main = paste(
      "International education is Australia's fourth-largest export, generating",
      "an estimated $51.2 billion in export revenue in 2024. Revenue per student has",
      "climbed steadily, reflecting tuition increases and longer average study durations."
    ),
    why = paste(
      "Export earnings support universities, regional economies, and national trade",
      "balances — making sector health a macroeconomic as well as an education issue."
    ),
    summary = "International education delivers $51.2B in export revenue (2024) — Australia's 4th largest export."
  ),
  Global = list(
    main = paste(
      "Australia holds 3rd place globally among study destinations (behind the USA",
      "and UK), but Canada is closing the gap with aggressive recruitment and",
      "post-study work policies. Competitive pressure is intensifying across English-speaking markets."
    ),
    why = paste(
      "Global positioning shapes visa policy, marketing spend, and bilateral",
      "education agreements — all critical to maintaining Australia's market share."
    ),
    summary = "Australia ranks 3rd globally for international students but faces rising competition from Canada."
  )
)

# --- Shared ggplot theme -----------------------------------------------------
theme_dashboard <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", color = "#14527F", size = 13),
      plot.subtitle   = element_text(color = "#5A6B7D", size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.margin     = margin(8, 12, 8, 12)
    )
}

# Ordinal suffix for rank display (1st, 2nd, 3rd, …)
ordinal <- function(n) {
  suffix <- c("th", "st", "nd", "rd")
  v <- n %% 100
  paste0(n, suffix[ifelse(v %in% c(11, 12, 13), 1, (n %% 10) + 1)])
}

# Helper: convert ggplot to plotly with consistent config
to_plotly <- function(p, height = NULL) {
  plt <- ggplotly(p, tooltip = "text") %>%
    layout(
      font = list(family = "Segoe UI, sans-serif"),
      margin = list(t = 40, b = 40, l = 50, r = 20)
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
  if (!is.null(height)) plt <- plt %>% layout(height = height)
  plt
}

# =============================================================================
# UI
# =============================================================================
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    primary = "#1B6CA8",
    bg = "#F0F4F8",
    fg = "#1A2B3C",
    base_font = font_collection("Segoe UI", "system-ui", "sans-serif")
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")),
  padding = 15,

  # Top bar: title + filters
  div(
    class = "dashboard-header",
    h1(class = "dashboard-title", "Australia's International Student Impact Dashboard"),
    div(
      class = "filter-bar",
      div(
        selectInput(
          "filter_year",
          "Year",
          choices = setNames(all_years, all_years),
          selected = 2024L
        )
      ),
      div(
        selectInput(
          "filter_sector",
          "Sector",
          choices = c("All", SECTOR_LEVELS),
          selected = "All"
        )
      ),
      div(
        selectInput(
          "filter_country",
          "Country",
          choices = c("All", sort(unique(country_raw$country))),
          selected = "All"
        )
      ),
      div(
        actionButton("btn_reset", "Reset", class = "btn-reset")
      )
    )
  ),

  # KPI cards (always visible)
  uiOutput("kpi_row"),

  # Story tabs
  div(
    class = "story-tabs-wrap",
    tabsetPanel(
      id = "story_tabs",
      type = "pills",
      tabPanel("Overview",  value = "Overview"),
      tabPanel("Growth",    value = "Growth"),
      tabPanel("COVID-19",  value = "COVID-19"),
      tabPanel("Countries", value = "Countries"),
      tabPanel("Economy",   value = "Economy"),
      tabPanel("Global",    value = "Global")
    )
  ),

  # Main content: story (left) + charts (right)
  div(
    class = "main-grid",
    div(
      class = "story-panel",
      uiOutput("story_text"),
      div(
        class = "story-nav",
        actionButton("btn_back", "← Back", class = "btn-outline-primary"),
        actionButton("btn_next", "Next →", class = "btn-primary")
      )
    ),
    div(
      class = "charts-panel",
      div(class = "chart-main",  plotlyOutput("chart_main",  height = "340px")),
      div(class = "chart-support", plotlyOutput("chart_support", height = "220px"))
    )
  ),

  # Summary takeaway bar
  uiOutput("summary_bar")
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # Current tab name (reactive)
  active_tab <- reactive({
    req(input$story_tabs)
    input$story_tabs
  })

  # --- KPI cards -------------------------------------------------------------
  output$kpi_row <- renderUI({
    yr <- as.integer(input$filter_year)

    total_enr <- totals_by_year %>%
      filter(year == yr) %>%
      pull(total_enrolments)
    if (length(total_enr) == 0) total_enr <- NA

    rev_row <- revenue %>% filter(year == yr)
    export_b  <- if (nrow(rev_row)) rev_row$export_revenue_billion else NA
    rev_stud  <- if (nrow(rev_row)) rev_row$revenue_per_student else NA

    conc <- concentration_full %>%
      filter(year == yr) %>%
      pull(concentration_pct)
    if (length(conc) == 0) conc <- NA

    rank_val <- global_rank %>%
      filter(year == yr) %>%
      pull(australia_rank)
    if (length(rank_val) == 0) {
      rank_val <- global_rank %>% filter(year == max(year)) %>% pull(australia_rank)
    }

    fmt_enr <- if (is.na(total_enr)) "—" else paste0(format(round(total_enr / 1e6, 2), nsmall = 2), "M")
    fmt_rev <- if (is.na(export_b)) "—" else paste0("$", format(export_b, nsmall = 1), "B")
    fmt_rps <- if (is.na(rev_stud)) "—" else paste0("$", format(rev_stud, big.mark = ","))
    fmt_con <- if (is.na(conc)) "—" else paste0(conc, "%")
    fmt_rnk <- if (length(rank_val) == 0) "—" else paste0(ordinal(rank_val[1]))

    kpi_card <- function(icon, value, label, year_note) {
      div(
        class = "kpi-card",
        div(class = "kpi-icon", icon),
        div(
          div(class = "kpi-value", value),
          div(class = "kpi-label", label),
          div(class = "kpi-year", year_note)
        )
      )
    }

    div(
      class = "kpi-row",
      kpi_card("👥", fmt_enr, "Total Enrolments", paste0("(", yr, ")")),
      kpi_card("💰", fmt_rev, "Export Revenue", paste0("(", yr, ")")),
      kpi_card("📊", fmt_rps, "Revenue per Student", paste0("(", yr, ")")),
      kpi_card("🌏", fmt_con, "Source Concentration", "(top 5)"),
      kpi_card("🏆", fmt_rnk, "Global Rank", paste0("(", yr, ")"))
    )
  })

  # --- Story panel text ------------------------------------------------------
  output$story_text <- renderUI({
    tab <- active_tab()
    content <- STORY_CONTENT[[tab]]
    tagList(
      div(class = "story-tab-label", paste("Selected tab:", tab)),
      h3("Main message"),
      p(content$main),
      h4("Why it matters"),
      p(content$why)
    )
  })

  # --- Summary bar -----------------------------------------------------------
  output$summary_bar <- renderUI({
    tab <- active_tab()
    takeaway <- STORY_CONTENT[[tab]]$summary
    div(
      class = "summary-bar",
      HTML(paste0("<strong>Takeaway:</strong> ", takeaway))
    )
  })

  # --- Main chart (tab-dependent) --------------------------------------------
  output$chart_main <- renderPlotly({
    tab  <- active_tab()
    yr   <- as.integer(input$filter_year)
    sect <- input$filter_sector
    ctry <- input$filter_country

    if (tab == "Overview") {
      p <- totals_by_year %>%
        ggplot(aes(x = year, y = total_enrolments / 1e6,
                   text = paste0("Year: ", year,
                                 "<br>Total: ", format(total_enrolments, big.mark = ",")))) +
        geom_area(fill = "#1B6CA8", alpha = 0.35) +
        geom_line(color = "#1B6CA8", linewidth = 1.1) +
        geom_vline(xintercept = yr, linetype = "dashed", color = "#E76F51", linewidth = 0.8) +
        labs(
          title = "Total International Student Enrolments",
          subtitle = "Australia, 2005–2024 (all sectors)",
          x = "Year", y = "Enrolments (millions)"
        ) +
        scale_x_continuous(breaks = seq(2005, 2024, 3)) +
        theme_dashboard()
      return(to_plotly(p, 340))
    }

    if (tab == "Growth") {
      df <- enrolments %>%
        mutate(
          alpha = if (sect == "All") 1 else ifelse(as.character(sector) == sect, 1, 0.25),
          enrol_k = enrolments / 1000
        )
      p <- ggplot(df, aes(x = year, y = enrol_k, color = sector, group = sector,
                          text = paste0(sector, "<br>", year, ": ",
                                        format(enrolments, big.mark = ",")))) +
        geom_line(aes(alpha = alpha), linewidth = 1.1) +
        geom_point(aes(alpha = alpha), size = 1.5) +
        scale_color_manual(values = SECTOR_COLORS, name = "Sector") +
        scale_alpha_identity() +
        labs(
          title = "Sector Enrolment Trends",
          subtitle = "2005–2024 · dashed line = selected year",
          x = "Year", y = "Enrolments (thousands)"
        ) +
        geom_vline(xintercept = yr, linetype = "dashed", color = "grey50") +
        theme_dashboard()
      return(to_plotly(p, 340))
    }

    if (tab == "COVID-19") {
      df <- enrolments %>%
        filter(year >= 2018) %>%
        mutate(
          alpha = if (sect == "All") 1 else ifelse(as.character(sector) == sect, 1, 0.25),
          enrol_k = enrolments / 1000
        )
      p <- ggplot(df, aes(x = year, y = enrol_k, color = sector, group = sector,
                          text = paste0(sector, "<br>", year, ": ",
                                        format(enrolments, big.mark = ",")))) +
        geom_line(aes(alpha = alpha), linewidth = 1.2) +
        geom_point(aes(alpha = alpha), size = 2) +
        scale_color_manual(values = SECTOR_COLORS, name = "Sector") +
        scale_alpha_identity() +
        annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
                 fill = "#FFE4E1", alpha = 0.4) +
        labs(
          title = "COVID-19 Impact & Recovery by Sector",
          subtitle = "Pink band = peak disruption years (2020–2021)",
          x = "Year", y = "Enrolments (thousands)"
        ) +
        theme_dashboard()
      return(to_plotly(p, 340))
    }

    if (tab == "Countries") {
      # Country enrolment data available for select years; fall back to nearest year
      data_year <- if (yr %in% country_raw$year) yr else max(country_raw$year)
      df <- country_raw %>%
        filter(year == data_year) %>%
        arrange(desc(enrolments)) %>%
        mutate(
          country = reorder(country, enrolments),
          highlight = if (ctry != "All") as.character(country) == ctry else TRUE
        )
      p <- ggplot(df, aes(x = country, y = enrolments / 1000, fill = highlight,
                          text = paste0(country, "<br>", format(enrolments, big.mark = ",")))) +
        geom_col() +
        scale_fill_manual(
          values = c("TRUE" = "#1B6CA8", "FALSE" = "#B0C4DE"),
          guide = "none"
        ) +
        coord_flip() +
        labs(
          title = paste0("Top Source Countries (", data_year, ")"),
          subtitle = paste(
            if (data_year != yr) paste0("Nearest data year to ", yr, " · "),
            if (ctry != "All") paste("Highlighting:", ctry) else "Top 10 source countries"
          ),
          x = NULL, y = "Enrolments (thousands)"
        ) +
        theme_dashboard()
      return(to_plotly(p, 340))
    }

    if (tab == "Economy") {
      p <- revenue %>%
        ggplot(aes(x = year, y = export_revenue_billion,
                   text = paste0("Year: ", year,
                                   "<br>Revenue: $", export_revenue_billion, "B"))) +
        geom_area(fill = "#2A9D8F", alpha = 0.35) +
        geom_line(color = "#2A9D8F", linewidth = 1.1) +
        geom_vline(xintercept = yr, linetype = "dashed", color = "#E76F51") +
        labs(
          title = "International Education Export Revenue",
          subtitle = "Australia, 2010–2024 (AUD billions)",
          x = "Year", y = "Export revenue ($B)"
        ) +
        theme_dashboard()
      return(to_plotly(p, 340))
    }

    if (tab == "Global") {
      # Country filter highlights a competitor line when name matches; else emphasise Australia
      competitor_names <- c("USA", "UK", "Australia", "Canada")
      highlight_ctry <- if (!is.null(ctry) && ctry != "All" && ctry %in% competitor_names) {
        ctry
      } else {
        "Australia"
      }

      df <- competitors %>%
        mutate(
          bold = as.character(country) == highlight_ctry,
          students = international_students_millions
        )
      p <- ggplot(df, aes(x = year, y = students, color = country, group = country,
                          text = paste0(country, "<br>", year, ": ",
                                        round(students, 2), "M students"))) +
        geom_line(aes(linewidth = ifelse(bold, 1.4, 0.7)), alpha = 0.9) +
        scale_linewidth_identity() +
        scale_color_manual(
          values = c("USA" = "#3C5488", "UK" = "#E64B35",
                     "Australia" = "#1B6CA8", "Canada" = "#2A9D8F"),
          name = "Destination"
        ) +
        labs(
          title = "Australia vs Global Competitors",
          subtitle = "International students (millions), 2010–2024",
          x = "Year", y = "Students (millions)"
        ) +
        theme_dashboard()
      return(to_plotly(p, 340))
    }

    plotly_empty()
  })

  # --- Supporting chart ------------------------------------------------------
  output$chart_support <- renderPlotly({
    tab  <- active_tab()
    yr   <- as.integer(input$filter_year)
    sect <- input$filter_sector
    ctry <- input$filter_country

    if (tab == "Overview") {
      p <- enrolments %>%
        filter(year == yr) %>%
        ggplot(aes(x = "", y = enrolments, fill = sector,
                   text = paste0(sector, ": ", format(enrolments, big.mark = ",")))) +
        geom_col(width = 1, position = "stack") +
        coord_polar(theta = "y") +
        scale_fill_manual(values = SECTOR_COLORS, name = "Sector") +
        labs(title = paste0("Sector Mix (", yr, ")"), x = NULL, y = NULL) +
        theme_dashboard() +
        theme(axis.text = element_blank(), axis.ticks = element_blank())
      return(to_plotly(p, 220))
    }

    if (tab == "Growth") {
      df <- enrolments %>%
        mutate(pct = enrolments / ave(enrolments, year, FUN = sum) * 100)
      p <- ggplot(df, aes(x = year, y = pct, fill = sector,
                          text = paste0(sector, "<br>", round(pct, 1), "%"))) +
        geom_area(position = "stack", alpha = 0.85) +
        scale_fill_manual(values = SECTOR_COLORS, name = "Sector") +
        labs(
          title = "Sector Share of Enrolments",
          x = "Year", y = "Share (%)"
        ) +
        theme_dashboard()
      return(to_plotly(p, 220))
    }

    if (tab == "COVID-19") {
      recovery <- enrolments %>%
        filter(year %in% c(2019, 2021, 2024)) %>%
        mutate(period = factor(year, levels = c(2019, 2021, 2024),
                              labels = c("Pre-COVID (2019)", "Trough (2021)", "Recovery (2024)")))
      p <- ggplot(recovery, aes(x = period, y = enrolments / 1000, fill = sector,
                                text = paste0(sector, "<br>", format(enrolments, big.mark = ",")))) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = SECTOR_COLORS, name = "Sector") +
        labs(title = "Pre-COVID vs Trough vs Recovery", x = NULL, y = "Thousands") +
        theme_dashboard()
      return(to_plotly(p, 220))
    }

    if (tab == "Countries") {
      p <- concentration_by_year %>%
        ggplot(aes(x = year, y = concentration_pct,
                   text = paste0("Top-5 share: ", concentration_pct, "%"))) +
        geom_line(color = "#1B6CA8", linewidth = 1) +
        geom_point(color = "#1B6CA8", size = 2) +
        geom_vline(xintercept = yr, linetype = "dashed", color = "#E76F51") +
        labs(
          title = "Source Country Concentration (Top 5 Share)",
          x = "Year", y = "Share (%)"
        ) +
        ylim(60, 80) +
        theme_dashboard()
      return(to_plotly(p, 220))
    }

    if (tab == "Economy") {
      p <- revenue %>%
        ggplot(aes(x = year, y = revenue_per_student,
                   text = paste0("$", format(revenue_per_student, big.mark = ",")))) +
        geom_line(color = "#F4A261", linewidth = 1.1) +
        geom_point(color = "#F4A261", size = 2) +
        geom_vline(xintercept = yr, linetype = "dashed", color = "#E76F51") +
        scale_y_continuous(labels = dollar_format()) +
        labs(
          title = "Revenue per International Student",
          x = "Year", y = "AUD per student"
        ) +
        theme_dashboard()
      return(to_plotly(p, 220))
    }

    if (tab == "Global") {
      p <- global_rank %>%
        ggplot(aes(x = year, y = australia_rank,
                   text = paste0("Rank: ", ordinal(australia_rank)))) +
        geom_line(color = "#1B6CA8", linewidth = 1.1) +
        geom_point(color = "#1B6CA8", size = 3) +
        scale_y_reverse(breaks = 1:5, limits = c(5, 1)) +
        labs(
          title = "Australia's Global Destination Rank",
          subtitle = "1 = largest market",
          x = "Year", y = "Rank"
        ) +
        theme_dashboard()
      return(to_plotly(p, 220))
    }

    plotly_empty()
  })

  # --- Tab navigation: Next / Back -------------------------------------------
  observeEvent(input$btn_next, {
    tab <- active_tab()
    idx <- match(tab, TAB_ORDER)
    if (!is.na(idx) && idx < length(TAB_ORDER)) {
      updateTabsetPanel(session, "story_tabs", selected = TAB_ORDER[idx + 1])
    }
  })

  observeEvent(input$btn_back, {
    tab <- active_tab()
    idx <- match(tab, TAB_ORDER)
    if (!is.na(idx) && idx > 1) {
      updateTabsetPanel(session, "story_tabs", selected = TAB_ORDER[idx - 1])
    }
  })

  # --- Reset: clear filters and return to Overview ---------------------------
  observeEvent(input$btn_reset, {
    updateSelectInput(session, "filter_year",   selected = 2024L)
    updateSelectInput(session, "filter_sector", selected = "All")
    updateSelectInput(session, "filter_country", selected = "All")
    updateTabsetPanel(session, "story_tabs", selected = "Overview")
  })
}

# =============================================================================
# Launch app
# =============================================================================
shinyApp(ui = ui, server = server)
