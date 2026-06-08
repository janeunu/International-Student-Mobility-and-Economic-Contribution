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

# Sector dataset alias (used in share charts)
sector_data <- enrolments

# Sector growth index (2005 = 100) for Growth supporting chart
growth_index <- sector_data %>%
  group_by(sector) %>%
  mutate(index = enrolments / enrolments[year == 2005] * 100) %>%
  ungroup() %>%
  mutate(sector = factor(sector, levels = c("Higher Education", "VET", "ELICOS", "Schools")))

# Enrolments vs export revenue scatter (Economy supporting chart)
econ_scatter <- data.frame(
  year = 2010:2024,
  enrolments = c(0.56, 0.59, 0.62, 0.65, 0.67, 0.70, 0.74,
                 0.78, 0.83, 0.92, 0.68, 0.72, 0.88, 0.96, 1.05),
  revenue = c(17.2, 18.1, 19.5, 21.3, 23.0, 25.1, 27.8,
              31.2, 34.6, 38.9, 29.1, 32.4, 39.2, 45.1, 51.2)
)

# Top source countries share change 2010 vs 2024 (Countries supporting chart)
country_change <- data.frame(
  country = rep(c("China", "India", "Nepal", "Vietnam", "Indonesia", "Malaysia"), 2),
  year = c(rep("2010", 6), rep("2024", 6)),
  share = c(36.2, 8.1, 3.2, 4.5, 3.8, 3.1,
            28.4, 29.6, 8.9, 6.2, 4.1, 3.5)
)

# Australia enrolments as % of USA over time (Global supporting chart)
gap_data <- data.frame(
  year = 2010:2023,
  aus_pct_of_usa = c(58, 59, 61, 63, 64, 66, 68, 69, 71,
                     72, 64, 66, 72, 78)
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
      "India and China together dominate Australia's international student intake,",
      "with India recently surpassing China as the top source country. Nepal, Vietnam,",
      "and other markets have grown rapidly, suggesting gradual diversification despite",
      "top-country concentration."
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

# Year filter sentinel value
YEAR_ALL <- "ALL"

# TRUE when the dashboard year filter is set to show all years
year_is_all <- function(year_input) {
  identical(as.character(year_input), YEAR_ALL)
}

# Returns integer year, or NULL when "ALL" is selected
parse_selected_year <- function(year_input) {
  if (year_is_all(year_input)) NULL else as.integer(year_input)
}

# Year used for KPI lookups (latest year when filter is ALL)
kpi_lookup_year <- function(year_input, years = all_years) {
  if (year_is_all(year_input)) max(years) else as.integer(year_input)
}

# Add optional vertical year marker (skipped when ALL is selected)
add_year_vline <- function(p, yr_val, color = "#E76F51", linetype = "dashed") {
  if (!is.null(yr_val)) {
    p <- p + geom_vline(xintercept = yr_val, linetype = linetype, color = color, linewidth = 0.8)
  }
  p
}

# Helper: convert ggplot to plotly with consistent config
to_plotly <- function(p, height = NULL) {
  # Use text tooltips when present; fall back if conversion fails
  plt <- tryCatch(
    ggplotly(p, tooltip = "text"),
    error = function(e) ggplotly(p)
  )
  plt <- plt %>%
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
          choices = c("ALL" = YEAR_ALL, setNames(as.character(all_years), as.character(all_years))),
          selected = "2024"
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

  # Parsed year filter: NULL = ALL years selected
  selected_year <- reactive({
    req(input$filter_year)
    parse_selected_year(input$filter_year)
  })

  # Current tab name (reactive)
  active_tab <- reactive({
    req(input$story_tabs)
    input$story_tabs
  })

  # --- KPI cards -------------------------------------------------------------
  output$kpi_row <- renderUI({
    yr_all <- year_is_all(input$filter_year)
    yr     <- kpi_lookup_year(input$filter_year)

    # Build a filter label reflecting all active filters
    filter_parts <- c()
    if (!is.null(input$filter_sector) && input$filter_sector != "All")
      filter_parts <- c(filter_parts, input$filter_sector)
    if (!yr_all)
      filter_parts <- c(filter_parts, as.character(yr))
    if (!is.null(input$filter_country) && input$filter_country != "All")
      filter_parts <- c(filter_parts, input$filter_country)

    yr_note <- if (length(filter_parts) == 0) {
      paste0("(ALL \u00b7 ", yr, " snapshot)")
    } else {
      paste0("(", paste(filter_parts, collapse = " \u00b7 "), ")")
    }

    total_enr <- totals_by_year %>%
      filter(year == yr) %>%
      pull(total_enrolments)
    if (length(total_enr) == 0) total_enr <- NA

    rev_row <- revenue %>% filter(year == yr)
    export_b  <- if (nrow(rev_row)) rev_row$export_revenue_billion else NA
    rev_stud  <- if (nrow(rev_row)) rev_row$revenue_per_student else NA

    rank_val <- global_rank %>%
      filter(year == yr) %>%
      pull(australia_rank)
    if (length(rank_val) == 0) {
      rank_val <- global_rank %>% filter(year == max(year)) %>% pull(australia_rank)
    }

    fmt_enr <- if (is.na(total_enr)) "—" else paste0(format(round(total_enr / 1e6, 2), nsmall = 2), "M")
    fmt_rev <- if (is.na(export_b)) "—" else paste0("$", format(export_b, nsmall = 1), "B")
    fmt_rps <- if (is.na(rev_stud)) "—" else paste0("$", format(rev_stud, big.mark = ","))
    fmt_con <- "72%"
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
      kpi_card("👥", fmt_enr, "Total Enrolments", yr_note),
      kpi_card("💰", fmt_rev, "Export Revenue", yr_note),
      kpi_card("📊", fmt_rps, "Revenue per Student", yr_note),
      kpi_card("🌏", fmt_con, "Source Concentration", "(Top 5, 2024)"),
      kpi_card("🏆", fmt_rnk, "Global Rank", yr_note)
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
    yr   <- selected_year()
    sect <- input$filter_sector
    ctry <- input$filter_country

    if (tab == "Overview") {
      p <- totals_by_year %>%
        mutate(
          total_m = total_enrolments / 1e6,
          hover = paste0("Year: ", year,
                         "<br>Total: ", format(total_enrolments, big.mark = ","))
        ) %>%
        ggplot(aes(x = year, group = 1)) +
        geom_ribbon(aes(ymin = 0, ymax = total_m, text = hover),
                    fill = "#1B6CA8", alpha = 0.35) +
        geom_line(aes(y = total_m, text = hover), color = "#1B6CA8", linewidth = 1.1) +
        labs(
          title = "Total International Student Enrolments",
          subtitle = if (is.null(yr)) {
            "Australia, 2005–2024 (all sectors · all years)"
          } else {
            paste0("Australia, 2005–2024 · highlight: ", yr)
          },
          x = "Year", y = "Enrolments (millions)"
        ) +
        scale_x_continuous(breaks = seq(2005, 2024, 3)) +
        geom_vline(xintercept = 2020, linetype = "dashed",
                   colour = "grey50", linewidth = 0.6) +
        annotate("text", x = 2020.3, y = 0.05,
                 label = "COVID-19\nborder closures",
                 hjust = 0, size = 2.8, colour = "grey40") +
        theme_dashboard()
      p <- add_year_vline(p, yr)
      return(to_plotly(p, 340))
    }

    if (tab == "Growth") {
      df <- enrolments %>%
        mutate(
          alpha = if (sect == "All") 1 else ifelse(as.character(sector) == sect, 1, 0.2),
          lw    = if (sect == "All") 1.5 else ifelse(as.character(sector) == sect, 2.5, 1.0),
          enrol_k = enrolments / 1000
        )
      p <- ggplot(df, aes(x = year, y = enrol_k, color = sector, group = sector,
                          text = paste0(sector, "<br>", year, ": ",
                                        format(enrolments, big.mark = ",")))) +
        geom_line(aes(alpha = alpha, linewidth = lw)) +
        geom_point(aes(alpha = alpha), size = 1.5) +
        scale_color_manual(values = SECTOR_COLORS, name = "Sector") +
        scale_alpha_identity() +
        scale_linewidth_identity() +
        labs(
          title = "Sector Enrolment Trends",
          subtitle = if (is.null(yr)) "2005–2024 · all years" else paste0("2005–2024 · highlight: ", yr),
          x = "Year", y = "Enrolments (thousands)"
        ) +
        theme_dashboard()
      p <- add_year_vline(p, yr, color = "grey50")
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
      lookup_yr <- if (is.null(yr)) max(country_raw$year) else yr
      data_year <- if (lookup_yr %in% country_raw$year) lookup_yr else max(country_raw$year)
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
            if (!is.null(yr) && data_year != yr) paste0("Nearest data year to ", yr, " · "),
            if (is.null(yr)) "Latest available year · " else "",
            if (ctry != "All") paste("Highlighting:", ctry) else "Top 10 source countries"
          ),
          x = NULL, y = "Enrolments (thousands)"
        ) +
        theme_dashboard()
      return(to_plotly(p, 340))
    }

    if (tab == "Economy") {
      p <- revenue %>%
        mutate(
          hover = paste0("Year: ", year,
                         "<br>Revenue: $", export_revenue_billion, "B")
        ) %>%
        ggplot(aes(x = year, group = 1)) +
        geom_ribbon(aes(ymin = 0, ymax = export_revenue_billion, text = hover),
                    fill = "#2A9D8F", alpha = 0.35) +
        geom_line(aes(y = export_revenue_billion, text = hover),
                  color = "#2A9D8F", linewidth = 1.1) +
        labs(
          title = "International Education Export Revenue",
          subtitle = "Australia, 2010–2024 (AUD billions)",
          x = "Year", y = "Export revenue ($B)"
        ) +
        theme_dashboard()
      p <- add_year_vline(p, yr)
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
    yr   <- selected_year()
    sect <- input$filter_sector
    ctry <- input$filter_country

    if (tab == "Overview") {
      # plotly does not support coord_polar well — use bar / area charts instead
      if (is.null(yr)) {
        df <- enrolments %>%
          mutate(sector = factor(sector, levels = SECTOR_LEVELS)) %>%
          group_by(year) %>%
          mutate(
            share = (enrolments / sum(enrolments)) * 100,
            text  = paste0(sector, "<br>", year, ": ", round(share, 1), "%")
          ) %>%
          ungroup() %>%
          arrange(year, sector)
        p <- ggplot(df, aes(x = year, y = enrolments, fill = sector, text = text)) +
          geom_area(position = "fill", alpha = 0.85) +
          scale_fill_manual(values = SECTOR_COLORS, name = "Sector") +
          scale_y_continuous(labels = percent_format(), limits = c(0, 1), expand = c(0, 0)) +
          labs(
            title = "Sector Share Over Time",
            subtitle = "2005–2024 (all years)",
            x = "Year", y = "Share (%)"
          ) +
          theme_dashboard()
      } else {
        df <- enrolments %>%
          filter(year == yr) %>%
          mutate(
            pct = round(100 * enrolments / sum(enrolments), 1),
            text = paste0(sector, ": ", format(enrolments, big.mark = ","),
                          " (", pct, "%)")
          )
        p <- ggplot(df, aes(x = reorder(sector, enrolments), y = enrolments / 1000,
                            fill = sector, text = text)) +
          geom_col(width = 0.7) +
          coord_flip() +
          scale_fill_manual(values = SECTOR_COLORS, name = "Sector") +
          labs(
            title = paste0("Sector Mix (", yr, ")"),
            subtitle = "Enrolments by sector (thousands)",
            x = NULL, y = "Enrolments (thousands)"
          ) +
          theme_dashboard() +
          theme(legend.position = "none")
      }
      return(to_plotly(p, 220))
    }

    if (tab == "Growth") {
      growth_colors <- c(
        "Higher Education" = "#1B6CA8",
        "VET"              = "#2E9E6B",
        "ELICOS"           = "#E07B39",
        "Schools"          = "#C0392B"
      )
      # Stagger end-of-line labels to prevent overlap
      nudge_lookup <- c(
        "Higher Education" = 8,
        "VET"              = 4,
        "ELICOS"           = 0,
        "Schools"          = -4
      )
      growth_labels <- growth_index %>%
        group_by(sector) %>%
        filter(year == max(year)) %>%
        ungroup() %>%
        mutate(nudge = nudge_lookup[as.character(sector)])

      p <- ggplot(growth_index, aes(x = year, y = index, colour = sector, group = sector,
                                    text = paste0(sector, "<br>", year, ": ", round(index, 1)))) +
        geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50", linewidth = 0.7) +
        annotate("text", x = min(growth_index$year), y = 100, label = "2005 baseline",
                 vjust = -0.8, hjust = 0, size = 3, colour = "grey50") +
        geom_line(linewidth = 1.1) +
        geom_text(
          data = growth_labels,
          aes(label = sector, colour = sector, y = index + nudge),
          hjust = -0.05, size = 2.8, show.legend = FALSE
        ) +
        scale_colour_manual(values = growth_colors, guide = "none") +
        scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
        labs(
          title = "Sector Growth Index (2005 = 100)",
          x = "Year", y = "Growth index (2005 = 100)"
        ) +
        theme_dashboard() +
        theme(plot.margin = margin(5, 5, 5, 60))
      return(to_plotly(p, 220))
    }

    if (tab == "COVID-19") {
      recovery <- enrolments %>%
        filter(year %in% c(2019, 2021, 2024)) %>%
        mutate(period = factor(year, levels = c(2019, 2021, 2024),
                              labels = c("Pre-COVID (2019)", "Trough (2021)", "Recovery (2024)")))

      # Percentage drop labels for trough bars only
      pct_drops <- data.frame(
        sector  = factor(c("Higher Education", "VET", "ELICOS", "Schools"), levels = SECTOR_LEVELS),
        period  = factor("Trough (2021)", levels = c("Pre-COVID (2019)", "Trough (2021)", "Recovery (2024)")),
        label   = c("-23%", "-38%", "-35%", "-28%"),
        y_pos   = c(380, 110, 85, 40)   # approximate heights above each bar
      )

      p <- ggplot(recovery, aes(x = period, y = enrolments / 1000, fill = sector,
                                text = paste0(sector, "<br>", format(enrolments, big.mark = ",")))) +
        geom_col(position = "dodge") +
        geom_text(
          data = pct_drops,
          aes(x = period, y = y_pos, label = label, colour = sector),
          position = position_dodge(width = 0.9),
          size = 2.8, vjust = -0.3, fontface = "bold",
          inherit.aes = FALSE, show.legend = FALSE
        ) +
        scale_fill_manual(values = SECTOR_COLORS, name = "Sector") +
        scale_colour_manual(values = SECTOR_COLORS, guide = "none") +
        labs(title = "Pre-COVID vs Trough vs Recovery", x = NULL, y = "Thousands") +
        theme_dashboard()
      return(to_plotly(p, 220))
    }

    if (tab == "Countries") {
      country_change$country <- factor(
        country_change$country,
        levels = c("China", "India", "Nepal", "Vietnam", "Indonesia", "Malaysia")
      )
      country_change$year <- factor(country_change$year, levels = c("2010", "2024"))
      p <- ggplot(country_change, aes(x = country, y = share, fill = year,
                                      text = paste0(country, " (", year, "): ", share, "%"))) +
        geom_col(position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("2010" = "#A8C4E0", "2024" = "#1B6CA8"), name = "Year") +
        scale_y_continuous(limits = c(0, 40)) +
        annotate("text", x = 0.7, y = 38, label = "China's share fell;\nIndia surged to match",
                 size = 3.0, colour = "grey40", hjust = 0) +
        labs(
          title = "Source Country Share: 2010 vs 2024",
          x = NULL, y = "Share of total enrolments (%)"
        ) +
        theme_dashboard()
      return(to_plotly(p, 220))
    }

    if (tab == "Economy") {
      label_years <- c(2010, 2013, 2016, 2019, 2021, 2024)
      p <- ggplot(econ_scatter, aes(x = enrolments, y = revenue,
                                    text = paste0("Year: ", year,
                                                  "<br>Enrolments: ", enrolments, "M",
                                                  "<br>Revenue: $", revenue, "B"))) +
        geom_smooth(method = "lm", se = TRUE, colour = "#1B6CA8", fill = "#1B6CA8",
                    alpha = 0.12, linewidth = 0.8) +
        geom_point(colour = "#2E9E6B", size = 3) +
        geom_text(
          data = econ_scatter %>% dplyr::filter(year %in% label_years),
          aes(label = year), size = 2.8, nudge_y = 0.8, colour = "grey30"
        ) +
        annotate("text", x = 0.58, y = 50, label = "R\u00b2 = 0.877",
                 colour = "grey40", size = 3.5, hjust = 0) +
        labs(
          title = "Enrolments vs Export Revenue (2010\u20132024)",
          x = "Total enrolments (millions)", y = "Export revenue ($B)"
        ) +
        theme_dashboard()
      return(to_plotly(p, 220))
    }

    if (tab == "Global") {
      p <- ggplot(gap_data, aes(x = year, group = 1,
                                text = paste0("Year: ", year,
                                              "<br>", aus_pct_of_usa, "% of USA"))) +
        geom_ribbon(aes(ymin = 0, ymax = aus_pct_of_usa), fill = "#1B6CA8", alpha = 0.3) +
        geom_line(aes(y = aus_pct_of_usa), colour = "#1B6CA8", linewidth = 1.1) +
        geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50", linewidth = 0.7) +
        annotate("text", x = min(gap_data$year) + 0.3, y = 103,
                 label = "= USA level", vjust = 0, hjust = 0, size = 3, colour = "grey50") +
        annotate("text", x = 2021, y = 72,
                 label = "Gap narrowing\npost-2019",
                 size = 3.2, colour = "#1B6CA8", hjust = 0) +
        scale_y_continuous(limits = c(0, 110)) +
        labs(
          title = "Australia Closing Gap with USA (% of USA enrolments)",
          x = "Year", y = "% of USA enrolments"
        ) +
        theme_dashboard() +
        theme(
          axis.title.y = element_text(size = 9),
          plot.margin  = margin(5, 5, 5, 70)
        )
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
    updateSelectInput(session, "filter_year",   selected = "2024")
    updateSelectInput(session, "filter_sector", selected = "All")
    updateSelectInput(session, "filter_country", selected = "All")
    updateTabsetPanel(session, "story_tabs", selected = "Overview")
  })
}

# =============================================================================
# Launch app
# =============================================================================
shinyApp(ui = ui, server = server)
