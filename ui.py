import streamlit as st

st.set_page_config(
    page_title="International Student Mobility Dashboard",
    page_icon="🎓",
    layout="wide"
)

# -----------------------------
# SIDEBAR
# -----------------------------
st.sidebar.title("Dashboard Filters")

year_range = st.sidebar.slider(
    "Select Year Range",
    min_value=2005,
    max_value=2028,
    value=(2005, 2024)
)

sector = st.sidebar.multiselect(
    "Select Sector",
    ["Higher Education", "VET", "ELICOS", "Schools"],
    default=["Higher Education", "VET", "ELICOS", "Schools"]
)

source_country = st.sidebar.multiselect(
    "Select Source Countries",
    ["China", "India", "Nepal", "Vietnam", "Philippines", "Colombia", "Brazil", "Thailand", "Malaysia", "Pakistan"],
    default=["China", "India", "Nepal", "Vietnam"]
)

compare_countries = st.sidebar.multiselect(
    "Compare Study Destinations",
    ["Australia", "Canada", "United Kingdom", "United States", "China", "Germany", "France", "Japan", "New Zealand", "Netherlands"],
    default=["Australia", "Canada", "United Kingdom", "United States", "China"]
)

st.sidebar.markdown("---")
st.sidebar.info(
    "This interactive dashboard explores Australia’s international education sector through enrolment trends, source-country patterns, export revenue, and global mobility."
)

# -----------------------------
# HEADER
# -----------------------------
st.title("🎓 International Student Mobility and Economic Contribution")
st.subheader("Australia’s Position in the Global Education Market")

st.markdown("""
This dashboard explores long-term enrolment trends, economic contribution, and Australia’s global position
in the international education market.
""")

# -----------------------------
# KPI SECTION
# -----------------------------
st.markdown("## Overview")

col1, col2, col3, col4 = st.columns(4)

with col1:
    st.metric("Total Enrolments (Latest)", "📌 990K+", "+8.2%")

with col2:
    st.metric("Export Revenue", "📌 $51.0B", "+6.5%")

with col3:
    st.metric("Revenue per Student", "📌 $51.5K", "+3.4%")

with col4:
    st.metric("Australia Global Rank", "📌 #1", "27.2% intl share")

st.markdown("---")

# -----------------------------
# TABS
# -----------------------------
tab1, tab2, tab3, tab4, tab5 = st.tabs([
    "Sector Trends",
    "Source Countries",
    "Economic Contribution",
    "Trend & Forecast",
    "Global Position"
])

# -----------------------------
# TAB 1: SECTOR TRENDS
# -----------------------------
with tab1:
    st.markdown("## Sector Enrolment Dynamics")

    c1, c2 = st.columns(2)

    with c1:
        st.markdown("### Enrolments by Sector Over Time")
        st.info("Placeholder: multi-series line chart for sector enrolments (2005–2024)")

    with c2:
        st.markdown("### Sector Share of Total Enrolments")
        st.info("Placeholder: stacked area chart showing proportional sector composition")

    st.markdown("### Key Insight")
    st.success("""
    Higher Education and VET dominate total enrolments. COVID-19 caused a major disruption,
    followed by an uneven recovery across sectors.
    """)

# -----------------------------
# TAB 2: SOURCE COUNTRIES
# -----------------------------
with tab2:
    st.markdown("## Source Country Analysis")

    c1, c2 = st.columns(2)

    with c1:
        st.markdown("### Top 10 Source Countries")
        st.info("Placeholder: horizontal bar chart ranking top source countries by enrolment volume")

    with c2:
        st.markdown("### 2015 vs 2025 Comparison")
        st.info("Placeholder: grouped bar chart comparing country enrolments across two years")

    st.markdown("### Key Insight")
    st.success("""
    Australia remains highly dependent on a small number of key source markets,
    especially China and India, which suggests both strength and vulnerability.
    """)

# -----------------------------
# TAB 3: ECONOMIC CONTRIBUTION
# -----------------------------
with tab3:
    st.markdown("## Economic Contribution")

    c1, c2 = st.columns(2)

    with c1:
        st.markdown("### Revenue per International Student")
        st.info("Placeholder: line chart with LOESS smoothing for revenue per student")

    with c2:
        st.markdown("### Enrolments vs Export Revenue")
        st.info("Placeholder: scatter plot with regression line")

    st.markdown("### Key Insight")
    st.success("""
    International education is strongly linked to Australia’s export economy.
    The relationship between enrolments and revenue is strong, although COVID-19 created a temporary break.
    """)

# -----------------------------
# TAB 4: TREND & FORECAST
# -----------------------------
with tab4:
    st.markdown("## Long-Run Trend and Forecast")

    c1, c2 = st.columns(2)

    with c1:
        st.markdown("### Actual vs Long-Run Trend")
        st.info("Placeholder: line chart showing actual enrolments vs expected trend")

    with c2:
        st.markdown("### Annual Deviation from Trend")
        st.info("Placeholder: diverging bar chart for above/below trend years")

    st.markdown("### Forecast (2025–2028)")
    st.info("Placeholder: forecast chart with confidence intervals")

    st.markdown("### Key Insight")
    st.success("""
    COVID-19 caused the largest negative deviation in the series, but recent recovery suggests
    strong underlying demand. Forecasts show continued growth with some uncertainty.
    """)

# -----------------------------
# TAB 5: GLOBAL POSITION
# -----------------------------
with tab5:
    st.markdown("## Australia’s Global Position")

    c1, c2 = st.columns(2)

    with c1:
        st.markdown("### Share of International Students Over Time")
        st.info("Placeholder: ranking line chart comparing Australia with major destinations")

    with c2:
        st.markdown("### International Student Share by Country")
        st.info("Placeholder: bar chart comparing latest available internationalisation rates")

    st.markdown("### Global Mobility Map")
    st.info("Placeholder: choropleth map showing inbound international student mobility rate by country")

    st.markdown("### Key Insight")
    st.success("""
    Australia is one of the world’s most internationalised education destinations,
    ranking strongly against major competitors such as the UK, Canada, and the US.
    """)

# -----------------------------
# FOOTER
# -----------------------------
st.markdown("---")
st.caption("Draft UI only — based on the report: International Student Mobility and Economic Contribution")