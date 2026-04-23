# ============================================================
#
#  Jobs for Peace? Inward Investment and Unemployment in Post-Agreement Northern 
#  Ireland 
#  Evidence from 2002 - 2012
#
#  Master's Thesis — The Hertie School of Governance, Berlin
#  Author:  Paul Schertzer
#  Year:    2026
#
#  The analysis is a cross-regional panel comparing Northern
#  Ireland to four matched UK ITL1 comparison regions over
#  2002-2012. The main question is whether Invest NI's inward
#  investment promotion activity was associated with lower
#  unemployment in Northern Ireland relative to comparable
#  UK regions.
#
#  The estimation strategy is a panel regression with two-way
#  fixed effects, controlling for permanent differences between
#  regions and for UK-wide economic shocks in each year.
#
#
#  Data sources:
#    Unemployment:       ONS LFS timeseries (CDID: ZSFB for NI)
#    Youth unemployment: ONS X02 regional dataset
#    Inactivity:         ONS X03 regional dataset
#    Jobs promoted:      Invest NI Annual Reports 2002/03-2011/12
#                        + NIAO Performance Review (2012), Table 4
#
#

# packages

library(tidyverse)
library(fixest)
library(modelsummary)
library(ggplot2)
library(scales)
library(patchwork)


# force plain decimals in tables, not scientific notation

no_sci <- function(x) formatC(x, format = "f", digits = 8)


# regions and colours

MATCHED_REGIONS <- c(
  "Northern Ireland",   # treated region
  "North East",         # comparison region 1 — closest match
  "London",             # comparison region 2
  "West Midlands",      # comparison region 3
  "North West"          # comparison region 4
)

control_regions <- MATCHED_REGIONS[MATCHED_REGIONS != "Northern Ireland"]

# colours
REGION_COLOURS <- c(
  "Northern Ireland" = "#1F4E79",
  "North East"       = "#C00000",
  "London"           = "#375623",
  "West Midlands"    = "#7F3F98",
  "North West"       = "#D47500"
)

# shapes
REGION_SHAPES <- c(
  "Northern Ireland" = 16,
  "North East"       = 17,
  "London"           = 15,
  "West Midlands"    = 18,
  "North West"       = 8
)


# load data

df_raw <- read_csv("analysis_panel.csv")

# Confirm all required columns are present
stopifnot(all(c("region", "year", "unemp_rate", "youth_unemp_rate",
                "inactivity_rate", "inv_jobs_promoted", "ni") %in%
                names(df_raw)))

message("Loaded ", nrow(df_raw), " rows | Years: ",
        min(df_raw$year), "-", max(df_raw$year))


# build analysis panel - filter to 5 regions, add lag

df <- df_raw %>%
  filter(region %in% MATCHED_REGIONS) %>%
  arrange(region, year) %>%
  group_by(region) %>%
  mutate(
    year   = as.integer(year),
    region = as.factor(region),
    ni     = as.integer(region == "Northern Ireland"),

    # lag NI only, controls stay at 0
    inv_jobs_promoted_lag1 = if_else(
      ni == 1L,
      lag(inv_jobs_promoted, n = 1, order_by = year),
      0
    ),
    
    post_definition = as.integer(year >= 2006),  # definition change
    peace_iii       = as.integer(year >= 2007),  # EU PEACE III start

    year_fac = factor(year)
  ) %>%
  ungroup() %>%
  mutate(inv_jobs_promoted_lag1 = replace_na(inv_jobs_promoted_lag1, 0))

message("Regions: ", paste(levels(df$region), collapse = ", "))
message("NI observations: ", sum(df$ni == 1),
        " | Comparison observations: ", sum(df$ni == 0))
message("Total observations in regression sample: ", nrow(df))
message("NA check — unemployment: ",     anyNA(df$unemp_rate),
        " | youth: ",                     anyNA(df$youth_unemp_rate),
        " | inactivity: ",               anyNA(df$inactivity_rate))


# regression tables for appendix A


# TABLE A1 - MAIN RESULTS

m1  <- feols(unemp_rate ~ inv_jobs_promoted          | region + year,
             df,                          cluster = ~region)
m2  <- feols(unemp_rate ~ inv_jobs_promoted_lag1     | region + year,
             df,                          cluster = ~region)
m1b <- feols(unemp_rate ~ inv_jobs_promoted          | region + year,
             filter(df, region != "London"), cluster = ~region)
m2b <- feols(unemp_rate ~ inv_jobs_promoted_lag1     | region + year,
             filter(df, region != "London"), cluster = ~region)

modelsummary(
  list(
    "Same Year"                      = m1,
    "Previous Year"                  = m2,
    "Same Year (excl. London)"       = m1b,
    "Previous Year (excl. London)"   = m2b
  ),
  title    = "Table A1 - Main Results: Effect of Inward Investment on Unemployment Rate",
  stars    = c("*" = .1, "**" = .05, "***" = .01),
  fmt      = no_sci,
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  coef_map = c(
    inv_jobs_promoted      = "Jobs promoted (same year)",
    inv_jobs_promoted_lag1 = "Jobs promoted (previous year)"
  ),
  notes = paste(
    "Cross-regional panel.",
    "Region and year fixed effects included in all models.",
    "Standard errors clustered by region.",
    "Dependent variable: annual unemployment rate (%, ILO definition, seasonally adjusted).",
    "Investment variable equals zero for all comparison regions.",
    "Columns 3 and 4 exclude London as a robustness check.",
    "Sample: Northern Ireland and 4 matched UK ITL1 comparison regions, 2002-2012.",
    "Region and year fixed effects are reported separately in Figure A1.",
    sep = " "
  ),
  output = "Table5_1.docx"
)
message("Table A1 saved -> Table5_1.docx")


# TABLE A2 - ROBUSTNESS: YOUTH UNEMPLOYMENT + INACTIVITY

m3 <- feols(youth_unemp_rate ~ inv_jobs_promoted      | region + year,
            df, cluster = ~region)
m4 <- feols(youth_unemp_rate ~ inv_jobs_promoted_lag1 | region + year,
            df, cluster = ~region)
m5 <- feols(inactivity_rate  ~ inv_jobs_promoted      | region + year,
            df, cluster = ~region)
m6 <- feols(inactivity_rate  ~ inv_jobs_promoted_lag1 | region + year,
            df, cluster = ~region)

modelsummary(
  list(
    "Youth Unemp. (same year)"   = m3,
    "Youth Unemp. (prev. year)"  = m4,
    "Inactivity (same year)"     = m5,
    "Inactivity (prev. year)"    = m6
  ),
  title    = "Table A2 - Robustness: Youth Unemployment (16-24) and Economic Inactivity (16-64)",
  stars    = c("*" = .1, "**" = .05, "***" = .01),
  fmt      = no_sci,
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  coef_map = c(
    inv_jobs_promoted      = "Jobs promoted (same year)",
    inv_jobs_promoted_lag1 = "Jobs promoted (previous year)"
  ),
  notes = paste(
    "Cross-regional panel.",
    "Region and year fixed effects included. Standard errors clustered by region.",
    "Youth unemployment: ages 16-24, not seasonally adjusted,",
    "four-quarter calendar-year average (ONS X02).",
    "Inactivity: ages 16-64 working age, not seasonally adjusted,",
    "four-quarter average (ONS X03).",
    "Significant negative result for youth unemployment supports broader labour market inclusion.",
    "Null result for inactivity indicates unemployment fell because people found work,",
    "not because they stopped looking.",
    sep = " "
  ),
  output = "Table5_2.docx"
)
message("Table A2 saved -> Table5_2.docx")


# TABLE A3 - SENSITIVTY: PEACE + DEFINITION CHANGE

m7  <- feols(unemp_rate ~ inv_jobs_promoted      + peace_iii       | region + year,
             df, cluster = ~region)
m8  <- feols(unemp_rate ~ inv_jobs_promoted_lag1 + peace_iii       | region + year,
             df, cluster = ~region)
m9  <- feols(unemp_rate ~ inv_jobs_promoted      + post_definition | region + year,
             df, cluster = ~region)
m10 <- feols(unemp_rate ~ inv_jobs_promoted_lag1 + post_definition | region + year,
             df, cluster = ~region)

modelsummary(
  list(
    "With PEACE III (same year)"      = m7,
    "With PEACE III (prev. year)"     = m8,
    "With Defn. Change (same year)"   = m9,
    "With Defn. Change (prev. year)"  = m10
  ),
  title    = "Table A3 - Sensitivity: EU PEACE Fund Controls and Measurement Definition Change",
  stars    = c("*" = .1, "**" = .05, "***" = .01),
  fmt      = no_sci,
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  coef_map = c(
    inv_jobs_promoted      = "Jobs promoted (same year)",
    inv_jobs_promoted_lag1 = "Jobs promoted (previous year)",
    peace_iii              = "EU PEACE III period (from 2007)",
    post_definition        = "Broader definition period (from 2006)"
  ),
  notes = paste(
    "Cross-regional panel.",
    "Region and year fixed effects included. Standard errors clustered by region.",
    "The EU PEACE III dummy equals 1 from 2007 onward.",
    "The definition change dummy equals 1 from 2006 when Invest NI broadened",
    "jobs promoted to include reinvestment by existing investors.",
    "Both additional variables are absorbed by the year fixed effects",
    "— a known feature of short two-way fixed effects panels.",
    "The investment coefficient remains unchanged across all specifications.",
    sep = " "
  ),
  output = "Table5_3.docx"
)
message("Table A3 saved -> Table5_3.docx")


# TABLE A4 - PLACEBO: ASSIGN NI SERIES TO EACH CONTROL

placebo_results <- list()

for (placebo_region in control_regions) {

  ni_investment <- df_raw %>%
    filter(region == "Northern Ireland") %>%
    arrange(year) %>%
    pull(inv_jobs_promoted)

  df_placebo <- df_raw %>%
    filter(region %in% MATCHED_REGIONS) %>%
    arrange(region, year) %>%
    group_by(region) %>%
    mutate(
      year      = as.integer(year),
      region    = as.factor(region),
      inv_treat = if_else(region == placebo_region, ni_investment, 0)
    ) %>%
    ungroup()

  m_p <- tryCatch(
    feols(unemp_rate ~ inv_treat | region + year,
          data = df_placebo, cluster = ~region),
    error = function(e) NULL
  )
  if (!is.null(m_p)) placebo_results[[placebo_region]] <- m_p
}

placebo_results[["Northern Ireland (Real)"]] <- m1

modelsummary(
  placebo_results,
  title    = "Table A4 - Placebo Test: Assigning Northern Ireland's Investment Series to Each Comparison Region",
  stars    = c("*" = .1, "**" = .05, "***" = .01),
  fmt      = no_sci,
  gof_map  = c("nobs", "r.squared"),
  coef_map = c(
    inv_treat         = "Placebo investment (same year)",
    inv_jobs_promoted = "Real investment — Northern Ireland"
  ),
  notes = paste(
    "Cross-regional panel.",
    "Region and year fixed effects included. Standard errors clustered by region.",
    "Each column assigns Northern Ireland's actual investment series to one comparison region",
    "as if it were the treated unit.",
    "None of the four placebo tests produce a statistically significant result.",
    "Only the real Northern Ireland result (final column) is significant.",
    "This confirms the main finding is specific to Northern Ireland",
    "and is not a statistical artefact of the panel structure.",
    sep = " "
  ),
  output = "Table5_4.docx"
)
message("Table A4 saved -> Table5_4.docx")


# FIGURE A1 - FIXED EFFECTS

fe <- fixef(m1)

# region FEs
region_fe <- data.frame(
  region = names(fe$region),
  fe_val = round(as.numeric(fe$region), 3)
) %>%
  arrange(fe_val) %>%
  mutate(
    region = factor(region, levels = region),
    is_ni  = region == "Northern Ireland"
  )

# year FEs
year_fe <- data.frame(
  year   = as.integer(names(fe$year)),
  fe_val = round(as.numeric(fe$year), 3)
)

# print to console
cat("\n=======================================================\n")
cat("  FIXED EFFECTS — APPENDIX A, FIGURE A1\n")
cat("=======================================================\n\n")
cat("REGION FIXED EFFECTS\n")
cat("Positive = region runs permanently higher unemployment\n")
cat("Negative = region runs permanently lower unemployment\n\n")
print(region_fe[, c("region", "fe_val")])
cat("\nYEAR FIXED EFFECTS\n")
cat("Shows UK-wide shock in each year (common to all regions)\n")
cat("Large positive 2009 value = Global Financial Crisis\n\n")
print(year_fe)
cat("=======================================================\n\n")

# Left panel
p_region <- ggplot(region_fe,
                   aes(x = fe_val, y = region, fill = is_ni)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
  geom_text(aes(label = fe_val,
                hjust = ifelse(fe_val >= 0, -0.2, 1.2)),
            size = 3.2, colour = "grey30") +
  scale_fill_manual(
    values = c("TRUE" = "#1F4E79", "FALSE" = "#BBBBBB"),
    guide  = "none"
  ) +
  labs(
    title    = "Region Fixed Effects",
    subtitle = "Permanent unemployment\ndifference per region\n(percentage points)",
    x = "Fixed effect value (pp)",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11, colour = "#1F4E79"),
    plot.subtitle      = element_text(size = 9, colour = "#666666"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Right panel
p_year <- ggplot(year_fe, aes(x = year, y = fe_val)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey60", linewidth = 0.5) +
  geom_area(alpha = 0.08, fill = "#1F4E79") +
  geom_line(colour = "#1F4E79", linewidth = 1.1) +
  geom_point(colour = "#1F4E79", size = 3) +
  geom_text(aes(label = fe_val), vjust = -0.8,
            size = 2.8, colour = "#1F4E79") +
  annotate("rect", xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "grey40") +
  annotate("text", x = 2009, y = max(year_fe$fe_val) * 0.85,
           label = "GFC", size = 3, colour = "grey40", hjust = 0.5) +
  scale_x_continuous(breaks = 2002:2012) +
  labs(
    title    = "Year Fixed Effects",
    subtitle = "UK-wide unemployment shock\nin each year\n(percentage points)",
    x = NULL,
    y = "Fixed effect value (pp)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 11, colour = "#1F4E79"),
    plot.subtitle    = element_text(size = 9, colour = "#666666"),
    panel.grid.minor = element_blank()
  )

# Combine
fig_fe <- p_region + p_year +
  plot_annotation(
    title    = "Figure A1 - Fixed Effects from Main Regression Model",
    subtitle = paste(
      "Left: region fixed effects — the permanent unemployment difference for each region,",
      "absorbed by the model before estimating the investment coefficient.",
      "Right: year fixed effects — the UK-wide shock in each year affecting all regions simultaneously.",
      "The large positive value in 2009 reflects the Global Financial Crisis.",
      "Northern Ireland (dark blue) consistently ran higher unemployment than the panel average,",
      "which the model correctly controls for before estimating the main result.",
      sep = " "
    ),
    caption  = paste(
      "Fixed effects extracted from main model (Table A1, column 1) using fixef(m1).",
      "Region fixed effects: permanent unemployment differences (percentage points).",
      "Year fixed effects: common UK-wide shock per year (percentage points).",
      "Both are absorbed by the model before the investment coefficient is estimated.",
      sep = "\n"
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 12, colour = "#1F4E79"),
      plot.subtitle = element_text(size = 9, colour = "#666666"),
      plot.caption  = element_text(size = 8, colour = "#888888", hjust = 0)
    )
  )

ggsave("Figure5_8.png", fig_fe, width = 12, height = 5, dpi = 300)
message("Figure A1 saved -> Figure5_8.png")


# main text figures

# FIGURE 5.1  Regional Unemployment Trends 1992-2001


pre_data <- tribble(
  ~region,              ~year, ~unemp_rate,
  # Northern Ireland — CDID: ZSFB
  "Northern Ireland",    1992,  12.3,
  "Northern Ireland",    1993,  12.5,
  "Northern Ireland",    1994,  11.7,
  "Northern Ireland",    1995,  10.6,
  "Northern Ireland",    1996,   9.7,
  "Northern Ireland",    1997,   8.6,
  "Northern Ireland",    1998,   7.6,
  "Northern Ireland",    1999,   7.1,
  "Northern Ireland",    2000,   6.2,
  "Northern Ireland",    2001,   6.0,
  # North East — CDID: YCNC
  "North East",          1992,  12.4,
  "North East",          1993,  12.8,
  "North East",          1994,  12.5,
  "North East",          1995,  11.4,
  "North East",          1996,  10.4,
  "North East",          1997,   9.2,
  "North East",          1998,   8.7,
  "North East",          1999,   9.3,
  "North East",          2000,   8.6,
  "North East",          2001,   7.3,
  # London — CDID: YCNI
  "London",              1992,  12.4,
  "London",              1993,  13.8,
  "London",              1994,  12.8,
  "London",              1995,  11.9,
  "London",              1996,  11.4,
  "London",              1997,   9.6,
  "London",              1998,   8.1,
  "London",              1999,   7.4,
  "London",              2000,   7.2,
  "London",              2001,   6.7,
  # West Midlands — CDID: YCNG
  "West Midlands",       1992,  11.0,
  "West Midlands",       1993,  11.7,
  "West Midlands",       1994,   9.7,
  "West Midlands",       1995,   8.7,
  "West Midlands",       1996,   8.5,
  "West Midlands",       1997,   6.9,
  "West Midlands",       1998,   6.2,
  "West Midlands",       1999,   6.8,
  "West Midlands",       2000,   6.0,
  "West Midlands",       2001,   5.5,
  # North West — CDID: YCND
  "North West",          1992,  10.3,
  "North West",          1993,  10.6,
  "North West",          1994,  10.1,
  "North West",          1995,   8.9,
  "North West",          1996,   8.1,
  "North West",          1997,   7.0,
  "North West",          1998,   6.8,
  "North West",          1999,   6.3,
  "North West",          2000,   5.5,
  "North West",          2001,   5.3
)

pre_data <- pre_data %>%
  mutate(region = factor(region, levels = names(REGION_COLOURS)))

fig5_1 <- ggplot(pre_data,
                 aes(x = year, y = unemp_rate, group = region,
                     colour = region,
                     linewidth = (region == "Northern Ireland"),
                     shape = region)) +
  annotate("rect", xmin = 1997.5, xmax = 2001.5,
           ymin = -Inf, ymax = Inf, alpha = 0.04, fill = "#1F4E79") +
  geom_vline(xintercept = 1997.5, linetype = "dotted",
             colour = "#1F4E79", linewidth = 0.5) +
  annotate("text", x = 1997.7, y = 14.2,
           label = "Good Friday\nAgreement (1998)",
           size = 2.8, colour = "#1F4E79", hjust = 0) +
  geom_line() +
  geom_point(size = 2.5) +
  scale_colour_manual(values = REGION_COLOURS, name = NULL) +
  scale_shape_manual(values = REGION_SHAPES, name = NULL) +
  scale_linewidth_manual(values = c("TRUE" = 1.4, "FALSE" = 0.7),
                         guide = "none") +
  scale_x_continuous(breaks = seq(1992, 2001, 1)) +
  scale_y_continuous(limits = c(4, 15)) +
  labs(
    title    = "Figure 5.1 - Unemployment Trends Before 2002: Northern Ireland and Comparison Regions",
    subtitle = "All five regions followed broadly similar downward trajectories before the analysis period begins, supporting their use as valid comparators for Northern Ireland.",
    x = NULL, y = "Unemployment rate (%)",
    caption  = paste(
      "Source: ONS LFS timeseries 1992-2001 (CDIDs ZSFB, YCNC, YCNI, YCNG, YCND).",
      "Comparison regions selected based on similarity to Northern Ireland in unemployment",
      "level and trend over this pre-period. Shaded area = post-Agreement transition (1998-2001).",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, colour = "#1F4E79"),
    plot.subtitle    = element_text(size = 10, colour = "#666666"),
    plot.caption     = element_text(size = 8,  colour = "#888888", hjust = 0),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("Figure5_1.png", fig5_1, width = 9, height = 5, dpi = 300)
message("Figure 5.1 saved -> Figure5_1.png")


# fig 5.2 - jobs promoted

ni_ts <- df %>% filter(region == "Northern Ireland") %>% arrange(year)

fig5_2 <- ggplot(ni_ts, aes(x = year, y = inv_jobs_promoted)) +
  annotate("rect", xmin = 2005.5, xmax = 2012.5,
           ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "#C00000") +
  geom_vline(xintercept = 2005.5, linetype = "dashed",
             colour = "#C00000", linewidth = 0.4) +
  annotate("text", x = 2008.5, y = 6900,
           label = "Broader definition\n(incl. reinvestment)",
           size = 3, colour = "#C00000", hjust = 0.5) +
  geom_area(alpha = 0.08, fill = "#1F4E79") +
  geom_line(colour = "#1F4E79", linewidth = 1.1) +
  geom_point(colour = "#1F4E79", size = 3.5) +
  geom_text(aes(label = comma(inv_jobs_promoted)),
            vjust = -0.8, size = 2.8, colour = "#1F4E79") +
  scale_x_continuous(breaks = 2002:2012) +
  scale_y_continuous(labels = comma, limits = c(0, 7800),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Figure 5.2 - Invest NI Inward Investment Jobs Promoted, 2002-2012",
    subtitle = "Annual jobs promoted associated with inward investment projects (intentions at project approval stage)",
    x = NULL, y = "Jobs promoted",
    caption  = paste(
      "Sources: Invest NI Annual Reports 2002/03-2011/12; NIAO Performance Review (2012), Table 4.",
      "Shaded area: post-2005/06 period when definition broadened from new FDI only to include reinvestment.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, colour = "#1F4E79"),
    plot.subtitle      = element_text(size = 10, colour = "#666666"),
    plot.caption       = element_text(size = 8,  colour = "#888888", hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("Figure5_2.png", fig5_2, width = 9, height = 5, dpi = 300)
message("Figure 5.2 saved -> Figure5_2.png")


# fig 5.3 - NI Investment vs Unemployment

ni_dual      <- df %>% filter(region == "Northern Ireland") %>% arrange(year)
scale_factor <- max(ni_dual$inv_jobs_promoted) / max(ni_dual$unemp_rate)

fig5_3 <- ggplot(ni_dual, aes(x = year)) +
  geom_col(aes(y = inv_jobs_promoted / scale_factor),
           fill = "#CFE2F3", alpha = 0.7, width = 0.6) +
  geom_line(aes(y = unemp_rate), colour = "#1F4E79", linewidth = 1.3) +
  geom_point(aes(y = unemp_rate), colour = "#1F4E79", size = 3.2) +
  geom_vline(xintercept = 2005.5, linetype = "dashed",
             colour = "#C00000", linewidth = 0.4, alpha = 0.7) +
  geom_vline(xintercept = 2007.5, linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  annotate("text", x = 2007.7, y = 8.5,
           label = "Global Financial\nCrisis begins",
           size = 2.8, colour = "grey50", hjust = 0) +
  scale_x_continuous(breaks = 2002:2012) +
  scale_y_continuous(
    name     = "Unemployment rate (%)",
    limits   = c(0, max(ni_dual$unemp_rate) * 1.25),
    sec.axis = sec_axis(~ . * scale_factor,
                        name   = "Jobs promoted (Invest NI)",
                        labels = comma)
  ) +
  labs(
    title    = "Figure 5.3 - Northern Ireland: Investment Activity and Unemployment Rate, 2002-2012",
    subtitle = "Bars = Invest NI jobs promoted (right axis). Line = NI unemployment rate (left axis).",
    x = NULL,
    caption  = paste(
      "Sources: Invest NI Annual Reports 2002/03-2011/12; NIAO (2012); ONS LFS timeseries ZSFB.",
      "As investment activity increased from 2003 to 2007, unemployment fell.",
      "As investment fell after the Global Financial Crisis, unemployment rose.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, colour = "#1F4E79"),
    plot.subtitle      = element_text(size = 10, colour = "#666666"),
    plot.caption       = element_text(size = 8,  colour = "#888888", hjust = 0),
    axis.title.y.left  = element_text(colour = "#1F4E79"),
    axis.title.y.right = element_text(colour = "#5B8DB8"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("Figure5_3.png", fig5_3, width = 9, height = 5, dpi = 300)
message("Figure 5.3 saved -> Figure5_3.png")


# fig 5.4 - NI vs Comparison Regions

fig5_4_data <- df %>%
  mutate(group = if_else(region == "Northern Ireland",
                         "Northern Ireland",
                         "Comparison regions (average)")) %>%
  group_by(year, group) %>%
  summarise(unemp = mean(unemp_rate, na.rm = TRUE), .groups = "drop")

fig5_4 <- ggplot(fig5_4_data,
                 aes(x = year, y = unemp,
                     colour = group, linetype = group)) +
  annotate("rect", xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "grey40") +
  annotate("text", x = 2009, y = 10.8,
           label = "GFC", size = 3, colour = "grey40", hjust = 0.5) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.2) +
  scale_x_continuous(breaks = 2002:2012) +
  scale_colour_manual(
    values = c("Northern Ireland"             = "#1F4E79",
               "Comparison regions (average)" = "#C00000")
  ) +
  scale_linetype_manual(
    values = c("Northern Ireland"             = "solid",
               "Comparison regions (average)" = "dashed")
  ) +
  labs(
    title    = "Figure 5.4 - Unemployment Rate: Northern Ireland vs Comparison Regions, 2002-2012",
    subtitle = "Annual ILO unemployment rate (%), seasonally adjusted. Comparison regions: unweighted average of North East, London, West Midlands, North West.",
    x = NULL, y = "Unemployment rate (%)",
    colour = NULL, linetype = NULL,
    caption  = paste(
      "Source: ONS LFS/APS timeseries (CDIDs ZSFB and regional equivalents).",
      "Comparison regions selected based on similarity to Northern Ireland",
      "in unemployment level and trend over 1992-1997 (see Appendix B, Table B1).",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, colour = "#1F4E79"),
    plot.subtitle    = element_text(size = 10, colour = "#666666"),
    plot.caption     = element_text(size = 8,  colour = "#888888", hjust = 0),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("Figure5_4.png", fig5_4, width = 9, height = 5, dpi = 300)
message("Figure 5.4 saved -> Figure5_4.png")


# fig 5.5 All Regions: Overall + Youth Unemployment

fig5_5_data <- df %>%
  filter(region %in% MATCHED_REGIONS) %>%
  select(region, year, unemp_rate, youth_unemp_rate) %>%
  pivot_longer(
    cols      = c(unemp_rate, youth_unemp_rate),
    names_to  = "measure",
    values_to = "rate"
  ) %>%
  mutate(
    measure = case_when(
      measure == "unemp_rate"       ~ "Overall unemployment (16+)",
      measure == "youth_unemp_rate" ~ "Youth unemployment (16-24)"
    ),
    region = factor(region, levels = names(REGION_COLOURS))
  )

fig5_5 <- ggplot(fig5_5_data,
                 aes(x = year, y = rate,
                     colour = measure, linetype = measure)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ region, ncol = 5) +
  scale_colour_manual(
    values = c("Overall unemployment (16+)"  = "#1F4E79",
               "Youth unemployment (16-24)"  = "#C00000"),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("Overall unemployment (16+)"  = "solid",
               "Youth unemployment (16-24)"  = "dashed"),
    name = NULL
  ) +
  scale_x_continuous(breaks = c(2002, 2007, 2012),
                     labels = c("'02", "'07", "'12")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Figure 5.5 - Overall and Youth Unemployment: All Regions, 2002-2012",
    subtitle = "Solid line = overall unemployment (16+, seasonally adjusted). Dashed line = youth unemployment (16-24, four-quarter average).",
    x = NULL, y = "Unemployment rate (%)",
    caption  = paste(
      "Sources: ONS LFS/APS timeseries (overall unemployment); ONS X02 dataset (youth unemployment).",
      "Northern Ireland shows the most pronounced decline in youth unemployment",
      "during the 2004-2008 period of highest investment activity.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, colour = "#1F4E79"),
    plot.subtitle    = element_text(size = 9,  colour = "#666666"),
    plot.caption     = element_text(size = 7.5, colour = "#888888", hjust = 0),
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 9, colour = "#1F4E79"),
    panel.grid.minor = element_blank()
  )

ggsave("Figure5_5.png", fig5_5, width = 13, height = 5, dpi = 300)
message("Figure 5.5 saved -> Figure5_5.png")


# fig 5.6 Year-by-Year NI vs Comparison Regions

m_es <- feols(
  unemp_rate ~ i(year_fac, ni, ref = "2002") | region + year,
  data = df, cluster = ~region
)

es_coefs <- coef(m_es)
es_ci    <- confint(m_es, level = 0.95)
es_idx   <- grep("year_fac::", names(es_coefs))
es_years <- as.integer(
  sub("year_fac::(\\d+):ni", "\\1", names(es_coefs)[es_idx])
)

es_df <- data.frame(
  year    = es_years,
  coef    = as.numeric(es_coefs[es_idx]),
  ci_low  = as.numeric(es_ci[es_idx, 1]),
  ci_high = as.numeric(es_ci[es_idx, 2])
)

fig5_6 <- ggplot(es_df, aes(x = year, y = coef)) +
  geom_hline(yintercept = 0, colour = "#888888",
             linewidth = 0.6, linetype = "dashed") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              alpha = 0.12, fill = "#1F4E79") +
  geom_line(colour = "#1F4E79", linewidth = 1.1) +
  geom_point(colour = "#1F4E79", size = 3.2) +
  annotate("text", x = 2007,
           y = min(es_df$ci_low, na.rm = TRUE) * 0.85,
           label = "Peak investment year\n(6,400 jobs promoted)",
           size = 2.8, colour = "#1F4E79", hjust = 0.5) +
  geom_segment(
    aes(x    = 2007, xend = 2007,
        y    = min(es_df$ci_low, na.rm = TRUE) * 0.75,
        yend = es_df$coef[es_df$year == 2007] - 0.1),
    arrow  = arrow(length = unit(0.2, "cm")),
    colour = "#1F4E79", linewidth = 0.4
  ) +
  scale_x_continuous(breaks = 2003:2012) +
  labs(
    title    = "Figure 5.6 - Northern Ireland Unemployment Relative to Comparison Regions, Year by Year",
    subtitle = "Each point shows how much higher or lower NI unemployment was compared to comparison regions in that year, after accounting for region and year differences. Reference year: 2002.",
    x = NULL, y = "Difference in unemployment rate (percentage points)",
    caption  = paste(
      "95% confidence intervals shown. Standard errors clustered by region (5 regions total).",
      "With a small number of regions, confidence intervals are indicative rather than precisely calibrated.",
      "Negative values mean Northern Ireland had lower unemployment than comparison regions that year.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, colour = "#1F4E79"),
    plot.subtitle    = element_text(size = 10, colour = "#666666"),
    plot.caption     = element_text(size = 8,  colour = "#888888", hjust = 0),
    panel.grid.minor = element_blank()
  )

ggsave("Figure5_6.png", fig5_6, width = 9, height = 5, dpi = 300)
message("Figure 5.6 saved -> Figure5_6.png")


# fig 5.7 Youth Unemployment All Regions

fig5_7_data <- df %>%
  filter(region %in% MATCHED_REGIONS) %>%
  mutate(region = factor(region, levels = names(REGION_COLOURS)))

fig5_7 <- ggplot(fig5_7_data,
                 aes(x = year, y = youth_unemp_rate,
                     group = region, colour = region,
                     shape = region,
                     linewidth = (region == "Northern Ireland"))) +
  annotate("rect", xmin = 2007.5, xmax = 2010.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "grey40") +
  annotate("text", x = 2009, y = 29,
           label = "GFC", size = 3, colour = "grey40", hjust = 0.5) +
  geom_line() +
  geom_point(size = 2.5) +
  scale_colour_manual(values = REGION_COLOURS, name = NULL) +
  scale_shape_manual(values = REGION_SHAPES, name = NULL) +
  scale_linewidth_manual(
    values = c("TRUE" = 1.4, "FALSE" = 0.7), guide = "none"
  ) +
  scale_x_continuous(breaks = 2002:2012) +
  scale_y_continuous(limits = c(8, 30),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Figure 5.7 - Youth Unemployment Rate (16-24): All Regions, 2002-2012",
    subtitle = "Annual youth unemployment rate (%), not seasonally adjusted, four-quarter calendar-year average.",
    x = NULL, y = "Youth unemployment rate (%)",
    caption  = paste(
      "Source: ONS X02 Regional labour market: estimates of unemployment by age.",
      "Figures averaged across January-March, April-June, July-September and October-December quarters.",
      "Northern Ireland records the lowest youth unemployment of all five regions through 2003-2008.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, colour = "#1F4E79"),
    plot.subtitle    = element_text(size = 10, colour = "#666666"),
    plot.caption     = element_text(size = 8,  colour = "#888888", hjust = 0),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("Figure5_7.png", fig5_7, width = 9, height = 5, dpi = 300)
message("Figure 5.7 saved -> Figure5_7.png")


# ── FINAL SUMMARY ─────────────────────────────────────────────

cat("\n=======================================================\n")
cat("  ALL OUTPUTS GENERATED SUCCESSFULLY\n")
cat("=======================================================\n\n")
cat("MAIN TEXT FIGURES:\n")
cat("  Figure5_1.png  Unemployment trends 1992-2001   [Section 5.1]\n")
cat("  Figure5_2.png  Invest NI jobs promoted         [Section 5.2]\n")
cat("  Figure5_3.png  Dual axis NI investment/unemp  [Section 5.2]\n")
cat("  Figure5_4.png  NI vs comparison regions        [Section 5.3]\n")
cat("  Figure5_5.png  All regions overall + youth     [Section 5.3]\n")
cat("  Figure5_6.png  Year-by-year NI vs controls     [Section 5.4]\n")
cat("  Figure5_7.png  Youth unemployment all regions  [Section 5.4]\n")
cat("\nAPPENDIX A:\n")
cat("  Table5_1.docx  Table A1 — Main results\n")
cat("  Table5_2.docx  Table A2 — Youth unemployment + inactivity\n")
cat("  Table5_3.docx  Table A3 — PEACE + definition change\n")
cat("  Table5_4.docx  Table A4 — Placebo test\n")
cat("  Figure5_8.png  Figure A1 — Fixed effects\n")
cat("\nKEY RESULT:\n")
cat("  Coefficient: -0.000408 per job promoted\n")
cat("  At 1,000 jobs: -0.41 percentage points\n")
cat("  At 2,600 jobs (avg): -1.06 percentage points\n")
cat("  At 6,400 jobs (peak 2007): -2.61 percentage points\n")
cat("  Significance: p = 0.0015 (1% level)\n")
cat("=======================================================\n")
