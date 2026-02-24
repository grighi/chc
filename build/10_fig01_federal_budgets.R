#!/usr/bin/env Rscript
# ============================================================================
# 10_fig01_federal_budgets.R — Federal Mental Health Budgets, 1965-2023
# Adapted from: federal_budgets/plot_mental_health_budgets.R
# ============================================================================
log_section("10: Figure 1 — Federal Budgets")

budgets <- read_csv(file.path(FED_DIR, "mental_health_budgets.csv"), show_col_types = FALSE)

mh_programs <- c("Early CMHC", "ADAMHA Mental Health",
                 "ADMS Block Grant", "Mental Health Block Grant", "Mental Health Block Grant6",
                 "CCBHC Program6")

group_map <- c(
  "Early CMHC"                 = "CMHC Funding",
  "ADAMHA Mental Health"       = "CMHC Funding",
  "ADMS Block Grant"           = "Mental Health Block Grant",
  "Mental Health Block Grant"  = "Mental Health Block Grant",
  "Mental Health Block Grant6" = "Mental Health Block Grant",
  "CCBHC Program6"             = "CCBHC Program Funding"
)

mh <- budgets %>%
  filter(program %in% mh_programs) %>%
  mutate(
    # Two CSV layouts exist:
    #  Old (with line_code): year=fiscal_year, amount=numeric, unit=unit_label
    #  New (no line_code):   budget_year=fiscal_year, estim=numeric, amount=unit_label
    year_num    = suppressWarnings(as.integer(year)),
    estim_num   = suppressWarnings(as.numeric(estim)),
    amount_num  = suppressWarnings(as.numeric(amount)),
    fiscal_year = if_else(!is.na(year_num), year_num, as.integer(budget_year)),
    raw_amount  = if_else(!is.na(amount_num), amount_num, estim_num),
    raw_unit    = if_else(!is.na(amount_num), unit, amount),
    amount_millions = case_when(
      raw_unit == "thousands" ~ raw_amount / 1000,
      raw_unit == "millions"  ~ raw_amount,
      TRUE ~ raw_amount
    ),
    group = group_map[program]
  ) %>%
  filter(!is.na(amount_millions)) %>%
  select(group, year = fiscal_year, amount_millions) %>%
  group_by(group, year) %>%
  summarize(amount_millions = max(amount_millions, na.rm = TRUE), .groups = "drop")

# Fetch CPI-U from FRED (with fallback)
cpi_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2025-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2026-02-11&revision_date=2026-02-11&nd=1947-01-01"

cpi_raw <- tryCatch(read_csv(cpi_url, show_col_types = FALSE), error = function(e) NULL)

if (!is.null(cpi_raw)) {
  cpi <- cpi_raw %>%
    rename(date = 1, cpi = 2) %>%
    mutate(year = as.integer(format(as.Date(date), "%Y"))) %>%
    select(year, cpi)
} else {
  cpi <- tibble(
    year = 1963:2023,
    cpi = c(30.6, 31.0, 31.5, 32.4, 33.4, 34.8, 36.7, 38.8, 40.5, 41.8,
            44.4, 49.3, 53.8, 56.9, 60.6, 65.2, 72.6, 82.4, 90.9, 96.5,
            99.6, 103.9, 107.6, 109.6, 113.6, 118.3, 124.0, 130.7, 136.2,
            140.3, 144.5, 148.2, 152.4, 156.9, 160.5, 163.0, 166.6, 172.2,
            177.1, 179.9, 184.0, 188.9, 195.3, 201.6, 207.3, 215.3, 214.5,
            218.1, 224.9, 229.6, 233.0, 236.7, 237.0, 240.0, 245.1, 251.1,
            255.7, 258.8, 271.0, 292.7, 304.7)
  )
}

cpi_2023 <- cpi %>% filter(year == 2023) %>% pull(cpi)
cpi <- cpi %>% mutate(deflator = cpi_2023 / cpi)

group_levels <- c("CMHC Funding", "Mental Health Block Grant", "CCBHC Program Funding")

mh <- mh %>%
  left_join(cpi, by = "year") %>%
  mutate(
    amount_real_2023 = amount_millions * deflator,
    group = factor(group, levels = group_levels)
  )

program_colors <- c(
  "CMHC Funding"               = "#1b7837",
  "Mental Health Block Grant"  = "#ef8a62",
  "CCBHC Program Funding"      = "#b2182b"
)

p <- ggplot(mh, aes(x = year, y = amount_real_2023, color = group)) +
  # Shaded eras
  annotate("rect", xmin = 1963, xmax = 1981, ymin = -Inf, ymax = Inf,
           fill = "#2166ac", alpha = 0.05) +
  annotate("rect", xmin = 2017, xmax = 2025, ymin = -Inf, ymax = Inf,
           fill = "#b2182b", alpha = 0.05) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = program_colors) +
  scale_x_continuous(breaks = seq(1965, 2025, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  annotate("text", x = 1972, y = max(mh$amount_real_2023, na.rm = TRUE) * 0.95,
           label = "CMHC Era", fontface = "italic", size = 3.5, color = "#2166ac") +
  annotate("text", x = 2021, y = max(mh$amount_real_2023, na.rm = TRUE) * 0.95,
           label = "CCBHC Era", fontface = "italic", size = 3.5, color = "#b2182b") +
  labs(
    title = "Federal Mental Health Budgets, 1965-2023",
    subtitle = "Inflation-adjusted to 2023 dollars (CPI-U)",
    x = "Fiscal Year",
    y = "Budget (Millions, 2023 $)",
    color = "Program"
  ) +
  theme_paper(base_size = 13) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

save_fig(p, "fig01_federal_budgets.pdf", width = 11, height = 7)

# Save underlying data
save_csv(mh %>% select(group, year, amount_millions, amount_real_2023),
         "fig01_federal_budgets.csv")
