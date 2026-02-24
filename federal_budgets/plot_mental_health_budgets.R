#!/usr/bin/env Rscript
# Plot federal mental health budgets over time, inflation-adjusted to 2023 dollars
# Four series: ADAMHA Mental Health, ADAMHA Block Grant,
#              Mental Health Block Grant, Mental Health

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# ==========================================
# 1. Load budget data
# ==========================================

budgets <- read_csv("federal_budgets/mental_health_budgets.csv", show_col_types = FALSE)

# The four programs of interest
mh_programs <- c("Early CMHC", "ADAMHA Mental Health", "ADMS Block Grant",
                 "Mental Health Block Grant", "Mental Health")

mh <- budgets %>%
  filter(program %in% mh_programs) %>%
  mutate(
    # Standardize to millions
    amount_millions = case_when(
      unit == "thousands" ~ amount / 1000,
      unit == "millions"  ~ amount,
      TRUE ~ amount
    )
  ) %>%
  select(program, year, amount_millions) %>%
  # Take one observation per program-year (some duplicates from different budget docs)
  group_by(program, year) %>%
  summarize(amount_millions = mean(amount_millions, na.rm = TRUE), .groups = "drop")

cat("Budget series loaded:\n")
mh %>%
  group_by(program) %>%
  summarize(years = paste0(min(year), "-", max(year)), n = n()) %>%
  print()

# ==========================================
# 2. Fetch CPI-U (All Urban Consumers) from FRED
# ==========================================

cat("\nFetching CPI-U from FRED...\n")

# FRED provides annual average CPI-U without authentication
cpi_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2025-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2026-02-11&revision_date=2026-02-11&nd=1947-01-01"

cpi_raw <- tryCatch(
  read_csv(cpi_url, show_col_types = FALSE),
  error = function(e) {
    cat("FRED download failed. Using hardcoded CPI-U values.\n")
    NULL
  }
)

if (!is.null(cpi_raw)) {
  cpi <- cpi_raw %>%
    rename(date = 1, cpi = 2) %>%
    mutate(year = as.integer(format(as.Date(date), "%Y"))) %>%
    select(year, cpi)
  cat("CPI-U downloaded:", nrow(cpi), "years\n")
} else {
  # Fallback: hardcoded CPI-U annual averages (BLS, 1982-84=100)
  cpi <- tibble(
    year = 1973:2023,
    cpi = c(
      44.4, 49.3, 53.8, 56.9, 60.6, 65.2, 72.6, 82.4, 90.9, 96.5,  # 1973-1982
      99.6, 103.9, 107.6, 109.6, 113.6, 118.3, 124.0, 130.7, 136.2, # 1983-1991
      140.3, 144.5, 148.2, 152.4, 156.9, 160.5, 163.0, 166.6, 172.2,# 1992-2000
      177.1, 179.9, 184.0, 188.9, 195.3, 201.6, 207.3, 215.3, 214.5,# 2001-2009
      218.1, 224.9, 229.6, 233.0, 236.7, 237.0, 240.0, 245.1, 251.1,# 2010-2018
      255.7, 258.8, 271.0, 292.7, 304.7                               # 2019-2023
    )
  )
  cat("Using hardcoded CPI-U values\n")
}

# 2023 CPI for deflation
cpi_2023 <- cpi %>% filter(year == 2023) %>% pull(cpi)
cat("CPI-U 2023:", cpi_2023, "\n")

cpi <- cpi %>%
  mutate(deflator = cpi_2023 / cpi)

# ==========================================
# 3. Inflation-adjust budgets
# ==========================================

mh <- mh %>%
  left_join(cpi, by = "year") %>%
  mutate(amount_real_2023 = amount_millions * deflator)

# cat("\nInflation-adjusted budgets (2023 $, millions):\n")
# mh %>%
#   select(program, year, amount_millions, amount_real_2023) %>%
#   print(n = 50)

# ==========================================
# 4. Plot
# ==========================================

# Order programs chronologically for legend
mh <- mh %>%
  mutate(program = factor(program, levels = mh_programs))

# Color palette: distinct colors for 4 series
program_colors <- c(
  "Early CMHC"                = "#1b7837",
  "ADAMHA Mental Health"      = "#2166ac",
  "ADMS Block Grant"          = "#67a9cf",
  "Mental Health Block Grant" = "#ef8a62",
  "Mental Health"             = "#b2182b"
)

p <- ggplot(mh, aes(x = year, y = amount_real_2023, color = program)) +
  geom_line(aes(linetype = program == "ADMS Block Grant"), linewidth = 1.2) +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"), guide = "none") +
  geom_point(size = 2.5) +
  scale_color_manual(values = program_colors) +
  scale_x_continuous(breaks = seq(1965, 2025, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Federal Mental Health Budgets, 1973\u20132023",
    subtitle = "Inflation-adjusted to 2023 dollars (CPI-U, All Urban Consumers)",
    x = "Fiscal Year",
    y = "Budget (Millions, 2023 $)",
    color = "Program"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("federal_budgets/mental_health_budgets_real.png", p, width = 11, height = 7, dpi = 300)
cat("\nPlot saved to: federal_budgets/mental_health_budgets_real.png\n")

# Also save nominal version
p_nom <- ggplot(mh, aes(x = year, y = amount_millions, color = program)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = program_colors) +
  scale_x_continuous(breaks = seq(1975, 2025, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Federal Mental Health Budgets, 1973\u20132023 (Nominal)",
    x = "Fiscal Year",
    y = "Budget (Millions, Nominal $)",
    color = "Program"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("federal_budgets/mental_health_budgets_nominal.png", p_nom, width = 11, height = 7, dpi = 300)
cat("Nominal plot saved to: federal_budgets/mental_health_budgets_nominal.png\n")

# Save the underlying data
write_csv(mh %>% select(program, year, amount_millions, amount_real_2023),
          "federal_budgets/mental_health_budgets_adjusted.csv")
cat("Adjusted data saved to: federal_budgets/mental_health_budgets_adjusted.csv\n")
