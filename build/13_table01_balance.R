#!/usr/bin/env Rscript
# ============================================================================
# 13_table01_balance.R — Balance Table: CMHC vs Non-CMHC Counties (1960)
# ============================================================================
log_section("13: Table 1 — Balance Table")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Cross-section at 1960
xsec <- panel %>%
  filter(year == 1960) %>%
  mutate(treated = ifelse(cmhc_treated == 1, "CMHC", "Control"))

# Variables for balance table
bal_vars <- c("copop", "_60pcturban", "_60pctrurf", "_60pctnonwhit",
              "_pct59inclt3k", "_60medschlmt24", "amr", "amr_ad", "amr_eld",
              "H_bpc")

# Rename for display
var_labels <- c(
  "copop"           = "Population (1960)",
  "_60pcturban"     = "\\% Urban",
  "_60pctrurf"      = "\\% Rural Farm",
  "_60pctnonwhit"   = "\\% Nonwhite",
  "_pct59inclt3k"   = "\\% Income $<$ \\$3k",
  "_60medschlmt24"  = "Median Schooling $>$ 24",
  "amr"             = "AMR, All Ages",
  "amr_ad"          = "AMR, Ages 20-49",
  "amr_eld"         = "AMR, Ages 50+",
  "H_bpc"           = "Hospital Beds per Capita"
)

# Compute balance statistics
bal_rows <- lapply(bal_vars, function(v) {
  x_t <- xsec[[v]][xsec$treated == "CMHC"]
  x_c <- xsec[[v]][xsec$treated == "Control"]
  x_t <- x_t[!is.na(x_t)]
  x_c <- x_c[!is.na(x_c)]

  tt <- tryCatch(t.test(x_t, x_c), error = function(e) NULL)

  data.frame(
    variable   = var_labels[v],
    mean_treat = mean(x_t, na.rm = TRUE),
    mean_ctrl  = mean(x_c, na.rm = TRUE),
    diff       = if (!is.null(tt)) tt$estimate[1] - tt$estimate[2] else NA,
    se         = if (!is.null(tt)) tt$stderr else NA,
    pval       = if (!is.null(tt)) tt$p.value else NA,
    n_treat    = length(x_t),
    n_ctrl     = length(x_c),
    stringsAsFactors = FALSE
  )
})

bal_df <- do.call(rbind, bal_rows)

# Save CSV for fill_tables.py
save_csv(bal_df, "table01_balance.csv")

# Generate LaTeX table shell
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\label{tab:balance}",
  "\\centering",
  "\\caption{Balance Table: CMHC Counties vs.\\ Non-CMHC Counties, 1960 Characteristics}",
  "\\label{tab:balance}",
  "\\small",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & Mean & Mean & & & \\\\",
  "Variable & (CMHC) & (Control) & Difference & SE & $p$-value \\\\",
  "\\midrule",
  "%%TABLE_BODY%%",
  "\\midrule",
  sprintf("$N$ & %d & %d & & & \\\\",
          bal_df$n_treat[1], bal_df$n_ctrl[1]),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex_lines, file.path(TABLE_DIR, "table01_balance_shell.tex"))
cat(sprintf("  Shell saved: %s\n", file.path(TABLE_DIR, "table01_balance_shell.tex")))
