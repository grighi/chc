#!/usr/bin/env Rscript
# ============================================================================
# 19c_table_poisson.R — Poisson PPML vs TWFE Event Study Comparison
#
#   Compares OLS (age-adjusted AMR) with Poisson PPML (death counts with
#   log-population offset) using the same event-study specification.
#
#   Panels:
#     A: All-cause + cause decomposition (causes 2-8)
#     B: External cause decomposition
#     C.1: White external causes
#     C.2: Nonwhite external causes
#
#   Reads: nber_deaths_pop.rds, panel_nber_cause.fst (saved by 19a)
#   Outputs: table_poisson_{a,b,c}.tex
#
#   Adapted from: server_cmhc/aer_data/tmp_15ab_poisson.R
# ============================================================================
log_section("19c: Poisson PPML Comparison")

# ============================================================================
# 1. LOAD DATA
# ============================================================================
cat("Loading data ...\n")

panel <- as.data.table(fst::read_fst(file.path(DATA_DIR, "panel_nber_cause.fst")))
cat(sprintf("  Panel: %s rows, %d counties\n",
            format(nrow(panel), big.mark = ","), uniqueN(panel$fips)))

dat <- readRDS(file.path(DATA_DIR, "nber_deaths_pop.rds"))
deaths_by_bin <- as.data.table(dat$deaths_by_bin)
deaths_ext    <- as.data.table(dat$deaths_ext)
deaths_race   <- as.data.table(dat$deaths_race)
pop_by_bin    <- as.data.table(dat$pop_by_bin)
rm(dat)

cat(sprintf("  Deaths (by cause): %s rows\n", format(nrow(deaths_by_bin), big.mark = ",")))
cat(sprintf("  Deaths (ext):      %s rows\n", format(nrow(deaths_ext), big.mark = ",")))
cat(sprintf("  Deaths (race):     %s rows\n", format(nrow(deaths_race), big.mark = ",")))
cat(sprintf("  Pop:               %s rows\n", format(nrow(pop_by_bin), big.mark = ",")))

# ============================================================================
# 2. BUILD STACKED POISSON PANEL
# ============================================================================
cat("\nBuilding stacked (fips, year, age_bin) Poisson panel ...\n")

treat_vars <- c("fips", "year", "stfips", "Durb",
                "event_time_binned", "D_tot_act_md_t", "H_bpc", "popwt_ad")
treat <- panel[, ..treat_vars]

W_1960 <- data.table(
  nber_bin = c("15-24", "25-34", "35-44", "45-54"),
  w_1960   = c(0.157452, 0.332723, 0.351184, 0.158642)
)

panel_keys <- unique(panel[, .(fips, year)])

grid <- CJ(fips = unique(panel_keys$fips),
           year = unique(panel_keys$year),
           age_bin = c("15-24", "25-34", "35-44", "45-54"))
grid <- merge(grid, panel_keys, by = c("fips", "year"))

build_cause_panel <- function(cause_deaths, grid_dt, pop_dt, w_dt, treat_dt) {
  dt <- merge(grid_dt, cause_deaths,
              by = c("fips", "year", "age_bin"), all.x = TRUE)
  dt[is.na(deaths), deaths := 0]
  dt <- merge(dt, pop_dt, by.x = c("fips", "year", "age_bin"),
              by.y = c("fips", "year", "nber_bin"), all.x = TRUE)
  dt <- merge(dt, w_dt, by.x = "age_bin", by.y = "nber_bin", all.x = TRUE)
  dt <- merge(dt, treat_dt, by = c("fips", "year"), all.x = TRUE)
  dt <- dt[!is.na(pop_bin) & pop_bin > 0]
  dt[, log_pop := log(pop_bin)]
  dt
}

# All-cause
deaths_allcause <- deaths_by_bin[cause == 0L, .(fips, year, age_bin, deaths)]
poisson_base <- build_cause_panel(deaths_allcause, grid, pop_by_bin, W_1960, treat)

cat(sprintf("  Stacked panel: %s rows, %d counties, %d age bins\n",
            format(nrow(poisson_base), big.mark = ","),
            uniqueN(poisson_base$fips),
            uniqueN(poisson_base$age_bin)))

# ============================================================================
# 3. BUILD CAUSE-SPECIFIC POISSON PANELS
# ============================================================================
cat("Building cause-specific death panels ...\n")

cause_panels <- list()
for (cc in c(2:8)) {
  d <- deaths_by_bin[cause == cc, .(fips, year, age_bin, deaths)]
  cause_panels[[as.character(cc)]] <- build_cause_panel(d, grid, pop_by_bin, W_1960, treat)
}

ext_panels <- list()
for (ec in unique(deaths_ext$ext_cause)) {
  d <- deaths_ext[ext_cause == ec, .(fips, year, age_bin, deaths)]
  ext_panels[[ec]] <- build_cause_panel(d, grid, pop_by_bin, W_1960, treat)
}

race_panels <- list()
for (rc in unique(deaths_race$race_cause)) {
  d <- deaths_race[race_cause == rc, .(fips, year, age_bin, deaths)]
  race_panels[[rc]] <- build_cause_panel(d, grid, pop_by_bin, W_1960, treat)
}

cat(sprintf("  Built %d cause, %d ext, %d race panels\n",
            length(cause_panels), length(ext_panels), length(race_panels)))

# ============================================================================
# 4. ESTIMATION FUNCTIONS
# ============================================================================

time_bins <- list(
  "[-6,-1]" = -6:-1,
  "[0,3]"   = 0:3,
  "[4,7]"   = 4:7,
  "[8,13]"  = 8:13
)

pois_extract_es_coefs <- function(mod, max_et = 13) {
  cf <- coef(mod)
  se <- sqrt(diag(vcov(mod)))
  nms <- names(cf)
  idx <- grepl("^event_time_binned::", nms)
  et_vals <- as.integer(sub("^event_time_binned::(-?[0-9]+)$", "\\1", nms[idx]))
  df <- data.frame(event_time = et_vals, coefficient = unname(cf[idx]),
                   se = unname(se[idx]), stringsAsFactors = FALSE)
  df <- rbind(df, data.frame(event_time = -1L, coefficient = 0, se = 0))
  df <- df[order(df$event_time), ]
  df <- df[df$event_time <= max_et, ]
  rownames(df) <- NULL
  df
}

compute_bins <- function(mod) {
  coef_df <- pois_extract_es_coefs(mod, max_et = 13)
  bin_vals <- sapply(time_bins, function(et_range) {
    rows <- coef_df[coef_df$event_time %in% et_range, ]
    if (nrow(rows) == 0) return(list(mean = NA, se = NA, p = NA))
    avg    <- mean(rows$coefficient)
    avg_se <- sqrt(mean(rows$se^2))
    t_stat <- avg / avg_se
    p_val  <- 2 * pnorm(-abs(t_stat))
    list(mean = avg, se = avg_se, p = p_val)
  }, simplify = FALSE)
  bin_vals
}

star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

run_twfe <- function(dv, data = panel) {
  fml <- as.formula(sprintf(
    "%s ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year + year^Durb + year^stfips + fips[year]",
    dv
  ))
  mod <- feols(fml, data = data, weights = ~popwt_ad, cluster = ~fips)
  compute_bins(mod)
}

run_poisson <- function(data) {
  mod <- fepois(
    deaths ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc |
      fips + year + year^Durb + year^stfips + age_bin,
    data       = data,
    offset     = ~log_pop,
    weights    = ~w_1960,
    cluster    = ~fips,
    fixef.rm   = "singleton"
  )
  compute_bins(mod)
}

# ============================================================================
# 5. RUN COMPARISONS
# ============================================================================

# Panel A: All-Cause + Cause Decomposition
cat("\n  Panel A: All-Cause + Cause Decomposition\n")

causes_a <- list(
  list(dv = "amr_ad_nber",   label = "All Causes",      pois_data = poisson_base),
  list(dv = "amr_ad_2_nber", label = "Cardiovascular",   pois_data = cause_panels[["2"]]),
  list(dv = "amr_ad_3_nber", label = "Cerebrovascular",  pois_data = cause_panels[["3"]]),
  list(dv = "amr_ad_4_nber", label = "Cancer",           pois_data = cause_panels[["4"]]),
  list(dv = "amr_ad_5_nber", label = "Infectious",       pois_data = cause_panels[["5"]]),
  list(dv = "amr_ad_6_nber", label = "Diabetes",         pois_data = cause_panels[["6"]]),
  list(dv = "amr_ad_7_nber", label = "Accidents",        pois_data = cause_panels[["7"]]),
  list(dv = "amr_ad_8_nber", label = "Other",            pois_data = cause_panels[["8"]])
)

twfe_a <- list()
pois_a <- list()
for (ca in causes_a) {
  cat(sprintf("  Estimating: %s ...\n", ca$label))
  twfe_a[[ca$dv]] <- tryCatch(run_twfe(ca$dv), error = function(e) {
    cat(sprintf("    TWFE error: %s\n", e$message)); NULL
  })
  pois_a[[ca$dv]] <- tryCatch(run_poisson(ca$pois_data), error = function(e) {
    cat(sprintf("    Poisson error: %s\n", e$message)); NULL
  })
}

# Panel B: External Cause Decomposition
cat("\n  Panel B: External Cause Decomposition\n")

causes_b <- list(
  list(dv = "amr_ad_external_nber", label = "All External", ext = "external"),
  list(dv = "amr_ad_suicide_nber",  label = "Suicide",      ext = "suicide"),
  list(dv = "amr_ad_homicide_nber", label = "Homicide",     ext = "homicide"),
  list(dv = "amr_ad_poison_nber",   label = "Poisoning",    ext = "poison"),
  list(dv = "amr_ad_alcohol_nber",  label = "Alcohol",      ext = "alcohol")
)

twfe_b <- list()
pois_b <- list()
for (cb in causes_b) {
  cat(sprintf("  Estimating: %s ...\n", cb$label))
  twfe_b[[cb$dv]] <- tryCatch(run_twfe(cb$dv), error = function(e) {
    cat(sprintf("    TWFE error: %s\n", e$message)); NULL
  })
  pois_b[[cb$dv]] <- tryCatch(run_poisson(ext_panels[[cb$ext]]), error = function(e) {
    cat(sprintf("    Poisson error: %s\n", e$message)); NULL
  })
}

# Panel C.1: White
cat("\n  Panel C.1: White External Causes\n")

causes_c1 <- list(
  list(dv = "amr_ad_w_external_nber", label = "External",  rc = "w_external"),
  list(dv = "amr_ad_w_suicide_nber",  label = "Suicide",   rc = "w_suicide"),
  list(dv = "amr_ad_w_homicide_nber", label = "Homicide",  rc = "w_homicide"),
  list(dv = "amr_ad_w_poison_nber",   label = "Poisoning", rc = "w_poison"),
  list(dv = "amr_ad_w_alcohol_nber",  label = "Alcohol",   rc = "w_alcohol")
)

twfe_c1 <- list()
pois_c1 <- list()
for (cc in causes_c1) {
  cat(sprintf("  Estimating: %s ...\n", cc$label))
  twfe_c1[[cc$dv]] <- tryCatch(run_twfe(cc$dv), error = function(e) {
    cat(sprintf("    TWFE error: %s\n", e$message)); NULL
  })
  pois_c1[[cc$dv]] <- tryCatch(run_poisson(race_panels[[cc$rc]]), error = function(e) {
    cat(sprintf("    Poisson error: %s\n", e$message)); NULL
  })
}

# Panel C.2: Nonwhite
cat("\n  Panel C.2: Nonwhite External Causes\n")

causes_c2 <- list(
  list(dv = "amr_ad_nw_external_nber", label = "External",  rc = "nw_external"),
  list(dv = "amr_ad_nw_suicide_nber",  label = "Suicide",   rc = "nw_suicide"),
  list(dv = "amr_ad_nw_homicide_nber", label = "Homicide",  rc = "nw_homicide"),
  list(dv = "amr_ad_nw_poison_nber",   label = "Poisoning", rc = "nw_poison"),
  list(dv = "amr_ad_nw_alcohol_nber",  label = "Alcohol",   rc = "nw_alcohol")
)

twfe_c2 <- list()
pois_c2 <- list()
for (cc in causes_c2) {
  cat(sprintf("  Estimating: %s ...\n", cc$label))
  twfe_c2[[cc$dv]] <- tryCatch(run_twfe(cc$dv), error = function(e) {
    cat(sprintf("    TWFE error: %s\n", e$message)); NULL
  })
  pois_c2[[cc$dv]] <- tryCatch(run_poisson(race_panels[[cc$rc]]), error = function(e) {
    cat(sprintf("    Poisson error: %s\n", e$message)); NULL
  })
}

# ============================================================================
# 6. GENERATE LATEX TABLES
# ============================================================================
cat("\nGenerating LaTeX tables ...\n")

make_comparison_tex <- function(twfe_results, pois_results,
                                 cause_list, caption, label) {
  note_pois <- paste(
    "TWFE rows report OLS coefficients on age-adjusted mortality rates (deaths per 100,000).",
    "Poisson rows report semi-elasticities from Poisson PPML with log-population offset",
    "(approximate \\% change $\\approx 100 \\times$ coefficient).",
    "Both use event-study bins relative to CMHC opening.",
    "All specifications include county, year, urban$\\times$year, state$\\times$year,",
    "and county-specific trend FE; controls for physician supply and hospital beds.",
    "Population-weighted. Clustered at county level.",
    "* $p<0.10$, ** $p<0.05$, *** $p<0.01$."
  )

  tex <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{tab:%s}", label),
    "\\footnotesize",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & $[-6,-1]$ & $[0,3]$ & $[4,7]$ & $[8,13]$ \\\\",
    "\\midrule"
  )

  for (i in seq_along(cause_list)) {
    ca <- cause_list[[i]]
    dv <- ca$dv
    lbl <- ca$label

    # TWFE row
    tw <- twfe_results[[dv]]
    if (!is.null(tw)) {
      coef_vals <- se_vals <- character(4)
      for (j in seq_along(names(time_bins))) {
        bn <- names(time_bins)[j]
        bv <- tw[[bn]]
        coef_vals[j] <- sprintf("%.3f%s", bv$mean, star(bv$p))
        se_vals[j]   <- sprintf("(%.3f)", bv$se)
      }
      tex <- c(tex,
        sprintf("%s (TWFE) & %s \\\\", lbl, paste(coef_vals, collapse = " & ")),
        sprintf(" & %s \\\\", paste(se_vals, collapse = " & "))
      )
    }

    # Poisson row
    po <- pois_results[[dv]]
    if (!is.null(po)) {
      coef_vals <- se_vals <- character(4)
      for (j in seq_along(names(time_bins))) {
        bn <- names(time_bins)[j]
        bv <- po[[bn]]
        coef_vals[j] <- sprintf("%.4f%s", bv$mean, star(bv$p))
        se_vals[j]   <- sprintf("(%.4f)", bv$se)
      }
      tex <- c(tex,
        sprintf("%s (Poisson) & %s \\\\", lbl, paste(coef_vals, collapse = " & ")),
        sprintf(" & %s \\\\", paste(se_vals, collapse = " & "))
      )
    }

    if (i < length(cause_list)) {
      tex <- c(tex, "\\addlinespace")
    }
  }

  tex <- c(tex,
    "\\bottomrule",
    "\\end{tabular}",
    sprintf("\\begin{minipage}{\\textwidth}"),
    sprintf("\\vspace{0.5em}\\footnotesize \\textit{Notes:} %s", note_pois),
    "\\end{minipage}",
    "\\end{table}"
  )
  tex
}

# Panel A
tex_a <- make_comparison_tex(
  twfe_a, pois_a, causes_a,
  "TWFE vs Poisson PPML: Cause Decomposition (Ages 20--49)",
  "poisson_cause"
)
writeLines(tex_a, file.path(TABLE_DIR, "table_poisson_cause.tex"))
cat(sprintf("  Saved %s\n", file.path(TABLE_DIR, "table_poisson_cause.tex")))

# Panel B
tex_b <- make_comparison_tex(
  twfe_b, pois_b, causes_b,
  "TWFE vs Poisson PPML: External Causes (Ages 20--49)",
  "poisson_ext"
)
writeLines(tex_b, file.path(TABLE_DIR, "table_poisson_ext.tex"))
cat(sprintf("  Saved %s\n", file.path(TABLE_DIR, "table_poisson_ext.tex")))

# Panel C.1 + C.2 combined
tex_c <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{TWFE vs Poisson PPML: Race-Stratified External Causes (Ages 20--49)}",
  "\\label{tab:poisson_race}",
  "\\footnotesize"
)

# C.1 subtable
note_race <- paste(
  "See Table~\\ref{tab:poisson_cause} notes.",
  "Panels show white and nonwhite populations separately."
)

tex_c <- c(tex_c,
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "\\multicolumn{5}{l}{\\textit{Panel C.1: White}} \\\\",
  "\\midrule",
  " & $[-6,-1]$ & $[0,3]$ & $[4,7]$ & $[8,13]$ \\\\",
  "\\midrule"
)

for (i in seq_along(causes_c1)) {
  ca <- causes_c1[[i]]
  dv <- ca$dv; lbl <- ca$label

  tw <- twfe_c1[[dv]]
  if (!is.null(tw)) {
    cv <- sv <- character(4)
    for (j in 1:4) { bn <- names(time_bins)[j]; bv <- tw[[bn]]
      cv[j] <- sprintf("%.3f%s", bv$mean, star(bv$p))
      sv[j] <- sprintf("(%.3f)", bv$se)
    }
    tex_c <- c(tex_c,
      sprintf("%s (TWFE) & %s \\\\", lbl, paste(cv, collapse = " & ")),
      sprintf(" & %s \\\\", paste(sv, collapse = " & ")))
  }
  po <- pois_c1[[dv]]
  if (!is.null(po)) {
    cv <- sv <- character(4)
    for (j in 1:4) { bn <- names(time_bins)[j]; bv <- po[[bn]]
      cv[j] <- sprintf("%.4f%s", bv$mean, star(bv$p))
      sv[j] <- sprintf("(%.4f)", bv$se)
    }
    tex_c <- c(tex_c,
      sprintf("%s (Poisson) & %s \\\\", lbl, paste(cv, collapse = " & ")),
      sprintf(" & %s \\\\", paste(sv, collapse = " & ")))
  }
  if (i < length(causes_c1)) tex_c <- c(tex_c, "\\addlinespace")
}

tex_c <- c(tex_c,
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel C.2: Nonwhite}} \\\\",
  "\\midrule"
)

for (i in seq_along(causes_c2)) {
  ca <- causes_c2[[i]]
  dv <- ca$dv; lbl <- ca$label

  tw <- twfe_c2[[dv]]
  if (!is.null(tw)) {
    cv <- sv <- character(4)
    for (j in 1:4) { bn <- names(time_bins)[j]; bv <- tw[[bn]]
      cv[j] <- sprintf("%.3f%s", bv$mean, star(bv$p))
      sv[j] <- sprintf("(%.3f)", bv$se)
    }
    tex_c <- c(tex_c,
      sprintf("%s (TWFE) & %s \\\\", lbl, paste(cv, collapse = " & ")),
      sprintf(" & %s \\\\", paste(sv, collapse = " & ")))
  }
  po <- pois_c2[[dv]]
  if (!is.null(po)) {
    cv <- sv <- character(4)
    for (j in 1:4) { bn <- names(time_bins)[j]; bv <- po[[bn]]
      cv[j] <- sprintf("%.4f%s", bv$mean, star(bv$p))
      sv[j] <- sprintf("(%.4f)", bv$se)
    }
    tex_c <- c(tex_c,
      sprintf("%s (Poisson) & %s \\\\", lbl, paste(cv, collapse = " & ")),
      sprintf(" & %s \\\\", paste(sv, collapse = " & ")))
  }
  if (i < length(causes_c2)) tex_c <- c(tex_c, "\\addlinespace")
}

tex_c <- c(tex_c,
  "\\bottomrule",
  "\\end{tabular}",
  sprintf("\\begin{minipage}{\\textwidth}"),
  sprintf("\\vspace{0.5em}\\footnotesize \\textit{Notes:} %s", note_race),
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tex_c, file.path(TABLE_DIR, "table_poisson_race.tex"))
cat(sprintf("  Saved %s\n", file.path(TABLE_DIR, "table_poisson_race.tex")))

# Save CSV
pois_csv <- data.table()
all_panels <- list(
  A = list(twfe = twfe_a, pois = pois_a, causes = causes_a),
  B = list(twfe = twfe_b, pois = pois_b, causes = causes_b),
  C1 = list(twfe = twfe_c1, pois = pois_c1, causes = causes_c1),
  C2 = list(twfe = twfe_c2, pois = pois_c2, causes = causes_c2)
)

for (pn in names(all_panels)) {
  ap <- all_panels[[pn]]
  for (ca in ap$causes) {
    dv <- ca$dv; lbl <- ca$label
    for (method in c("twfe", "pois")) {
      res <- ap[[method]][[dv]]
      if (is.null(res)) next
      for (bn in names(time_bins)) {
        bv <- res[[bn]]
        pois_csv <- rbind(pois_csv, data.table(
          panel = pn, outcome = dv, label = lbl, method = method,
          bin = bn, mean_coef = bv$mean, se = bv$se, p = bv$p
        ))
      }
    }
  }
}
save_csv(pois_csv, "table_poisson_comparison.csv")
