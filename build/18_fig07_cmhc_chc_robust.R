#!/usr/bin/env Rscript
# ============================================================================
# 18_fig07_cmhc_chc_robust.R — CMHC Robustness to CHC Controls
# Adapted from: cmhc_event_study.R Step 12c
# ============================================================================
log_section("18: Figure 7 — CHC Robustness")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Replace NA CHC funding with 0
panel <- panel %>% mutate(pcrfund_chc = replace_na(pcrfund_chc, 0))

# --- Baseline ---
m_base <- feols(
  amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
  | fips + year^Durb + year^stfips,
  data = panel, weights = ~popwt_ad, cluster = ~fips
)
coef_base <- extract_es_coefs(m_base, max_et = CMHC_PLOT_MAX)
coef_base$model <- "Baseline"

# --- Panel A: + CHC per-capita funding control ---
m_chcfund <- feols(
  amr_ad ~ i(event_time_binned, ref = -1) + pcrfund_chc + D_tot_act_md_t + H_bpc
  | fips + year^Durb + year^stfips,
  data = panel, weights = ~popwt_ad, cluster = ~fips
)
coef_chcfund <- extract_es_coefs(m_chcfund, max_et = CMHC_PLOT_MAX)
coef_chcfund$model <- "+ CHC Funding Control"

# --- Panel B: Horse race with CHC event-time indicators ---
m_horse <- feols(
  amr_ad ~ i(event_time_binned, ref = -1) + i(chc_event_time_binned, ref = -1)
    + D_tot_act_md_t + H_bpc
  | fips + year^Durb + year^stfips,
  data = panel, weights = ~popwt_ad, cluster = ~fips
)
coef_horse <- extract_es_coefs(m_horse, max_et = CMHC_PLOT_MAX)
coef_horse$model <- "Horse Race (+ CHC Event Time)"

# --- Panel A plot ---
coef_a <- bind_rows(coef_base, coef_chcfund)

p_a <- ggplot(coef_a, aes(x = event_time, y = coefficient, color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.1, color = NA) +
  geom_point(size = 2, position = position_dodge(0.4)) +
  geom_line(linewidth = 0.8, position = position_dodge(0.4)) +
  scale_color_manual(values = c("Baseline" = "steelblue", "+ CHC Funding Control" = "darkorange")) +
  scale_fill_manual(values = c("Baseline" = "steelblue", "+ CHC Funding Control" = "darkorange")) +
  labs(title = "A. Controlling for CHC Per-Capita Funding",
       x = "Years Relative to CMHC Opening",
       y = "Change in Deaths per 100,000",
       color = NULL, fill = NULL) +
  theme_paper()

# --- Panel B plot ---
coef_b <- bind_rows(coef_base, coef_horse)

p_b <- ggplot(coef_b, aes(x = event_time, y = coefficient, color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.1, color = NA) +
  geom_point(size = 2, position = position_dodge(0.4)) +
  geom_line(linewidth = 0.8, position = position_dodge(0.4)) +
  scale_color_manual(values = c("Baseline" = "steelblue",
                                 "Horse Race (+ CHC Event Time)" = "forestgreen")) +
  scale_fill_manual(values = c("Baseline" = "steelblue",
                                "Horse Race (+ CHC Event Time)" = "forestgreen")) +
  labs(title = "B. Horse Race with CHC Event-Time Indicators",
       x = "Years Relative to CMHC Opening",
       y = "Change in Deaths per 100,000",
       color = NULL, fill = NULL) +
  theme_paper()

p <- p_a / p_b +
  plot_annotation(
    title = "CMHC Mortality Effects Are Not Driven by CHC Rollout",
    subtitle = "AMR, Ages 20-49. CMHC coefficients shown in all panels.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "gray30")
    )
  )

save_fig(p, "fig07_cmhc_chc_robust.pdf", width = 10, height = 11)
save_csv(coef_a, "fig07_panel_a_coefs.csv")
save_csv(coef_b, "fig07_panel_b_coefs.csv")







# --- Combined Plot ---
coef_all <- bind_rows(coef_base, coef_chcfund, coef_horse)

p_c <- ggplot(coef_all, aes(x = event_time, y = coefficient, color = model, fill = model)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.1, color = NA) +
      geom_point(size = 2, position = position_dodge(0.4)) +
      geom_line(linewidth = 0.8, position = position_dodge(0.4)) +
      scale_color_manual(values = c("Baseline" = "steelblue", 
        "+ CHC Funding Control" = "darkorange", 
        "Horse Race (+ CHC Event Time)" = "forestgreen")) +
  scale_fill_manual(values = c("Baseline" = "steelblue", 
        "+ CHC Funding Control" = "darkorange", 
        "Horse Race (+ CHC Event Time)" = "forestgreen")) +
  labs(title = "A. Controlling for CHC Per-Capita Funding",
       x = "Years Relative to CMHC Opening",
       y = "Change in Deaths per 100,000",
       color = NULL, fill = NULL) +
  theme_paper()


save_fig(p_c, "fig07_cmhc_chc_robust.pdf", width = 10, height = 6)

