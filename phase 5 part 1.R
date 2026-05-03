# ============================================================
# Phase 5 - Part 1: Stress Scenario Analysis & Portfolio Impact
# ============================================================
library(tidyverse)
library(ggplot2)

# 1. Solvency II Mortality Shock Application
# Shocks apply ONLY to future years (>2024), leaving historical rates untouched
apply_mortality_shock <- function(mhat, shock_factor, shock_type = c("longevity", "mortality"), hist_cutoff = 2024) {
  shock_type <- match.arg(shock_type)
  mhat_shocked <- mhat
  years <- as.numeric(colnames(mhat))
  future_mask <- years > hist_cutoff
  
  if (shock_type == "longevity") {
    # Longevity shock: lower mortality → people live longer → higher liabilities
    mhat_shocked[, future_mask] <- mhat_shocked[, future_mask] * shock_factor
  } else {
    # Mortality shock: higher mortality → people die sooner → lower liabilities
    mhat_shocked[, future_mask] <- mhat_shocked[, future_mask] * shock_factor
  }
  return(mhat_shocked)
}

# Create shocked matrices
mhat_long_shock <- apply_mortality_shock(mhat_forecast_full, shock_factor = 0.8, shock_type = "longevity")
mhat_mort_shock <- apply_mortality_shock(mhat_forecast_full, shock_factor = 1.15, shock_type = "mortality")

# 2. Interest Rate Sensitivity: Parallel Shift ±50 bps
shift_ir_curve <- function(rfr, shift_bps) {
  rfr_shifted <- rfr
  rfr_shifted$i <- pmax(rfr_shifted$i + shift_bps / 10000, 0.0001) # floor at 0.01% to avoid numerical issues
  return(rfr_shifted)
}
rfr_plus50  <- shift_ir_curve(rfr, 50)
rfr_minus50 <- shift_ir_curve(rfr, -50)

# 3. Run Portfolio Valuations under All Stress Scenarios
cat("Running stress scenario valuations...\n")
stress_results <- data.frame(
  Portfolio = c("Green", "Silver", "Gold"),
  Base_Liab = base_results_df$Base_Liability_EUR,
  Longevity_Shock_Liab = sapply(c("Green","Silver","Gold"), function(p)
    value_portfolio(mhat_long_shock, rfr, portfolio_exposures, p)),
  Mortality_Shock_Liab = sapply(c("Green","Silver","Gold"), function(p)
    value_portfolio(mhat_mort_shock, rfr, portfolio_exposures, p)),
  IR_Plus50_Liab = sapply(c("Green","Silver","Gold"), function(p)
    value_portfolio(mhat_forecast_full, rfr_plus50, portfolio_exposures, p)),
  IR_Minus50_Liab = sapply(c("Green","Silver","Gold"), function(p)
    value_portfolio(mhat_forecast_full, rfr_minus50, portfolio_exposures, p))
) %>%
  pivot_longer(cols = -Portfolio, names_to = "Scenario", values_to = "Liability_EUR")

# 4. Merge with Stochastic Distribution Percentiles
stoch_percentiles <- stoch_df %>%
  group_by(Portfolio) %>%
  summarise(
    P5 = quantile(Liability_EUR, 0.05),
    P25 = quantile(Liability_EUR, 0.25),
    Median = quantile(Liability_EUR, 0.50),
    P75 = quantile(Liability_EUR, 0.75),
    P95 = quantile(Liability_EUR, 0.95),
    .groups = "drop"
  )

comparison_df <- stress_results %>%
  left_join(stoch_percentiles, by = "Portfolio") %>%
  mutate(
    Scenario_Label = case_when(
      Scenario == "Base_Liab" ~ "Base (Deterministic)",
      Scenario == "Longevity_Shock_Liab" ~ "Longevity Shock (×0.8)",
      Scenario == "Mortality_Shock_Liab" ~ "Mortality Shock (×1.15)",
      Scenario == "IR_Plus50_Liab" ~ "IR +50 bps",
      Scenario == "IR_Minus50_Liab" ~ "IR -50 bps",
      TRUE ~ Scenario
    ),
    Scenario_Label = factor(Scenario_Label, 
                            levels = c("Base (Deterministic)", "Longevity Shock (×0.8)", "Mortality Shock (×1.15)",
                                       "IR +50 bps", "IR -50 bps"))
  )

# 5. CRO-Preferred Visualization: Stressed Points vs Stochastic Distribution
p_stress_vs_stoch <- ggplot() +
  # Background: Stochastic distribution (violin + boxplot)
  geom_violin(data = stoch_df, aes(x = Portfolio, y = Liability_EUR / 1e6, fill = Portfolio),
              alpha = 0.25, trim = FALSE, scale = "width") +
  geom_boxplot(data = stoch_df, aes(x = Portfolio, y = Liability_EUR / 1e6),
               width = 0.15, outlier.shape = NA, alpha = 0.6) +
  # Foreground: Deterministic stress & base values
  geom_point(data = comparison_df, 
             aes(x = Portfolio, y = Liability_EUR / 1e6, color = Scenario_Label, shape = Scenario_Label),
             size = 3.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Base (Deterministic)" = "#000000",
                                "Longevity Shock (×0.8)" = "#2E7D32",
                                "Mortality Shock (×1.15)" = "#C62828",
                                "IR +50 bps" = "#1565C0",
                                "IR -50 bps" = "#E65100")) +
  scale_shape_manual(values = c("Base (Deterministic)" = 16,
                                "Longevity Shock (×0.8)" = 17,
                                "Mortality Shock (×1.15)" = 15,
                                "IR +50 bps" = 18,
                                "IR -50 bps" = 19)) +
  labs(title = "Phase 5 Part 1: Stress Scenarios vs Stochastic Portfolio Liabilities",
       subtitle = "Deterministic stress points overlaid on 100-scenario distributions (Millions EUR)",
       x = "Portfolio", y = "Liability (Millions EUR)", 
       color = "Scenario", shape = "Scenario", fill = "Portfolio") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank())

print(p_stress_vs_stoch)
ggsave("Phase5_Part1_Stress_vs_Stochastic.png", p_stress_vs_stoch, width = 10, height = 6, dpi = 300, bg = "white")

# 6. Executive Summary Table
cat("\n=== STRESS SCENARIO IMPACT SUMMARY (Millions EUR) ===\n")
summary_table <- comparison_df %>%
  select(Portfolio, Scenario_Label, Liability_EUR) %>%
  pivot_wider(names_from = Scenario_Label, values_from = Liability_EUR) %>%
  mutate(across(where(is.numeric), ~round(.x / 1e6, 3)))

print(summary_table, row.names = FALSE)

# Calculate shock impact percentages vs Base
impact_pct <- summary_table %>%
  mutate(
    `Longevity Shock Δ%` = round((`Longevity Shock (×0.8)` - `Base (Deterministic)`) / `Base (Deterministic)` * 100, 2),
    `Mortality Shock Δ%` = round((`Mortality Shock (×1.15)` - `Base (Deterministic)`) / `Base (Deterministic)` * 100, 2),
    `IR +50bps Δ%` = round((`IR +50 bps` - `Base (Deterministic)`) / `Base (Deterministic)` * 100, 2),
    `IR -50bps Δ%` = round((`IR -50 bps` - `Base (Deterministic)`) / `Base (Deterministic)` * 100, 2)
  ) %>%
  select(Portfolio, `Longevity Shock Δ%`, `Mortality Shock Δ%`, `IR +50bps Δ%`, `IR -50bps Δ%`)

cat("\n=== RELATIVE IMPACT OF STRESSES VS BASE (%) ===\n")
print(impact_pct, row.names = FALSE)

# Save for Phase 5 synthesis
write_csv(impact_pct, "Phase5_Part1_Stress_Impact_Pct.csv")
