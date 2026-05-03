# ============================================================
# Phase 5 - Part 2: Cross-Portfolio Risk Comparison & Diversification
# ============================================================
library(tidyverse)
library(ggplot2)
library(corrplot)

# Ensure stoch_df is in environment (from Phase 4 Part 3)
# Expected columns: Scenario, Portfolio, Liability_EUR

# 1. Calculate Advanced Risk Metrics per Portfolio
cat("Calculating advanced risk metrics...\n")
risk_metrics_detailed <- stoch_df %>%
  group_by(Portfolio) %>%
  summarise(
    Mean_Liab       = mean(Liability_EUR),
    SD_Liab         = sd(Liability_EUR),
    CV              = SD_Liab / Mean_Liab,
    VaR_95          = quantile(Liability_EUR, 0.95),
    VaR_99          = quantile(Liability_EUR, 0.99),
    # Expected Shortfall / CVaR (tail expectation beyond VaR)
    CVaR_95         = mean(Liability_EUR[Liability_EUR >= VaR_95]),
    CVaR_99         = mean(Liability_EUR[Liability_EUR >= VaR_99]),
    Skewness        = sum(((Liability_EUR - mean(Liability_EUR)) / SD_Liab)^3) / n(),
    Excess_Kurtosis = sum(((Liability_EUR - mean(Liability_EUR)) / SD_Liab)^4) / n() - 3,
    .groups         = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

# 2. Pivot to Wide Format for Correlation & Aggregation
stoch_wide <- stoch_df %>%
  pivot_wider(names_from = Portfolio, values_from = Liability_EUR, names_prefix = "Liab_")

# 3. Correlation & Dependence Analysis
corr_matrix <- cor(select(stoch_wide, starts_with("Liab_")), method = "pearson")
rownames(corr_matrix) <- c("Green", "Silver", "Gold")
colnames(corr_matrix) <- c("Green", "Silver", "Gold")

print("Recalculated Correlation Matrix:")
print(corr_matrix)

# 4. Portfolio Aggregation & Diversification Benefit
cat("Calculating diversification benefits...\n")
stoch_wide <- stoch_wide %>%
  mutate(Combined = Liab_Green + Liab_Silver + Liab_Gold)

combined_metrics <- stoch_wide %>%
  summarise(
    Mean_Combined      = mean(Combined),
    SD_Combined        = sd(Combined),
    CV_Combined        = SD_Combined / Mean_Combined,
    VaR_95_Combined    = quantile(Combined, 0.95),
    VaR_99_Combined    = quantile(Combined, 0.99),
    CVaR_95_Combined   = mean(Combined[Combined >= VaR_95_Combined]),
    .groups            = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

# Diversification Ratio (Sum of individual 95% VaR / Combined 95% VaR)
div_ratio_95 <- sum(risk_metrics_detailed$VaR_95) / combined_metrics$VaR_95_Combined
div_ratio_99 <- sum(risk_metrics_detailed$VaR_99) / combined_metrics$VaR_99_Combined

cat(sprintf("\nDiversification Ratio (95%% VaR): %.3f\n", div_ratio_95))
cat(sprintf("Diversification Ratio (99%% VaR): %.3f\n", div_ratio_99))

# 5. CRO-Preferred Visualizations

# Plot 1: Risk-Liability Scatter (Mean vs CV)
p_risk_scatter <- ggplot(risk_metrics_detailed, aes(x = Mean_Liab / 1e6, y = CV * 100, 
                                                    label = Portfolio, color = Portfolio)) +
  geom_point(size = 4) +
  geom_text(vjust = -1.2, size = 4, fontface = "bold") +
  labs(title = "Risk-Liability Tradeoff by Portfolio",
       subtitle = "Mean Liability (M€) vs Coefficient of Variation (%)",
       x = "Mean Liability (Millions EUR)",
       y = "Coefficient of Variation (%)",
       color = "Portfolio") +
  scale_color_manual(values = c("Green" = "#4CAF50", "Silver" = "#9E9E9E", "Gold" = "#FFC107")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 13))

# Plot 2: Correlation Heatmap
p_corr <- ggplot(corr_data, aes(x = Portfolio_1, y = Portfolio_2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.3f", Correlation)), size = 5, fontface = "bold") +
  # Adjusted scale: Pension liabilities usually correlate >0.9, so we zoom in to see differences
  scale_fill_gradient2(low = "#2E7D32", mid = "#FFFFFF", high = "#C62828", 
                       midpoint = 0.95, limits = c(0.90, 1.00), name = "Correlation") +
  labs(title = "Liability Correlation Across Portfolios",
       subtitle = "Pearson correlation of 100 stochastic scenarios",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13),
        panel.grid = element_blank())

# Plot 3: Diversification Benefit Bar Chart
div_data <- data.frame(
  Metric = c("Green VaR95", "Silver VaR95", "Gold VaR95", "Combined VaR95"),
  Value_M = c(risk_metrics_detailed$VaR_95 / 1e6, combined_metrics$VaR_95_Combined / 1e6),
  Group = c(rep("Individual", 3), "Aggregated")
) %>%
  mutate(Metric = factor(Metric, levels = Metric))

p_div <- ggplot(div_data, aes(x = Group, y = Value_M, fill = Metric)) +
  geom_col(position = "dodge", width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf("€%.2fM", Value_M)), 
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5) +
  labs(title = "Diversification Benefit: Individual vs Combined Portfolio",
       subtitle = "95% Value-at-Risk (Millions EUR)",
       x = "Portfolio Structure", y = "95% VaR (M€)", fill = "Component") +
  scale_fill_manual(values = c("Green VaR95" = "#4CAF50", "Silver VaR95" = "#9E9E9E", 
                               "Gold VaR95" = "#FFC107", "Combined VaR95" = "#1976D2")) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank())

# 6. Display & Save Visualizations
print(p_risk_scatter)
print(p_corr)
print(p_div)

ggsave("Phase5_Part2_RiskScatter.png", p_risk_scatter, width = 7, height = 5, dpi = 300)
ggsave("Phase5_Part2_Correlation.png", p_corr, width = 6, height = 5, dpi = 300)
ggsave("Phase5_Part2_Diversification.png", p_div, width = 8, height = 5, dpi = 300)

# 7. Export Risk & Diversification Tables
write_csv(risk_metrics_detailed, "Phase5_Part2_Portfolio_Risk_Metrics.csv")
write_csv(combined_metrics, "Phase5_Part2_Combined_Portfolio_Metrics.csv")

# 8. Executive Summary Output
cat("\n=== CROSS-PORTFOLIO RISK SUMMARY ===\n")
print(risk_metrics_detailed, row.names = FALSE)

cat("\n=== DIVERSIFICATION INSIGHTS ===\n")
cat(sprintf("Sum of individual 95%% VaR: €%.2fM\n", sum(risk_metrics_detailed$VaR_95) / 1e6))
cat(sprintf("Combined portfolio 95%% VaR: €%.2fM\n", combined_metrics$VaR_95_Combined / 1e6))
cat(sprintf("Capital relief from diversification: €%.2fM (%.1f%%)\n", 
            (sum(risk_metrics_detailed$VaR_95) - combined_metrics$VaR_95_Combined) / 1e6,
            (1 - 1/div_ratio_95) * 100))

# Risk concentration note
max_cv_port <- risk_metrics_detailed$Portfolio[which.max(risk_metrics_detailed$CV)]
cat(sprintf("\nHighest relative uncertainty: %s portfolio (CV = %.2f%%)\n", 
            max_cv_port, max(risk_metrics_detailed$CV) * 100))