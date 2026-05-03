# ============================================================
# FINAL REPORT VERIFICATION BLOCK
# Run this after completing Phases 1-5
# ============================================================
library(tidyverse)

cat("\n============================================================\n")
cat(" FINAL REPORT VERIFICATION CHECKLIST\n")
cat("============================================================\n")

# 1. DATA INTEGRITY & BOUNDS
cat("\n 1. DATA INTEGRITY & BOUNDS\n")
cat("• mhat_clean dimensions:", dim(mhat_clean), "(expected: 45 × 111)\n")
cat("• mhat_forecast_full dimensions:", dim(mhat_forecast_full), "(expected: 111 × 95)\n")
cat("• Any NA in forecast matrix?", any(is.na(mhat_forecast_full)), "\n")
cat("• All mortality rates ∈ [0,1]?", all(mhat_forecast_full >= 0 & mhat_forecast_full <= 1), "\n")
cat("• RFR curve length:", nrow(rfr), "(expected: 151)\n")
cat("• All RFR rates > 0?", all(rfr$i > 0), "\n")

# 2. LEE-CARTER PARAMETERS (Appendix Table)
cat("\n 2. LEE-CARTER PARAMETERS (Appendix)\n")
ax <- as.numeric(LC_fit_sub$ax)
bx <- as.numeric(LC_fit_sub$bx)
kt_obs <- as.numeric(LC_fit_sub$kt)
drift <- mean(diff(kt_obs))
sigma_kt <- sd(diff(kt_obs))

cat(sprintf("• αₓ range: [%.2f, %.2f] | mean: %.2f\n", min(ax), max(ax), mean(ax)))
cat(sprintf("• βₓ range: [%.3f, %.3f] | mean: %.3f\n", min(bx), max(bx), mean(bx)))
cat(sprintf("• κₜ drift: %.4f | residual σ: %.4f\n", drift, sigma_kt))

# 3. BASE LIABILITIES (Executive Summary Table)
cat("\n 3. BASE LIABILITIES (Executive Summary)\n")
print(base_results_df, row.names = FALSE)

# 4. STOCHASTIC METRICS (Risk Tables)
cat("\n 4. STOCHASTIC RISK METRICS\n")
print(risk_metrics_detailed %>% 
        select(Portfolio, Mean_Liab, SD_Liab, VaR_95, VaR_99, CV) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))), 
      row.names = FALSE)

# 5. STRESS IMPACTS (Section 2 & 5 Tables)
cat("\n 5. STRESS SCENARIO IMPACTS (%) vs BASE\n")
print(impact_pct, row.names = FALSE)

# 6. DIVERSIFICATION & CORRELATION (Section 3)
cat("\n 6. DIVERSIFICATION & CORRELATION\n")
print(round(corr_matrix, 3))

cat(sprintf("• 95%% VaR Diversification Ratio: %.3f\n", div_ratio_95))
cat(sprintf("• Capital Relief: €%.3fM (%.1f%%)\n", 
            (sum(risk_metrics_detailed$VaR_95) - combined_metrics$VaR_95_Combined) / 1e6,
            (1 - 1/div_ratio_95) * 100))

# 7. LOGICAL CONSISTENCY CHECKS
cat("\n7. LOGICAL CONSISTENCY\n")
cat("• Stochastic mean ≈ Base? ", 
    all(abs((risk_metrics_detailed$Mean_Liab - base_results_df$Base_Liability_EUR) / 
              base_results_df$Base_Liability_EUR) < 0.05), "\n")
cat("• VaR95 > Mean for all portfolios? ", 
    all(risk_metrics_detailed$VaR_95 > risk_metrics_detailed$Mean_Liab), "\n")
cat("• CV > 0 for all portfolios? ", 
    all(risk_metrics_detailed$CV > 0), "\n")
cat("• Longevity shock increases liabilities? ", 
    all(impact_pct$`Longevity Shock Δ%` > 0), "\n")
cat("• Mortality shock decreases liabilities? ", 
    all(impact_pct$`Mortality Shock Δ%` < 0), "\n")

# 8. FIGURE FILE CHECK
cat("\n️ 8. REQUIRED FIGURES IN WORKING DIRECTORY\n")
required_figs <- c(
  "Phase1_Historical_Mortality_Heatmap.png",
  "Phase1_LC_Kt_Series.png",
  "Phase2_Stress_Impact_BarChart.png",
  "Phase4_Part4_Violin_Boxplot.png",
  "Phase4_Part4_Density_Overlay.png",
  "Phase5_Part1_Stress_vs_Stochastic.png",
  "Phase5_Part2_RiskScatter.png",
  "Phase5_Part2_Correlation.png",
  "Phase5_Part2_Diversification.png"
)
missing_figs <- required_figs[!file.exists(required_figs)]
if(length(missing_figs) == 0) {
  cat("All 9 required figures found.\n")
} else {
  cat("Missing:", paste(missing_figs, collapse = ", "), "\n")
}

cat("\n============================================================\n")
cat(" COPY-PASTE READY TABLE VALUES FOR LATEX\n")
cat("============================================================\n")
cat("Base Liabilities (M€):", paste(round(base_results_df$Base_Liability_Millions, 3), collapse = ", "), "\n")
cat("VaR95 (M€):", paste(round(risk_metrics_detailed$VaR_95 / 1e6, 3), collapse = ", "), "\n")
cat("CV (%):", paste(round(risk_metrics_detailed$CV * 100, 1), collapse = ", "), "\n")
cat("Stress Δ% Longevity:", paste(impact_pct$`Longevity Shock Δ%`, collapse = ", "), "\n")
cat("Stress Δ% Mortality:", paste(impact_pct$`Mortality Shock Δ%`, collapse = ", "), "\n")
cat("Correlations:", paste(c(corr_matrix[1,2], corr_matrix[1,3], corr_matrix[2,3]), collapse = ", "), "\n")
cat("============================================================\n")


# κₜ Drift Value Seems Extremely Large problem
# Use vectors already in your environment from Phase 1 Part 2
# 1. Rebuild named vectors directly from the StMoMo fit object
fit_ages <- rownames(LC_fit_sub$Dxt)  # Guaranteed to be "0","1",...,"79"
ax_val <- setNames(as.numeric(LC_fit_sub$ax), fit_ages)
bx_val <- setNames(as.numeric(LC_fit_sub$bx), fit_ages)

cat("Reconstructed ax/bx with", length(ax_val), "ages.\n")

# 2. Prepare series for forecasting
year_series <- c(years_obs, future_years)
kt_series   <- c(kt_obs, kt_forecast_manual)

test_ages  <- c(30, 65)
test_years <- c(2026, 2050, 2074)

cat("\nForecast mortality rate checks:\n")
for (age in test_ages) {
  for (yr in test_years) {
    idx <- which(year_series == yr)
    if(length(idx) > 0) {
      kt <- kt_series[idx]
      a  <- ax_val[as.character(age)]
      b  <- bx_val[as.character(age)]
      
      if(!is.na(a) && !is.na(b)) {
        mx <- exp(a + b * kt)
        cat(sprintf("  Age %2d, Year %4d: mx = %.6f [valid: %s]\n", 
                    age, yr, mx, 
                    ifelse(mx >= 0 & mx <= 1, "✓", "✗")))
      } else {
        cat(sprintf("  Age %2d, Year %4d: Parameters still missing!\n", age, yr))
      }
    }
  }
}

cat("Age 90 mortality rates from forecast matrix:\n")
cat("2026:", mhat_forecast_full["90", "2026"], "\n")
cat("2050:", mhat_forecast_full["90", "2050"], "\n")
cat("2074:", mhat_forecast_full["90", "2074"], "\n")

# Interest Rate Sensitivity for -50bps Seems Too Small Problem
# ============================================================
# CORRECTED RFR LOADING (Column-Index Method)
# ============================================================
library(readxl)

# 1. Read raw data by skipping metadata, ignoring headers
rfr_raw <- read_excel("RFR_spot_with_VA.xlsx", sheet = 1, skip = 10, col_names = FALSE)

# 2. Extract Maturity (Column 1) and Euro Area Rates (Column 2)
rfr <- data.frame(
  T = as.integer(rfr_raw[[1]]),
  i = as.numeric(rfr_raw[[2]])
)

# 3. Handle percentage format safely
if (max(rfr$i, na.rm = TRUE) > 1) {
  rfr$i <- rfr$i / 100  # Convert 2.787 -> 0.02787
} else if (max(rfr$i, na.rm = TRUE) < 0.005) {
  rfr$i <- rfr$i * 100  # Fix double-division edge case
}

# 4. Add T=0 (convention: spot rate at T=0 equals T=1)
rfr <- rbind(data.frame(T = 0, i = rfr$i[1]), rfr)

# 5. Verify
cat("=== VERIFIED RFR RATES ===\n")
print(head(rfr, 10))
cat("Max rate:", max(rfr$i), "| Min rate:", min(rfr$i), "\n")

# ============================================================
# Re-Test Interest Rate Sensitivity with Corrected RFR
# ============================================================

# 1. Create the shifted curve (-50 bps)
rfr_test <- rfr
rfr_test$i <- pmax(rfr$i - 0.0050, 0.0001)  # Floor at 0.01%

# 2. Calculate Base EPV (Age 65, BirthYear 1961, €10,000 payment)
test_base <- LA_cohort(mhat_forecast_full, 
                       Age = 65, 
                       BirthYear = 1961, 
                       amount = 10000, 
                       rfr = rfr)

# 3. Calculate Shifted EPV
test_minus50 <- LA_cohort(mhat_forecast_full, 
                          Age = 65, 
                          BirthYear = 1961, 
                          amount = 10000, 
                          rfr = rfr_test)

# 4. Calculate percentage change
pct_change <- (test_minus50 - test_base) / test_base * 100

cat("=== INTEREST RATE SENSITIVITY RESULTS ===\n")
cat(sprintf("Base EPV: €%.2f\n", test_base))
cat(sprintf("Shifted EPV (-50bps): €%.2f\n", test_minus50))
cat(sprintf("Impact: %+.2f%%\n", pct_change))

# 5. Quick duration approximation check
cat("\n=== DURATION CHECK ===\n")
cat("Theoretical sensitivity ≈ -Duration × Δi\n")
cat("If Duration ≈ 12-15 years, expected impact: +0.6% to +0.75%\n")
cat("Your result: ", sprintf("%+.2f%%\n", pct_change))